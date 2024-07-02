library(ncdf4)
library(data.table)
library(zoo)
library(lubridate)
library(ggplot2)
library(RcppRoll)

PrecipitationComponent <- setRefClass(
  "PrecipitationComponent",
  fields = list(
    precipitation_file = "character",
    mask_file = "character",
    reference_period = "character",
    rolling_window = "numeric",
    precip_nc = "ANY",
    mask_nc = "ANY",
    precip_dt = "data.table",
    mask_dt = "data.table",
    precip_masked_dt = "data.table",
    monthly_max = "data.table"
  ),
  methods = list(
    initialize = function(precipitation_file, mask_file, reference_period = c("1961-01-01", "1990-12-31"), rolling_window = 5) {
      .self$precipitation_file <- precipitation_file
      .self$mask_file <- mask_file
      .self$reference_period <- reference_period
      .self$rolling_window <- rolling_window
      .self$load_data()
      .self$apply_mask()
    },
    
    load_data = function() {
      .self$precip_nc <- nc_open(.self$precipitation_file)
      .self$mask_nc <- nc_open(.self$mask_file)
      
      # Extraire les variables nécessaires
      tp <- ncvar_get(.self$precip_nc, "tp")
      longitude <- ncvar_get(.self$precip_nc, "longitude")
      latitude <- ncvar_get(.self$precip_nc, "latitude")
      time <- ncvar_get(.self$precip_nc, "time")
      
      # Vérifier l'origine du temps
      time_units <- ncatt_get(.self$precip_nc, "time", "units")$value
      time_origin <- sub("hours since ", "", time_units)
      time_origin <- as.POSIXct(time_origin, tz = "UTC")
      
      # Convertir le temps en dates
      time_dates <- time_origin + hours(time)
      
      mask_data <- ncvar_get(.self$mask_nc, "country")
      longitude_mask <- ncvar_get(.self$mask_nc, "lon")
      latitude_mask <- ncvar_get(.self$mask_nc, "lat")
      
      nc_close(.self$precip_nc)
      nc_close(.self$mask_nc)
      
      # Création d'un data.table pour la précipitation
      .self$precip_dt <- data.table(
        expand.grid(
          longitude = longitude,
          latitude = latitude,
          time = time_dates
        ),
        tp = as.vector(tp)
      )
      
      # Création d'un data.table pour le masque
      .self$mask_dt <- data.table(
        expand.grid(
          longitude = longitude_mask,
          latitude = latitude_mask
        ),
        country = as.vector(mask_data)
      )
    },
    
    apply_mask = function() {
      # Fusionner les tables de précipitation et de masque
      setkey(.self$precip_dt, longitude, latitude)
      setkey(.self$mask_dt, longitude, latitude)
      
      .self$precip_masked_dt <- merge(.self$precip_dt, .self$mask_dt, by = c("longitude", "latitude"), all.x = TRUE)
      
      # Application du masque avec un seuil de 0.8
      .self$precip_masked_dt[, tp := ifelse(country >= 0.8, tp, NA)]
    },
    
    calculate_monthly_max = function() {
      # Calculer la somme glissante sur la fenêtre définie par l'utilisateur
      .self$precip_masked_dt <- .self$precip_masked_dt[order(time)]
      
      # Utilisation de RcppRoll::roll_sum pour améliorer les performances
      .self$precip_masked_dt[, tp_rolling := roll_sum(tp, n = .self$rolling_window, fill = NA, align = "right"), by = .(longitude, latitude)]
      
      # Ajouter des colonnes pour l'année et le mois
      .self$precip_masked_dt[, year := year(time)]
      .self$precip_masked_dt[, month := month(time)]
      
      # Calculer le maximum mensuel
      .self$monthly_max <- .self$precip_masked_dt[, .(monthly_max = max(tp_rolling, na.rm = TRUE)), by = .(longitude, latitude, year, month)]
      
      # Calculer les statistiques de référence pour la période définie par l'utilisateur
      reference_period <- .self$monthly_max[year >= year(as.Date(.self$reference_period[1])) & year <= year(as.Date(.self$reference_period[2]))]
      reference_stats <- reference_period[, .(mean = mean(monthly_max, na.rm = TRUE), sd = sd(monthly_max, na.rm = TRUE)), by = month]
      
      # Fusionner les statistiques de référence avec les données mensuelles
      .self$monthly_max <- merge(.self$monthly_max, reference_stats, by = "month", all.x = TRUE)
      .self$monthly_max[, rx5day := (monthly_max - mean) / sd]
    },
    
    visualize = function(selected_latitude, selected_longitude) {
      # Filtrer les données pour la latitude et longitude sélectionnées
      location_data <- .self$monthly_max[latitude == selected_latitude & longitude == selected_longitude]
      
      # Créer une colonne 'date' à partir de l'année et du mois
      location_data[, date := ymd(paste(year, month, "01", sep = "-"))]
      
      # Visualisation de Rx5day pour la latitude et longitude sélectionnées
      ggplot(location_data, aes(x = date, y = rx5day)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Ajuster les étiquettes de l'axe x pour afficher les années
        labs(title = paste("Rx5day for Latitude", selected_latitude, "and Longitude", selected_longitude),
             x = "Time", y = "Rx5day") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    },
    
    plot_rx5day_map = function(year, month) {
      # Filtrer les données pour l'année et le mois sélectionnés
      map_data <- .self$monthly_max[year == year & month == month]
      
      # Visualisation de Rx5day sur une carte
      ggplot(map_data, aes(x = longitude, y = latitude, fill = rx5day)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
        labs(title = paste("Rx5day for", year, "-", month),
             x = "Longitude", y = "Latitude", fill = "Rx5day") +
        theme_minimal() +
        coord_fixed()
    },
    
    plot_rx5day_mean_time = function(smooth_years = 2) {
      # Calculer la moyenne de rx5day pour chaque période
      mean_rx5day <- .self$monthly_max[, .(mean_rx5day = mean(rx5day, na.rm = TRUE)), by = .(year, month)]
      
      # Créer une colonne 'date' à partir de l'année et du mois
      mean_rx5day[, date := ymd(paste(year, month, "01", sep = "-"))]
      
      # Appliquer un lissage sur la période définie
      mean_rx5day[, mean_rx5day_smooth := rollmean(mean_rx5day, k = smooth_years * 12, fill = NA, align = "right")]
      
      # Supprimer les lignes contenant des valeurs NA
      mean_rx5day <- mean_rx5day[!is.na(mean_rx5day_smooth)]
      
      # Visualisation de la moyenne de Rx5day en fonction du temps avec lissage
      ggplot(mean_rx5day, aes(x = date)) +
        geom_line(aes(y = mean_rx5day, color = "Original")) +
        geom_line(aes(y = mean_rx5day_smooth, color = "Smoothed")) +
        scale_color_manual(values = c("Original" = "blue", "Smoothed" = "red")) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Ajuster les étiquettes de l'axe x pour afficher les années
        labs(title = "Mean Rx5day over Time with Smoothing",
             x = "Time", y = "Mean Rx5day") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(legend.title = element_blank(), legend.position = "top")
    }
  )
)


