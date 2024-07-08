library(ncdf4)
library(data.table)
library(zoo)
library(lubridate)
library(ggplot2)
library(RcppRoll)

DroughtComponent <- setRefClass(
  "DroughtComponent",
  fields = list(
    precipitation_file = "character",
    mask_file = "character",
    reference_period = "character",
    precip_nc = "ANY",
    mask_nc = "ANY",
    precip_dt = "data.table",
    mask_dt = "data.table",
    precip_masked_dt = "data.table",
    max_dry_days_dt = "data.table",
    standardized_dry_days_dt = "data.table"
  ),
  methods = list(
    initialize = function(precipitation_file, mask_file, reference_period = c("1961-01-01", "1990-12-31")) {
      .self$precipitation_file <- precipitation_file
      .self$mask_file <- mask_file
      .self$reference_period <- reference_period
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
      time_origin <- sub("days since ", "", time_units)
      time_origin <- as.Date(time_origin, tz = "UTC")
      
      # Convertir le temps en dates
      time_dates <- time_origin + time
      
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
    
    max_consecutive_dry_days = function() {
      .self$precip_masked_dt[, dry_day := ifelse(tp < 0.001, 1, 0)]
      .self$precip_masked_dt[, dry_spell := rleid(dry_day), by = .(longitude, latitude)]
      .self$precip_masked_dt[, dry_spell_length := .N, by = .(longitude, latitude, dry_spell)]
      .self$precip_masked_dt[dry_day == 0, dry_spell_length := 0]
      
      daily_max_dry_days <- .self$precip_masked_dt[, .(max_dry_days = max(dry_spell_length, na.rm = TRUE)), by = .(longitude, latitude, time_day = as.Date(time))]
      
      .self$max_dry_days_dt <- daily_max_dry_days
    },
    
    standardize_metric = function(reference_period = NULL) {
      if (!is.null(reference_period)) {
        .self$reference_period <- reference_period
      }
      
      # Resampler mensuellement et calculer le maximum mensuel
      monthly_max_dry_days <- .self$max_dry_days_dt[, .(max_dry_days = max(max_dry_days, na.rm = TRUE)), by = .(longitude, latitude, year(time_day), month(time_day))]
      
      # Ajouter une colonne de date
      monthly_max_dry_days[, date := as.Date(paste0(year, "-", month, "-01"))]
      
      # Calculer les statistiques de référence par mois
      reference_data <- monthly_max_dry_days[year(date) >= year(as.Date(.self$reference_period[1])) & year(date) <= year(as.Date(.self$reference_period[2]))]
      reference_stats <- reference_data[, .(mean = mean(max_dry_days, na.rm = TRUE), sd = sd(max_dry_days, na.rm = TRUE)), by = .(longitude, latitude, month(date))]
      
      # Fusionner les statistiques de référence avec les données mensuelles
      monthly_max_dry_days <- merge(monthly_max_dry_days, reference_stats, by = c("longitude", "latitude", "month"), all.x = TRUE)
      
      # Standardiser les jours secs
      monthly_max_dry_days[, standardized_dry_days := (max_dry_days - mean) / sd]
      
      .self$standardized_dry_days_dt <- monthly_max_dry_days
    },
    
    visualize = function(selected_latitude, selected_longitude) {
      location_data <- .self$standardized_dry_days_dt[latitude == selected_latitude & longitude == selected_longitude]
      
      ggplot(location_data, aes(x = date, y = standardized_dry_days)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        labs(title = paste("Standardized Dry Days for Latitude", selected_latitude, "and Longitude", selected_longitude),
             x = "Time", y = "Standardized Dry Days") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  )
)
