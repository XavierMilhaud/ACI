library(httr)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

setwd("../R")
# Charger le fichier getSeaLevelData.R
source("getSeaLevelData.R")

# SeaLevelComponent.R
SeaLevelComponent <- setRefClass(
  "SeaLevelComponent",
  fields = list(
    country_abrev = "character",
    study_period = "character",
    reference_period = "character",
    directory = "character"
  ),
  methods = list(
    initialize = function(country_abrev, study_period, reference_period) {
      .self$country_abrev <- country_abrev
      .self$study_period <- study_period
      .self$reference_period <- reference_period
      .self$directory <- paste0("../data/sealevel_data_", country_abrev)
      copy_and_rename_files_by_country(country_abrev)
    },
    
    load_data = function() {
      dataframes <- list()
      for (filename in list.files(.self$directory, pattern = "\\.txt$")) {
        file_path <- file.path(.self$directory, filename)
        temp_data <- fread(file_path, sep = ";", select = 1:2, col.names = c("Date", paste0("Measurement_", sub("\\.txt$", "", filename))))
        temp_data$Date <- as.numeric(temp_data$Date)
        setkey(temp_data, Date)
        dataframes[[length(dataframes) + 1]] <- temp_data
      }
      combined_data <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), dataframes)
      return(combined_data)
    },
    
    correct_date_format = function(data) {
      month_mapping <- list(
        "0417" = "01", "125" = "02", "2083" = "03", "2917" = "04", "375" = "05",
        "4583" = "06", "5417" = "07", "625" = "08", "7083" = "09", "7917" = "10",
        "875" = "11", "9583" = "12"
      )
      
      convert_date <- function(date) {
        date_str <- as.character(date)
        tryCatch({
          year <- as.integer(floor(as.numeric(date_str)))
          month_fraction <- substr(strsplit(date_str, ".", fixed = TRUE)[[1]][2], 1, 4)
          month <- month_mapping[[month_fraction]]
          if (!is.null(month)) {
            return(as.Date(paste0(year, "-", month, "-01")))
          } else {
            return(NA)
          }
        }, error = function(e) {
          return(NA)
        })
      }
      
      corrected_dates <- sapply(data$Date, convert_date)
      data$Corrected_Date <- corrected_dates
      data <- data[!is.na(data$Corrected_Date), ]
      data$Corrected_Date <- as.Date(data$Corrected_Date, origin = "1970-01-01")
      setkey(data, Corrected_Date)
      return(data)
    },
    
    clean_data = function(data) {
      for (col in names(data)) {
        set(data, j = col, value = fifelse(data[[col]] == -99999, NA, data[[col]]))
      }
      return(data)
    },
    
    compute_monthly_stats = function(data, reference_period, stats) {
      reference_data <- data[Corrected_Date >= as.Date(reference_period[1]) & Corrected_Date < as.Date(reference_period[2])]
      measurement_cols <- grep("Measurement_", names(reference_data), value = TRUE)
      monthly_data <- reference_data[, lapply(.SD, function(x) if (stats == "means") mean(x, na.rm = TRUE) else sd(x, na.rm = TRUE)), 
                                     by = .(month = month(Corrected_Date)), .SDcols = measurement_cols]
      
      if (stats == "means") {
        setnames(monthly_data, old = names(monthly_data)[-1], new = paste0("mean_", names(monthly_data)[-1]))
      } else if (stats == "std") {
        setnames(monthly_data, old = names(monthly_data)[-1], new = paste0("std_", names(monthly_data)[-1]))
      }
      
      return(monthly_data)
    },
    
    standardize_data = function(data, monthly_means, monthly_std_devs, study_period) {
      study_data <- data[Corrected_Date >= as.Date(study_period[1]) & Corrected_Date < as.Date(study_period[2])]
      study_data[, month := month(Corrected_Date)]
      
      # Fusionner les moyennes mensuelles et les écarts-types avec les données d'étude
      study_data <- merge(study_data, monthly_means, by = "month", all.x = TRUE)
      study_data <- merge(study_data, monthly_std_devs, by = "month", all.x = TRUE)
      
      # Calculer les scores z pour chaque colonne de mesure
      measurement_cols <- grep("Measurement_", names(study_data), value = TRUE)
      for (col in measurement_cols) {
        mean_col <- paste0("mean_", col)
        std_col <- paste0("std_", col)
        if (mean_col %in% names(study_data) && std_col %in% names(study_data)) {
          study_data[, paste0(col, "_zscore") := (get(col) - get(mean_col)) / get(std_col)]
        }
      }
      
      # Filtrer pour conserver seulement les colonnes z-score et Corrected_Date
      zscore_cols <- grep("_zscore$", names(study_data), value = TRUE)
      standardized_data <- study_data[, c("Corrected_Date", zscore_cols), with = FALSE]
      
      return(standardized_data)
    },
    
    process = function() {
      sea_level_data <- .self$load_data()
      sea_level_data <- .self$correct_date_format(sea_level_data)
      sea_level_data <- .self$clean_data(sea_level_data)
      monthly_means <- .self$compute_monthly_stats(sea_level_data, .self$reference_period, "means")
      monthly_std_devs <- .self$compute_monthly_stats(sea_level_data, .self$reference_period, "std")
      standardized_data <- .self$standardize_data(sea_level_data, monthly_means, monthly_std_devs, .self$study_period)
      return(standardized_data)
    },
    
    plot_rolling_mean = function(data, window = 60) {
      
      data$mean_value <- rowMeans(standardized_data[, -1, with = FALSE], na.rm = TRUE)
      ggplot(data, aes(x = Corrected_Date, y = mean_value)) +
        geom_line() +
        labs(title = "Mean Value of Sea Level Data", x = "Date", y = "Mean Value (Standardized)") +
        theme_minimal()
      
    },
    
    convert_to_xarray = function(data) {
      library(reticulate)
      np <- import("numpy")
      xr <- import("xarray")
      data_array <- as.data.table(data)
      return(xr$DataArray(np$array(data_array)))
    },
    
    resample_data = function(data) {
      data[, Corrected_Date := as.Date(Corrected_Date)]
      resampled_data <- data[, lapply(.SD, mean, na.rm = TRUE), by = .(Corrected_Date = floor_date(Corrected_Date, "3 months"))]
      return(resampled_data)
    },
    
    save_to_netcdf = function(data, filename) {
      xr <- .self$convert_to_xarray(data)
      xr$to_netcdf(filename)
    }
  )
)
