library(httr)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

# Charger le fichier getSeaLevelData.R
#setwd("../")
source("getSeaLevelData.R")

#' SeaLevelComponent Class
#'
#' A class to process and analyze sea level data, compute monthly statistics, standardize data, 
#' and visualize sea level trends over time.
#'
#' @field country_abrev Character. Abbreviation for the country being analyzed.
#' @field study_period Character vector. The start and end dates of the study period for analyzing sea level data.
#' @field reference_period Character vector. The start and end dates of the reference period for calculating anomalies.
#' @field directory Character. The directory where sea level data files are stored.

SeaLevelComponent <- setRefClass(
  "SeaLevelComponent",
  fields = list(
    country_abrev = "character",       # Abbreviation for the country being analyzed
    study_period = "character",        # The start and end dates of the study period for analyzing sea level data
    reference_period = "character",    # The start and end dates of the reference period for calculating anomalies
    directory = "character"            # The directory where sea level data files are stored
  ),
  methods = list(
    initialize = function(country_abrev, study_period, reference_period) {
      #' Initialize a SeaLevelComponent object
      #'
      #' This method initializes a new instance of the SeaLevelComponent class by setting the country abbreviation, 
      #' study period, reference period, and data directory. It also copies and renames files by country.
      #'
      #' @param country_abrev A string specifying the country abbreviation for the sea level data.
      #' @param study_period A character vector of length two specifying the start and end dates of the study period for analyzing sea level data.
      #' @param reference_period A character vector of length two specifying the start and end dates of the reference period for calculating anomalies.
      .self$country_abrev <- country_abrev
      .self$study_period <- study_period
      .self$reference_period <- reference_period
      .self$directory <- paste0("../data/sealevel_data_", country_abrev)
      copy_and_rename_files_by_country(country_abrev)
    },
    
    load_data = function() {
      #' Load sea level data from text files
      #'
      #' This method reads sea level data from text files in the specified directory and merges them into a single data table.
      #'
      #' @return A data.table containing combined sea level data from all text files.
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
      #' Correct date format in sea level data
      #'
      #' This method converts the numeric date format in the sea level data to a standard Date format.
      #'
      #' @param data A data.table containing sea level data with numeric date format.
      #' @return A data.table with corrected date format.
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
      #' Clean sea level data
      #'
      #' This method replaces missing values (denoted by -99999) with NA in the sea level data.
      #'
      #' @param data A data.table containing sea level data.
      #' @return A cleaned data.table with missing values replaced by NA.
      for (col in names(data)) {
        set(data, j = col, value = fifelse(data[[col]] == -99999, NA, data[[col]]))
      }
      return(data)
    },
    
    compute_monthly_stats = function(data, reference_period, stats) {
      #' Compute monthly statistics for sea level data
      #'
      #' This method computes monthly means or standard deviations for sea level data within a reference period.
      #'
      #' @param data A data.table containing sea level data with corrected dates.
      #' @param reference_period A character vector specifying the start and end of the reference period for calculating statistics.
      #' @param stats A string specifying the type of statistic to compute: "means" or "std".
      #' @return A data.table containing monthly means or standard deviations of sea level data.
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
      #' Standardize sea level data
      #'
      #' This method standardizes sea level data by calculating z-scores based on monthly means and standard deviations.
      #'
      #' @param data A data.table containing sea level data with corrected dates.
      #' @param monthly_means A data.table containing monthly means of sea level data.
      #' @param monthly_std_devs A data.table containing monthly standard deviations of sea level data.
      #' @param study_period A character vector specifying the start and end of the study period for analyzing sea level data.
      #' @return A data.table containing standardized sea level data with z-scores.
      study_data <- data[Corrected_Date >= as.Date(study_period[1]) & Corrected_Date < as.Date(study_period[2])]
      study_data[, month := month(Corrected_Date)]
      
      # Merge monthly means and standard deviations with study data
      study_data <- merge(study_data, monthly_means, by = "month", all.x = TRUE)
      study_data <- merge(study_data, monthly_std_devs, by = "month", all.x = TRUE)
      
      # Calculate z-scores for each measurement column
      measurement_cols <- grep("Measurement_", names(study_data), value = TRUE)
      for (col in measurement_cols) {
        mean_col <- paste0("mean_", col)
        std_col <- paste0("std_", col)
        if (mean_col %in% names(study_data) && std_col %in% names(study_data)) {
          study_data[, paste0(col, "_zscore") := (get(col) - get(mean_col)) / get(std_col)]
        }
      }
      
      # Filter to keep only z-score columns and Corrected_Date
      zscore_cols <- grep("_zscore$", names(study_data), value = TRUE)
      standardized_data <- study_data[, c("Corrected_Date", zscore_cols), with = FALSE]
      
      return(standardized_data)
    },
    
    process = function() {
      #' Process sea level data
      #'
      #' This method processes sea level data by loading, correcting date format, cleaning, computing monthly statistics,
      #' standardizing, and returning the standardized data.
      #'
      #' @return A data.table containing standardized sea level data with z-scores.
      sea_level_data <- .self$load_data()
      sea_level_data <- .self$correct_date_format(sea_level_data)
      sea_level_data <- .self$clean_data(sea_level_data)
      monthly_means <- .self$compute_monthly_stats(sea_level_data, .self$reference_period, "means")
      monthly_std_devs <- .self$compute_monthly_stats(sea_level_data, .self$reference_period, "std")
      standardized_data <- .self$standardize_data(sea_level_data, monthly_means, monthly_std_devs, .self$study_period)
      return(standardized_data)
    },
    
    plot_rolling_mean = function(data, window = 60) {
      #' Plot rolling mean of sea level data
      #'
      #' This method creates a plot of the rolling mean of standardized sea level data over a specified window.
      #'
      #' @param data A data.table containing standardized sea level data with z-scores.
      #' @param window A numeric value specifying the rolling window size. Default is 60.
      #' @return A ggplot object showing the rolling mean of sea level data over time.
      data$mean_value <- rowMeans(standardized_data[, -1, with = FALSE], na.rm = TRUE)
      ggplot(data, aes(x = Corrected_Date, y = mean_value)) +
        geom_line() +
        labs(title = "Mean Value of Sea Level Data", x = "Date", y = "Mean Value (Standardized)") +
        theme_minimal()
    },
    
    convert_to_xarray = function(data) {
      #' Convert data to xarray format
      #'
      #' This method converts a data.table to an xarray DataArray using the reticulate package to interface with Python.
      #'
      #' @param data A data.table containing sea level data.
      #' @return An xarray DataArray object representing the sea level data.
      library(reticulate)
      np <- import("numpy")
      xr <- import("xarray")
      data_array <- as.data.table(data)
      return(xr$DataArray(np$array(data_array)))
    },
    
    resample_data = function(data) {
      #' Resample sea level data
      #'
      #' This method resamples sea level data to a quarterly frequency by computing the mean for each quarter.
      #'
      #' @param data A data.table containing sea level data with corrected dates.
      #' @return A data.table containing resampled sea level data with quarterly means.
      data[, Corrected_Date := as.Date(Corrected_Date)]
      resampled_data <- data[, lapply(.SD, mean, na.rm = TRUE), by = .(Corrected_Date = floor_date(Corrected_Date, "3 months"))]
      return(resampled_data)
    },
    
    save_to_netcdf = function(data, filename) {
      #' Save data to NetCDF format
      #'
      #' This method saves a data.table to a NetCDF file using the xarray Python library.
      #'
      #' @param data A data.table containing sea level data.
      #' @param filename A string specifying the path and name of the NetCDF file to save.
      xr <- .self$convert_to_xarray(data)
      xr$to_netcdf(filename)
    }
  )
)
