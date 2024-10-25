library(data.table)

#' TemperatureComponent Class
#'
#' A class for processing and analyzing temperature data from NetCDF files.
#'
#' @field temperature_file Character. Path to the NetCDF file containing temperature data.
#' @field mask_file Character. Path to the NetCDF file containing mask data.
#' @field reference_period Character. Reference period used for calculating anomalies.
#' @field temperature_dt Data.table. Data table to store temperature data.
#' @field mask_dt Data.table. Data table to store mask data.

TemperatureComponent <- setRefClass(
  "TemperatureComponent",
  fields = list(
    temperature_file = "character",  # Path to the NetCDF file containing temperature data
    mask_file = "character",         # Path to the NetCDF file containing mask data
    reference_period = "character",  # Reference period used for calculating anomalies
    temperature_dt = "data.table",   # Data table to store temperature data
    mask_dt = "data.table"           # Data table to store mask data
  ),
  methods = list(
    initialize = function(temperature_file, mask_file, reference_period) {
      #' Initialize TemperatureComponent
      #'
      #' Initializes a new instance of the TemperatureComponent class.
      #'
      #' @param temperature_file A string representing the path to the temperature NetCDF file.
      #' @param mask_file A string representing the path to the mask NetCDF file.
      #' @param reference_period A character vector specifying the start and end dates of the reference period.

      .self$temperature_file <- temperature_file
      .self$mask_file <- mask_file
      .self$reference_period <- reference_period
      .self$load_data()
      .self$apply_mask()
    },

    load_data = function() {
      #' Load temperature and mask data
      #'
      #' Loads the temperature and mask data from NetCDF files and stores them in data tables.
      temperature_nc <- ncdf4::nc_open(.self$temperature_file)
      mask_nc <- ncdf4::nc_open(.self$mask_file)

      temperature <- ncdf4::ncvar_get(temperature_nc, "t2m")
      longitude <- ncdf4::ncvar_get(temperature_nc, "longitude")
      latitude <- ncdf4::ncvar_get(temperature_nc, "latitude")
      time <- ncdf4::ncvar_get(temperature_nc, "time")

      time_units <- ncdf4::ncatt_get(temperature_nc, "time", "units")$value
      # Detect the time unit and origin
      if (grepl("days since", time_units)) {
        time_origin <- sub("days since ", "", time_units)
        time_origin <- as.Date(time_origin)
        time_dates <- time_origin + time
      } else if (grepl("hours since", time_units)) {
        time_origin <- sub("hours since ", "", time_units)
        time_origin <- as.POSIXct(time_origin, tz = "UTC")
        time_dates <- time_origin + lubridate::hours(time)
      } else {
        stop("Unsupported time units format in NetCDF file.")
      }

      mask_data <- ncdf4::ncvar_get(mask_nc, "country")
      longitude_mask <- ncdf4::ncvar_get(mask_nc, "lon")
      latitude_mask <- ncdf4::ncvar_get(mask_nc, "lat")

      ncdf4::nc_close(temperature_nc)
      ncdf4::nc_close(mask_nc)

      .self$temperature_dt <- data.table::data.table(
        expand.grid(
          longitude = longitude,
          latitude = latitude,
          time = time_dates
        ),
        t2m = as.vector(temperature)
      )

      .self$mask_dt <- data.table::data.table(
        expand.grid(
          longitude = longitude_mask,
          latitude = latitude_mask
        ),
        country = as.vector(mask_data)
      )
    },

    apply_mask = function() {
      #' Apply mask to temperature data
      #'
      #' Applies a geographic mask to the temperature data to filter out unwanted regions.
      data.table::setkey(.self$temperature_dt, longitude, latitude)
      data.table::setkey(.self$mask_dt, longitude, latitude)

      temperature_masked_dt <- data.table::merge(.self$temperature_dt, .self$mask_dt,
                                                 by = c("longitude", "latitude"), all.x = TRUE)
      temperature_masked_dt[, t2m := ifelse(country >= 0.8, t2m, NA)]
      .self$temperature_dt <- temperature_masked_dt
    },

    convert_to_posixct = function(dt) {
      #' Convert time column to POSIXct format
      #'
      #' Converts the 'time' column in a data table to POSIXct format if it is not already.
      #'
      #' @param dt A data.table with a 'time' column.
      #' @return A data.table with the 'time' column converted to POSIXct format.
      #'
      if (class(dt$time)[1] != "POSIXct") {
        dt$time <- as.POSIXct(dt$time, origin="1970-01-01", tz="UTC")
      }
      return(dt)
    },

    temp_extremum = function(temp_data, extremum) {
      #' Calculate temperature extremum
      #'
      #' Calculates the maximum or minimum temperature for each day at each location.
      #'
      #' @param temp_data A data.table containing temperature data.
      #' @param extremum A string specifying whether to calculate the "max" or "min" temperature.
      #' @return A data.table with the calculated temperature extremum.
      #'
      if (extremum == "max") {
        temp_extremum <- temp_data[, .(temp_extremum = max(t2m, na.rm = TRUE)), by = .(longitude, latitude, time)]
      } else if (extremum == "min") {
        temp_extremum <- temp_data[, .(temp_extremum = min(t2m, na.rm = TRUE)), by = .(longitude, latitude, time)]
      }
      return(temp_extremum)
    },

    percentiles = function(temp_data, n, reference_period) {
      #' Calculate temperature percentiles
      #'
      #' Calculates the n-th percentile temperature for each day of the year over a reference period.
      #'
      #' @param temp_data A data.table containing temperature data.
      #' @param n An integer specifying the percentile to calculate.
      #' @param reference_period A character vector specifying the start and end dates of the reference period.
      #' @return A data.table with the calculated percentiles.
      #'
      temp_reference <- temp_data[time >= as.Date(reference_period[1]) & time <= as.Date(reference_period[2])]
      temp_reference[, dayofyear := yday(time)]

      percentile_reference <- temp_reference[, .(percentile = quantile(t2m, probs = n/100, na.rm = TRUE)),
                                             by = .(longitude, latitude, dayofyear)]
      return(percentile_reference)
    },

    t90 = function(reference_period) {
      #' Calculate T90 metric
      #'
      #' Calculates the T90 metric, which is the frequency of days with maximum temperatures above the 90th percentile.
      #'
      #' @param reference_period A character vector specifying the start and end dates of the reference period.
      #' @return A data.table with the calculated T90 values.
      #'
      temperature_days_max <- .self$temp_extremum(.self$temperature_dt, "max")
      temperature_days_max[, dayofyear := yday(time)]
      temperature_days_max <- .self$convert_to_posixct(temperature_days_max)

      percentile_90_calendar_days <- .self$percentiles(.self$temperature_dt, 90, reference_period)
      percentile_90_calendar_days <- .self$convert_to_posixct(percentile_90_calendar_days)

      percentile_90_calendar_days <- percentile_90_calendar_days[, .(longitude, latitude, dayofyear, percentile)]
      temperature_days_max <- data.table::merge(temperature_days_max, percentile_90_calendar_days,
                                                by = c("longitude", "latitude", "dayofyear"), all.x = TRUE)
      temperature_days_max[, difference := temp_extremum - percentile]
      temperature_days_max[, difference := as.double(difference)]

      days_90_above_thresholds <- temperature_days_max[, .(days_above = as.numeric(ifelse(difference > 0, 1, 0))),
                                                       by = .(longitude, latitude, time)]
      tx90 <- days_90_above_thresholds[, .(frequency = sum(days_above, na.rm = TRUE) / .N),
                                       by = .(month = lubridate::floor_date(time, "month"))]

      temperature_nights_max <- .self$temp_extremum(.self$temperature_dt, "max")
      temperature_nights_max[, dayofyear := yday(time)]
      temperature_nights_max <- .self$convert_to_posixct(temperature_nights_max)

      percentile_90_calendar_nights <- .self$percentiles(.self$temperature_dt, 90, reference_period)
      percentile_90_calendar_nights <- .self$convert_to_posixct(percentile_90_calendar_nights)

      percentile_90_calendar_nights <- percentile_90_calendar_nights[, .(longitude, latitude, dayofyear, percentile)]
      temperature_nights_max <- data.table::merge(temperature_nights_max, percentile_90_calendar_nights,
                                                  by = c("longitude", "latitude", "dayofyear"), all.x = TRUE)
      temperature_nights_max[, difference := temp_extremum - percentile]
      temperature_nights_max[, difference := as.double(difference)]

      nights_90_above_thresholds <- temperature_nights_max[, .(days_above = as.numeric(ifelse(difference > 0, 1, 0))),
                                                           by = .(longitude, latitude, time)]
      tn90 <- nights_90_above_thresholds[, .(frequency = sum(days_above, na.rm = TRUE) / .N),
                                         by = .(month = lubridate::floor_date(time, "month"))]
      t90_values <- data.table::merge(tx90, tn90, by = "month", suffixes = c("_day", "_night"))
      t90_values[, t90 := 0.5 * (frequency_day + frequency_night)]

      return(t90_values)
    },

    t10 = function(reference_period) {
      #' Calculate T10 metric
      #'
      #' Calculates the T10 metric, which is the frequency of days with minimum temperatures below the 10th percentile.
      #'
      #' @param reference_period A character vector specifying the start and end dates of the reference period.
      #' @return A data.table with the calculated T10 values.
      #'
      temperature_days_min <- .self$temp_extremum(.self$temperature_dt, "min")
      temperature_days_min[, dayofyear := yday(time)]
      temperature_days_min <- .self$convert_to_posixct(temperature_days_min)

      percentile_10_calendar_days <- .self$percentiles(.self$temperature_dt, 10, reference_period)
      percentile_10_calendar_days <- .self$convert_to_posixct(percentile_10_calendar_days)

      percentile_10_calendar_days <- percentile_10_calendar_days[, .(longitude, latitude, dayofyear, percentile)]
      temperature_days_min <- merge(temperature_days_min, percentile_10_calendar_days,
                                    by = c("longitude", "latitude", "dayofyear"), all.x = TRUE)
      temperature_days_min[, difference := temp_extremum - percentile]
      temperature_days_min[, difference := as.double(difference)]

      days_10_below_thresholds <- temperature_days_min[, .(days_below = as.numeric(ifelse(difference < 0, 1, 0))),
                                                       by = .(longitude, latitude, time)]
      tx10 <- days_10_below_thresholds[, .(frequency = sum(days_below, na.rm = TRUE) / .N),
                                       by = .(month = lubridate::floor_date(time, "month"))]
      temperature_nights_min <- .self$temp_extremum(.self$temperature_dt, "min")
      temperature_nights_min[, dayofyear := yday(time)]
      temperature_nights_min <- .self$convert_to_posixct(temperature_nights_min)

      percentile_10_calendar_nights <- .self$percentiles(.self$temperature_dt, 10, reference_period)
      percentile_10_calendar_nights <- .self$convert_to_posixct(percentile_10_calendar_nights)

      percentile_10_calendar_nights <- percentile_10_calendar_nights[, .(longitude, latitude, dayofyear, percentile)]
      temperature_nights_min <- data.table::merge(temperature_nights_min, percentile_10_calendar_nights,
                                                  by = c("longitude", "latitude", "dayofyear"), all.x = TRUE)
      temperature_nights_min[, difference := temp_extremum - percentile]
      temperature_nights_min[, difference := as.double(difference)]

      nights_10_below_thresholds <- temperature_nights_min[, .(days_below = as.numeric(ifelse(difference < 0, 1, 0))),
                                                           by = .(longitude, latitude, time)]
      tn10 <- nights_10_below_thresholds[, .(frequency = sum(days_below, na.rm = TRUE) / .N),
                                         by = .(month = lubridate::floor_date(time, "month"))]
      t10_values <- data.table::merge(tx10, tn10, by = "month", suffixes = c("_day", "_night"))
      t10_values[, t10 := 0.5 * (frequency_day + frequency_night)]

      return(t10_values)
    },

    standardize_metric = function(metric_values, reference_period) {
      #' Standardize a metric
      #'
      #' Standardizes a given metric over a reference period.
      #'
      #' @param metric_values A data.table containing the metric values to be standardized.
      #' @param reference_period A character vector specifying the start and end dates of the reference period.
      #' @return A data.table with the standardized metric.
      #'
      if (!inherits(metric_values$month, "Date")) {
        metric_values[, month := as.Date(month)]
      }
      reference_values <- metric_values[month >= as.Date(reference_period[1]) & month <= as.Date(reference_period[2])]
      metric_mean <- mean(reference_values[[2]], na.rm = TRUE)
      metric_sd <- sd(reference_values[[2]], na.rm = TRUE)
      metric_values[, standardized := (get(names(metric_values)[2]) - metric_mean) / metric_sd]
      return(metric_values[, .(month, standardized)])
    },

    std_t90 = function(reference_period) {
      #' Standardize T90 metric
      #'
      #' Standardizes the T90 metric over a reference period.
      #'
      #' @param reference_period A character vector specifying the start and end dates of the reference period.
      #' @return A data.table with the standardized T90 values.
      #'
      t90_values <- .self$t90(reference_period)
      standardized_t90 <- .self$standardize_metric(t90_values, reference_period)
      return(standardized_t90)
    },

    std_t10 = function(reference_period) {
      #' Standardize T10 metric
      #'
      #' Standardizes the T10 metric over a reference period.
      #'
      #' @param reference_period A character vector specifying the start and end dates of the reference period.
      #' @return A data.table with the standardized T10 values.
      #'
      t10_values <- .self$t10(reference_period)
      standardized_t10 <- .self$standardize_metric(t10_values, reference_period)
      return(standardized_t10)
    },

    days_above_thresholds = function(reference_period) {
      #' Calculate days above temperature thresholds
      #'
      #' Calculates the number of days above the 90th percentile temperature threshold.
      #'
      #' @param reference_period A character vector specifying the start and end dates of the reference period.
      #' @return A data.table with the number of days above the threshold.
      #'
      temperature_days_max <- .self$temp_extremum(.self$temperature_dt, "max")
      temperature_days_max[, dayofyear := yday(time)]
      temperature_days_max <- .self$convert_to_posixct(temperature_days_max)

      percentile_90_calendar_days <- .self$percentiles(.self$temperature_dt, 90, reference_period)
      percentile_90_calendar_days <- .self$convert_to_posixct(percentile_90_calendar_days)

      percentile_90_calendar_days <- percentile_90_calendar_days[, .(longitude, latitude, dayofyear, percentile)]
      temperature_days_max <- merge(temperature_days_max, percentile_90_calendar_days,
                                    by = c("longitude", "latitude", "dayofyear"), all.x = TRUE)
      temperature_days_max[, difference := temp_extremum - percentile]
      temperature_days_max[, difference := as.double(difference)]

      days_above_thresholds <- temperature_days_max[, .(days_above = as.numeric(ifelse(difference > 0, 1, 0))),
                                                    by = .(longitude, latitude, time)]
      return(days_above_thresholds)
    },

    days_below_thresholds = function(reference_period) {
      #' Calculate days below temperature thresholds
      #'
      #' Calculates the number of days below the 10th percentile temperature threshold.
      #'
      #' @param reference_period A character vector specifying the start and end dates of the reference period.
      #' @return A data.table with the number of days below the threshold.
      #'
      temperature_days_min <- .self$temp_extremum(.self$temperature_dt, "min")
      temperature_days_min[, dayofyear := yday(time)]
      temperature_days_min <- .self$convert_to_posixct(temperature_days_min)

      percentile_10_calendar_days <- .self$percentiles(.self$temperature_dt, 10, reference_period)
      percentile_10_calendar_days <- .self$convert_to_posixct(percentile_10_calendar_days)

      percentile_10_calendar_days <- percentile_10_calendar_days[, .(longitude, latitude, dayofyear, percentile)]
      temperature_days_min <- merge(temperature_days_min, percentile_10_calendar_days,
                                    by = c("longitude", "latitude", "dayofyear"), all.x = TRUE)
      temperature_days_min[, difference := temp_extremum - percentile]
      temperature_days_min[, difference := as.double(difference)]

      days_below_thresholds <- temperature_days_min[, .(days_below = as.numeric(ifelse(difference < 0, 1, 0))),
                                                    by = .(longitude, latitude, time)]
      return(days_below_thresholds)
    },

    plot_standardized_components = function(standardized_t10, standardized_t90, n) {
      #' Plot rolling mean of standardized temperatures
      #'
      #' Plots the rolling mean of standardized T10 and T90 over time.
      #'
      #' @param standardized_t10 A data.table containing standardized T10 values.
      #' @param standardized_t90 A data.table containing standardized T90 values.
      #' @param n An integer specifying the window size for the rolling mean.
      #'
      standardized_t10[, month := as.Date(month)]
      standardized_t90[, month := as.Date(month)]

      standardized_t10[, rolling_mean := zoo::rollmean(standardized, k = n, fill = NA, align = "center")]
      standardized_t90[, rolling_mean := zoo::rollmean(standardized, k = n, fill = NA, align = "center")]

      ggplot2::ggplot() +
        ggplot2::geom_line(data = standardized_t10, ggplot2::aes(x = month, y = rolling_mean, color = "Standardized T10")) +
        ggplot2::geom_line(data = standardized_t90, ggplot2::aes(x = month, y = rolling_mean, color = "Standardized T90")) +
        ggplot2::labs(title = "Rolling Mean of Standardized T10 and T90", x = "Time", y = "Standardized Temperature") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.title = element_blank())
    },

    plot_standardized_components_simple = function(standardized_t90, standardized_t10) {
      #' Plot standardized temperatures over time
      #'
      #' Plots the standardized T90 and T10 over time.
      #'
      #' @param standardized_t90 A data.table containing standardized T90 values.
      #' @param standardized_t10 A data.table containing standardized T10 values.
      #'
      combined_data <- rbind(
        data.table(month = standardized_t90$month, value = standardized_t90$standardized, metric = "T90"),
        data.table(month = standardized_t10$month, value = standardized_t10$standardized, metric = "T10")
      )

      ggplot2::ggplot(combined_data, ggplot2::aes(x = month, y = value, color = metric)) +
        ggplot2::geom_line() +
        ggplot2::labs(title = "Standardized T90 and T10 over Time",
                      x = "Time",
                      y = "Standardized Value",
                      color = "Metric") +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  )
)
