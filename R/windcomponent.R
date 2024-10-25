library(data.table)

#' WindComponent Class
#'
#' A class for analyzing wind data from NetCDF files, including calculating wind power, wind thresholds, days above thresholds, wind exceedance frequency, and plotting results.
#'
#' @field u10_file Character. File path for the u10 component of wind.
#' @field v10_file Character. File path for the v10 component of wind.
#' @field mask_file Character. File path for the mask file.
#' @field reference_period Character. Reference period for calculating statistics.
#' @field u10_dt Data.table. Data table for u10 component data.
#' @field v10_dt Data.table. Data table for v10 component data.
#' @field mask_dt Data.table. Data table for mask data.
#' @field wind_power_dt Data.table. Data table for calculated wind power.
#' @field wind_thresholds_dt Data.table. Data table for calculated wind thresholds.
WindComponent <- setRefClass(
  "WindComponent",
  fields = list(
    u10_file = "character",
    v10_file = "character",
    mask_file = "character",
    reference_period = "character",
    u10_dt = "data.table",
    v10_dt = "data.table",
    mask_dt = "data.table",
    wind_power_dt = "data.table",
    wind_thresholds_dt = "data.table"
  ),
  methods = list(
    #' Initialize WindComponent object
    #'
    #' @param u10_file Character. File path for the u10 component of wind.
    #' @param v10_file Character. File path for the v10 component of wind.
    #' @param mask_file Character. File path for the mask file.
    #' @param reference_period Character vector. Start and end dates for the reference period.
    initialize = function(u10_file, v10_file, mask_file, reference_period = c("1960-01-01", "1964-12-31")) {
      .self$u10_file <- u10_file
      .self$v10_file <- v10_file
      .self$mask_file <- mask_file
      .self$reference_period <- reference_period
      .self$load_data()
      .self$apply_mask()
    },

    #' Load data from NetCDF files
    #'
    #' Loads u10, v10, and mask data from the specified NetCDF files and stores them in data tables.
    load_data = function() {
      u10_nc <- ncdf4::nc_open(.self$u10_file)
      v10_nc <- ncdf4::nc_open(.self$v10_file)
      mask_nc <- ncdf4::nc_open(.self$mask_file)

      u10 <- ncdf4::ncvar_get(u10_nc, "u10")
      v10 <- ncdf4::ncvar_get(v10_nc, "v10")
      longitude <- ncdf4::ncvar_get(u10_nc, "longitude")
      latitude <- ncdf4::ncvar_get(u10_nc, "latitude")
      time <- ncdf4::ncvar_get(u10_nc, "time")

      time_units <- ncdf4::ncatt_get(u10_nc, "time", "units")$value
      if (grepl("since", time_units)) {
        time_origin <- sub(".*since ", "", time_units)
        if (grepl("days", time_units)) {
          time_dates <- as.Date(time, origin = as.Date(time_origin))
        } else if (grepl("hours", time_units)) {
          time_origin <- as.POSIXct(time_origin, tz = "UTC")
          time_dates <- time_origin + lubridate::hours(time)
          time_dates <- as.Date(time_dates)
        } else {
          stop("Unsupported time units: ", time_units)
        }
      } else {
        stop("Unexpected time format in NetCDF file")
      }

      mask_data <- ncdf4::ncvar_get(mask_nc, "country")
      longitude_mask <- ncdf4::ncvar_get(mask_nc, "lon")
      latitude_mask <- ncdf4::ncvar_get(mask_nc, "lat")

      ncdf4::nc_close(u10_nc)
      ncdf4::nc_close(v10_nc)
      ncdf4::nc_close(mask_nc)

      .self$u10_dt <- data.table::data.table(
        expand.grid(
          longitude = longitude,
          latitude = latitude,
          time = time_dates
        ),
        u10 = as.vector(u10)
      )

      .self$v10_dt <- data.table::data.table(
        expand.grid(
          longitude = longitude,
          latitude = latitude,
          time = time_dates
        ),
        v10 = as.vector(v10)
      )

      .self$mask_dt <- data.table::data.table(
        expand.grid(
          longitude = longitude_mask,
          latitude = latitude_mask
        ),
        country = as.vector(mask_data)
      )
    },

    #' Apply mask to wind data
    #'
    #' Applies the mask data to u10 and v10 data tables, removing rows where the mask value is below 0.8.
    apply_mask = function() {
      data.table::setkey(.self$u10_dt, longitude, latitude)
      data.table::setkey(.self$v10_dt, longitude, latitude)
      data.table::setkey(.self$mask_dt, longitude, latitude)

      u10_masked_dt <- data.table::merge(.self$u10_dt, .self$mask_dt, by = c("longitude", "latitude"), all.x = TRUE)
      v10_masked_dt <- data.table::merge(.self$v10_dt, .self$mask_dt, by = c("longitude", "latitude"), all.x = TRUE)

      u10_masked_dt[, u10 := ifelse(country >= 0.8, u10, NA)]
      v10_masked_dt[, v10 := ifelse(country >= 0.8, v10, NA)]

      .self$u10_dt <- data.table::unique(u10_masked_dt)
      .self$v10_dt <- data.table::unique(v10_masked_dt)
    },

    #' Remove duplicates from data table
    #'
    #' @param dt Data.table. The data table from which duplicates are to be removed.
    #' @return Data.table. Data table with duplicates removed.
    #'
    remove_duplicates = function(dt) {
      dt <- dt[!data.table::duplicated(dt, by = c("longitude", "latitude", "time"))]
      return(dt)
    },

    #' Calculate wind power
    #'
    #' @param reference_period Character vector. Optional reference period to filter data by date.
    #' @return Data.table. Wind power data table.
    wind_power = function(reference_period = NULL) {
      .self$u10_dt <- .self$remove_duplicates(.self$u10_dt)
      .self$v10_dt <- .self$remove_duplicates(.self$v10_dt)

      merged_dt <- data.table::merge(.self$u10_dt, .self$v10_dt,
                                     by = c("longitude", "latitude", "time"), allow.cartesian = FALSE)
      merged_dt[, ws := sqrt(u10^2 + v10^2)]
      rho <- 1.23
      merged_dt[, wind_power := 0.5 * rho * ws^3]

      .self$wind_power_dt <- merged_dt[, .(time, longitude, latitude, wind_power)]

      if (!is.null(reference_period)) {
        .self$wind_power_dt <- .self$wind_power_dt[time >= as.Date(reference_period[1]) &
                                                     time <= as.Date(reference_period[2])]
      }
      return(.self$wind_power_dt)
    },

    #' Calculate wind power thresholds
    #'
    #' @param reference_period Character vector. Reference period for calculating thresholds.
    #' @return Data.table. Wind power thresholds data table.
    #'
    wind_thresholds = function(reference_period) {
      wind_power_dt <- .self$wind_power(reference_period)
      wind_power_dt[, dayofyear := yday(time)]

      reference_stats <- wind_power_dt[, .(
        mean_power = mean(wind_power, na.rm = TRUE),
        std_power = sd(wind_power, na.rm = TRUE)
      ), by = .(dayofyear, latitude, longitude)]

      reference_stats[, threshold := mean_power + 1.28 * std_power]

      .self$wind_thresholds_dt <- reference_stats[, .(dayofyear, latitude, longitude, threshold)]
      return(.self$wind_thresholds_dt)
    },

    #' Calculate days above wind power thresholds
    #'
    #' @param reference_period Character vector. Reference period for calculating days above thresholds.
    #' @return Data.table. Days above threshold data table.
    #'
    days_above_thresholds = function(reference_period) {
      wind_thresholds <- .self$wind_thresholds(reference_period)
      wind_power_dt <- .self$wind_power()
      wind_power_dt[, dayofyear := yday(time)]
      days_above <- data.table::merge(wind_power_dt, wind_thresholds,
                                      by = c("dayofyear", "latitude", "longitude"), allow.cartesian = FALSE)
      days_above[, days_above_threshold := ifelse(wind_power > threshold, 1, 0)]

      return(days_above[, .(time, latitude, longitude, days_above_threshold)])
    },

    #' Calculate wind exceedance frequency
    #'
    #' @param reference_period Character vector. Reference period for calculating exceedance frequency.
    #' @return Data.table. Wind exceedance frequency data table.
    #'
    wind_exceedance_frequency = function(reference_period) {
      days_above_dt <- .self$days_above_thresholds(reference_period)
      days_above_dt[, year_month := lubridate::floor_date(time, "month")]
      exceedance_freq <- days_above_dt[, .(
        exceedance_frequency = sum(days_above_threshold, na.rm = TRUE) / .N
      ), by = .(year_month, latitude, longitude)]
      return(exceedance_freq)
    },

    #' Calculate standardized wind exceedance frequency
    #'
    #' @param reference_period Character vector. Reference period for calculating standardized exceedance frequency.
    #' @return Data.table. Standardized wind exceedance frequency data table.
    #'
    std_wind_exceedance_frequency = function(reference_period) {
      exceedance_freq <- .self$wind_exceedance_frequency(reference_period)
      mean_frequency <- mean(exceedance_freq$exceedance_frequency, na.rm = TRUE)
      std_frequency <- sd(exceedance_freq$exceedance_frequency, na.rm = TRUE)
      exceedance_freq[, std_frequency := (exceedance_frequency - mean_frequency) / std_frequency]
      return(exceedance_freq)
    },

    #' Plot wind exceedance frequency
    #'
    #' @param lat Numeric. Latitude of the location to plot.
    #' @param lon Numeric. Longitude of the location to plot.
    #' @param reference_period Character vector. Reference period for calculating exceedance frequency.
    #' @return ggplot object. Plot of wind exceedance frequency for the specified location.
    plot_wind_exceedance_frequency = function(lat, lon, reference_period) {
      monthly_total_days_above <- .self$wind_exceedance_frequency(reference_period)
      location_data <- monthly_total_days_above[latitude == lat & longitude == lon]
      if (nrow(location_data) == 0) {
        cat("No data available for the specified latitude and longitude.\n")
        return(NULL)
      }

      ggplot2::ggplot(location_data, ggplot2::aes(x = year_month, y = exceedance_frequency)) +
        ggplot2::geom_line() +
        ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        ggplot2::labs(title = paste("Wind Exceedance Frequency for Latitude", lat, "and Longitude", lon),
                      x = "Time", y = "Frequency") +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    },

    #' Plot wind exceedance frequency map
    #'
    #' @param reference_period Character vector. Reference period for calculating exceedance frequency.
    #' @return ggplot object. Map of wind exceedance frequency.
    plot_wind_exceedance_frequency_map = function(reference_period) {
      monthly_total_days_above <- .self$wind_exceedance_frequency(reference_period)

      ggplot2::ggplot(monthly_total_days_above, ggplot2::aes(x = longitude, y = latitude, fill = exceedance_frequency)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
        ggplot2::labs(title = "Wind Exceedance Frequency Map",
                      x = "Longitude", y = "Latitude", fill = "Frequency") +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed()
    }
  )
)
