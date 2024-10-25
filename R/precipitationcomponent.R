library(data.table)

#' PrecipitationComponent Class
#'
#' A class to analyze precipitation data, calculate monthly maximum anomalies,
#' and visualize precipitation trends over time and space.
#'
#' @field precipitation_file Character. Path to the NetCDF file containing precipitation data.
#' @field mask_file Character. Path to the NetCDF file containing mask data.
#' @field reference_period Character vector. The start and end dates for the reference period used for standardizing precipitation metrics.
#' @field rolling_window Numeric. The size of the rolling window to calculate the sum of precipitation.
#' @field precip_nc ANY. NetCDF object for precipitation data.
#' @field mask_nc ANY. NetCDF object for mask data.
#' @field precip_dt data.table. Data table containing precipitation data.
#' @field mask_dt data.table. Data table containing mask data.
#' @field precip_masked_dt data.table. Data table with precipitation data after applying the mask.
#' @field monthly_max data.table. Data table with monthly maximum precipitation anomalies.

PrecipitationComponent <- setRefClass(
  "PrecipitationComponent",
  fields = list(
    precipitation_file = "character",   # Path to the NetCDF file containing precipitation data
    mask_file = "character",            # Path to the NetCDF file containing mask data
    reference_period = "character",     # The reference period for calculating anomalies
    rolling_window = "numeric",         # The size of the rolling window to calculate the sum of precipitation
    precip_nc = "ANY",                  # NetCDF object for precipitation data
    mask_nc = "ANY",                    # NetCDF object for mask data
    precip_dt = "data.table",           # Data table containing precipitation data
    mask_dt = "data.table",             # Data table containing mask data
    precip_masked_dt = "data.table",    # Data table with masked precipitation data
    monthly_max = "data.table"          # Data table with monthly maximum precipitation anomalies
  ),
  methods = list(
    initialize = function(precipitation_file, mask_file, reference_period = c("1961-01-01", "1990-12-31"), rolling_window = 5) {
      #' Initialize a PrecipitationComponent object
      #'
      #' This method initializes a new instance of the PrecipitationComponent class by loading precipitation and mask data
      #' from NetCDF files and applying the mask to filter relevant data.
      #'
      #' @param precipitation_file A string specifying the path to the NetCDF file containing precipitation data.
      #' @param mask_file A string specifying the path to the NetCDF file containing mask data.
      #' @param reference_period A character vector of length two specifying the start and end dates of the reference period
      #' for calculating anomalies. The default is c("1961-01-01", "1990-12-31").
      #' @param rolling_window A numeric value specifying the size of the rolling window for calculating the sum of precipitation.
      #' The default is 5.
      .self$precipitation_file <- precipitation_file
      .self$mask_file <- mask_file
      .self$reference_period <- reference_period
      .self$rolling_window <- rolling_window
      .self$load_data()
      .self$apply_mask()
    },

    load_data = function() {
      #' Load precipitation and mask data from NetCDF files
      #'
      #' This method reads precipitation data and mask data from their respective NetCDF files and stores them in
      #' data tables for further processing. It also handles the conversion of time data to appropriate date formats.
      #'
      #' @return None
      .self$precip_nc <- ncdf4::nc_open(.self$precipitation_file)
      .self$mask_nc <- ncdf4::nc_open(.self$mask_file)

      # Extract necessary variables
      tp <- ncdf4::ncvar_get(.self$precip_nc, "tp")
      longitude <- ncdf4::ncvar_get(.self$precip_nc, "longitude")
      latitude <- ncdf4::ncvar_get(.self$precip_nc, "latitude")
      time <- ncdf4::ncvar_get(.self$precip_nc, "time")

      # Check time origin and convert to dates
      time_units <- ncdf4::ncatt_get(.self$precip_nc, "time", "units")$value
      if (grepl("days since", time_units)) {
        time_origin <- sub("days since ", "", time_units)
        time_origin <- as.Date(time_origin, format="%Y-%m-%d")
        time_dates <- time_origin + time
      } else if (grepl("hours since", time_units)) {
        time_origin <- sub("hours since ", "", time_units)
        time_origin <- as.POSIXct(time_origin, tz = "UTC")
        time_dates <- time_origin + lubridate::hours(time)
      } else {
        stop("Unsupported time format in NetCDF file.")
      }

      mask_data <- ncdf4::ncvar_get(.self$mask_nc, "country")
      longitude_mask <- ncdf4::ncvar_get(.self$mask_nc, "lon")
      latitude_mask <- ncdf4::ncvar_get(.self$mask_nc, "lat")

      ncdf4::nc_close(.self$precip_nc)
      ncdf4::nc_close(.self$mask_nc)

      # Create a data.table for precipitation
      .self$precip_dt <- data.table::data.table(
        expand.grid(
          longitude = longitude,
          latitude = latitude,
          time = time_dates
        ),
        tp = as.vector(tp)
      )

      # Create a data.table for the mask
      .self$mask_dt <- data.table::data.table(
        expand.grid(
          longitude = longitude_mask,
          latitude = latitude_mask
        ),
        country = as.vector(mask_data)
      )
    },

    apply_mask = function() {
      #' Apply mask to precipitation data
      #'
      #' This method applies the mask to the precipitation data, setting precipitation values to NA where the mask
      #' indicates it should not be considered (e.g., non-land areas or outside of the study region).
      #'
      #' @return None
      #'
      data.table::setkey(.self$precip_dt, longitude, latitude)
      data.table::setkey(.self$mask_dt, longitude, latitude)
      .self$precip_masked_dt <- data.table::merge(.self$precip_dt, .self$mask_dt, by = c("longitude", "latitude"), all.x = TRUE)
      # Apply mask with a threshold of 0.8
      .self$precip_masked_dt[, tp := ifelse(country >= 0.8, tp, NA)]
    },

    calculate_monthly_max = function() {
      #' Calculate monthly maximum precipitation anomalies
      #'
      #' This method calculates the monthly maximum of a rolling sum of precipitation and then standardizes these
      #' maxima based on a reference period. The standardized anomalies are stored for further analysis or visualization.
      #'
      #' @return None
      #'
      .self$precip_masked_dt <- .self$precip_masked_dt[order(time)]

      # Calculate rolling sum using RcppRoll for better performance
      .self$precip_masked_dt[, tp_rolling := RcppRoll::roll_sum(tp, n = .self$rolling_window, fill = NA, align = "right"),
                             by = .(longitude, latitude)]
      # Add columns for year and month
      .self$precip_masked_dt[, year := lubridate::year(time)]
      .self$precip_masked_dt[, month := lubridate::month(time)]
      # Calculate the monthly maximum
      .self$monthly_max <- .self$precip_masked_dt[, .(monthly_max = max(tp_rolling, na.rm = TRUE)),
                                                  by = .(longitude, latitude, year, month)]

      # Calculate reference statistics for the user-defined reference period
      reference_period <- .self$monthly_max[year >= lubridate::year(as.Date(.self$reference_period[1])) &
                                              year <= lubridate::year(as.Date(.self$reference_period[2]))]
      reference_stats <- reference_period[, .(mean = mean(monthly_max, na.rm = TRUE),
                                              sd = sd(monthly_max, na.rm = TRUE)),
                                          by = month]
      # Merge reference statistics with monthly data
      .self$monthly_max <- data.table::merge(.self$monthly_max, reference_stats, by = "month", all.x = TRUE)
      .self$monthly_max[, rx5day := (monthly_max - mean) / sd]
    },

    visualize = function(selected_latitude, selected_longitude) {
      #' Visualize standardized monthly maximum precipitation anomalies
      #'
      #' This method creates a time series plot of the standardized monthly maximum precipitation anomalies for a specific
      #' location defined by latitude and longitude.
      #'
      #' @param selected_latitude A numeric value specifying the latitude of the location to visualize.
      #' @param selected_longitude A numeric value specifying the longitude of the location to visualize.
      #' @return A ggplot object showing the time series of standardized monthly maximum precipitation anomalies.
      #'
      location_data <- .self$monthly_max[latitude == selected_latitude & longitude == selected_longitude]

      ggplot2::ggplot(location_data, ggplot2::aes(x = as.Date(paste(year, month, "01", sep = "-")), y = rx5day)) +
        ggplot2::geom_line() +
        ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        ggplot2::labs(title = paste("Standardized Monthly Maximum Precipitation for Latitude", selected_latitude, "and Longitude", selected_longitude),
                      x = "Time", y = "Standardized Monthly Maximum Precipitation") +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    },

    plot_rx5day_map = function(year, month) {
      #' Plot standardized anomaly map for a specific month and year
      #'
      #' This method creates a spatial plot of standardized precipitation anomalies for a specified month and year.
      #'
      #' @param year A numeric value specifying the year for the anomaly map.
      #' @param month A numeric value specifying the month for the anomaly map.
      #' @return A ggplot object showing the spatial distribution of standardized precipitation anomalies for the specified month and year.
      #'
      map_data <- .self$monthly_max[year == year & month == month]
      # Plot Rx5day on a map
      ggplot2::ggplot(map_data, ggplot2::aes(x = longitude, y = latitude, fill = rx5day)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
        ggplot2::labs(title = paste("Rx5day for", year, "-", month),
                      x = "Longitude", y = "Latitude", fill = "Rx5day") +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed()
    },

    plot_rx5day_mean_time = function(smooth_years = 2) {
      #' Plot mean Rx5day over time with smoothing
      #'
      #' This method creates a time series plot of the mean Rx5day over time with an optional smoothing period.
      #'
      #' @param smooth_years A numeric value specifying the smoothing period in years. Default is 2 years.
      #' @return A ggplot object showing the mean Rx5day over time with smoothing.
      #'
      mean_rx5day <- .self$monthly_max[, .(mean_rx5day = mean(rx5day, na.rm = TRUE)), by = .(year, month)]
      # Create a 'date' column from year and month
      mean_rx5day[, date := ymd(paste(year, month, "01", sep = "-"))]
      # Apply smoothing over the defined period
      mean_rx5day[, mean_rx5day_smooth := RcppRoll::rollmean(mean_rx5day, k = smooth_years * 12, fill = NA, align = "right")]
      # Remove rows with NA values
      mean_rx5day <- mean_rx5day[!is.na(mean_rx5day_smooth)]

      # Plot mean Rx5day over time with smoothing
      ggplot2::ggplot(mean_rx5day, ggplot2::aes(x = date)) +
        ggplot2::geom_line(aes(y = mean_rx5day, color = "Original")) +
        ggplot2::geom_line(aes(y = mean_rx5day_smooth, color = "Smoothed")) +
        ggplot2::scale_color_manual(values = c("Original" = "blue", "Smoothed" = "red")) +
        ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Adjust x-axis labels to display years
        ggplot2::labs(title = "Mean Rx5day over Time with Smoothing",
                      x = "Time", y = "Mean Rx5day") +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggplot2::theme(legend.title = element_blank(), legend.position = "top")
    }
  )
)
