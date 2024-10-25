library(data.table)

#' DroughtComponent Class
#'
#' A class to handle drought data analysis based on precipitation data and a mask.
#' This class can calculate and standardize the maximum number of consecutive dry days,
#' apply spatial masks, visualize standardized drought anomalies, and plot drought anomaly maps.
#'
#' @field precipitation_file Character. The path to the NetCDF file containing precipitation data.
#' @field mask_file Character. The path to the NetCDF file containing mask data.
#' @field reference_period Character vector. The start and end dates for the reference period used for standardizing drought metrics.
#' @field precip_nc ANY. The NetCDF object for precipitation data.
#' @field mask_nc ANY. The NetCDF object for mask data.
#' @field precip_dt data.table. Data table containing precipitation data.
#' @field mask_dt data.table. Data table containing mask data.
#' @field precip_masked_dt data.table. Data table with precipitation data after applying the mask.
#' @field max_dry_days_dt data.table. Data table containing the maximum consecutive dry days.
#' @field standardized_dry_days_dt data.table. Data table with standardized drought anomalies.
DroughtComponent <- setRefClass(
  "DroughtComponent",
  fields = list(
    precipitation_file = "character",   # Path to the NetCDF file containing precipitation data
    mask_file = "character",            # Path to the NetCDF file containing mask data
    reference_period = "character",     # The reference period for calculating anomalies
    precip_nc = "ANY",                  # NetCDF object for precipitation data
    mask_nc = "ANY",                    # NetCDF object for mask data
    precip_dt = "data.table",           # Data table containing precipitation data
    mask_dt = "data.table",             # Data table containing mask data
    precip_masked_dt = "data.table",    # Data table with masked precipitation data
    max_dry_days_dt = "data.table",     # Data table with maximum consecutive dry days
    standardized_dry_days_dt = "data.table"  # Data table with standardized drought anomalies
  ),
  methods = list(
    initialize = function(precipitation_file, mask_file, reference_period = c("1961-01-01", "1990-12-31")) {
      #' Initialize a DroughtComponent object
      #'
      #' This method initializes a new instance of the DroughtComponent class by loading precipitation and mask data
      #' from netCDF files and applying the mask to filter relevant data.
      #'
      #' @param precipitation_file A string specifying the path to the netCDF file containing precipitation data.
      #' @param mask_file A string specifying the path to the netCDF file containing mask data.
      #' @param reference_period A character vector of length two specifying the start and end dates of the reference period
      #' for calculating anomalies. The default is c("1961-01-01", "1990-12-31").
      .self$precipitation_file <- precipitation_file
      .self$mask_file <- mask_file
      .self$reference_period <- reference_period
      .self$load_data()
      .self$apply_mask()
      .self$max_consecutive_dry_days()  # Calculate initial dry days
      .self$drought_interpolate()  # Interpolate the drought data after calculating dry days
    },

    load_data = function() {
      #' Load precipitation and mask data from netCDF files
      #'
      #' This method reads precipitation data and mask data from their respective netCDF files and stores them in
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
      print(paste("Time units attribute:", time_units))  # Debugging line to print time units

      if (grepl("since", time_units)) {
        time_origin <- sub(".*since ", "", time_units)

        # Try multiple formats for parsing
        possible_formats <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d", "%Y-%m-%d %H:%M")
        time_origin_parsed <- NA

        for (fmt in possible_formats) {
          time_origin_parsed <- as.POSIXct(time_origin, format = fmt, tz = "UTC")
          if (!is.na(time_origin_parsed)) {
            break
          }
        }

        if (is.na(time_origin_parsed)) {
          stop(paste("Failed to parse time origin from NetCDF file. Expected formats: ", paste(possible_formats, collapse = ", ")))
        }

        # Assign the correctly parsed time origin
        time_origin <- time_origin_parsed

      } else {
        stop("Unexpected time units format in NetCDF file.")
      }

      # Convert time to dates
      time_dates <- time_origin + as.numeric(time) * (ifelse(grepl("hours", time_units), 3600, 86400))

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
          time = as.Date(time_dates)  # Ensure time is converted to Date
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
      data.table::setkey(.self$precip_dt, longitude, latitude)
      data.table::setkey(.self$mask_dt, longitude, latitude)
      .self$precip_masked_dt <- data.table::merge(.self$precip_dt, .self$mask_dt,
                                                  by = c("longitude", "latitude"), all.x = TRUE)

      # Apply mask with a threshold of 0.8
      .self$precip_masked_dt[, tp := ifelse(country >= 0.8, tp, NA)]
    },

    max_consecutive_dry_days = function() {
      #' Calculate the maximum consecutive dry days
      #'
      #' This method calculates the maximum number of consecutive dry days (days with precipitation below a certain threshold)
      #' for each location and time period in the masked precipitation data.
      #'
      #' @return None
      #'
      .self$precip_masked_dt[, dry_day := ifelse(tp < 0.001, 1, 0)]
      .self$precip_masked_dt[, dry_spell := data.table::rleid(dry_day), by = .(longitude, latitude)]
      .self$precip_masked_dt[, dry_spell_length := .N, by = .(longitude, latitude, dry_spell)]
      .self$precip_masked_dt[dry_day == 0, dry_spell_length := 0]
      daily_max_dry_days <- .self$precip_masked_dt[, .(max_dry_days = max(dry_spell_length, na.rm = TRUE)),
                                                   by = .(longitude, latitude, time_day = as.Date(time))]
      .self$max_dry_days_dt <- daily_max_dry_days
    },

    drought_interpolate = function() {
      #' Interpolate drought data between years
      #'
      #' This method interpolates drought data for each month between years to provide a continuous dataset. It handles the
      #' interpolation using a linear weighting scheme between consecutive years and repeats the last year's values.

      interpolated_values <- list()
      years <- unique(lubridate::year(.self$max_dry_days_dt$time_day))

      for (i in seq_along(years)[-length(years)]) {
        year_k <- .self$max_dry_days_dt[lubridate::year(time_day) == years[i]]
        year_k_plus_1 <- .self$max_dry_days_dt[lubridate::year(time_day) == years[i + 1]]

        # Ensure the years have the same length for interpolation
        if (nrow(year_k) != nrow(year_k_plus_1)) {
          min_rows <- min(nrow(year_k), nrow(year_k_plus_1))
          year_k <- year_k[1:min_rows]
          year_k_plus_1 <- year_k_plus_1[1:min_rows]
        }

        for (month in 1:12) {
          weight1 <- (12 - month) / 12
          weight2 <- month / 12
          interpolated_value <- weight1 * year_k$max_dry_days + weight2 * year_k_plus_1$max_dry_days
          time <- as.Date(paste0(years[i], "-", sprintf("%02d", month), "-01"))
          interpolated_values[[length(interpolated_values) + 1]] <- data.table::data.table(
            longitude = year_k$longitude,
            latitude = year_k$latitude,
            time = time,
            max_dry_days = interpolated_value
          )
        }
      }

      # Handle the last year by repeating the values
      last_year <- .self$max_dry_days_dt[lubridate::year(time_day) == utils::tail(years, 1)]
      for (month in 1:12) {
        time <- as.Date(paste0(tail(years, 1), "-", sprintf("%02d", month), "-01"))
        interpolated_values[[length(interpolated_values) + 1]] <- data.table::data.table(
          longitude = last_year$longitude,
          latitude = last_year$latitude,
          time = time,
          max_dry_days = last_year$max_dry_days
        )
      }

      # Combine all interpolated data
      interpolated_dt <- data.table::rbindlist(interpolated_values, fill = TRUE)
      # Set the order of data to ensure consistency
      data.table::setorder(interpolated_dt, longitude, latitude, time)
      .self$max_dry_days_dt <- interpolated_dt
    },

    standardize_metric = function(reference_period = NULL) {
      #' Standardize the drought metric
      #'
      #' This method standardizes the drought metric (maximum consecutive dry days) based on a reference period.
      #' The standardization is performed by calculating z-scores relative to the reference period.
      #'
      #' @param reference_period A character vector specifying the start and end of the reference period.
      #'                         If NULL, the object's reference period is used.
      #' @return None
      #'
      if (!is.null(reference_period)) {
        .self$reference_period <- reference_period
      }
      # Ensure the 'time' column is of Date type
      .self$max_dry_days_dt[, time_day := as.Date(time_day)]
      # Resample monthly and calculate the maximum monthly value
      monthly_max_dry_days <- .self$max_dry_days_dt[, .(max_dry_days = max(max_dry_days, na.rm = TRUE)),
                                                    by = .(longitude, latitude, lubridate::year(time_day),
                                                           lubridate::month(time_day))]
      # Add a date column and ensure it is of Date type
      monthly_max_dry_days[, date := as.Date(paste0(year, "-", month, "-01"))]

      # Calculate reference statistics by month
      reference_data <- monthly_max_dry_days[lubridate::year(date) >= lubridate::year(as.Date(.self$reference_period[1])) &
                                               lubridate::year(date) <= lubridate::year(as.Date(.self$reference_period[2]))]
      reference_stats <- reference_data[, .(mean = mean(max_dry_days, na.rm = TRUE),
                                            sd = sd(max_dry_days, na.rm = TRUE)),
                                        by = .(longitude, latitude, month(date))]

      # Merge reference statistics with monthly data
      monthly_max_dry_days <- data.table::merge(monthly_max_dry_days, reference_stats,
                                                by = c("longitude", "latitude", "month"), all.x = TRUE)
      # Standardize dry days
      monthly_max_dry_days[, standardized_dry_days := (max_dry_days - mean) / sd]
      .self$standardized_dry_days_dt <- monthly_max_dry_days
    },

    visualize = function(selected_latitude, selected_longitude) {
      #' Visualize standardized drought anomalies over time
      #'
      #' This method creates a time series plot of the standardized drought anomalies for a specific location defined by
      #' latitude and longitude.
      #'
      #' @param selected_latitude A numeric value specifying the latitude of the location to visualize.
      #' @param selected_longitude A numeric value specifying the longitude of the location to visualize.
      #' @return A ggplot object showing the time series of standardized drought anomalies.
      #'
      location_data <- .self$standardized_dry_days_dt[latitude == selected_latitude &
                                                        longitude == selected_longitude]
      ggplot2::ggplot(location_data, ggplot2::aes(x = date, y = standardized_dry_days)) +
        ggplot2::geom_line() +
        ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        ggplot2::labs(title = paste("Standardized Dry Days for Latitude", selected_latitude, "and Longitude", selected_longitude),
                      x = "Time", y = "Standardized Dry Days") +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    },

    plot_anomaly_map = function(month_year) {
      #' Plot standardized drought anomaly map for a specific month and year
      #'
      #' This method creates a spatial plot of standardized drought anomalies for a specified month and year.
      #'
      #' @param month_year A string in the format "YYYY-MM" specifying the month and year for the anomaly map.
      #' @return A ggplot object showing the spatial distribution of standardized drought anomalies for the specified month and year.
      #'
      plot_data <- .self$standardized_dry_days_dt[format(date, "%Y-%m") == month_year]
      ggplot2::ggplot(plot_data, ggplot2::aes(x = longitude, y = latitude, fill = standardized_dry_days)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
        ggplot2::labs(title = paste("Standardized Drought Anomaly Map for", month_year),
                      x = "Longitude", y = "Latitude", fill = "Anomaly") +
        ggplot2::theme_minimal()
    }
  )
)
