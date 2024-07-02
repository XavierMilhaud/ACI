library(ncdf4)
library(data.table)
library(zoo)
library(lubridate)
library(ggplot2)
library(RcppRoll)

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
    initialize = function(u10_file, v10_file, mask_file, reference_period = c("1960-01-01", "1964-12-31")) {
      .self$u10_file <- u10_file
      .self$v10_file <- v10_file
      .self$mask_file <- mask_file
      .self$reference_period <- reference_period
      .self$load_data()
      .self$apply_mask()
    },
    
    load_data = function() {
      u10_nc <- nc_open(.self$u10_file)
      v10_nc <- nc_open(.self$v10_file)
      mask_nc <- nc_open(.self$mask_file)
      
      u10 <- ncvar_get(u10_nc, "u10")
      v10 <- ncvar_get(v10_nc, "v10")
      longitude <- ncvar_get(u10_nc, "longitude")
      latitude <- ncvar_get(u10_nc, "latitude")
      time <- ncvar_get(u10_nc, "time")
      
      time_units <- ncatt_get(u10_nc, "time", "units")$value
      time_origin <- sub("hours since ", "", time_units)
      time_origin <- as.POSIXct(time_origin, tz = "UTC")
      time_dates <- time_origin + hours(time)
      
      mask_data <- ncvar_get(mask_nc, "country")
      longitude_mask <- ncvar_get(mask_nc, "lon")
      latitude_mask <- ncvar_get(mask_nc, "lat")
      
      nc_close(u10_nc)
      nc_close(v10_nc)
      nc_close(mask_nc)
      
      .self$u10_dt <- data.table(
        expand.grid(
          longitude = longitude,
          latitude = latitude,
          time = time_dates
        ),
        u10 = as.vector(u10)
      )
      
      .self$v10_dt <- data.table(
        expand.grid(
          longitude = longitude,
          latitude = latitude,
          time = time_dates
        ),
        v10 = as.vector(v10)
      )
      
      .self$mask_dt <- data.table(
        expand.grid(
          longitude = longitude_mask,
          latitude = latitude_mask
        ),
        country = as.vector(mask_data)
      )
    },
    
    apply_mask = function() {
      setkey(.self$u10_dt, longitude, latitude)
      setkey(.self$v10_dt, longitude, latitude)
      setkey(.self$mask_dt, longitude, latitude)
      
      u10_masked_dt <- merge(.self$u10_dt, .self$mask_dt, by = c("longitude", "latitude"), all.x = TRUE)
      v10_masked_dt <- merge(.self$v10_dt, .self$mask_dt, by = c("longitude", "latitude"), all.x = TRUE)
      
      u10_masked_dt[, u10 := ifelse(country >= 0.8, u10, NA)]
      v10_masked_dt[, v10 := ifelse(country >= 0.8, v10, NA)]
      
      .self$u10_dt <- u10_masked_dt
      .self$v10_dt <- v10_masked_dt
    },
    
    wind_power = function(reference_period = NULL) {
      merged_dt <- merge(.self$u10_dt, .self$v10_dt, by = c("longitude", "latitude", "time"))
      merged_dt[, ws := sqrt(u10^2 + v10^2)]
      rho <- 1.23
      merged_dt[, daily_mean_ws := mean(ws, na.rm = TRUE), by = .(longitude, latitude, time = as.Date(time))]
      merged_dt[, wind_power := 0.5 * rho * daily_mean_ws^3]
      
      wind_power_dt <- merged_dt[, .(time, longitude, latitude, wind_power)]
      
      if (!is.null(reference_period)) {
        wind_power_dt <- wind_power_dt[time >= as.Date(reference_period[1]) & time <= as.Date(reference_period[2])]
      }
      
      .self$wind_power_dt <- wind_power_dt
      return(wind_power_dt)
    },
    
    wind_thresholds = function(reference_period) {
      wind_power_dt <- .self$wind_power(reference_period)
      wind_power_dt[, dayofyear := yday(time)]
      
      dset_mean <- wind_power_dt[, .(mean_power = mean(wind_power, na.rm = TRUE)), by = dayofyear]
      dset_std <- wind_power_dt[, .(std_power = sd(wind_power, na.rm = TRUE)), by = dayofyear]
      wind_power_thresholds <- merge(dset_mean, dset_std, by = "dayofyear")
      wind_power_thresholds[, threshold := mean_power + 1.28 * std_power]
      
      .self$wind_thresholds_dt <- wind_power_thresholds
      return(wind_power_thresholds)
    },
    
    days_above_thresholds = function(reference_period) {
      wind_power_thresholds <- .self$wind_thresholds(reference_period)
      wind_power_dt <- .self$wind_power()
      wind_power_dt[, dayofyear := yday(time)]
      wind_power_thresholds <- merge(wind_power_dt, wind_power_thresholds, by = "dayofyear")
      
      wind_power_thresholds[, days_above := ifelse(wind_power > threshold, 1, 0)]
      
      return(wind_power_thresholds)
    },
    
    wind_exceedance_frequency = function(reference_period) {
      days_above_dt <- .self$days_above_thresholds(reference_period)
      days_above_dt[, `:=`(latitude = latitude, longitude = longitude)]
      monthly_total_days_above <- days_above_dt[, .(frequency = sum(days_above, na.rm = TRUE) / .N), by = .(month = floor_date(time, "month"), latitude, longitude)]
      monthly_total_days_above[, month := as.Date(month)]
      
      return(monthly_total_days_above)
    },
    
    plot_wind_exceedance_frequency = function(lat, lon, reference_period) {
      monthly_total_days_above <- .self$wind_exceedance_frequency(reference_period)
      location_data <- monthly_total_days_above[latitude == lat & longitude == lon]
      
      if (nrow(location_data) == 0) {
        cat("No data available for the specified latitude and longitude.\n")
        return(NULL)
      }
      
      ggplot(location_data, aes(x = month, y = frequency)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        labs(title = paste("Wind Exceedance Frequency for Latitude", lat, "and Longitude", lon),
             x = "Time", y = "Frequency") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    },
    
    plot_wind_exceedance_frequency_map = function(reference_period) {
      monthly_total_days_above <- .self$wind_exceedance_frequency(reference_period)
      
      ggplot(monthly_total_days_above, aes(x = longitude, y = latitude, fill = frequency)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
        labs(title = "Wind Exceedance Frequency Map",
             x = "Longitude", y = "Latitude", fill = "Frequency") +
        theme_minimal() +
        coord_fixed()
    }
  )
)
