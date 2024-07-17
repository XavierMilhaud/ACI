library(ncdf4)
library(lubridate)
library(data.table)

WindComponent <- setRefClass(
  "WindComponent",
  fields = list(
    data_dt = "data.table",
    times = "Date",
    latitudes = "numeric",
    longitudes = "numeric"
  ),
  methods = list(
    initialize = function(u10_path, v10_path, mask_path) {
      # Function to convert NetCDF to data.table
      nc_to_dt <- function(nc_file, var_name, time_var = "time", lat_var = "latitude", lon_var = "longitude") {
        nc_data <- ncdf4::nc_open(nc_file)
        var_data <- ncvar_get(nc_data, var_name)
        
        if (!is.null(time_var) && time_var %in% names(nc_data$dim)) {
          time <- as.Date(ncvar_get(nc_data, time_var), origin = "2000-01-01")
        } else {
          time <- NULL
        }
        
        latitude <- as.numeric(ncvar_get(nc_data, lat_var))
        longitude <- as.numeric(ncvar_get(nc_data, lon_var))
        
        nc_close(nc_data)
        
        if (!is.null(time)) {
          dt <- data.table(
            time = rep(time, each = length(latitude) * length(longitude)),
            latitude = rep(rep(latitude, each = length(longitude)), times = length(time)),
            longitude = rep(longitude, times = length(latitude) * length(time)),
            var = as.vector(var_data)
          )
        } else {
          dt <- data.table(
            latitude = rep(latitude, each = length(longitude)),
            longitude = rep(longitude, times = length(latitude)),
            var = as.vector(var_data)
          )
        }
        
        return(dt)
      }
      
      u10_dt <- nc_to_dt(u10_path, "u10", time_var = "time", lat_var = "latitude", lon_var = "longitude")
      v10_dt <- nc_to_dt(v10_path, "v10", time_var = "time", lat_var = "latitude", lon_var = "longitude")
      mask_dt <- nc_to_dt(mask_path, "country", time_var = NULL, lat_var = "lat", lon_var = "lon")
      
      setnames(u10_dt, "var", "u10")
      setnames(v10_dt, "var", "v10")
      setnames(mask_dt, "var", "mask")
      
      mask_dt <- mask_dt[, .(latitude, longitude, mask)]
      
      data <- merge(u10_dt, v10_dt, by = c("time", "latitude", "longitude"))
      data <- merge(data, mask_dt, by = c("latitude", "longitude"))
      
      data[, u10 := u10 * mask]
      data[, v10 := v10 * mask]
      
      .self$data_dt <<- data
      .self$times <<- unique(data$time)
      .self$latitudes <<- unique(data$latitude)
      .self$longitudes <<- unique(data$longitude)
    },
    
    wind_power = function() {
      data_dt <- .self$data_dt
      data_dt[, ws := sqrt(u10^2 + v10^2)]
      
      dailymean_ws <- data_dt[, .(dailymean_ws = mean(ws, na.rm = TRUE)), by = .(time, latitude, longitude)]
      
      rho <- 1.23  # Air density constant
      dailymean_ws[, wind_power := 0.5 * rho * dailymean_ws^3]
      
      return(dailymean_ws)
    },
    
    wind_thresholds = function(reference_period) {
      wind_power_data <- .self$wind_power()
      wind_power_data[, dayofyear := yday(time)]
      
      reference_data <- wind_power_data[time >= as.Date(reference_period[1]) & time <= as.Date(reference_period[2])]
      reference_stats <- reference_data[, .(mean_wind_power = mean(wind_power, na.rm = TRUE), 
                                            std_wind_power = sd(wind_power, na.rm = TRUE)), 
                                        by = .(dayofyear, latitude, longitude)]
      
      wind_power_data <- merge(wind_power_data, reference_stats, by = c("dayofyear", "latitude", "longitude"), all.x = TRUE)
      wind_power_data[, threshold := mean_wind_power + 1.28 * std_wind_power]
      
      return(wind_power_data[, .(time, latitude, longitude, threshold)])
    },
    
    days_above_thresholds = function(reference_period) {
      wind_power_data <- .self$wind_power()
      thresholds <- .self$wind_thresholds(reference_period)
      
      days_above <- merge(wind_power_data, thresholds, by = c("time", "latitude", "longitude"))
      days_above[, days_above_threshold := ifelse(wind_power > threshold, 1, 0)]
      
      return(days_above[, .(time, latitude, longitude, days_above_threshold)])
    },
    
    wind_exceedance_frequency = function(reference_period) {
      days_above <- .self$days_above_thresholds(reference_period)
      days_above[, year_month := floor_date(time, "month")]
      exceedance_freq <- days_above[, .(exceedance_frequency = sum(days_above_threshold, na.rm = TRUE) / .N), 
                                    by = .(year_month, latitude, longitude)]
      return(exceedance_freq)
    },
    
    std_wind_exceedance_frequency = function(reference_period) {
      exceedance_freq <- .self$wind_exceedance_frequency(reference_period)
      mean_frequency <- mean(exceedance_freq$exceedance_frequency, na.rm = TRUE)
      std_frequency <- sd(exceedance_freq$exceedance_frequency, na.rm = TRUE)
      exceedance_freq[, std_frequency := (exceedance_frequency - mean_frequency) / std_frequency]
      return(exceedance_freq)
    }
  )
)
