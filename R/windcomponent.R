library(ncdf4)
library(data.table)
library(lubridate)

WindComponent <- setRefClass(
  "WindComponent",
  fields = list(
    u10_path = "character",
    v10_path = "character",
    mask_path = "character",
    reference_period = "character",
    u10 = "array",
    v10 = "array",
    mask = "array",
    time_index = "Date"
  ),
  methods = list(
    initialize = function(u10_path, v10_path, mask_path, reference_period) {
      .self$u10_path <<- u10_path
      .self$v10_path <<- v10_path
      .self$mask_path <<- mask_path
      .self$reference_period <<- reference_period
      .self$load_data()
    },
    
    load_data = function() {
      u10_nc <- ncdf4::nc_open(.self$u10_path)
      v10_nc <- ncdf4::nc_open(.self$v10_path)
      mask_nc <- ncdf4::nc_open(.self$mask_path)
      
      .self$u10 <<- ncdf4::ncvar_get(u10_nc, "u10")
      .self$v10 <<- ncdf4::ncvar_get(v10_nc, "v10")
      .self$mask <<- ncdf4::ncvar_get(mask_nc, "country")
      .self$time_index <<- as.Date(ncdf4::ncvar_get(u10_nc, "time"), origin="1970-01-01")
      
      ncdf4::nc_close(u10_nc)
      ncdf4::nc_close(v10_nc)
      ncdf4::nc_close(mask_nc)
    },
    
    wind_power = function() {
      sqrt(.self$u10^2 + .self$v10^2)
    },
    
    wind_thresholds = function(reference_period) {
      start_date <- as.Date(reference_period[1])
      end_date <- as.Date(reference_period[2])
      
      time_index_subset <- .self$time_index[.self$time_index >= start_date & .self$time_index <= end_date]
      wind_power_subset <- .self$wind_power()[, , which(.self$time_index %in% time_index_subset)]
      
      threshold <- apply(wind_power_subset, c(2, 3), function(x) quantile(x, probs = 0.9))
      
      dt <- data.table(
        time = rep(time_index_subset, each = length(.self$mask)),
        longitude = rep(seq(2.20, 2.30, by = 0.1), each = length(.self$mask) * length(time_index_subset)),
        latitude = rep(seq(48.80, 48.90, by = 0.1), times = length(.self$mask) * length(time_index_subset)),
        threshold = as.vector(threshold)
      )
      
      dt
    },
    
    days_above_thresholds = function(reference_period) {
      thresholds <- .self$wind_thresholds(reference_period)$threshold
      wind_power <- .self$wind_power()
      
      days_above <- apply(wind_power, c(2, 3), function(x) sum(x > thresholds))
      
      dt <- data.table(
        time = seq.Date(as.Date(reference_period[1]), as.Date(reference_period[2]), by = "day"),
        longitude = rep(seq(2.20, 2.30, by = 0.1), each = length(days_above)),
        latitude = rep(seq(48.80, 48.90, by = 0.1), times = length(days_above)),
        days_above_threshold = as.vector(days_above)
      )
      
      dt
    },
    
    wind_exceedance_frequency = function(reference_period) {
      days_above <- .self$days_above_thresholds(reference_period)
      total_days <- length(seq.Date(as.Date(reference_period[1]), as.Date(reference_period[2]), by = "day"))
      
      dt <- days_above[, .(
        exceedance_frequency = days_above_threshold / total_days
      ), by = .(longitude, latitude)]
      
      dt
    },
    
    std_wind_exceedance_frequency = function(reference_period) {
      exceedance_frequency <- .self$wind_exceedance_frequency(reference_period)$exceedance_frequency
      
      mean_freq <- mean(exceedance_frequency, na.rm = TRUE)
      std_freq <- sd(exceedance_frequency, na.rm = TRUE)
      
      std_frequency <- (exceedance_frequency - mean_freq) / std_freq
      
      dt <- data.table(
        time = seq.Date(as.Date(reference_period[1]), as.Date(reference_period[2]), by = "month"),
        longitude = rep(seq(2.20, 2.30, by = 0.1), each = length(std_frequency)),
        latitude = rep(seq(48.80, 48.90, by = 0.1), times = length(std_frequency)),
        std_frequency = as.vector(std_frequency)
      )
      
      dt
    }
  )
)
