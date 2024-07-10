library(ncdf4)
library(lubridate)
library(data.table)

WindComponent <- setRefClass(
  "WindComponent",
  fields = list(
    u10 = "array",
    v10 = "array",
    mask = "matrix",
    times = "Date",
    latitudes = "numeric",
    longitudes = "numeric"
  ),
  methods = list(
    initialize = function(u10_path, v10_path, mask_path, reference_period) {
      u10_data <- ncdf4::nc_open(u10_path)
      v10_data <- ncdf4::nc_open(v10_path)
      mask_data <- ncdf4::nc_open(mask_path)
      
      .self$u10 <- ncdf4::ncvar_get(u10_data, "u10")
      .self$v10 <- ncdf4::ncvar_get(v10_data, "v10")
      .self$mask <- ncdf4::ncvar_get(mask_data, "country")
      
      .self$times <- as.Date(ncdf4::ncvar_get(u10_data, "time"), origin = "2000-01-01")
      .self$latitudes <- as.numeric(ncdf4::ncvar_get(u10_data, "lat"))
      .self$longitudes <- as.numeric(ncdf4::ncvar_get(u10_data, "lon"))
      
      ncdf4::nc_close(u10_data)
      ncdf4::nc_close(v10_data)
      ncdf4::nc_close(mask_data)
    },
    
    apply_mask = function(data) {
      mask_expanded <- array(.self$mask, dim = dim(data))
      masked_data <- data * mask_expanded
      return(masked_data)
    },
    
    wind_power = function(reference_period = NULL) {
      u10_masked <- .self$apply_mask(.self$u10)
      v10_masked <- .self$apply_mask(.self$v10)
      
      ws <- sqrt(u10_masked^2 + v10_masked^2)
      rho <- 1.23  # Air density constant
      
      dailymean_ws <- apply(ws, c(2, 3), function(x) tapply(x, format(.self$times, "%Y-%m-%d"), mean))
      wind_power <- 0.5 * rho * dailymean_ws^3
      
      if (!is.null(reference_period)) {
        start_date <- as.Date(reference_period[1])
        end_date <- as.Date(reference_period[2])
        time_indices <- which(.self$times >= start_date & .self$times <= end_date)
        wind_power <- wind_power[time_indices, , drop = FALSE]
      }
      
      # Transform to data.table
      wind_power_dt <- data.table::data.table(
        time = rep(as.Date(rownames(wind_power)), each = ncol(wind_power)),
        latitude = rep(.self$latitudes, times = nrow(wind_power)),
        longitude = rep(.self$longitudes, each = nrow(wind_power)),
        wind_power = as.vector(wind_power)
      )
      
      return(wind_power_dt)
    },
    
    wind_thresholds = function(reference_period) {
      wind_power_data <- .self$wind_power(reference_period)
      threshold <- apply(wind_power_data, c(2, 3), function(x) quantile(x, 0.95))
      return(threshold)
    },
    
    days_above_thresholds = function(reference_period) {
      wind_power_data <- .self$wind_power(reference_period)
      thresholds <- .self$wind_thresholds(reference_period)
      
      days_above <- array(0, dim = dim(wind_power_data))
      for (i in 1:dim(wind_power_data)[1]) {
        for (j in 1:dim(wind_power_data)[2]) {
          for (k in 1:dim(wind_power_data)[3]) {
            if (wind_power_data[i, j, k] > thresholds[j, k]) {
              days_above[i, j, k] <- 1
            }
          }
        }
      }
      return(days_above)
    },
    
    wind_exceedance_frequency = function(reference_period) {
      days_above <- .self$days_above_thresholds(reference_period)
      exceedance_frequency <- apply(days_above, c(2, 3), mean)
      return(exceedance_frequency)
    },
    
    std_wind_exceedance_frequency = function(reference_period) {
      exceedance_frequency <- .self$wind_exceedance_frequency(reference_period)
      mean_frequency <- mean(exceedance_frequency)
      std_frequency <- sd(exceedance_frequency)
      standardized_frequency <- (exceedance_frequency - mean_frequency) / std_frequency
      return(standardized_frequency)
    }
  )
)
