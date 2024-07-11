library(ncdf4)
library(data.table)
library(zoo)
library(lubridate)
library(ggplot2)
library(RcppRoll)

TemperatureComponent <- setRefClass(
  "TemperatureComponent",
  fields = list(
    temperature_file = "character",
    mask_file = "character",
    reference_period = "character",
    temperature_dt = "data.table",
    mask_dt = "data.table"
  ),
  methods = list(
    initialize = function(temperature_file, mask_file, reference_period) {
      .self$temperature_file <- temperature_file
      .self$mask_file <- mask_file
      .self$reference_period <- reference_period
      .self$load_data()
      .self$apply_mask()
    },
    
    load_data = function() {
      temperature_nc <- nc_open(.self$temperature_file)
      mask_nc <- nc_open(.self$mask_file)
      
      temperature <- ncvar_get(temperature_nc, "t2m")
      longitude <- ncvar_get(temperature_nc, "longitude")
      latitude <- ncvar_get(temperature_nc, "latitude")
      time <- ncvar_get(temperature_nc, "time")
      
      time_units <- ncatt_get(temperature_nc, "time", "units")$value
      print(time_units)
      time_origin <- sub("days since ", "", time_units)
      time_origin <- as.Date(time_origin, format="%Y-%m-%d")
      time_dates <- time_origin + time
      
      mask_data <- ncvar_get(mask_nc, "country")
      longitude_mask <- ncvar_get(mask_nc, "lon")
      latitude_mask <- ncvar_get(mask_nc, "lat")
      
      nc_close(temperature_nc)
      nc_close(mask_nc)
      
      .self$temperature_dt <- data.table(
        expand.grid(
          longitude = longitude,
          latitude = latitude,
          time = time_dates
        ),
        t2m = as.vector(temperature)
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
      setkey(.self$temperature_dt, longitude, latitude)
      setkey(.self$mask_dt, longitude, latitude)
      
      temperature_masked_dt <- merge(.self$temperature_dt, .self$mask_dt, by = c("longitude", "latitude"), all.x = TRUE)
      temperature_masked_dt[, t2m := ifelse(country >= 0.8, t2m, NA)]
      .self$temperature_dt <- temperature_masked_dt
    },
    
    convert_to_posixct = function(dt) {
      if (class(dt$time)[1] != "POSIXct") {
        dt$time <- as.POSIXct(dt$time, origin="1970-01-01", tz="UTC")
      }
      return(dt)
    },
    
    temp_extremum = function(temp_data, extremum) {
      if (extremum == "max") {
        temp_extremum <- temp_data[, .(temp_extremum = max(t2m, na.rm = TRUE)), by = .(longitude, latitude, time)]
      } else if (extremum == "min") {
        temp_extremum <- temp_data[, .(temp_extremum = min(t2m, na.rm = TRUE)), by = .(longitude, latitude, time)]
      }
      return(temp_extremum)
    },
    
    percentiles = function(temp_data, n, reference_period) {
      temp_reference <- temp_data[time >= as.Date(reference_period[1]) & time <= as.Date(reference_period[2])]
      temp_reference[, dayofyear := yday(time)]
      
      percentile_reference <- temp_reference[, .(percentile = quantile(t2m, probs = n/100, na.rm = TRUE)), by = .(longitude, latitude, dayofyear)]
      return(percentile_reference)
    },
    
    t90 = function(reference_period) {
      temperature_days_max <- .self$temp_extremum(.self$temperature_dt, "max")
      temperature_days_max[, dayofyear := yday(time)]
      temperature_days_max <- .self$convert_to_posixct(temperature_days_max)
      
      percentile_90_calendar_days <- .self$percentiles(.self$temperature_dt, 90, reference_period)
      percentile_90_calendar_days <- .self$convert_to_posixct(percentile_90_calendar_days)
      
      percentile_90_calendar_days <- percentile_90_calendar_days[, .(longitude, latitude, dayofyear, percentile)]
      temperature_days_max <- merge(temperature_days_max, percentile_90_calendar_days, by = c("longitude", "latitude", "dayofyear"), all.x = TRUE)
      
      temperature_days_max[, difference := temp_extremum - percentile]
      temperature_days_max[, difference := as.double(difference)]
      
      days_90_above_thresholds <- temperature_days_max[, .(days_above = as.numeric(ifelse(difference > 0, 1, 0))), by = .(longitude, latitude, time)]
      
      tx90 <- days_90_above_thresholds[, .(frequency = sum(days_above, na.rm = TRUE) / .N), by = .(month = floor_date(time, "month"))]
      
      temperature_nights_max <- .self$temp_extremum(.self$temperature_dt, "max")
      temperature_nights_max[, dayofyear := yday(time)]
      temperature_nights_max <- .self$convert_to_posixct(temperature_nights_max)
      
      percentile_90_calendar_nights <- .self$percentiles(.self$temperature_dt, 90, reference_period)
      percentile_90_calendar_nights <- .self$convert_to_posixct(percentile_90_calendar_nights)
      
      percentile_90_calendar_nights <- percentile_90_calendar_nights[, .(longitude, latitude, dayofyear, percentile)]
      temperature_nights_max <- merge(temperature_nights_max, percentile_90_calendar_nights, by = c("longitude", "latitude", "dayofyear"), all.x = TRUE)
      
      temperature_nights_max[, difference := temp_extremum - percentile]
      temperature_nights_max[, difference := as.double(difference)]
      
      nights_90_above_thresholds <- temperature_nights_max[, .(days_above = as.numeric(ifelse(difference > 0, 1, 0))), by = .(longitude, latitude, time)]
      
      tn90 <- nights_90_above_thresholds[, .(frequency = sum(days_above, na.rm = TRUE) / .N), by = .(month = floor_date(time, "month"))]
      
      t90_values <- merge(tx90, tn90, by = "month", suffixes = c("_day", "_night"))
      t90_values[, t90 := 0.5 * (frequency_day + frequency_night)]
      
      return(t90_values)
    },
    
    t10 = function(reference_period) {
      temperature_days_min <- .self$temp_extremum(.self$temperature_dt, "min")
      temperature_days_min[, dayofyear := yday(time)]
      temperature_days_min <- .self$convert_to_posixct(temperature_days_min)
      
      percentile_10_calendar_days <- .self$percentiles(.self$temperature_dt, 10, reference_period)
      percentile_10_calendar_days <- .self$convert_to_posixct(percentile_10_calendar_days)
      
      percentile_10_calendar_days <- percentile_10_calendar_days[, .(longitude, latitude, dayofyear, percentile)]
      temperature_days_min <- merge(temperature_days_min, percentile_10_calendar_days, by = c("longitude", "latitude", "dayofyear"), all.x = TRUE)
      
      temperature_days_min[, difference := temp_extremum - percentile]
      temperature_days_min[, difference := as.double(difference)]
      
      days_10_below_thresholds <- temperature_days_min[, .(days_below = as.numeric(ifelse(difference < 0, 1, 0))), by = .(longitude, latitude, time)]
      
      tx10 <- days_10_below_thresholds[, .(frequency = sum(days_below, na.rm = TRUE) / .N), by = .(month =       floor_date(time, "month"))]
      
      temperature_nights_min <- .self$temp_extremum(.self$temperature_dt, "min")
      temperature_nights_min[, dayofyear := yday(time)]
      temperature_nights_min <- .self$convert_to_posixct(temperature_nights_min)
      
      percentile_10_calendar_nights <- .self$percentiles(.self$temperature_dt, 10, reference_period)
      percentile_10_calendar_nights <- .self$convert_to_posixct(percentile_10_calendar_nights)
      
      percentile_10_calendar_nights <- percentile_10_calendar_nights[, .(longitude, latitude, dayofyear, percentile)]
      temperature_nights_min <- merge(temperature_nights_min, percentile_10_calendar_nights, by = c("longitude", "latitude", "dayofyear"), all.x = TRUE)
      
      temperature_nights_min[, difference := temp_extremum - percentile]
      temperature_nights_min[, difference := as.double(difference)]
      
      nights_10_below_thresholds <- temperature_nights_min[, .(days_below = as.numeric(ifelse(difference < 0, 1, 0))), by = .(longitude, latitude, time)]
      
      tn10 <- nights_10_below_thresholds[, .(frequency = sum(days_below, na.rm = TRUE) / .N), by = .(month = floor_date(time, "month"))]
      
      t10_values <- merge(tx10, tn10, by = "month", suffixes = c("_day", "_night"))
      t10_values[, t10 := 0.5 * (frequency_day + frequency_night)]
      
      return(t10_values)
    },
    
    standardize_metric = function(metric_values, reference_period) {
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
      t90_values <- .self$t90(reference_period)
      standardized_t90 <- .self$standardize_metric(t90_values, reference_period)
      return(standardized_t90)
    },
    
    std_t10 = function(reference_period) {
      t10_values <- .self$t10(reference_period)
      standardized_t10 <- .self$standardize_metric(t10_values, reference_period)
      return(standardized_t10)
    },
    
    days_above_thresholds = function(reference_period) {
      temperature_days_max <- .self$temp_extremum(.self$temperature_dt, "max")
      temperature_days_max[, dayofyear := yday(time)]
      temperature_days_max <- .self$convert_to_posixct(temperature_days_max)
      
      percentile_90_calendar_days <- .self$percentiles(.self$temperature_dt, 90, reference_period)
      percentile_90_calendar_days <- .self$convert_to_posixct(percentile_90_calendar_days)
      
      percentile_90_calendar_days <- percentile_90_calendar_days[, .(longitude, latitude, dayofyear, percentile)]
      temperature_days_max <- merge(temperature_days_max, percentile_90_calendar_days, by = c("longitude", "latitude", "dayofyear"), all.x = TRUE)
      
      temperature_days_max[, difference := temp_extremum - percentile]
      temperature_days_max[, difference := as.double(difference)]
      
      days_above_thresholds <- temperature_days_max[, .(days_above = as.numeric(ifelse(difference > 0, 1, 0))), by = .(longitude, latitude, time)]
      return(days_above_thresholds)
    },
    
    days_below_thresholds = function(reference_period) {
      temperature_days_min <- .self$temp_extremum(.self$temperature_dt, "min")
      temperature_days_min[, dayofyear := yday(time)]
      temperature_days_min <- .self$convert_to_posixct(temperature_days_min)
      
      percentile_10_calendar_days <- .self$percentiles(.self$temperature_dt, 10, reference_period)
      percentile_10_calendar_days <- .self$convert_to_posixct(percentile_10_calendar_days)
      
      percentile_10_calendar_days <- percentile_10_calendar_days[, .(longitude, latitude, dayofyear, percentile)]
      temperature_days_min <- merge(temperature_days_min, percentile_10_calendar_days, by = c("longitude", "latitude", "dayofyear"), all.x = TRUE)
      
      temperature_days_min[, difference := temp_extremum - percentile]
      temperature_days_min[, difference := as.double(difference)]
      
      days_below_thresholds <- temperature_days_min[, .(days_below = as.numeric(ifelse(difference < 0, 1, 0))), by = .(longitude, latitude, time)]
      return(days_below_thresholds)
    },
    
    plot_standardized_components = function(standardized_t10, standardized_t90, n) {
      standardized_t10[, month := as.Date(month)]
      standardized_t90[, month := as.Date(month)]
      
      standardized_t10[, rolling_mean := zoo::rollmean(standardized, k = n, fill = NA, align = "center")]
      standardized_t90[, rolling_mean := zoo::rollmean(standardized, k = n, fill = NA, align = "center")]
      
      ggplot() +
        geom_line(data = standardized_t10, aes(x = month, y = rolling_mean, color = "Standardized T10")) +
        geom_line(data = standardized_t90, aes(x = month, y = rolling_mean, color = "Standardized T90")) +
        labs(title = "Rolling Mean of Standardized T10 and T90", x = "Time", y = "Standardized Temperature") +
        theme_minimal() +
        theme(legend.title = element_blank())
    },
    
    plot_standardized_components_simple = function(standardized_t90, standardized_t10) {
      combined_data <- rbind(
        data.table(month = standardized_t90$month, value = standardized_t90$standardized, metric = "T90"),
        data.table(month = standardized_t10$month, value = standardized_t10$standardized, metric = "T10")
      )
      
      ggplot(combined_data, aes(x = month, y = value, color = metric)) +
        geom_line() +
        labs(title = "Standardized T90 and T10 over Time",
             x = "Time",
             y = "Standardized Value",
             color = "Metric") +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  )
)
