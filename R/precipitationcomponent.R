library(ncdf4)
library(data.table)
library(zoo)
library(lubridate)
library(ggplot2)
library(RcppRoll)

PrecipitationComponent <- setRefClass(
  "PrecipitationComponent",
  fields = list(
    precipitation_file = "character",
    mask_file = "character",
    reference_period = "character",
    precip_dt = "data.table",
    mask_dt = "data.table",
    monthly_max = "data.table"
  ),
  methods = list(
    initialize = function(precipitation_file, mask_file, reference_period = c("1961-01-01", "1990-12-31")) {
      .self$precipitation_file <- precipitation_file
      .self$mask_file <- mask_file
      .self$reference_period <- reference_period
      .self$load_data()
      .self$apply_mask()
    },
    
    load_data = function() {
      precip_nc <- nc_open(.self$precipitation_file)
      mask_nc <- nc_open(.self$mask_file)
      
      precip <- ncvar_get(precip_nc, "tp")
      longitude <- ncvar_get(precip_nc, "longitude")
      latitude <- ncvar_get(precip_nc, "latitude")
      time <- ncvar_get(precip_nc, "time")
      
      time_units <- ncatt_get(precip_nc, "time", "units")$value
      time_origin <- sub("days since ", "", time_units)
      time_origin <- as.Date(time_origin, format="%Y-%m-%d")
      time_dates <- time_origin + time
      
      mask_data <- ncvar_get(mask_nc, "country")
      longitude_mask <- ncvar_get(mask_nc, "lon")
      latitude_mask <- ncvar_get(mask_nc, "lat")
      
      nc_close(precip_nc)
      nc_close(mask_nc)
      
      .self$precip_dt <- data.table(
        expand.grid(
          longitude = longitude,
          latitude = latitude,
          time = time_dates
        ),
        tp = as.vector(precip)
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
      setkey(.self$precip_dt, longitude, latitude)
      setkey(.self$mask_dt, longitude, latitude)
      
      precip_masked_dt <- merge(.self$precip_dt, .self$mask_dt, by = c("longitude", "latitude"), all.x = TRUE)
      precip_masked_dt[, tp := ifelse(country >= 0.8, tp, NA)]
      .self$precip_dt <- precip_masked_dt
    },
    
    calculate_monthly_max = function() {
      .self$precip_dt[, year := year(time)]
      .self$precip_dt[, month := month(time)]
      .self$precip_dt[, dayofyear := yday(time)]
      
      # Calculate the 5-day running sum
      .self$precip_dt[, roll_sum := rollsum(tp, 5, fill = NA, align = "right"), by = .(longitude, latitude)]
      
      # Aggregate to monthly maximum of 5-day running sum
      monthly_max <- .self$precip_dt[, .(rx5day = max(roll_sum, na.rm = TRUE)), by = .(longitude, latitude, year, month)]
      
      # Calculate anomalies with respect to the reference period
      reference_data <- monthly_max[year >= year(as.Date(.self$reference_period[1])) & year <= year(as.Date(.self$reference_period[2]))]
      reference_stats <- reference_data[, .(mean_rx5day = mean(rx5day, na.rm = TRUE), sd_rx5day = sd(rx5day, na.rm = TRUE)), by = .(month, longitude, latitude)]
      
      # Merge and calculate the anomalies
      monthly_max <- merge(monthly_max, reference_stats, by = c("month", "longitude", "latitude"))
      monthly_max[, rx5day_anomaly := (rx5day - mean_rx5day) / sd_rx5day]
      
      .self$monthly_max <- monthly_max[, .(year, month, longitude, latitude, rx5day = rx5day_anomaly)]
    },
    
    visualize = function(selected_latitude, selected_longitude) {
      location_data <- .self$monthly_max[latitude == selected_latitude & longitude == selected_longitude]
      
      ggplot(location_data, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = rx5day)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        labs(title = paste("Standardized Monthly Maximum Precipitation for Latitude", selected_latitude, "and Longitude", selected_longitude),
             x = "Time", y = "Standardized Monthly Maximum Precipitation") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  )
)
