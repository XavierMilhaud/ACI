library(data.table)
library(lubridate)

source("windcomponent.R")
source("temperaturecomponent.R")
source("sealevel.R")
source("droughtcomponent.R")
source("precipitationcomponent.R")

ActuarialClimateIndex <- setRefClass(
  "ActuarialClimateIndex",
  fields = list(
    temperature_component = "ANY",
    precipitation_component = "ANY",
    drought_component = "ANY",
    wind_component = "ANY",
    sealevel_component = "ANY",
    study_period = "character",
    reference_period = "character"
  ),
  methods = list(
    initialize = function(temperature_data_path, precipitation_data_path, wind_u10_data_path, 
                          wind_v10_data_path, country_abbrev, mask_data_path, study_period, reference_period) {
      temperature_component <<- TemperatureComponent$new(temperature_data_path, mask_data_path, reference_period)
      precipitation_component <<- PrecipitationComponent$new(precipitation_data_path, mask_data_path, reference_period)
      drought_component <<- DroughtComponent$new(precipitation_data_path, mask_data_path, reference_period)
      wind_component <<- WindComponent$new(wind_u10_data_path, wind_v10_data_path, mask_data_path, reference_period)
      sealevel_component <<- SeaLevelComponent$new(country_abbrev, study_period, reference_period)
      study_period <<- study_period
      reference_period <<- reference_period
    },
    
    ACI = function(factor = 1) {
      # Calculate anomalies for each component
      preci_std <- precipitation_component$monthly_max_anomaly("tp", 5, reference_period)
      p_dt <- preci_std[, .(time, latitude, longitude, precipitation = value)]
      
      wind_std <- wind_component$std_wind_exceedance_frequency(reference_period)
      w_dt <- wind_std[, .(time, latitude, longitude, windpower = std_frequency)]
      
      drought_std <- drought_component$std_max_consecutive_dry_days(reference_period)
      cdd_dt <- drought_std[, .(time, latitude, longitude, drought = std_cdd)]
      
      temp90_std <- temperature_component$std_t90(reference_period)
      t90_dt <- temp90_std[, .(time, latitude, longitude, T90 = std_t90)]
      
      temp10_std <- temperature_component$std_t10(reference_period)
      t10_dt <- temp10_std[, .(time, latitude, longitude, T10 = std_t10)]
      
      sea_lev <- sealevel_component$process()
      sea_std <- sea_lev[, .(time, sea_mean = mean(value, na.rm = TRUE))]
      sea_dt <- data.table(time = sea_std$time, sea_mean = sea_std$sea_mean)
      
      # Merge DataTables
      merged_dt <- merge(w_dt, p_dt, by = c("time", "latitude", "longitude"))
      merged_dt <- merge(merged_dt, cdd_dt, by = c("time", "latitude", "longitude"))
      merged_dt <- merge(merged_dt, sea_dt, by = "time")
      merged_dt <- merge(merged_dt, t90_dt, by = c("time", "latitude", "longitude"))
      merged_dt <- merge(merged_dt, t10_dt, by = c("time", "latitude", "longitude"))
      
      # Calculate ACI
      merged_dt[, ACI := (T90 - T10 + precipitation + drought + factor * sea_mean + windpower) / 6]
      
      return(merged_dt)
    }
  )
)