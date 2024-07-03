library(ncdf4)
library(data.table)
library(zoo)
library(lubridate)
library(ggplot2)
library(RcppRoll)
library(dplyr)

source("temperaturecomponent.R")
source("precipitationcomponent.R")
source("droughtcomponent.R")
source("windcomponent.R")
source("sealevel.R")

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
      .self$temperature_component <- TemperatureComponent$new(temperature_data_path, mask_data_path)
      .self$precipitation_component <- PrecipitationComponent$new(precipitation_data_path, mask_data_path)
      .self$drought_component <- DroughtComponent$new(precipitation_data_path, mask_data_path)
      .self$wind_component <- WindComponent$new(wind_u10_data_path, wind_v10_data_path, mask_data_path)
      .self$sealevel_component <- SeaLevelComponent$new(country_abbrev, study_period, reference_period)
      .self$study_period <- study_period
      .self$reference_period <- reference_period
    },

    ACI = function(factor = 1) {
      # Calculate anomalies and convert to DataFrames
      preci_std <- .self$precipitation_component$calculate_monthly_max("tp", 5, .self$reference_period, TRUE)
      p_df <- as.data.frame(preci_std)
      colnames(p_df) <- c("time", "precipitation")

      wind_std <- .self$wind_component$wind_exceedance_frequency(.self$reference_period, TRUE)
      w_df <- as.data.frame(wind_std)
      colnames(w_df) <- c("time", "windpower")

      drought_std <- .self$drought_component$standardize_metric(.self$reference_period, TRUE)
      cdd_df <- as.data.frame(drought_std)
      colnames(cdd_df) <- c("time", "drought")

      temp90_std <- .self$temperature_component$std_t90(.self$reference_period, TRUE)
      t90_df <- as.data.frame(temp90_std)
      colnames(t90_df) <- c("time", "T90")

      temp10_std <- .self$temperature_component$std_t10(.self$reference_period, TRUE)
      t10_df <- as.data.frame(temp10_std)
      colnames(t10_df) <- c("time", "T10")

      sea_lev <- .self$sealevel_component$process()
      sea_std <- rowMeans(sea_lev, na.rm = TRUE)
      sea_df <- data.frame(time = as.Date(rownames(sea_lev)) + months(1) - days(1), sea_mean = sea_std)

      # Merge DataFrames
      df1 <- merge(w_df, p_df, by = "time")
      df2 <- merge(df1, cdd_df, by = "time")
      df3 <- merge(df2, sea_df, by = "time")
      df4 <- merge(df3, t90_df, by = "time")
      IACF_composites <- merge(df4, t10_df, by = "time")

      # Calculate ACI
      IACF_composites$ACI <- (IACF_composites$T90 - IACF_composites$T10 +
                                IACF_composites$precipitation + IACF_composites$drought +
                                factor * IACF_composites$sea_mean + IACF_composites$windpower) / 6

      return(IACF_composites)
    }
  )
)

