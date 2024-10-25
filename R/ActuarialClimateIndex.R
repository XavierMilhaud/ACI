
#' ActuarialClimateIndex Class
#'
#' This class represents the Actuarial Climate Index, which is calculated from several climate components such as temperature, precipitation, drought, wind, and sea level.
#'
#' @field temperature_component Temperature component of type ANY
#' @field precipitation_component Precipitation component of type ANY
#' @field drought_component Drought component of type ANY
#' @field wind_component Wind component of type ANY
#' @field sealevel_component Sea level component of type ANY
#' @field study_period Study period for analysis (vector of two dates)
#' @field reference_period Reference period for standardization (vector of two dates)

ActuarialClimateIndex <- setRefClass(
  "ActuarialClimateIndex",
  fields = list(
    temperature_component = "ANY",  # Temperature component
    precipitation_component = "ANY",  # Precipitation component
    drought_component = "ANY",  # Drought component
    wind_component = "ANY",  # Wind component
    sealevel_component = "ANY",  # Sea level component
    study_period = "character",  # Study period
    reference_period = "character"  # Reference period
  ),
  methods = list(
    #' Initialize ActuarialClimateIndex object
    #'
    #' This method initializes an object of the ActuarialClimateIndex class with the necessary data file paths and study/reference periods.
    #'
    #' @param temperature_data_path Path to the temperature data file
    #' @param precipitation_data_path Path to the precipitation data file
    #' @param wind_u10_data_path Path to the U component wind data file
    #' @param wind_v10_data_path Path to the V component wind data file
    #' @param country_abbrev Country abbreviation for sea level data
    #' @param mask_data_path Path to the geographical mask file
    #' @param study_period Study period for analysis (vector of two dates)
    #' @param reference_period Reference period for standardization (vector of two dates)
    #'
    #' @return A new ActuarialClimateIndex object
    #'
    initialize = function(temperature_data_path, precipitation_data_path,
                          wind_u10_data_path, wind_v10_data_path,
                          country_abbrev, mask_data_path,
                          study_period, reference_period) {

      .self$study_period <- study_period
      .self$reference_period <- reference_period

      .self$temperature_component <- TemperatureComponent$new(
        temperature_file = temperature_data_path,
        mask_file = mask_data_path,
        reference_period = reference_period
      )

      .self$precipitation_component <- PrecipitationComponent$new(
        precipitation_file = precipitation_data_path,
        mask_file = mask_data_path,
        reference_period = reference_period
      )

      .self$drought_component <- DroughtComponent$new(
        precipitation_file = precipitation_data_path,
        mask_file = mask_data_path,
        reference_period = reference_period
      )

      .self$wind_component <- WindComponent$new(
        u10_file = wind_u10_data_path,
        v10_file = wind_v10_data_path,
        mask_file = mask_data_path
      )

      .self$sealevel_component <- SeaLevelComponent$new(
        country_abrev = country_abbrev,
        study_period = study_period,
        reference_period = reference_period
      )
    },

    #' Calculate Actuarial Climate Index
    #'
    #' This method calculates the Actuarial Climate Index (ACI) using the initialized climate components.
    #'
    #' @param factor Adjustment factor for sea level
    #'
    #' @return A data.table containing the calculated values of the Actuarial Climate Index (ACI)
    #'
    calculate_aci = function(factor = 1) {

      # Execute necessary calculations with the initialized components
      preci_std <- .self$precipitation_component$calculate_monthly_max()
      p_dt <- preci_std[, .(year, month, latitude, longitude, precipitation = rx5day)]

      wind_std <- .self$wind_component$std_wind_exceedance_frequency(.self$reference_period)
      w_dt <- wind_std[, .(year_month, latitude, longitude, windpower = std_frequency)]
      w_dt[, `:=`(year = lubridate::year(year_month), month = lubridate::month(year_month))]

      .self$drought_component$max_consecutive_dry_days()
      .self$drought_component$standardize_metric()
      cdd_dt <- .self$drought_component$standardized_dry_days_dt[, .(year, month, latitude, longitude, drought = standardized_dry_days)]

      temp90_std <- .self$temperature_component$std_t90(.self$reference_period)
      t90_dt <- temp90_std[, .(month, T90 = standardized)]

      temp10_std <- .self$temperature_component$std_t10(.self$reference_period)
      t10_dt <- temp10_std[, .(month, T10 = standardized)]

      sea_lev <- .self$sealevel_component$process()
      # Select the Measurement_* columns and calculate their mean
      measurement_cols <- grep("^Measurement_", colnames(sea_lev), value = TRUE)
      sea_lev[, sea_mean := rowMeans(.SD, na.rm = TRUE), .SDcols = measurement_cols]
      sea_lev <- sea_lev[, .(Corrected_Date, sea_mean)]
      sea_lev[, `:=`(year = as.integer(lubridate::year(Corrected_Date)),
                     month = as.integer(lubridate::month(Corrected_Date)))]

      w_dt[, `:=`(year = as.integer(year), month = as.integer(month))]
      p_dt[, `:=`(year = as.integer(year), month = as.integer(month))]
      cdd_dt[, `:=`(year = as.integer(year), month = as.integer(month))]
      t90_dt[, `:=`(year = as.integer(lubridate::year(month)), month = as.integer(lubridate::month(month)))]
      t10_dt[, `:=`(year = as.integer(lubridate::year(month)), month = as.integer(lubridate::month(month)))]

      # Merge data.tables by year, month, latitude, and longitude
      df1 <- data.table::merge(w_dt, p_dt, by = c("year", "month", "latitude", "longitude"), all = TRUE)
      df2 <- data.table::merge(df1, cdd_dt, by = c("year", "month", "latitude", "longitude"), all = TRUE)
      df3 <- data.table::merge(df2, sea_lev, by = c("year", "month"), all = TRUE)
      df4 <- data.table::merge(df3, t90_dt, by = c("year", "month"), all = TRUE)
      aci_composites <- data.table::merge(df4, t10_dt, by = c("year", "month"), all = TRUE)

      # Calculate the ACI
      aci_composites[, ACI := (T90 - T10 + precipitation + drought + factor * sea_mean + windpower) / 6]
      aci_composites <- aci_composites[, .(year, month, latitude, longitude, windpower, precipitation, drought, T10, T90, sea_mean, ACI)]

      return(aci_composites)
    }
  )
)
