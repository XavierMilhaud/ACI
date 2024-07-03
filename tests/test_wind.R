library(testthat)
library(ncdf4)
library(data.table)
library(lubridate)
library(ggplot2)

source("../R/windcomponent.R")

create_test_nc <- function(file_path, var_name, data, times, latitudes, longitudes) {
  ncdims <- list(
    ncdf4::ncdim_def(name = "time", units = "days since 2000-01-01", vals = as.numeric(times - as.Date("2000-01-01")), unlim = TRUE),
    ncdf4::ncdim_def(name = "latitude", units = "degrees_north", vals = latitudes),
    ncdf4::ncdim_def(name = "longitude", units = "degrees_east", vals = longitudes)
  )
  
  ncvar <- ncdf4::ncvar_def(name = var_name, units = "m/s", dim = ncdims, missval = -9999.0)
  
  nc <- ncdf4::nc_create(file_path, ncvar)
  ncdf4::ncvar_put(nc, ncvar, data)
  ncdf4::nc_close(nc)
}

test_that("WindComponent: wind_power method works correctly", {
  mask_path <- "test_mask.nc"
  u10_path <- "test_u10.nc"
  v10_path <- "test_v10.nc"
  reference_period <- c("2000-01-01", "2009-12-31")
  
  times <- seq(as.Date('2000-01-01'), as.Date('2020-12-31'), by = "days")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  set.seed(0)
  wind_data <- array(runif(length(times) * length(latitudes) * length(longitudes)), dim = c(length(times), length(latitudes), length(longitudes)))
  
  create_test_nc(u10_path, "u10", wind_data, times, latitudes, longitudes)
  create_test_nc(v10_path, "v10", wind_data, times, latitudes, longitudes)
  
  mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
  nc <- ncdf4::nc_create(mask_path, list(
    ncdf4::ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
    ncdf4::ncdim_def(name = "lon", units = "degrees_east", vals = longitudes),
    ncdf4::ncvar_def(name = "country", units = "", dim = list(
      ncdf4::ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
      ncdf4::ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
    ))
  ))
  ncdf4::ncvar_put(nc, "country", mask_data)
  ncdf4::nc_close(nc)
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path, reference_period)
  wind_power <- wind$wind_power()
  
  expect_is(wind_power, "data.table")
  expect_true(all(c("time", "longitude", "latitude", "wind_power") %in% names(wind_power)))
  expect_true(min(wind_power$time) >= as.Date("2000-01-01"))
  expect_true(max(wind_power$time) <= as.Date("2020-12-31"))
  
  unlink(c(u10_path, v10_path, mask_path))
})

test_that("WindComponent: wind_thresholds method works correctly", {
  mask_path <- "test_mask.nc"
  u10_path <- "test_u10.nc"
  v10_path <- "test_v10.nc"
  reference_period <- c("2000-01-01", "2009-12-31")
  
  times <- seq(as.Date('2000-01-01'), as.Date('2020-12-31'), by = "days")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  set.seed(0)
  wind_data <- array(runif(length(times) * length(latitudes) * length(longitudes)), dim = c(length(times), length(latitudes), length(longitudes)))
  
  create_test_nc(u10_path, "u10", wind_data, times, latitudes, longitudes)
  create_test_nc(v10_path, "v10", wind_data, times, latitudes, longitudes)
  
  mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
  nc <- ncdf4::nc_create(mask_path, list(
    ncdf4::ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
    ncdf4::ncdim_def(name = "lon", units = "degrees_east", vals = longitudes),
    ncdf4::ncvar_def(name = "country", units = "", dim = list(
      ncdf4::ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
      ncdf4::ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
    ))
  ))
  ncdf4::ncvar_put(nc, "country", mask_data)
  ncdf4::nc_close(nc)
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path, reference_period)
  wind_thresholds <- wind$wind_thresholds(reference_period)
  
  expect_is(wind_thresholds, "data.table")
  expect_true(all(c("dayofyear", "mean_power", "std_power", "threshold") %in% names(wind_thresholds)))
  expect_true(all(wind_thresholds$dayofyear >= 1 & wind_thresholds$dayofyear <= 366))
  
  unlink(c(u10_path, v10_path, mask_path))
})

test_that("WindComponent: days_above_thresholds method works correctly", {
  mask_path <- "test_mask.nc"
  u10_path <- "test_u10.nc"
  v10_path <- "test_v10.nc"
  reference_period <- c("2000-01-01", "2009-12-31")
  
  times <- seq(as.Date('2000-01-01'), as.Date('2020-12-31'), by = "days")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  set.seed(0)
  wind_data <- array(runif(length(times) * length(latitudes) * length(longitudes)), dim = c(length(times), length(latitudes), length(longitudes)))
  
  create_test_nc(u10_path, "u10", wind_data, times, latitudes, longitudes)
  create_test_nc(v10_path, "v10", wind_data, times, latitudes, longitudes)
  
  mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
  nc <- ncdf4::nc_create(mask_path, list(
    ncdf4::ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
    ncdf4::ncdim_def(name = "lon", units = "degrees_east", vals = longitudes),
    ncdf4::ncvar_def(name = "country", units = "", dim = list(
      ncdf4::ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
      ncdf4::ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
    ))
  ))
  ncdf4::ncvar_put(nc, "country", mask_data)
  ncdf4::nc_close(nc)
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path, reference_period)
  days_above_thresholds <- wind$days_above_thresholds(reference_period)
  
  expect_is(days_above_thresholds, "data.table")
  expect_true(all(c("time", "longitude", "latitude", "days_above") %in% names(days_above_thresholds)))
  expect_true(min(days_above_thresholds$time) >= as.Date("2000-01-01"))
  expect_true(max(days_above_thresholds$time) <= as.Date("2020-12-31"))
  
  unlink(c(u10_path, v10_path, mask_path))
})

test_that("WindComponent: wind_exceedance_frequency method works correctly", {
  mask_path <- "test_mask.nc"
  u10_path <- "test_u10.nc"
  v10_path <- "test_v10.nc"
  reference_period <- c("2000-01-01", "2009-12-31")
  
  times <- seq(as.Date('2000-01-01'), as.Date('2020-12-31'), by = "days")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  set.seed(0)
  wind_data <- array(runif(length(times) * length(latitudes) * length(longitudes)), dim = c(length(times), length(latitudes), length(longitudes)))
  
  create_test_nc(u10_path, "u10", wind_data, times, latitudes, longitudes)
  create_test_nc(v10_path, "v10", wind_data, times, latitudes, longitudes)
  
  mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
  nc <- ncdf4::nc_create(mask_path, list(
    ncdf4::ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
    ncdf4::ncdim_def(name = "lon", units = "degrees_east", vals = longitudes),
    ncdf4::ncvar_def(name = "country", units = "", dim = list(
      ncdf4::ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
      ncdf4::ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
    ))
  ))
  ncdf4::ncvar_put(nc, "country", mask_data)
  ncdf4::nc_close(nc)
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path, reference_period)
  wind_exceedance_frequency <- wind$wind_exceedance_frequency(reference_period)
  
  expect_is(wind_exceedance_frequency, "data.table")
  expect_true(all(c("month", "longitude", "latitude", "frequency") %in% names(wind_exceedance_frequency)))
  expect_true(min(wind_exceedance_frequency$month) >= as.Date("2000-01-01"))
  expect_true(max(wind_exceedance_frequency$month) <= as.Date("2020-12-31"))
  
  unlink(c(u10_path, v10_path, mask_path))
})

test_that("WindComponent: std_wind_exceedance_frequency method works correctly", {
  mask_path <- "test_mask.nc"
  u10_path <- "test_u10.nc"
  v10_path <- "test_v10.nc"
  reference_period <- c("2000-01-01", "2009-12-31")
  
  times <- seq(as.Date('2000-01-01'), as.Date('2020-12-31'), by = "days")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  set.seed(0)
  wind_data <- array(runif(length(times) * length(latitudes) * length(longitudes)), dim = c(length(times), length(latitudes), length(longitudes)))
  
  create_test_nc(u10_path, "u10", wind_data, times, latitudes, longitudes)
  create_test_nc(v10_path, "v10", wind_data, times, latitudes, longitudes)
  
  mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
  nc <- ncdf4::nc_create(mask_path, list(
    ncdf4::ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
    ncdf4::ncdim_def(name = "lon", units = "degrees_east", vals = longitudes),
    ncdf4::ncvar_def(name = "country", units = "", dim = list(
      ncdf4::ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
      ncdf4::ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
    ))
  ))
  ncdf4::ncvar_put(nc, "country", mask_data)
  ncdf4::nc_close(nc)
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path, reference_period)
  standardized_frequency <- wind$std_wind_exceedance_frequency(reference_period)
  
  expect_is(standardized_frequency, "data.table")
  expect_true(all(c("month", "longitude", "latitude", "standardized_frequency") %in% names(standardized_frequency)))
  expect_true(min(standardized_frequency$month) >= as.Date("2000-01-01"))
  expect_true(max(standardized_frequency$month) <= as.Date("2020-12-31"))
  
  ref_standardized_frequency <- standardized_frequency[month >= as.Date(reference_period[1]) & month <= as.Date(reference_period[2])]
  mean_standardized_frequency <- mean(ref_standardized_frequency$standardized_frequency, na.rm = TRUE)
  expect_false(is.na(mean_standardized_frequency), "Mean standardized frequency should not be NaN.")
  expect_equal(mean_standardized_frequency, 0, tolerance = 1)
  
  std_standardized_frequency <- sd(ref_standardized_frequency$standardized_frequency, na.rm = TRUE)
  expect_false(is.na(std_standardized_frequency), "Std standardized frequency should not be NaN.")
  expect_equal(std_standardized_frequency, 1, tolerance = 1)
  
  unlink(c(u10_path, v10_path, mask_path))
})

test_that("WindComponent: std_wind_exceedance_frequency method works against precomputed reference anomalies", {
  test_cases <- c('test1', 'test2', 'test3', 'test4')
  reference_period_bis <- c("2000-01-01", "2000-12-31")
  data_dir <- '../data/tests_data/tests_data_wind'
  
  for (test_case in test_cases) {
    u10_path <- file.path(data_dir, paste0(test_case, '_u10.nc'))
    v10_path <- file.path(data_dir, paste0(test_case, '_v10.nc'))
    mask_path <- file.path(data_dir, paste0(test_case, '_mask.nc'))
    reference_anomalies_path <- file.path(data_dir, paste0(test_case, '_reference_anomalies.nc'))
    
    reference_anomalies <- ncdf4::nc_open(reference_anomalies_path)
    reference_variable_name <- names(reference_anomalies$var)[1]
    
    wind_component <- WindComponent$new(u10_path, v10_path, mask_path)
    calculated_anomalies <- wind_component$std_wind_exceedance_frequency(reference_period_bis, area = TRUE)
    
    reference_data <- ncvar_get(reference_anomalies, reference_variable_name)
    expect_equal(calculated_anomalies, reference_data, tolerance = 1e-5)
    
    ncdf4::nc_close(reference_anomalies)
  }
})

  