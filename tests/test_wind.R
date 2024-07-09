library(testthat)
library(ncdf4)
library(data.table)
library(lubridate)
library(ggplot2)

source("../R/windcomponent.R")

# Helper function to create NetCDF files for testing
create_test_nc <- function(file_path, var_name, var_data, times, latitudes, longitudes) {
  dim_time <- ncdim_def(name = "time", units = "days since 2000-01-01", vals = as.numeric(times - as.Date("2000-01-01")))
  dim_lat <- ncdim_def(name = "lat", units = "degrees_north", vals = latitudes)
  dim_lon <- ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
  
  var_def <- ncvar_def(name = var_name, units = "", dim = list(dim_time, dim_lat, dim_lon))
  nc <- nc_create(file_path, var_def)
  ncvar_put(nc, var_name, var_data)
  nc_close(nc)
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
  nc <- nc_create(mask_path, list(
    ncvar_def(name = "country", units = "", dim = list(
      ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
      ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
    ))
  ))
  ncvar_put(nc, "country", mask_data)
  nc_close(nc)
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path, reference_period)
  wind_power <- wind$wind_power()
  
  expect_is(wind_power, "array")
  expect_equal(dim(wind_power), c(length(times), length(latitudes), length(longitudes)))
  
  unlink(c(u10_path, v10_path, mask_path))
})

test_that("WindComponent: wind_thresholds method works correctly", {
  mask_path <- "test_mask.nc"
  u10_path <- "test_u10.nc"
  v10_path <- "test_v 10.nc"
  reference_period <- c("2000-01-01", "2009-12-31")
  
  times <- seq(as.Date('2000-01-01'), as.Date('2020-12-31'), by = "days")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  set.seed(0)
  wind_data <- array(runif(length(times) * length(latitudes) * length(longitudes)), dim = c(length(times), length(latitudes), length(longitudes)))
  
  create_test_nc(u10_path, "u10", wind_data, times, latitudes, longitudes)
  create_test_nc(v10_path, "v10", wind_data, times, latitudes, longitudes)
  
  mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
  nc <- nc_create(mask_path, list(
    ncvar_def(name = "country", units = "", dim = list(
      ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
      ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
    ))
  ))
  ncvar_put(nc, "country", mask_data)
  nc_close(nc)
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path, reference_period)
  wind_thresholds <- wind$wind_thresholds(reference_period)
  
  print(wind_thresholds)  # Impression pour le débogage
  
  expect_is(wind_thresholds, "data.table")
  expect_true(all(c("longitude", "latitude", "threshold") %in% colnames(wind_thresholds)))
  
  unlink(c(u10_path  , v10_path, mask_path))
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
  nc <- nc_create(mask_path, list(
    ncvar_def(name = "country", units = "", dim = list(
      ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
      ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
    ))
  ))
  ncvar_put(nc, "country", mask_data)
  nc_close(nc)
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path, reference_period)
  days_above_thresholds <- wind$days_above_thresholds(reference_period)
  
  print(days_above_thresholds)  # Impression pour le débogage
  
  expect_is(days_above_thresholds, "data.table")
  expect_true(all(c("longitude", "latitude", "days_above_threshold") %in% colnames(days_above_thresholds)))
  
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
  nc <- nc_create(mask_path, list(
    ncvar_def(name = "country", units = "", dim = list(
      ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
      ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
    ))
  ))
  ncvar_put(nc, "country", mask_data)
  nc_close(nc)
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path, reference_period)
  wind_exceedance_frequency <- wind$wind_exceedance_frequency(reference_period)
  
  print(wind_exceedance_frequency)  # Impression pour le débogage
  
  expect_is(wind_exceedance_frequency, "data.table")
  expect_true(all(c("longitude", "latitude", "exceedance_frequency") %in% colnames(wind_exceedance_frequency)))
  
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
  nc <- nc_create(mask_path, list(
    ncvar_def(name = "country", units = "", dim = list(
      ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
      ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
    ))
  ))
  ncvar_put(nc, "country", mask_data)
  nc_close(nc)
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path, reference_period)
  std_wind_exceedance_frequency <- wind$std_wind_exceedance_frequency(reference_period)
  
  print(std_wind_exceedance_frequency)  # Impression pour le débogage
  
  expect_is(std_wind_exceedance_frequency, "data.table")
  expect_true(all(c("longitude", "latitude", "std_frequency") %in% colnames(std_wind_exceedance_frequency)))
  
  unlink(c(u10_path, v10_path, mask_path))
})

test_that("WindComponent: std_wind_exceedance_frequency method works against precomputed reference anomalies", {
  data_dir <- '../data/tests_data/tests_data_wind'
  test_cases <- c('test1', 'test2', 'test3', 'test4')
  reference_period_bis <- c('2000-01-01', '2000-12-31')
  
  for (test_case in test_cases) {
    u10_path <- file.path(data_dir, paste0(test_case, '_u10.nc'))
    v10_path <- file.path(data_dir, paste0(test_case, '_v10.nc'))
    mask_path <- file.path(data_dir, paste0(test_case, '_mask.nc'))
    reference_anomalies_path <- file.path(data_dir, paste0(test_case, '_reference_anomalies.nc'))
    
    reference_anomalies <- ncdf4::nc_open(reference_anomalies_path)
    wind_component <- WindComponent$new(u10_path, v10_path, mask_path, reference_period_bis)
    
    calculated_anomalies <- wind_component$std_wind_exceedance_frequency(reference_period_bis)
    
    reference_variable_name <- "standardized_frequency"
    reference_data <- ncdf4::ncvar_get(reference_anomalies, reference_variable_name)
    ncdf4::nc_close(reference_anomalies)
    
    expect_equal(calculated_anomalies$std_frequency, reference_data)
  }
})


