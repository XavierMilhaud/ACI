library(testthat)
library(ncdf4)
library(data.table)
library(lubridate)
library(ggplot2)

source("../R/windcomponent.R")

create_test_nc <- function(file_path, var_name, var_data, times, latitudes, longitudes) {
  dim_time <- ncdim_def(name = "time", units = "days since 2000-01-01", vals = as.numeric(times - as.Date("2000-01-01")))
  dim_lat <- ncdim_def(name = "latitude", units = "degrees_north", vals = latitudes)
  dim_lon <- ncdim_def(name = "longitude", units = "degrees_east", vals = longitudes)
  
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
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path)
  wind_power <- wind$wind_power()
  
  expect_is(wind_power, "data.table")
  expect_true(all(c("time", "latitude", "longitude", "wind_power") %in% colnames(wind_power)))
  
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
  nc <- nc_create(mask_path, list(
    ncvar_def(name = "country", units = "", dim = list(
      ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
      ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
    ))
  ))
  ncvar_put(nc, "country", mask_data)
  nc_close(nc)
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path)
  wind_thresholds <- wind$wind_thresholds(reference_period)
  
  expect_is(wind_thresholds, "data.table")
  expect_true(all(c("time", "latitude", "longitude", "threshold") %in% colnames(wind_thresholds)))
  
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
  nc <- nc_create(mask_path, list(
    ncvar_def(name = "country", units = "", dim = list(
      ncdim_def(name = "lat", units = "degrees_north", vals = latitudes),
      ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
    ))
  ))
  ncvar_put(nc, "country", mask_data)
  nc_close(nc)
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path)
  days_above_thresholds <- wind$days_above_thresholds(reference_period)
  
  expect_is(days_above_thresholds, "data.table")
  expect_true(all(c("time", "latitude", "longitude", "days_above_threshold") %in% colnames(days_above_thresholds)))
  
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
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path)
  wind_exceedance_frequency <- wind$wind_exceedance_frequency(reference_period)
  
  expect_is(wind_exceedance_frequency, "data.table")
  expect_true(all(c("year_month", "latitude", "longitude", "exceedance_frequency") %in% colnames(wind_exceedance_frequency)))
  
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
  
  wind <- WindComponent$new(u10_path, v10_path, mask_path)
  std_wind_exceedance_frequency <- wind$std_wind_exceedance_frequency(reference_period)
  
  expect_is(std_wind_exceedance_frequency, "data.table")
  expect_true(all(c("year_month", "latitude", "longitude", "std_frequency") %in% colnames(std_wind_exceedance_frequency)))
  
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
    
    wind_component <- WindComponent$new(u10_path, v10_path, mask_path)
    calculated_anomalies <- wind_component$std_wind_exceedance_frequency(reference_period_bis)
    
    reference_anomalies <- nc_open(reference_anomalies_path)
    reference_variable_name <- "__xarray_dataarray_variable__"
    reference_data <- ncvar_get(reference_anomalies, reference_variable_name)
    nc_close(reference_anomalies)
    calculated_anomalies_avg <- calculated_anomalies[, .(mean_std_frequency = mean(std_frequency, na.rm = TRUE)), by = year_month]
    
    compare_vectors_with_nan <- function(vec1, vec2) {
      if (length(vec1) != length(vec2)) {
        return(FALSE)
      }
      for (i in seq_along(vec1)) {
        if (is.nan(vec1[i]) && is.nan(vec2[i])) {
          next
        }
        if (!all.equal(vec1[i], vec2[i], tolerance = 1e-6)) {
          return(FALSE)
        }
      }
      return(TRUE)
    }
    
    if (compare_vectors_with_nan(calculated_anomalies_avg$mean_std_frequency, reference_data)) {
      print("Passed")
    } else {
      print("Failed")
    }
    
    expect_true(compare_vectors_with_nan(calculated_anomalies_avg$mean_std_frequency, reference_data))
  }
})
