library(testthat)
library(ncdf4)
library(data.table)
library(zoo)
library(lubridate)

create_test_nc <- function(file_path, times, latitudes, longitudes, data) {
  time_dim <- ncdf4::ncdim_def("time", units = "days since 2000-01-01", vals = as.numeric(times - as.Date("2000-01-01")))
  lat_dim <- ncdf4::ncdim_def("latitude", units = "degrees_north", vals = latitudes)
  lon_dim <- ncdf4::ncdim_def("longitude", units = "degrees_east", vals = longitudes)

  var_def <- ncdf4::ncvar_def("tp", "mm", list(time_dim, lat_dim, lon_dim), -999, prec = "double")

  nc <- ncdf4::nc_create(file_path, list(var_def))
  ncdf4::ncvar_put(nc, var_def, data)
  ncdf4::nc_close(nc)
}

create_test_mask_nc <- function(file_path, latitudes, longitudes) {
  lat_dim <- ncdf4::ncdim_def("lat", "degrees_north", vals = latitudes)
  lon_dim <- ncdf4::ncdim_def("lon", "degrees_east", vals = longitudes)

  var_def <- ncdf4::ncvar_def("country", "1", list(lat_dim, lon_dim), -999, prec = "integer")

  nc <- ncdf4::nc_create(file_path, list(var_def))
  ncdf4::ncvar_put(nc, var_def, matrix(1, nrow = length(latitudes), ncol = length(longitudes)))
  ncdf4::nc_close(nc)
}


test_that("DroughtComponent: std_max_consecutive_dry_days method works correctly", {
  setUp <- function() {
    mask_path <- "test_mask.nc"
    data_path <- 'test_data.nc'
    reference_period <- c("2000-01-01", "2009-12-31")

    # Generating test precipitation data
    times <- seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = "day")
    latitudes <- seq(48.80, 48.90, by = 0.1)
    longitudes <- seq(2.20, 2.30, by = 0.1)
    set.seed(0)
    precipitation_data <- array(runif(length(times) * length(latitudes) * length(longitudes)),
                                dim = c(length(times), length(latitudes), length(longitudes)))

    create_test_nc(data_path, times, latitudes, longitudes, precipitation_data)

    # Generating test mask data
    mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
    create_test_mask_nc(mask_path, latitudes, longitudes)

    list(mask_path = mask_path, data_path = data_path, reference_period = reference_period)
  }

  tearDown <- function(env) {
    file.remove(env$data_path)
    file.remove(env$mask_path)
  }

  env <- setUp()

  on.exit(tearDown(env))
  #drought <- DroughtComponent$new(env$data_path, env$mask_path)
})
