#!/usr/bin/env Rscript

library(ncdf4)
library(tidyverse)

# Fonction pour lire les fichiers NetCDF
read_files <- function(precipitation_path, mask_path) {
  # Lire les données de précipitation
  cat("Lecture du fichier de précipitation...\n")
  nc_precipitation <- nc_open(precipitation_path)
  precip_dims <- list(
    time = nc_precipitation$dim$time$len,
    lon = nc_precipitation$dim$lon$len,
    lat = nc_precipitation$dim$lat$len
  )
  cat("Dimensions des données de précipitation:\n")
  print(precip_dims)
  
  # Lire les données de masque
  cat("Lecture du fichier de masque...\n")
  nc_mask <- nc_open(mask_path)
  mask_dims <- list(
    lon = nc_mask$dim$lon$len,
    lat = nc_mask$dim$lat$len
  )
  cat("Dimensions des données de masque:\n")
  print(mask_dims)
  
  nc_close(nc_precipitation)
  nc_close(nc_mask)
  
  return(list(precipitation = precip_dims, mask = mask_dims))
}

# Chemins des fichiers
precipitation_path <- "../ResPartOfParis_total_precipitation_1960-1970.nc"
mask_path <- "../countries_gridded_0.1deg_v0.1_FRo.nc"

# Lire les fichiers
data <- read_files(precipitation_path, mask_path)

# Comparer les dimensions
cat("Comparaison des dimensions des deux fichiers...\n")
cat("Dimensions des données de précipitation:\n")
print(data$precipitation)
cat("Dimensions des données de masque:\n")
print(data$mask)

# Vérifier la compatibilité des dimensions pour l'application du masque
if (data$precipitation$lon != data$mask$lon || data$precipitation$lat != data$mask$lat) {
  stop("Les dimensions des données de précipitation et de masque ne correspondent pas")
} else {
  cat("Les dimensions des données de précipitation et de masque correspondent. Vous pouvez appliquer le masque.\n")
}
