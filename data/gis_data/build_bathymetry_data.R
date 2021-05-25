
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(raster)
library(tidyverse)

# Directories
gisdir <- "data/gis_data"
outdir <- "data/gis_data/processed"

# Read bathymetry data
#  Grid registered is authoritative; cell registered is derived from grid registered
# https://catalog.data.gov/dataset/etopo1-1-arc-minute-global-relief-model
bathy_orig <- raster("/Users/cfree/Dropbox/Chris/UCSB/projects/dungeness/data/bathymetry/raw/ETOPO1_Ice_c_geotiff.tif")
crs(bathy_orig) <- "+init=epsg:4326"

# Read statistical areas
areas <- readRDS(file.path(outdir, "NE_statistical_areas.Rds"))
areas_proj <- areas %>% 
  sf::st_transform(crs="+init=epsg:4326")

# Format bathymetry data
################################################################################

# Crop
bathy1 <- bathy_orig %>% 
  raster::crop(y=extent(areas_proj))

# Project
bathy2 <- bathy1 %>% 
  raster::projectRaster(crs="+proj=longlat +datum=NAD83 +no_defs")

# Plot data
plot(bathy1)
plot(bathy2)

# Export data
writeRaster(bathy2, filename = file.path(outdir, "NE_bathymetry.tif"))

# If you wanted to mask
# bathy <- bathy_orig %>% 
#   raster::mask(mask=areas_proj)
