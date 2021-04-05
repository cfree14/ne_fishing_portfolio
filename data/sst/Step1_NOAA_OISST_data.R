
library(raster)
library(rvest)
library(tidyverse)

# Get list of available files
catalog_url <- "https://www.ncei.noaa.gov/thredds/catalog/OisstBase/NetCDF/V2.1/AVHRR/catalog.html"
catalog <- catalog_url %>% rvest::read_html() %>% rvest::html_table()


data <- raster::brick("/thredds/ncss/OisstBase/NetCDF/V2.1/AVHRR/198109/oisst-avhrr-v02r01.19810901.nc")


# Directories
rawdir <- "data/sst/oisst/raw"
outdir <- "data/sst/oisst/processed"

# Read data
data_orig <- raster::brick(file.path(rawdir, "oisst-avhrr-v02r01.19810901.nc"), varname="sst")
raster::plot(data_orig)

# Plot data
data <- data_orig %>% 
  raster::rotate()
raster::plot(data)
