
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/sst/cobe/raw"
outputdir <- "data/sst/cobe/processed"
plotdir <- "data/sst/cobe/figures"

# Read data
data_orig <- raster::brick(file.path(inputdir, "sst.mon.mean.nc"))


# Setup
################################################################################

# Plot data
raster::plot(data_orig, 1)

# Format data
data_mo <- data_orig %>% 
  raster::rotate()


# Plot data
raster::plot(data_mo, 1)
