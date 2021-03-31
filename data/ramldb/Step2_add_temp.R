
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/ramldb/data/raw"
outputdir <- "data/ramldb/data/processed"
plotdir <- "data/ramldb/figures"

# Read data
data_orig <- readRDS(file.path(outputdir, "RAM_NE_data.Rds"))

# Read SST data
sst_orig <- read.csv("/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/sst/data/averages/ramldb_sst_yearly_cobe.csv", as.is=T)

# Read ASSESSID-STOCKID key
stockid_key <- readxl::read_excel("/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/ram_boundaries/data/ramldb_v3.8_stock_boundary_table_v2_formatted.xlsx") 


# Build data
################################################################################

# Format SST data
sst <- sst_orig %>% 
  left_join(stockid_key %>% select(assessid, stockid))

# Add SST data to RAM data
data <- data_orig %>% 
  left_join(sst %>% select(stockid, year, sst_c))

# Inspect
str(data)
freeR::complete(data)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outputdir, "RAM_NE_data_w_sst.Rds"))
