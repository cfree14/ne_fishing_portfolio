

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "data/ramldb/data/processed"
outputdir <- "output"
tabledir <- "tables"
plotdir <- "figures"

# Read data
load(file.path(outputdir, "pella_1.00p_fixed_sst.Rdata"))
rm(results, hess, input.data, model, output, params, sd)

# Read NE stock key
stock_key_all <- read.csv(file.path(datadir, "RAM_new_england_stocks_used.csv"), as.is=T)


# Build data
################################################################################

# Stockids
stats <- data %>% 
  group_by(stockid) %>% 
  summarize(years=paste(min(year), max(year), sep="-"),
            nyrs=n()) %>% 
  ungroup()

# Build key
stock_key <- stock_key_all %>% 
  # Reduce to stocks used
  filter(stockid %in% stats$stockid) %>% 
  # Add years
  left_join(stats, by="stockid") %>% 
  # Arrange
  select(stockid, stocklong, area, comm_name, species, years, nyrs)

# Export 
write.csv(stock_key, file=file.path(tabledir, "TableX_stock_metadata.csv"), row.names=F)

# Species key
spp_key <- stock_key %>% 
  group_by(comm_name, species) %>% 
  summarize(nstocks=n(),
            stocks=paste(sort(stockid), collapse = ", "))
  
# Export 
write.csv(spp_key, file=file.path(tabledir, "TableX_species_sample_size.csv"), row.names=F)

