
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(splink)
library(tidyverse)

# Directories
inputdir <- "data/ramldb/data/raw"
outputdir <- "data/ramldb/data/processed"
plotdir <- "data/ramldb/figures"

# Read data
data_orig <- readRDS(file.path(outputdir, "RAM_NE_data_w_sst_trimmed.Rds"))


# Prepare data
################################################################################

# Prepare data
data <- data_orig %>% 
  # Reduce to years with data
  filter(!is.na(tb_mt) & !is.na(sp_mt)) %>%
  # Scale biomass, production, temperature
  group_by(stockid) %>%
  mutate(tb_scaled=tb_mt/max(tb_mt),
         sp_scaled=sp_mt/max(tb_mt),
         sst_c_scaled=scale(sst_c, center=T, scale=F)) %>% 
  ungroup()

# Export data
saveRDS(data, file=file.path(outputdir, "RAM_NE_data_w_sst_trimmed_prepped.Rds"))



# 
# # Export data
# ################################################################################
# 
# # Fit model
# output <- splink::fit_sp(data, p=0.2)
# 
# # Extract results
# results <- splink::get_results(output)
# 
# # Plot results
# splink::plot_results(results)
# 
# # Plot fits
# splink::plot_fits(output, plotdir, "SP_fits.pdf")





