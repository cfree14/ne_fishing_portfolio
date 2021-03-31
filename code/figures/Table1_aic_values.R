

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "output"
tabledir <- "tables"


# Build table
################################################################################

# Models
models <- list.files(datadir, pattern=".Rdata")

# Data frame
aic_df <- data.frame(model=models, k=NA, lik=NA, aic=NA, stringsAsFactors=F)

# Loop through models and calculate/record AIC value
for(i in 1:length(models)){
  
  # Load data
  load(paste(datadir, models[i], sep="/"))
  
  # Calculate/record AIC value
  k <- length(output[["par"]])
  lik <- output[["objective"]]
  aic_val <- TMBhelper::TMBAIC(output)
  aic_df$k[i] <- k
  aic_df$lik[i] <- lik
  aic_df$aic[i] <- aic_val
  
}

# Format
aic_final <- aic_df %>% 
  arrange(aic)


# Export table
################################################################################

# Export data
write.csv(aic_final, paste(tabledir, "Table1_model_aic_comparison.csv", sep="/"), row.names=F)

