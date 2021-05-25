
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

# Format land
################################################################################

# Get land
land <- rnaturalearth::ne_countries(country=c("Canada", "United States of America"),
                                    returnclass = "sf",
                                    scale="small") %>% 
  # Dissolve
  group_by() %>% 
  summarize() %>% 
  ungroup()

# Project land
land_proj <- land %>% 
  sf::st_transform(crs=CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 
                            +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# Plot land
g <- ggplot() +
  # Plot land
  geom_sf(data=land_proj, fill="grey80", color="white", lwd=0.3) +
  # Theme
  theme_bw()
g

# Helper function
################################################################################

# Calculate buffer
dist_m <- 50000
calc_buffer <- function(dist_m){
  
  # Buffer land
  buffer_sf <- land_proj %>% 
    # Buffer by distance
    sf::st_buffer(dist=dist_m) %>% 
    # Project
    sf::st_transform(crs=crs(land)) %>% 
    # Add buffer
    mutate(dist_km=dist_m/1000,
           dist_mi=measurements::conv_unit(dist_km, "km", "mi")) %>% 
    # Arrange
    dplyr::select(dist_mi, dist_km, everything())
  
  # Plot check
  g <- ggplot() +
    # Plot land
    geom_sf(data=land, fill="grey80", color="white", lwd=0.3, alpha=0.3) +
    # Plot buffers
    geom_sf(data=buffer_sf, fill=NA, color="grey30") +
    # Crop
    coord_sf(xlim = c(-80, -60), ylim = c(31, 51)) +
    # Theme
    theme_bw()
  g
  
  # Return
  return(buffer_sf)
  
}

# Buffers
################################################################################

# Miles
buffers_miles <- c(10, 25, 50, 100, 150, 200)
buffers_km <- measurements::conv_unit(buffers_miles, "mi", "km")

# Loop through
buffers <- purrr::map_df(buffers_km, function(x){
  
  buff <- calc_buffer(dist_m=x*1000)
  
})

# Export
saveRDS(buffers, file=file.path(outdir, "dist_to_shore_buffers.Rds"))

