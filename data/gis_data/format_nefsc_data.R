
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directories
gisdir <- "data/gis_data"
outdir <- "data/gis_data/processed"


# Build data
################################################################################

# Read data
areas_orig <- sf::st_read(file.path(gisdir, "nefsc", "Statistical_Areas_2010_withNames.shp"))

# Format data
areas <- areas_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(nafo_id=nafodiv) %>% 
  # Format
  mutate(full_name=stringr::str_to_title(full_name))

# Inspect
head(areas)
str(areas)

# Simplify data
areas_simple <- areas %>% 
  sf::st_simplify(dTolerance=0.05, preserveTopology=T)

plot(areas_simple)

# Export
saveRDS(areas, file.path(outdir, "NE_statistical_areas.Rds"))
saveRDS(areas_simple, file.path(outdir, "NE_statistical_areas_simplified.Rds"))


# Plot data
################################################################################

# Land
usa <- rnaturalearth::ne_states(country="United States of America", return="sf")
foreign <- rnaturalearth::ne_countries(country="Canada", scale="large",  returnclass = "sf")

# Plot
g <- ggplot() +
  # Plot statistical areas
  geom_sf(data=areas_simple, fill=NA, color="grey30", lwd=0.3) +
  # Plot land
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-80, -60), ylim = c(33, 48)) +
  # Theme
  theme_bw()
g
