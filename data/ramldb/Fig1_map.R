
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

# Read NE statistical areas
# zones <- sf::st_read("/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/ram_boundaries/data/gis_data/Divisions.shp")
zones <- sf::st_read("/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/ram_boundaries/data/boundaries_original/USA/nefsc_stat_areas/Statistical_Areas_2010_withNames.shp")

# Centroids
################################################################################

# Read centroid data
d1 <- read.csv("/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/ram_boundaries/data/ramldb_v3.8_stock_boundary_centroids_areas.csv", as.is=T)
d2 <- readxl::read_excel("/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/ram_boundaries/data/ramldb_v3.8_stock_boundary_table_v2_formatted.xlsx")

# Build stock centroids
stocks <- data_orig %>% 
  select(stockid) %>% 
  unique() %>% 
  arrange(stockid) %>% 
  left_join(d2 %>% select(assessid, stockid)) %>% 
  left_join(d1 %>% select(assessid, lat_dd, long_dd))

# Plot map
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country="Canada", scale="large",  returnclass = "sf")

# Theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_blank(),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot zones
  geom_sf(data=zones, fill=NA, color="grey60", lwd=0.2) +
  geom_sf_text(data=zones, mapping=aes(label=NAFODIV)) +
  # Plot centroids
  # geom_point(data=stocks, mapping=aes(x=long_dd, y=lat_dd)) + # range(stocks$lat_dd); range(stocks$long_dd)
  # ggrepel::geom_text_repel(data=stocks, mapping=aes(x=long_dd, y=lat_dd, label=stockid), 
  #                          min.segment.length = 0, segment.size=0.2, max.overlaps = 1000, size=2) +
  # Labels
  labs(x="", y="") +
  # Legends
  # Crop
  coord_sf(xlim = c(-80, -60), ylim = c(33, 48)) +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_stock_map.png"), 
       width=6.5, height=6, units="in", dpi=600)





