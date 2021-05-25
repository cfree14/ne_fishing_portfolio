
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directories
gisdir <- "data/gis_data/processed"
plotdir <- "data/fishing_grounds/figures"

# Read ports
ports <- read.csv("data/ports/ports.csv", as.is=T) %>% 
  mutate(port=recode(port, "Point Pleasant/Barnegat Light, NJ"="Point Pleasant, NJ"))

# Read statistical areas
areas <- readRDS(file.path(gisdir, "NE_statistical_areas_simplified.Rds"))

# Read bathymetry
bathy <- raster(file.path(gisdir, "NE_bathymetry.tif"))

# Read distance to shore
buffers <- readRDS(file.path(gisdir, "dist_to_shore_buffers.Rds")) %>% 
  filter(dist_mi>10) %>% 
  mutate(dist_mi=factor(dist_mi, levels=sort(dist_mi)))

# Land
usa <- rnaturalearth::ne_states(country="United States of America", return="sf")
foreign <- rnaturalearth::ne_countries(country="Canada", scale="large",  returnclass = "sf")


# Format
################################################################################

# Format bathymetry
bathy_df <- bathy %>%
  # Convert to dataframe
  as.data.frame(xy=T) %>% 
  # Rename
  rename(long_dd=x, lat_dd=y, bathy_m=NE_bathymetry) %>% 
  # Filter to columns of interest
  filter(!is.na(bathy_m) & bathy_m<0) %>% 
  # Format bathymetry
  mutate(bathy_m=bathy_m*-1) %>% 
  # Cut bathymetry
  mutate(bathy_catg=cut(bathy_m, breaks=c(0, 25, 50, 100, 200, 500, 7000)))

# Inspect
table(bathy_df$bathy_catg)

# Sample bathymetry for setting up plots
bathy_df_sample <- bathy_df %>% 
  dplyr::sample_frac(0.1)

# Plot data
################################################################################

# Params
ncatg <- n_distinct(bathy_df$bathy_catg)
nbuffers <- nrow(buffers)

# Theme
my_theme <- theme(axis.text=element_text(size=10),
                  axis.title=element_blank(),
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=9),
                  plot.title=element_blank(),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  axis.text.y = element_text(angle = 90, hjust = 0.5))

# Plot
g <- ggplot() +
  # Plot bathymetry
  geom_tile(data=bathy_df, mapping=aes(x=long_dd, y=lat_dd, fill=bathy_catg)) +
  # Plot land
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  # Plot distance to shore buffers
  geom_sf(data=buffers, mapping=aes(color=dist_mi), fill=NA) +
  # Plot statistical areas
  geom_sf(data=areas, fill=NA, color="black", lwd=0.1) +
  geom_sf_text(data=areas, mapping=aes(label=id), color="black", size=2.8) +
  # Plot ports
  geom_point(data=ports, mapping=aes(x=long_dd, y=lat_dd), size=4) +
  geom_text(data=ports, mapping=aes(x=long_dd, y=lat_dd, label=port), hjust=1.1) +
  # Labels
  labs(x="", y="") +
  # Legends
  scale_fill_manual(name="Depth range (m)",
                    values=RColorBrewer::brewer.pal(ncatg, "Blues")) +
  scale_color_manual(name="Distance\nfrom shore (mi)", 
                     values=RColorBrewer::brewer.pal(nbuffers, "Reds")) +
  # guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-80, -60), ylim = c(31, 51)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.9),
        legend.box = "horizontal")
g

# Export
ggsave(g, filename=file.path(plotdir, "stat_area_map.pdf"), 
       width=8.5, height=11, units="in", dpi=600)
