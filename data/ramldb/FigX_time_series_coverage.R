
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
data_orig <- readRDS(file.path(outputdir, "RAM_NE_data_w_sst_trimmed_prepped.Rds"))


# Plot data
################################################################################

# Stats
stats <- data_orig %>% 
  group_by(source, stockid) %>% 
  summarize(yr1=min(year),
            yr2=max(year),
            nyr=n()) %>% 
  arrange(yr1)

# By source
table(stats$source)

# Order data
data <- data_orig %>% 
  mutate(stockid=factor(stockid, levels = stats$stockid))

# Plot data
g <- ggplot(data, aes(x=year, y=stockid, fill=tb_scaled)) +
  geom_tile() +
  # Labels
  labs(x="Year", y="") +
  # Scales
  scale_x_continuous(breaks=seq(1955,2020,5)) +
  # Legends
  scale_fill_gradientn(name="Scaled biomass\n(proportion of max biomass)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + 
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        # Gridlines
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # Legend
        legend.position="bottom")
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_ram_coverage.png"), 
       width=6.5, height=3.5, units="in", dpi=600)
