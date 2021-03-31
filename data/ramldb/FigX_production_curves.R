
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

# Plot data
g <- ggplot(data_orig, aes(x=tb_scaled, y=sp_scaled, fill=sst_c_scaled)) +
  facet_wrap(~stockid, scales="free", ncol=5) +
  geom_hline(yintercept=0, color="grey20", linetype="dotted") +
  geom_point(pch=21, size=2) +
  # Labels
  labs(x="Biomass\n(scaled to maximum biomass)", y="Production\n(scaled to maximum biomass)") +
  # Legends
  scale_fill_gradient2(name="SST\n(°C, centered)", midpoint = 0, mid="white", high="darkred", low="navy") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        strip.text=element_text(size=6),
        # Gridlines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # Legend
        legend.position="bottom")
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_prodution_curves.png"), 
       width=6.5, height=6.5, units="in", dpi=600)
