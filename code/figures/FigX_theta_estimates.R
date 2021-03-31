

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
plotdir <- "figures"

# Read data
load(file.path(datadir, "pella_1.00p_fixed_sst.Rdata"))


# Build data
################################################################################

# Format data
data <- results %>% 
  # Reduce
  select(stockid, betaT, betaT_lo, betaT_hi, betaT_inf) %>% 
  # Order
  arrange(desc(betaT)) %>% 
  mutate(stockid=factor(stockid, levels=stockid))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   legend.position = "bottom")

# Spline bars
g <- ggplot() +
  # Vertical
  geom_vline(xintercept = 0, color="grey30") +
  # Lines
  geom_errorbar(data=data, mapping=aes(y=stockid, xmin=betaT_lo, xmax=betaT_hi, color=betaT_inf), width=0) +
  geom_point(data=data, mapping=aes(y=stockid, x=betaT, color=betaT_inf)) +
  # Labels
  labs(x="Temperature effect", y="") +
  # Legend
  scale_color_manual(name="Influence", values=c("red", "black", "blue")) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_theta_estimates.png"), 
       width=6.5, height=3.5, units="in", dpi=600)

