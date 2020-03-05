
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/ramldb/data"
plotdir <- "data/ramldb/figures"

# Read RAM Legacy Database v4.491
load("/Users/cfree/Dropbox/Chris/UCSB/data/ramldb/RAM v4.491 Files (1-14-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].Rdata")


# Build stock key
################################################################################

# Build stock key
stock_key <- stock %>% 
  select(-c(tsn, inmyersdb, myersstockid)) %>% 
  # Add area name
  left_join(select(area, areaid, country, areaname), by="areaid") %>% 
  # Rename columns
  rename(species=scientificname, comm_name=commonname, area=areaname) %>% 
  # Format columns
  mutate(comm_name=freeR::sentcase(comm_name),
         species=gsub("spp.", "spp", species),
         species=recode(species, 
                        "Chrysophrys auratus"="Pagrus auratus",
                        "Clupea pallasii"="Clupea pallasii pallasii",
                        "Epinephelus flavolimbatus"="Hyporthodus flavolimbatus",
                        "Epinephelus niveatus"="Hyporthodus niveatus",
                        "Etrumeus teres"="Etrumeus sadina",
                        "Loligo bleekeri"="Heterololigo bleekeri",
                        "Loligo pealeii"="Doryteuthis pealeii",
                        "Merluccius gayi"="Merluccius gayi gayi",
                        "Mullus barbatus"="Mullus barbatus barbatus",
                        "Neoplatycephalus richardsoni"="Platycephalus richardsoni",
                        "Psetta maxima"="Scophthalmus maximus",
                        "Tetrapturus albidus"="Kajikia albida",
                        "Sardinops melanostictus"="Sardinops sagax",
                        "Clupea bentincki"="Strangomera bentincki",
                        "Raja binoculata"="Beringraja binoculata",
                        "Raja rhina"="Beringraja rhina",
                        "Theragra chalcogramma"="Gadus chalcogrammus")) %>% 
  # Rearrange columns
  select(stockid, stocklong, country, region, area, species, comm_name)

# Check names
freeR::check_names(stock_key$species)
freeR::complete(stock_key)


# New England stocks and data
################################################################################

# NE stocks
ne_stocks <- stock_key %>% 
  filter(region=="US East Coast")

# NE data
ne_data <- timeseries_values_views %>% 
  # Reduce to NE stocks
  filter(stockid %in% ne_stocks$stockid) %>% 
  # Reduce columns
  select(stockid, stocklong, year, TC, TL, TN, TB, SSB) %>% 
  # Format columns names
  setNames(tolower(colnames(.)))

# Inspect
freeR::complete(ne_data)

# NE data stats
ne_data_stats <- ne_data %>% 
  # Calculate TB time series length
  group_by(stockid, stocklong) %>% 
  summarize(tb_n=sum(!is.na(tb))) %>% 
  # Sort and factor by TB time series length
  arrange(tb_n) %>% 
  ungroup() %>% 
  mutate(stocklong=factor(stocklong, levels=stocklong))

nrow(ne_data_stats)
sum(ne_data_stats$tb_n>0)
57-21

# Plot data availability
g <- ggplot(ne_data_stats, aes(x=stocklong, y=tb_n)) +
  geom_bar(stat="identity") +
  labs(y="Total biomass time series length (yr)") +
  geom_hline(yintercept=20, linetype="dotted") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y=element_blank())
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_ram_ne_tb_time_series_length.png"), 
       width=6.5, height=6.5, units="in", dpi=600)




