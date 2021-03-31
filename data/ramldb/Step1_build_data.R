
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


# RAM data
################################################################################

# NE stocks
ne_stocks <- stock_key %>% 
  filter(region=="US East Coast")

# Build data
data_ram1 <- timeseries_values_views %>% 
  # Reduce to NE stocks
  filter(stockid %in% ne_stocks$stockid) %>% 
  # Reduce to columns of interest
  select(stockid, year, TC, TL, TB, SSB, R) %>% 
  rename(tc=TC, tl=TL, tb=TB, ssb=SSB, r=R) %>% 
  # Add units
  left_join(timeseries_units_views %>% select(stockid, TC, TL, TB, SSB, R), by="stockid") %>% 
  rename(tc_units=TC, tl_units=TL, tb_units=TB, ssb_units=SSB, r_units=R) %>% 
  # Select best catch
  mutate(catch=ifelse(!is.na(tc) & tc>=tl, tc, tl),
         catch_units=ifelse(!is.na(tc) & tc>=tl, tc_units, tl_units),
         catch_type=ifelse(!is.na(tc) & tc>=tl, "TC", "TL")) %>% 
  # Arrange
  mutate(source="RAM") %>% 
  select(source, stockid, year, catch_type, catch, catch_units, tb, tb_units, ssb, ssb_units, r, r_units) %>% 
  # Filter
  filter(!is.na(catch) & !is.na(tb) & tb_units==catch_units)
  
# Inspect units
table(data_ram1$tb_units)
table(data_ram1$catch_units)
table(data_ram1$ssb_units)
table(data_ram1$r_units)

# Simplify data since units are the same
data_ram2 <- data_ram1 %>% 
  select(-c(tb_units, catch_units)) %>% 
  rename(tb_mt=tb, catch_mt=catch)

# Sample size
n_distinct(data_ram2$stockid)


# John's data
################################################################################

# Read John's data
# B_tot is total biomass
# B_exp is the exploitable biomass (B_tot adjusted for biomass and selectivity at age)
# Catch is the total catch
data_john_orig <- read.csv(file.path(inputdir, "biomass_estimates_for_Chris.csv"), as.is=T)

# Build key
stocks_john <- data_john_orig %>% 
  # Clean name
  janitor::clean_names(case="snake") %>% 
  rename(stockid_orig=stock) %>% 
  # Inspect sample size
  group_by(stockid_orig) %>% 
  filter(!is.na(b_tot)) %>% 
  summarize(yr1=min(year),
            yr2=max(year),
            nyr=n()) %>% 
  # Add stock name
  mutate(stocklong=recode(stockid_orig, "Atl_Herring"="Herring Northwestern Atlantic Coast", 
                                 "Bluefish"="Bluefish Atlantic Coast", 
                                 "BSB"="Black sea bass Mid-Atlantic Coast",  
                                 "CCGOM_Yellow"="Yellowtail flounder Cape Cod / Gulf of Maine", 
                                 "GB_Cod"="Atlantic cod Georges Bank", 
                                 "GB_Haddock"="Haddock Georges Bank", 
                                 "GB_Winter"="Winter flounder Georges Bank", 
                                 "GB_Yellow"="Yellowtail flounder Georges Bank", 
                                 "GOM_Cod"="Atlantic cod Gulf of Maine",  
                                 "GOM_Haddock"="Haddock Gulf of Maine", 
                                 "Plaice"="American plaice Gulf of Maine / Georges Bank", 
                                 "Pollock"="Pollock Gulf of Maine / Georges Bank", 
                                 "Redfish"="Acadian redfish Gulf of Maine / Georges Bank",  
                                 "Scup"="Scup Northwestern Atlantic Coast", 
                                 "SNEMA_Winter"="Winter flounder Southern New England /Mid Atlantic",  
                                 "SNEMA_Yellow"="Yellowtail flounder Southern New England /Mid Atlantic",  
                                 "Summer"="Summer flounder Mid-Atlantic Coast",  
                                 "White_Hake"="White hake Gulf of Maine / Georges Bank", 
                                 "Witch"="Witch flounder Gulf of Maine")) %>% 
  # Add stockid
  left_join(stock_key %>% select(stocklong, stockid), by="stocklong") %>% 
  # Arrange
  select(stockid_orig, stockid, stocklong, everything())

# Are all of John's stock's inside RAM database?
if(sum(!stocks_john$stocklong %in% ne_stocks$stocklong)>0){print("STOP: Some of John's stocks aren't in RAM.")}

# Format John's data
data_john <- data_john_orig %>% 
  # Simplify columns
  setNames(tolower(colnames(.))) %>% 
  rename(stockid_orig=stock, tb_mt=b_tot, catch_mt=catch) %>% 
  select(stockid_orig, year, tb_mt, catch_mt) %>% 
  # Add stockid
  left_join(stocks_john %>%  select(stockid_orig, stockid), by="stockid_orig") %>% 
  # Add new columns
  mutate(source="Wiedenmann", catch_type="TC") %>% 
  # Add SSB/R from RAM
  left_join(timeseries_values_views %>% select(stockid, year, SSB, R), by=c("stockid", "year")) %>% 
  rename(ssb=SSB, r=R) %>% 
  # Add SSB/R units from RAM
  left_join(timeseries_units_views %>% select(stockid, SSB, R), by=c("stockid")) %>% 
  rename(ssb_units=SSB, r_units=R) %>% 
  # Arrange columns
  select(source, stockid, year, catch_type, catch_mt, tb_mt, ssb, ssb_units, r, r_units) %>% 
  # Filter to required data
  filter(!is.na(tb_mt) & !is.na(catch_mt))

# Sample size
n_distinct(data_john$stockid)


# Merge RAM and Wiedenmann data
################################################################################

# Compare sample sizes for overlapping stocks
ram_n <- data_ram2 %>% 
  group_by(stockid) %>% 
  summarize(n_ram=n())

john_n <- data_john %>% 
  group_by(stockid) %>% 
  summarize(n_john=n())

# John's rarely have more time points -- leave out
data_n <- ram_n %>% 
  left_join(john_n)

# Use John's data over RAM data
data_john_use <- data_john %>% 
  filter(!stockid %in% data_ram2$stockid)

# Merge data
data <- bind_rows(data_ram2, data_john_use) %>% 
  # Arrange
  arrange(stockid, year) %>% 
  # Add surplus production
  group_by(stockid) %>% 
  mutate(sp_mt=splink::calc_sp(biomass=tb_mt, catch=catch_mt)) %>% 
  ungroup() %>% 
  # Arrange columns
  select(source:tb_mt, sp_mt, everything())

# Inspect
str(data)
freeR::complete(data)
n_distinct(data$stockid)


# Export data
################################################################################

# Export data
saveRDS(data, file.path(outputdir, "RAM_NE_data.Rds"))








