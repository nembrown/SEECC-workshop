install.packages("tidyverse")
library(tidyverse)

load("LPIdata_Feb2016.RData")
load("puffin_GBIF.RData")


puffins<-puffin_GBIF

head(puffins)

View(head(LPIdata_Feb2016))

names(LPIdata_Feb2016)


LPI_long <- gather(data = LPIdata_Feb2016, key = "year", value = "pop", select = 26:70)

LPI_long<-as.tibble(LPI_long)


LPI_long_distinct <- distinct(LPI_long)


LPI_long_fl <- filter(LPI_long, is.finite(pop))

LPI_long <- LPI_long_fl %>%
  group_by(genus_species_id) %>%  # group rows so that each group is one population
  mutate(maxyear = max(year), minyear = min(year),  # Create columns for the first and most recent years that data was collected
         lengthyear = maxyear-minyear,  # Create a column for the length of time data available
         scalepop = (pop-min(pop))/(max(pop)-min(pop))) %>%  # Scale population trend data so that all values are between 0 and 1
  filter(is.finite(scalepop),  # remove NAs
         lengthyear > 5) %>%  # Only keep rows with more than 5 years of data
  ungroup() 

