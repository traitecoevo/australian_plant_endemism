library(tidyverse)
library(rgbif)
apc <- read_csv("data/apc_native_only_mesaglio_6jun.csv")
apc$countries_present <- NA


for (i in 1:10) {
  key <-
    name_suggest(q = apc$canonicalName[i], rank = 'species')$data$key[1]
  first_species <- occ_search(taxonKey = key, limit = 100000)
  unique_countries <- unique(first_species$data$country)
  apc$countries_present[i] <- paste(unique_countries, collapse = "_")
}

write_csv(apc,"intermediate_data/natives_with_counties_added.csv")
