#joining ICUN redlist with endemic and non-endemics 

country <- read.csv("intermediate_data/country_dataset.csv")

ICUN <- read.csv("intermediate_data/assessments.csv")

ICUN <- rename(ICUN, 'species' = scientificName)

ICUN_subset <- select(ICUN, species, redlistCategory)

joined_data <- left_join(country, ICUN_subset, by = "species")

subset <- select(joined_data, species, redlistCategory, aus_endemic)

#endemic
endemic_subset <- subset %>%
  group_by(aus_endemic) %>% 
  filter(aus_endemic == "TRUE")

endemic_subset %>% 
  group_by(redlistCategory) %>% 
  summarise(endemic_subset=n())

#nonendemic 
nonendemic_subset <- subset %>%
  group_by(aus_endemic) %>% 
  filter(aus_endemic == "FALSE")

nonendemic_subset %>% 
  group_by(redlistCategory) %>% 
  summarise(nonendemic_subset=n())
