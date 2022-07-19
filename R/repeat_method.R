
install.packages("readr")

countries <- read_csv("intermediate_data/coutry_dataset.csv")
countries$genus  <- word(countries$species,1,1)

length(unique(countries$genus))
length(unique(countries$species))

Pollia <- filter(countries, genus == "Pollia")

group_by(genus) %>%
  summarize()