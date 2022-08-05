library(tidyverse)

countries<-read_csv("intermediate_data/country_dataset.csv")

countries %>%
 pivot_longer(!species,names_to = "Country",values_to = "presence") %>%
  group_by(Country) %>%
  summarise(sum(presence))->out
 
write_csv(out,"intermediate_data/country_counts.csv")
