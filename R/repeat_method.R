
#install.packages("readr")
#install.packages("rlang")
#install.packages("dplyr")

remotes::install_github("wcornwell/taxonlookup")
library(taxonlookup)
library(readr)
library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)

countries <- read_csv("intermediate_data/country_dataset.csv")
countries$genus <- word(countries$species,1,1)

Pollia <- filter(countries, genus == "Pollia")

countries %>%
  group_by(genus) %>%
  summarize(genus_endemic = all(aus_endemic),
            prop_sp_endemic = sum(aus_endemic)/n()) -> genera

genera %>%
  ggplot(aes(x=prop_sp_endemic))+geom_histogram()

devtools::install_github("wcornwell/taxonlookup")

library(taxonlookup)
lt <- lookup_table(genera$genus, by_species = TRUE)

genera %>%
  left_join(lt) -> genus_with_family

genus_with_family %>%
  group_by(family) %>%
  summarize(prop_endemic = sum(genus_endemic)/n(),n()) -> family_summary

family_summary %>%
  ggplot(aes(x=prop_endemic)) + geom_histogram()

lt <- lookup_table(countries$species, by_species = TRUE)
lt <- rownames_to_column(lt, "species")

left_join(countries, lt) -> species_with_family

species_with_family %>%
  group_by(group) %>%
  summarize(prop_endemic = sum(aus_endemic)/n(), n=n())

