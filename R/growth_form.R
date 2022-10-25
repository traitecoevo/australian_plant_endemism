#package installation 
#remotes::install_github("traitecoevo/austraits", build_vignettes = FALSE)
#install.packages("dplyr")
#install.packages("treemap")
#install.packages("RColorBrewer")
library(austraits) 
library(dplyr)
library(treemap)
library(RColorBrewer)

#austraits growth form data set 
austraits <- load_austraits(version = "3.0.2", path = "intermediate_data/austraits")

#create new data frame with just the growth form values 
growth_form <- filter(austraits$traits, trait_name == "plant_growth_form")

#load native plant dataset 
country <- read.csv("intermediate_data/country_dataset.csv")


#process of joining the two data sets 
growth_form <- select(growth_form, "species" = taxon_name, value) |> #select only necessary info and rename taxon to species 
  distinct(species, .keep_all = TRUE) #select only first one of each canonical name 

#join two datasets 
join_data <- left_join(country, growth_form, by = "species") |>
  select(species, aus_endemic, value) #select only relevant information


#creating larger groupings 
unique(join_data$value) #check what current growth forms the data set has

#create larger groupings using recode (there has got to be a better way of doing this)
#also need to update what I group as what
complete_group <- join_data |>
  group_by(value) |>
  mutate (group = recode(value, 
                         climber_herb = "climber", 
                         climber_liana = "climber",
                         climber_palm = "climber", 
                         "climber_liana climber_vine" = "climber",
                         climber_scrambler = "climber", 
                         climber_shrub = "climber", 
                         climber_twiner = "climber",
                         climber_vine = "climber", 
                         climber_vine_herbaceous = "climber",
                         climber_vine_woody = "climber",
                         climber_woody = "climber",
                         "climber_shrub climber_tree" = "climber",
                         aquatic_herb = "aquatic",
                         "epiphyte parasite" = "epiphyte", 
                         fern_tree = "fern",
                         graminoid_tussock = "graminoid",
                         "hemi-epiphyte" = "epiphyte", 
                         "herb shrub" = "herb", 
                         "herb subshrub" = "herb", 
                         herb_large = "herb", 
                         parasite_woody = "parasite", 
                         prostrate_herb = "prostrate", 
                         prostrate_shrub = "prostrate", 
                         rosette_erect = "rosette", 
                         "shrub subshrub" = "shrub", 
                         "shrub tree" = "shrub", 
                         "shrub treelet" = "shrub",
                         subshrub = "shrub", 
                         treelet ="tree",
                         cushion = "herb",
                         climber_shrub_climber_tree = "climber",
                         erect_leafy = "herb",
                         long_basal = "herb",
                         "rosette" = "herb",
                         rosette_erect = "herb",
                         semi_basal = "herb", 
                         short_basal = "herb",
                         succulent_short = "succulent",
                         "epiphyte_herb" = "epiphyte"
  ))

#check the grouping values 
unique(complete_group$group)

#select only necessary info
complete_group <- select(complete_group, species, value, aus_endemic, group)

#growth form of all natives larger groups
natives <- complete_group |>
  group_by(group) |>
  summarise(num_species=n())

#treemap
treemap(natives,
        index= "group",
        vSize="num_species",
        vColor = "num_species",
        type="value")

#ENDEMIC
endemic_group <- complete_group |>
  group_by(aus_endemic) |>
  filter(aus_endemic == "TRUE") 

endemic_group_counts <- endemic_group |>
  group_by(group) |>
  summarise(num_species=n())

treemap(endemic_group_counts, 
        index= "group",
        vSize="num_species",
        vColor = "num_species",
        type="value")

#NONENDEMIC
nonendemic_group <- complete_group |>
  group_by(aus_endemic) |> 
  filter(aus_endemic == "FALSE")

nonendemic_group_counts <- nonendemic_group |>
  group_by(group) |>
  summarise(num_species=n())

treemap(nonendemic_group_counts,
        index= "group",
        vSize="num_species",
        vColor = "num_species",
        type="value")


#possible stats??
chisq.test(complete_group$aus_endemic, complete_group$group)
