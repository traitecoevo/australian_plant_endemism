
#package installation 
remotes::install_github("traitecoevo/austraits", build_vignettes = FALSE)
install.packages("treemap")
library(austraits) 
library(dplyr)
library(treemap)

#austraits growth form data set 
austraits <- load_austraits(version = "3.0.2", path = "intermediate_data/austraits")

#create new data frame with just the growth form values 
growth_form <- filter(austraits$traits, trait_name == "plant_growth_form") 

#select only necessary info
growth_form_subset <- select(growth_form, taxon_name, value)

#select only first one of each canonical name 
growth_form_ver1 <- growth_form_subset %>%
  distinct(taxon_name, .keep_all = TRUE)


#Endemic and non-endemic treemap process 
#load endemic dataset 
country <- read.csv("intermediate_data/country_dataset.csv")

#rename taxon to match name_use 
growth_form_ver1 <- rename(growth_form_ver1, 'species' = taxon_name)

#join two datasets 
join_data <- left_join(country, growth_form_ver1, by = "species")

#subset so only endemic 
endemic_subset <- join_data %>%
  group_by(aus_endemic) %>% 
  filter(aus_endemic == "TRUE")

#check the right number of observations
join_data %>% 
  group_by(aus_endemic) %>% 
  summarise(aus_endemic=n())

#endemic treemap data 
endemic_counts <- endemic_subset %>% 
  group_by(value) %>% 
  summarise(num_species=n())

#basic treemap 
treemap(endemic_counts, 
        index="value",
        vSize ="num_species", 
        type = "index")  


#playing around with colours 
#install package 
install.packages("RColourBrewer")
library("RColorBrewer")

treemap(endemic_counts, 
        index="value",
        vSize ="num_species", 
        type = "index",
        palette ="RdYlBu")  
        
treemap(endemic_counts, 
        index="value",
        vSize ="num_species", 
        type = "index",
        palette ="Pastel2",)

treemap(endemic_counts, 
        index="value",
        vSize ="num_species", 
        type = "index",
        palette ="Set3",)

treemap(endemic_counts, 
        index="value",
        vSize ="num_species", 
        type = "index",
        palette ="Accent",)

treemap(endemic_counts, 
        index="value",
        vSize ="num_species", 
        type = "index",
        palette ="BrBG",)

#non-endemic tree map 
#subset so only non-endemic 
nonendemic_subset <- join_data %>% 
  group_by(species) %>% 
  filter(aus_endemic == "FALSE")

#check the right number of observations
join_data %>% 
  group_by(aus_endemic) %>% 
  summarise(aus_endemic=n())

#non-endemic treemap data 
 nonendemic_counts<- nonendemic_subset %>% 
  group_by(value) %>% 
  summarise(num_species=n())

#basic treemap no groups
treemap(nonendemic_counts, 
        index="value",
        vSize ="num_species", 
        type = "index",
        palette ="BrBG",
        title = "Non-endemic Growth Form")


#creating larger groupings of endemic using recode (find out if there is a more succinct way)

#select only necessary info
endemic_value <- select(endemic_subset, species, value, aus_endemic) #why does it have to have aus_endemic 

unique(endemic_value$value) #check for recode
# larger groupings using recode 
endemic_group <- endemic_value %>% 
  group_by(value) %>%
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
                         rosette = "herb",
                         rosette_erect = "herb",
                         semi_basal = "herb", 
                         short_basal = "herb",
                         succulent_short = "succulent"
  ))

unique(endemic_group$group) #check 

#endemic treemap of large groups data 
endemic_group_counts <- endemic_group %>% 
  group_by(group) %>% 
  summarise(num_species=n())

treemap(endemic_group_counts,
        index= "group",
        vSize="num_species",
        type="index", 
        palette = "BrBG") 


#tree map with groups and subgroups 
#endemic treemap data 
endemic_subgroup_counts <- endemic_subset %>% 
  group_by(value) %>% 
  summarise(num_species=n())

#clean data and remove NA 
install.packages("janitor")
library(janitor)

endemic_counts <- na.omit(endemic_counts)

endemic_counts$value <- make_clean_names(endemic_counts$value)  

# larger groupings using recode 
endemic_subgroup <- endemic_counts %>% 
  group_by(value) %>%
  mutate (group = recode(value, 
                         climber_herb = "climber", 
                         climber_liana = "climber",
                         climber_palm = "climber", 
                         climber_liana_climber_vine = "climber",
                         climber_scrambler = "climber", 
                         climber_shrub = "climber", 
                         climber_twiner = "climber",
                         climber_vine = "climber", 
                         climber_vine_herbaceous = "climber",
                         climber_vine_woody = "climber",
                         climber_woody = "climber",
                         aquatic_herb = "aquatic",
                         epiphyte_parasite = "epiphyte", 
                         fern_tree = "fern",
                         graminoid_tussock = "graminoid",
                         hemi_epiphyte = "epiphyte", 
                         herb_shrub = "herb", 
                         herb_subshrub = "herb", 
                         herb_large = "herb", 
                         parasite_woody = "parasite", 
                         prostrate_herb = "prostrate", 
                         prostrate_shrub = "prostrate", 
                         rosette_erect = "rosette", 
                         shrub_subshrub = "shrub", 
                         shrub_tree = "shrub", 
                         shrub_treelet = "shrub",
                         subshrub = "shrub", 
                         treelet ="tree",
                         cushion = "herb",
                         climber_shrub_climber_tree = "climber",
                         erect_leafy = "herb",
                         long_basal = "herb",
                         rosette = "herb",
                         rosette_erect = "herb",
                         semi_basal = "herb", 
                         short_basal = "herb"
  ))
#reorder columns 
endemic_subgroup <- endemic_subgroup %>% select(group, everything())


#sub grouping treemap 
#basic
treemap(endemic_subgroup,
        index=c("group","value"),
        vSize="num_species",
        type="index") 
#fancier
treemap(endemic_subgroup,
        index=c("group","value"),
        vSize="num_species",
        type="index", 
        palette = "BrBG", 
        title = "Endemic Flora Growth Forms",
        fontsize.labels=c(15,12),                
        fontcolor.labels=c("white","grey"),    
        fontface.labels=c(2,1),                  
        bg.labels=c("transparent"),            
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                   
        overlap.labels=0,                     
        inflate.labels=F,
        border.col=c("black","white"),
        border.lwds=c(4,1), 
) 




#possible other method of endemic treemap group and subgroup data to look into more
endemic_counts2 <- endemic_group %>% 
  group_by(value) %>% 
  summarise(num_species=n(), 
            group = unique(group))


#looking at interactive treemaps 
install.packages("treemapify")

geom_treemap

library(tidyverse)
library(highcharter) 
install.packages("highcharter")
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
hc <- endemic_counts %>%
  hchart(
    "treemap", 
    hcaes(x = value, value = num_species, color = value)
  )
hc


