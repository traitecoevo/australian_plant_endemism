
#package installation 
remotes::install_github("traitecoevo/austraits", build_vignettes = FALSE)
install.packages("treemap")
library(austraits) 
library(dplyr)
library(treemap)
library("RColorBrewer")

#austraits growth form data set 
austraits <- load_austraits(version = "3.0.2", path = "intermediate_data/austraits")

#create new data frame with just the growth form values 
growth_form <- filter(austraits$traits, trait_name == "plant_growth_form") 


#select only necessary info
growth_form_subset <- select(growth_form, taxon_name, value)

#select only first one of each canonical name 
growth_form <- growth_form_subset %>%
  distinct(taxon_name, .keep_all = TRUE)


#Endemic and non-endemic treemap process 
#load endemic dataset 
country <- read.csv("intermediate_data/country_dataset.csv")

country <- read.csv("intermediate_data/natives_with_endemic_column.csv")

aus_data <- country %>% 
  mutate(aus_endemic(across(.cols = everything(), if_any(= FLASE, TRUE, FALSE))))

aus_data <- country %>% 
  group_by(species) %>%
  mutate(aus_endemic(if_any(contains(TRUE), FALSE)))

aus_data <- country %>%
  group_by(species) %>%
  mutate(
    categrory = case_when(
      if_any(contains("TRUE"), FALSE)))

aus_data <- country %>% 
  group_by(species) %>%
  mutate(aus_endemic = complete.cases(across(everything("TRUE"))))

#rename taxon to match name_use 
growth_form <- rename(growth_form, 'species' = taxon_name)

#join two datasets 
join_data <- left_join(country, growth_form, by = "species")

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

#all endemic values treemap 
treemap(endemic_counts, 
        index="value",
        vSize ="num_species", 
        type = "index",
        palette ="BrBG")

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

#all non-endemic values treemap 
treemap(nonendemic_counts, 
        index="value",
        vSize ="num_species", 
        type = "index",
        palette ="BrBG",
        )


#creating larger groupings of endemic using recode (find out if there is a more succinct way)

#select only necessary 
#use original joined data 
#check values for recode 
unique(join_data$value)

#create larger groupings using recode 
complete_group <- join_data %>% 
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

#non-endemic larger group
#select only necessary info
nonendemic_value <- select(nonendemic_subset, species, value, aus_endemic) #why does it have to have aus_endemic 

unique(nonendemic_value$value) #check for recode
# larger groupings using recode 
nonendemic_group <- nonendemic_value %>% 
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
                         "climber_shrub climber_tree" = "climber",
                         erect_leafy = "herb",
                         long_basal = "herb",
                         rosette = "herb",
                         rosette_erect = "herb",
                         semi_basal = "herb", 
                         short_basal = "herb",
                         succulent_short = "succulent"
  ))

unique(nonendemic_group$group) #check 

#endemic treemap of large groups data 
nonendemic_group_counts <- nonendemic_group %>% 
  group_by(group) %>% 
  summarise(num_species=n())

treemap(nonendemic_group_counts,
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

endemic_subgroup_counts <- na.omit(endemic_subgroup_counts)

endemic_counts$value <- make_clean_names(endemic_counts$value)  

# larger groupings using recode 
endemic_subgroup <- endemic_subgroup_counts %>% 
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
hc <- endemic_group_counts %>%
  hchart(
    "treemap", 
    hcaes(x = group, value = num_species, color = value)
  )
hc

endemic_group_counts <- na.omit(endemic_group_counts)

hc <- endemic_group_counts %>%
  hchart(
    "treemap", 
    hcaes(x = group, value = num_species, color = value)
  )
hc


#other attempt

cols <- endemic_counts %>% 
  count(value, num_species,  sort = TRUE) %>% 
  pull(num_species) %>% 
  unique()

hchart(
  data_to_hierarchical(endemic_subgroup, c(group, value), num_species),
  type = "treemap",
  # levelIsConstant = FALSE,
  allowDrillToNode = TRUE,
  levels = lvl_opts,
  tooltip = list(valueDecimals = FALSE)
) %>% 
  hc_chart(
    style = list(fontFamily = "Arial")
  ) %>% 
  hc_title(
    text = "Endemic Growth Forms",
    style = list(fontFamily = "Arial")
  ) %>% 
  hc_size(height = 700)

lvl_opts <-  list(
  list(
    level = 1,
    borderWidth = 0,
    borderColor = "transparent",
    dataLabels = list(
      enabled = TRUE,
      align = "left",
      verticalAlign = "top",
      style = list(
        fontSize = "12px", 
        textOutline = FALSE,
        color = "white",
        fontWeight = "normal"
      )
    )
  ),
  list(
    level = 2,
    borderWidth = 0,
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 0.50),
    dataLabels = list(enabled = FALSE),
    style = list(
      fontSize = "10px",
      textOutline = FALSE, 
      color = "white", 
      fontWeight = "normal"
    )
  )
)

cols <- endemic_subgroup %>% 
  count(group, value, num_species,  sort = TRUE) %>% 
  pull(num_species) %>% 
  unique()


hchart(
  data_to_hierarchical(endemic_subgroup, c(group, value), num_species),
  type = "treemap",
  # levelIsConstant = FALSE,
  allowDrillToNode = TRUE,
  levels = lvl_opts,
  tooltip = list(valueDecimals = FALSE)
) %>% 
  hc_chart(
    style = list(fontFamily = "Arial")
  ) %>% 
  hc_title(
    text = "Endemic Growth Forms",
    style = list(fontFamily = "Arial")
  ) %>% 
  hc_size(height = 700)

