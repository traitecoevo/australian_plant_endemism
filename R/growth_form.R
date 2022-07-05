
#install.packages("remotes")
remotes::install_github("traitecoevo/austraits", build_vignettes = FALSE)
library(austraits) 
library(dplyr)

#import data files 
natives_we<- read.csv("intermediate_data/natives_with_endemic_column.csv")
austraits <- load_austraits(version = "3.0.2", path = "intermediate_data/austraits")

#create new data frame with just the growth form values 
growth_form <- filter(austraits$traits, trait_name == "plant_growth_form") 

#rename taxon to match canonical name 
growth_form <- rename(growth_form, 'name_use' = taxon_name)

#select only necessary columns
growth_form_subset <- select(growth_form, name_use, value)

#select only first one of each canonical name 
growth_form_ver2 <- growth_form_subset %>%
  distinct(canonicalName, .keep_all = TRUE)


growth_form_ver3 <- growth_form_subset %>% 
  group_by(name_use) %>% 
  summarise(value = paste(value, collapse = ",")) #adds all rows 



install.packages("treemap") #install treemap package 
library(treemap)

treemap(data,
        index="group",
        vSize="value",
        type="index"
)


df %>%
  group_by(name, type) %>%
  mutate(count = n())

treemap_dataframe <- e %>%
  group_by(value) %>% 
  mutate(count = n())

df <- data_frame(growth_form=c('herb', 'shrub', 'subshrub', 'tree', 'shrubtree', 'aquatic', 'aquatic_herb', 'climber', 'climber_herb', 'climber_lina', 'climber_shrub'), 
                 value=c(4867, 6785, 100, 1747, 392, 7, 3, 43, 4, 46, 13))
treemap(df, 
        index="growth_form",
        vSize ="value", 
        type = "index")

  

