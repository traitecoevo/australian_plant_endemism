country_dataset="intermediate_data/country_dataset.csv"
country_dataset<-read.csv(url(country_dataset))
View(country_dataset)

install.packages("cowplot")
install.packages("ggspatial")
install.packages("ggrepel")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturaldata")


library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)

country_count_basic = "https://raw.githubusercontent.com/traitecoevo/australian_plant_endemism/master/intermediate_data/country_counts.csv"
country_count_basic<-read_csv(url(country_count_basic))
View(country_count_basic)
country_count_basic$name<-gsub("_"," ",country_count_basic$Country)
country_count_basic$name<-str_to_title(country_count_basic$name)


country_count <- ne_countries(scale = "medium", returnclass = "sf")
class(country_count)

cc<-left_join(country_count,country_count_basic)

ggplot(data = cc) +
  geom_sf(aes(fill = `sum(presence)`),size=0.1)
