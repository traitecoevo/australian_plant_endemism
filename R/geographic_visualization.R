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

country_count = "https://raw.githubusercontent.com/traitecoevo/australian_plant_endemism/master/intermediate_data/country_counts.csv"
country_count<-read.csv(url(country_count))
View(country_count)
