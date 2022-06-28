
library(dplyr)
gris_us <- read_csv("gris_us_data.csv")


us_alien <- select(gris_us, scientificName, isInvasive, establishmentMeans)

us_inv <- us_alien %>%
  filter(isInvasive == "Invasive")

us_inv_rename <- rename(us_inv, species = scientificName, for_USA = isInvasive)

write_csv(us_inv_rename, "intermediate_data/US_invasive_method_1.csv")
