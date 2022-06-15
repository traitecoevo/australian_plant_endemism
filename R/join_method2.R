
library(dplyr)
natives_wc<- read.csv("intermediate_data/natives_with_counties_added.csv")
ausinv <- read.csv("data/aus_invasives_elsewhere.csv")

ausinv <- select(ausinv, -group)
ausinv <- rename(ausinv, 'canonicalName' = species)

join_na_ausinv <- left_join(natives_wc, ausinv, by = "canonicalName")

write.csv(join_na_ausinv,"intermediate_data/natives_with_invasives_method2.csv")
