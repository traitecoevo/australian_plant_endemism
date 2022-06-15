
library(dplyr)
natives_wc<- read.csv("intermediate_data/natives_with_counties_added.csv")
auinv <- read.csv("data/aus_invasives_elsewhere.csv")

auinv <- select(auinv, -group)
auinv <- rename(auinv, 'canonicalName' = species)

join_na_auinv <- left_join(natives_wc, auinv, by = "canonicalName")

write.csv(join_na_auinv,"intermediate_data/natives_with_invasives_method2.csv")
