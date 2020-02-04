
library(dplyr)
library(reshape2)

HL <- readRDS("ye-ins-catch-hl.rds")
TR <- readRDS("ye-ins-catch-tr.rds")

# Issue with length of vector
length(1969:2018)
length(HL)
length(TR)

res <- readRDS("Data/2019/yelloweye-rockfish-ins-privacy.rds")
res$catch$total <- res$catch$sum_landed_kg + res$catch$sum_discarded_kg
comm <- summarise(group_by(res$catch, year, gear), total = sum(total)) %>% right_join(data.frame(year = 1969:2019)) %>% acast(list("year", "gear")) 

# Hook and line from list and vector don't appear to match. Need to verify.
comm
HL
