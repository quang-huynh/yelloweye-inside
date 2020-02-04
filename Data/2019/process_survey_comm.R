

library(dplyr)
library(reshape2)

HL <- readRDS("ye-ins-catch-hl.rds")
TR <- readRDS("ye-ins-catch-tr.rds")

# Issue with length of vector
length(1969:2018)
length(HL)
length(TR)

res <- readRDS("yelloweye-rockfish-ins-privacy.rds")
res$catch$total <- res$catch$sum_landed_kg + res$catch$sum_discarded_kg

comm <- summarise(group_by(res$catch, year, gear), total = sum(total)) %>% right_join(data.frame(year = 1969:2019)) %>% acast(list("year", "gear")) 

comm <- summarise(group_by(res$catch, year), total = sum(total)) %>% right_join(data.frame(year = 1969:2019))

### Use reconstructed catch except 1986 - 2018
cat <- read.csv("Data/2019/catch.csv")

### Survey age comps

library(reshape2)

sur_age <- acast(res[[2]], year ~ age, value.var = "age_specimen_collected", fun.aggregate = sum)
yrs = rownames(sur_age) %>% as.numeric()
ages <- colnames(sur_age) %>% as.numeric()

plot(ages, sur_age[2, ], typ = 'l')

