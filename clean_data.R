library(dplyr)
library(tidyr)
library(countrycode)

world_bank_tall = read.csv("data/world_bank_health.csv", na.strings = c(".."))
world_bank = world_bank_tall %>%
  select(Series.Code, Country.Code, X2014..YR2014.) %>%
  spread(Series.Code, X2014..YR2014.)
world_bank$code = countrycode(world_bank$Country.Code, "wb", "iso2c", warn = TRUE, nomatch = NA)

load("data/WV6_health.Rda")
WV6_subset_health[WV6_subset_health < 0] = NA
wvs = WV6_subset_health %>% 
  group_by(V2) %>%
  summarise(
    prostitution = mean(V203A, na.rm = T),
    abortion = mean(V204, na.rm = T),
    sex_before_marriage = mean(V206, na.rm = T),
    beat_wife = mean(V208, na.rm = T)
  )
wvs[sapply(wvs, is.nan)] = NA
wvs$code = countrycode(wvs$V2, "wvs", "iso2c", warn = TRUE, nomatch = NA)

load("data/longlat.Rda")

data = merge(world_bank, wvs, by = "code")
data = merge(data, longlat, by = "code")
data = data[data$code != "PS",]

save(data, file = "data/data.RData")
