# library(forecast)
library(magrittr)
library(plyr)
library(dplyr)

function(){# create timeBin variable for calcultion of hourly means and plotting
rba$timeBin <- rba %>% 
  use_series(Time) %>% 
  cut(., unique(floor(.)), labels = FALSE, include.lowest = TRUE)

# calculate hourly means
rbh <- rba %>% filter(!is.na(timeBin)) %>%
  group_by(Date, Well, Param, timeBin) %>%
  summarise(value=mean(value))
rbh %>% head
rbh %>% use_series("Well") %>% class
rbh %>% use_series("Date") %>% class
rbh$value %>% class

md <- dbGetQuery(db, 'SELECT * FROM Treatment')
md %>% head
md %>% use_series("Well") %>% class
md %>% use_series("Date") %>% class

md$Well %<>% as.factor
md$Date %<>% as.factor
md$celldensity %<>% as.factor
md$GF %<>% as.factor
md$treatment %<>% as.factor

md$concGF %<>% as.numeric
md$dosestreatment %<>% as.numeric

inner_join(rbh, md)
}

