library(RSQLite)
library(magrittr)
library(plyr);library(dplyr)

db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection

dbListTables(db)
dbListFields(db, "Treatment")
dbListFields(db, "Data")

# load all impedance data for 'Model' 
imp <- dbGetQuery(db, 'SELECT *
                  FROM Data
                  INNER JOIN Treatment
                  using(Date,Well)
                  WHERE Mark = "Release" AND 
                  celldensity = 5000 AND Time <= 72')

pre <- dbGetQuery(db, 'SELECT *
                  FROM Data
                  INNER JOIN Treatment
                  using(Date,Well)
                  WHERE Mark = "Seed" OR Mark = "Starve" AND celldensity = 5000')
dbDisconnect(db)

# calculate hourly means
pre %<>% 
  mutate(timeBin=Time%>%cut(., floor(.)%>%unique, labels = FALSE, include.lowest = TRUE)) %>%
  filter(!is.na(timeBin)) %>%
  group_by(Date, Well, Param, timeBin) %>% 
  mutate(value=mean(value)) %>% 
  group_by(Well,Freq) %>% 
  mutate(value = value/value[1])

pre %>% glimpse
