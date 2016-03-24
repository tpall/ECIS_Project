library(RSQLite)
library(magrittr)
library(plyr);library(dplyr)
library(ggplot2)
library(ggthemes)

db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection
# dbListTables(db)
sqlcmd <- paste('SELECT * FROM Data 
                INNER JOIN Treatment using(Date,Well) 
                WHERE Mark = "Release" AND  
                celldensity = 5000')
imp <- dbGetQuery(db, sqlcmd)
dbDisconnect(db)

# Lets save data to 
# save(imp,file = "~/Dropbox//CD44_angiogenesis_paper/data/ecis_imp.R")

# Helper function ----
# function to convert factors to numerics
make.numeric <- . %>% as.character %>% as.numeric

# function to split experiments by different uninduced-induced pairs. Pairs is function importante!!!!
Pairs <- . %>% {
  fargs <- use_series(., concGF) %>%
    unique %>%
    (function(x) if(length(x) == 1) data.frame(ind = x) else 
      expand.grid(ctrl = x[1], ind = x[-1])) %>% 
    (function(x) split(x,f=x$ind))
  lapply(fargs, function(x) filter(.,concGF%in%x))
}

# Impedance etc data ----

# reset Times so that they all start from zero and filter data under 72 hours
imp %<>% 
  group_by(Date) %>% 
  mutate(Time = Time-Time[1]) %>%  
  filter(Time<=73)

# convert cols to numeric and factors
imp %<>% ungroup %>%
  mutate(concGF=concGF%>%make.numeric,
         dosestreatment=dosestreatment%>%make.numeric,
         Date=Date%>%as.factor,
         Well=Well%>%as.factor,
         Freq=Freq%>%as.factor,
         Param=Param%>%as.factor,
         GF=GF%>%as.factor,
         treatment=treatment%>%as.factor)

# dosestreatment should be binned for each growth factor: but only for summary
imp %<>% 
  filter(dosestreatment==0|dosestreatment>1000) %>% # throw out small doses with no effect
  filter(Date!="20131213") %>% # 1. experiment and we had different treatment doses
  mutate(dosestreatment = ceiling(dosestreatment)) # round up to integers

# calculate hourly means ----
imp.pairs <- imp %>% 
  group_by(Well, Date, Param, Freq) %>% # group and normalise to first 3 values each well in each experiment
  mutate(value = value / (value %>% head(n=3) %>% mean)) %>%
  mutate(timeBin = Time %>% cut(., floor(.)%>%unique, labels = FALSE, include.lowest = TRUE)) %>% # calculate hourly means and 
  filter(!is.na(timeBin)) %>% 
  group_by(Freq, Param, Date, concGF, dosestreatment, GF, treatment, timeBin) %>% # summarise each experiment
  summarise(value = mean(value)) %>%
  ungroup %>%
  mutate(treatment = ifelse(dosestreatment > 0, as.character(treatment), "untreated")) %>%
  inset(,"treat2", paste0(.$concGF," ng/ml ",.$GF,"\n+",.$treatment)) %>% # create summary variable treat2
  mutate(treat2 = gsub("SB101", "3MUT", treat2)) %>%
  dlply(.(GF)) %>% lapply(Pairs) %>% # split data into treated untreated pairs
  lapply(function(x) if (class(x) == "data.frame") list(x) else x)  %>% 
  unlist(recursive = FALSE) %>% # filter out controls from other experiments
  lapply({ .%>% {datestouse <- filter(., dosestreatment > 0) %>% use_series(Date) %>% unique %>% droplevels
  filter(., Date %in% datestouse)}}) 

imp.pairs %>% names()

# lets select only VEGF 25 data for plotting
imp.pairs[[1]] %>% filter(Param=="Z") %>%
  ggplot(aes(x = timeBin, y = value, color = treat2)) +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.data = "mean_se", geom = "errorbar") + 
  facet_grid(Freq~dosestreatment, scales = "free_y") +
  scale_color_colorblind()


