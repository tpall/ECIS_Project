# First part of the code is from summarise_treatments.R 

library(RSQLite)

db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection

dbListTables(db)
dbListFields(db, "Treatment")
dbListFields(db, "Data")

# load all modeling data for 'Model'
rba <- dbGetQuery(db, 'SELECT *
                  FROM Rbmodel
                  INNER JOIN Treatment
                  using(Date,Well)
                  WHERE celldensity = 5000')
dbDisconnect(db)            # Close connection

library(magrittr)
library(plyr);library(dplyr)

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

# Modeling data ----
rba %<>% Filter({. %>% unique %>% length %>% is_greater_than(.,1)}, .)
# reset Times so that they all start from zero and filter out time over 72 hours
rba %<>% group_by(Date) %>% mutate(Time = Time-Time[1])
# convert cols to numeric and factors
rba %<>% ungroup %>%
  mutate(concGF=concGF%>%make.numeric,
         dosestreatment=dosestreatment%>%make.numeric,
         Date=Date%>%as.factor,
         Well=Well%>%as.factor,
         Param=Param%>%as.factor,
         GF=GF%>%as.factor,
         treatment=treatment%>%as.factor)
rba %>% summary
rba %<>% 
  filter(dosestreatment==0|dosestreatment>1000) %>% # throw out small doses with no effect
#   filter(!Date=="20131213") %>% # 1. experiment and we had different treatment doses
  mutate(dosestreatment=ceiling(dosestreatment))

## For Modeling results split dataframe by date and growthfactor then simplify multilevel list structure
rba.pairs <- rba %>%
  dlply(.,.(Date,GF)) %>% lapply(Pairs) %>% 
  lapply(function(x) if (class(x) == "data.frame") list(x) else x)  %>% 
  unlist(recursive=FALSE)

rb.sum <- rba.pairs %>% 
  names %>% 
  sub("[0-9]{8}.([A-Zb2]{3,4}.[0-9]{1,2})","\\1",.) %>%
  unique %>% 
  sapply(.%>%paste0(.,"$")%>%grepl(.,rba.pairs%>%names)%>%"["(rba.pairs,.), simplify = FALSE) %>% 
  lapply(.%>%rbind_all%>%droplevels)


# Plotting compartment ----
library(ggplot2)
library(ggthemes)
library(Hmisc)
# load Myplot2 function
source("lib//Myplot2.R")

rb.sum[[2]] %>% Myplot2
rba.pairs %>% lapply({.%>%droplevels%>%summary})

rb.sum %>% lapply(summary)
rb.sum %>% lapply(.%>%select(dosestreatment)%>%unique)


rb.sum[[2]] %>% summary

rb.sum[[4]] %>% 
  ggplot(aes(x=RMSE)) +
  geom_histogram() +
  facet_wrap(~Date)

rb.sum[[4]] %>% 
  ggplot(aes(x=Drift)) +
  geom_histogram() +
  facet_wrap(~Date)

rb.sum %>% names
rb.sum[[2]] %>% 
  filter(!Date=="20131213") %>%
  filter(Time<=100) %>%
  group_by(Well,Date) %>%
  mutate(timeBin=Time%>%cut(., floor(.)%>%unique, labels = FALSE, include.lowest = TRUE)) %>%
  filter(!is.na(timeBin)) %>% 
  group_by(Param,Date,dosestreatment,treatment,timeBin,GF,concGF) %>% 
  summarise(value=median(value)) %>% 
  ungroup %>%
  mutate(treatment=ifelse(dosestreatment>0,as.character(treatment),"untreated")) %>%
  inset(,"treat2",paste0(.$concGF," ng/ml ",.$GF,"\n+",.$treatment)) %>% # create summary variable treat2
  mutate(treat2=gsub("SB101","3MUT",treat2)) %>%
  ggplot(.,aes(x=timeBin,y=value,color=treat2)) + 
  stat_summary(fun.data=mean_se,geom="errorbar",width=0.25,alpha=0.25) +
  stat_summary(fun.y=mean,geom="line") +
  facet_grid(Param~dosestreatment,scales = "free_y") +
  scale_color_colorblind()