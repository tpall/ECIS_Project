library(RSQLite)
library(magrittr)
library(plyr);library(dplyr)
library(ggplot2)
library(ggthemes)

db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection

pre <- dbGetQuery(db, 'SELECT *
                  FROM Data
                  INNER JOIN Treatment
                  using(Date,Well)
                  WHERE Mark = "Seed" OR Mark = "Starv" OR Mark = "Preinc" AND celldensity = 5000')
dbDisconnect(db)

# calculate hourly means
ecispre <- pre %>% 
  mutate(timeBin=Time%>%cut(., floor(.)%>%unique, labels = FALSE, include.lowest = TRUE)) %>%
  filter(!is.na(timeBin)) %>% 
  group_by(Date) %>%
  mutate(timeBin=timeBin-max(timeBin)) %>%
  filter(timeBin>=-40) %>%
  group_by(Date, Well, Param, Freq) %>% 
  mutate(value = value/last(value)) %>%
  group_by(Date, Well, Param, Freq, timeBin) %>% 
  mutate(value=median(value)) %>% 
  group_by(Freq,Param,timeBin) %>%
  summarise(Mean=mean(value),
            SD=sd(value)) 
rm(pre)
# Data for angiogenesis article figure panel A ----
write.csv(ecispre,"data/ecis_preinc_data.csv") # data for article figure 

# Segment timeBins according to Mark ("Seed","Starve",etc)
# library(reshape2)
# coord<-pre %>% 
#   mutate(timeBin=Time%>%cut(., floor(.)%>%unique, labels = FALSE, include.lowest = TRUE)) %>%
#   filter(!is.na(timeBin)) %>% 
#   select(Date,timeBin,Mark) %>%
#   group_by(Date) %>%
#   mutate(timeBin=timeBin-max(timeBin)) %>%
#   filter(timeBin>=-40) %>%
#   distinct(Date,timeBin,Mark) %>% 
#   table %>% {
#     preinc<-(.)%>%"["(,,1) %>% colMeans %>% round(1)
#     seed<-(.)%>%"["(,,2) %>% colMeans %>% round(1)
#     starve<-(.)%>%"["(,,3) %>% colMeans %>% round(1)
#     rbind(seed,starve,preinc)
#   } %>% 
#   melt %>%
#   filter(value>=0.8) %>%
#   set_colnames(c("Mark","timeBin","value")) %>%
#   select(Mark,timeBin) %>%
#   group_by(Mark) %>%
#   summarise(xmin=min(timeBin),
#             xmax=max(timeBin))

# Figure panel A ----
ecispre %>%
  filter(Freq=="16000"&Param=="Z") %>%
  ggplot(aes(x=timeBin,y=Mean)) + 
  geom_line() +
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),size=0.2,alpha=0.75) +
  geom_vline(xintercept = c(-17.5,-1), linetype = "longdash") +
  ylab(bquote(list(Normalised~resistance, R/R[0]~(8000~Hz)))) +
  xlab("Time before release (h)") +
  annotate("text",x=c(-29,-10),y=c(2.25,2.25),label=c("20% FBS","1% FBS")) +
  annotate("text",x=0,y=2,label=c("preincubation"),angle=90)

# Facets for supplementary info Figure panel A
# ecispre %>%
#   ggplot(aes(x=timeBin,y=Mean)) + #,color=Mark
#   geom_line() +
#   geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),size=0.2,alpha=0.25) +
#   facet_grid(Param~Freq, scale="free_y") + 
#   ylab(bquote(list(Normalised~value, C~R~Z/C~R~Z[0]))) +
#   xlab("Time before release (h)") +
#   scale_x_continuous(breaks=c(-40,-20,0)) +
#   ggtitle("Frequency (Hz)") +
#   annotate("rect",xmin=-17.5,xmax=-1,ymin=-Inf,ymax=Inf,alpha = 0.25)

# What's with the differences
# pre %>%
#   mutate(timeBin=Time%>%cut(., floor(.)%>%unique, labels = FALSE, include.lowest = TRUE)) %>%
#   filter(!is.na(timeBin)) %>% 
#   group_by(Date) %>%
#   mutate(timeBin=timeBin-max(timeBin)) %>%
#   filter(timeBin>=-40) %>%
#   group_by(Date, Well, Param, Freq) %>% 
#   mutate(value = value/last(value)) %>%
#   select(1:6,13) %>%
#   group_by(Date, Well, Param, Freq) %>% 
#   mutate(change = value-lag(value)) %>% #  ,change = change-lag(change)
#   filter(!is.na(change)) %>%
#   group_by(Date,Well,Param,Freq,timeBin) %>% 
#   summarise(change=median(change)) %>% 
#   group_by(Freq,Param,timeBin) %>%
#   summarise(Mean=mean(change),
#             SD=sd(change)) %>%
#   ggplot(aes(x=timeBin,y=Mean)) +
#   geom_line() +
#   geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),size=0.2,alpha=0.25) +
#   facet_grid(Param~Freq, scale="free_y")
rm(pre)

db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection

imp <- dbGetQuery(db, 'SELECT *
                  FROM Data
                  INNER JOIN Treatment
                  using(Date,Well)
                  WHERE Mark = "Release" AND 
                  celldensity = 5000 AND 
                  Param = "Z" AND
                  Freq = "16000"')
dbDisconnect(db)

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
# remove columns containing only single value/no variation
# imp %<>% 
#   Filter({. %>% unique %>% length %>% is_greater_than(.,1)}, .)

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
  filter(dosestreatment==0|dosestreatment>100) %>% # throw out small doses with no effect
  filter(!Date=="20131213") %>% # 1. experiment and we had different treatment doses
  mutate(dosestreatment=ceiling(dosestreatment)) # round up to integers

#-----------------workzone----------------------
# calculate hourly means
# split dataframe by date, growthfactor and parameter and then 
# simplify multilevel list structure
imp.pairs <-imp %>% 
  mutate(timeBin=Time%>%cut(., floor(.)%>%unique, labels = FALSE, include.lowest = TRUE)) %>%
  filter(!is.na(timeBin)) %>% 
  group_by(Date,Well) %>% 
  mutate(value = value/value[1]) %>%
  group_by(Date,Well,timeBin,Freq,Param,GF,concGF,dosestreatment,treatment) %>% 
  summarise(value=mean(value)) %>%
  dlply(.(GF)) %>% lapply(Pairs) %>% 
  lapply(function(x) if (class(x) == "data.frame") list(x) else x)  %>% 
  unlist(recursive=FALSE)

Myplot <- function(mydf) mydf %>% 
  group_by(Freq,Param,dosestreatment,treatment,timeBin) %>%
  summarise(Mean=mean(value),
            SD=sd(value)) %>% {
              Fq <- use_series(.,"Freq") %>% as.character %>% as.numeric
              Par <- use_series(.,"Param") %>% unique %>% as.character
              parameter <- switch(Par,Z="impedance",
                                  R="resistance",
                                  C="capacitance")
              p <- ggplot(.,aes(x=timeBin,y=Mean,color=treatment)) + 
                geom_line(size=1) +
                geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),alpha=0.1) +
                facet_grid(~dosestreatment) +
                scale_color_colorblind() +
                ylab(bquote(list(Normalised~.(parameter), .(Par)/.(Par)[0]~(.(Fq)~Hz)))) +
                xlab("Time after release (h)")
              p
            }


imp.pairs %>% 
  lapply(Myplot)


