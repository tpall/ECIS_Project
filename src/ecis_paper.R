library(RSQLite)
library(magrittr)
library(plyr);library(dplyr)
library(ggplot2)
library(ggthemes)

db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection

# load all impedance data for 'Model' 
# imp <- dbGetQuery(db, 'SELECT *
#                   FROM Data
#                   INNER JOIN Treatment
#                   using(Date,Well)
#                   WHERE Mark = "Release" AND 
#                   celldensity = 5000 AND Time <= 72')

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
  filter(Freq=="8000"&Param=="R") %>%
  ggplot(aes(x=timeBin,y=Mean)) + #,color=Mark
  geom_line() +
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),size=0.2,alpha=0.75) +
  geom_vline(xintercept = c(-17.5,-1), linetype = "longdash") +
  ylab(bquote(list(Normalised~resistance, R/R[0]~(8000~Hz)))) +
  xlab("Time before release (h)") +
  annotate("text",x=c(-29,-10),y=c(2.5,2.25),label=c("20% FBS","1% FBS")) +
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
