library(RSQLite)
library(magrittr)
library(plyr);library(dplyr)
library(ggplot2)
library(ggthemes)

db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection
# dbListTables(db)
sqlcmd <- paste('SELECT * FROM Data 
                INNER JOIN siRNA using(Date,Well) 
                WHERE Mark = "Release" AND  
                celldensity = 5000',sep="")
ecis.sirna <- dbGetQuery(db, sqlcmd)
dbDisconnect(db)

# reset time and create time bins
ecis.sirna %<>% 
  group_by(Date) %>%
  mutate(Time = Time-Time[1]) %>% 
  mutate(timeBin = cut(Time, 
                       breaks = unique(floor(Time)),
                       labels = FALSE,
                       include.lowest=TRUE)) %>%
  (function(x) x[complete.cases(x),]) %>% {
    finito <- ddply(.,"timeBin", summarise, N = unique(Date) %>% length) %>%
      filter(N>1) %>% max
    .[.$timeBin<=finito,]
  }

ecis.sirna %<>% mutate(concGF = as.numeric(as.character(concGF)))
ecis.sirna$treatment[ecis.sirna$treatment=="SCR"] <- "siNTP"
save(ecis.sirna,file = "~/Dropbox/CD44_angiogenesis_paper/data/ecis.sirna.RData")


# plot stuff -----
library(Hmisc)
library(ggthemes)

ylabeller <- function(x){
  if(x=="C"){out<-paste0("C, Capacitance (F)")}
  if(x=="Z"){out<-paste0("Z, Impedance (Ohm)")}
  if(x=="R"){out<-paste0("R, Resistance (Ohm)")}
  out
} 

<<<<<<< HEAD
=======

>>>>>>> 9448df4ce2392791799449025fb668e1a233d3b9
Myplot<-function(x) {
  ggplot(x, aes(timeBin, value, color=treatment)) + 
    facet_grid(Freq~concGF, scales="free") +
    stat_summary(fun.data = mean_se, geom="errorbar",width=0.25) +
    stat_summary(fun.y = mean, geom="line") +
    scale_color_colorblind(breaks=c("UT","siNTP","siCD44","siVIM")) +
    ylab(ylabeller(unique(x$Param))) + xlab("Time (h)")
}

# Raw VEGF data ----
ecis.sirna %>% 
  filter(GF%in%c("VEGF","FBS_5")) %>% 
  dlply(.,"Param", Myplot)
lapply(plist, function(x) 
  ggsave(paste0("graphs/siRNA_VEGF_",
                regmatches(x$labels$y, regexpr("^.",x$labels$y)),
                "_", Sys.Date(),
                ".pdf"), plot = x))

# norm data ---
ecis.sirna %>%
  filter(GF%in%c("VEGF","FBS_5")) %>%
  group_by(Date,Well,Param,Freq) %>%
  mutate(value=value-mean(head(value,3))) %>%
  group_by(Param) %>%
  do(plots=Myplot(.)) %>% 
  .$plots %>%
  lapply(.%>%{
    pam <- regmatches(.$labels$y, regexpr("^.",.$labels$y))
    ggsave(paste0("graphs/siRNA_VEGF_",pam,"_norm_", Sys.Date(),".pdf"),plot=.,width=5,height=7)})

# bFGF data ----
plist <- sirna %>% filter(GF==c("bFGF","FBS_5")) %>% 
  dlply(.,"Param", Myplot)
lapply(plist, function(x) 
  ggsave(paste0("graphs/siRNA_bFGF_",
                regmatches(x$labels$y, regexpr("^.",x$labels$y)),
                "_", Sys.Date(),
                ".pdf"), plot = x))

# norm data ----
sirna %>% filter(GF%in%c("bFGF","FBS_5")) %>% 
  ddply(., .(Date,Well,Param,Freq), mutate, value=value-head(value,3)) %>%
  dlply(.,"Param", Myplot) %>% 
  lapply({.%>%(function(x) {
    pam <- regmatches(x$labels$y, regexpr("^.",x$labels$y))
    ggsave(paste0("graphs/siRNA_FGF2_",pam,"_norm_", Sys.Date(),".pdf"), plot = x,width=5,height=7)}
  )})

# FBS data ----
plist <- sirna %>% filter(GF%in%c("FBS_20","FBS_5")) %>% 
  dlply(.,"Param", Myplot) %>%
  lapply(., function(p) p + facet_grid(Hz~GF, scales="free"))
lapply(plist, function(x) 
  ggsave(paste0("graphs/siRNA_FBS_",
                regmatches(x$labels$y, regexpr("^.",x$labels$y)),
                "_", Sys.Date(),
                ".pdf"), plot = x))

# norm data ----
sirna %>% filter(GF%in%c("FBS_20","FBS_5")) %>% 
  ddply(., .(Date,Well,Param,Freq), mutate, value=value-head(value,3)) %>%
  dlply(.,"Param", Myplot) %>% 
  lapply({.%>%(function(x) {
    pam <- regmatches(x$labels$y, regexpr("^.",x$labels$y))
    ggsave(paste0("graphs/siRNA_FBS_",pam,"_norm_", Sys.Date(),".pdf"), plot = x,width=3,height=7)}
  )})