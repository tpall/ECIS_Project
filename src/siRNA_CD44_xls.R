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
sirna <- dbGetQuery(db, sqlcmd)
dbDisconnect(db)

# reset time and create time bins
sirna %<>% 
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

sirna %<>% 
  mutate(concGF <- as.numeric(as.character(concGF)),
         celldensity <- as.factor(celldensity),
         GF <- as.factor(GF),
         treatment <- as.factor(treatment))
  


# plot stuff -----
library(Hmisc)
library(ggthemes)

ylabeller <- function(x){
  if(x=="C"){out<-paste0("C, Capacitance (F)")}
  if(x=="Z"){out<-paste0("Z, Impedance (Ohm)")}
  if(x=="R"){out<-paste0("R, Resistance (Ohm)")}
  out
} 


Myplot<-function(x) {
  ggplot(x, aes(timeBin, value, color=treatment)) + 
    facet_grid(Freq~concGF, scales="free") +
    stat_summary(fun.data = mean_se, geom="errorbar",width=0.25) +
    stat_summary(fun.y = mean, geom="line") +
    scale_color_colorblind(breaks=c("UT","SCR","siCD44","siVIM")) +
    ylab(ylabeller(unique(x$Param))) + xlab("Time (h)")
}

# Raw VEGF data ----
plist <- sirna %>% filter(GF%in%c("VEGF","FBS_5")) %>% 
  dlply(.,"Param", Myplot)
lapply(plist, function(x) 
  ggsave(paste0("graphs/siRNA_VEGF_",
                   regmatches(x$labels$y, regexpr("^.",x$labels$y)),
                   "_", Sys.Date(),
                   ".pdf"), plot = x))
# norm data ---
sirna %>% filter(GF%in%c("VEGF","FBS_5")) %>% 
  ddply(., .(Date,Well,Param,Freq), mutate, value=value-head(value,3)) %>%
  dlply(.,"Param", Myplot) %>% 
  lapply({.%>%(function(x) {
    pam <- regmatches(x$labels$y, regexpr("^.",x$labels$y))
    ggsave(paste0("graphs/siRNA_VEGF_",pam,"_norm_", Sys.Date(),".pdf"),plot=x,width=5,height=7)}
    )})


# bFGF data ----
plist <- sirna %>% filter(GF%in%c("bFGF","FBS_5")) %>% 
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