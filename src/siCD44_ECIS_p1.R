library(RSQLite)
library(magrittr)
library(plyr);library(dplyr)
library(ggplot2)
library(ggthemes)

Param <- "Z"
Freq <- 64000

db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection
# dbListTables(db)
sqlcmd <- paste('SELECT * FROM Data 
                INNER JOIN siRNA using(Date,Well) 
                WHERE Mark = "Release" AND  
                celldensity = 5000 AND 
                Param = "',Param,'" AND
                Freq = "',Freq,'"', sep="")
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
ecis.sirna$treatment %<>% factor(.,levels=c("UT","siNTP","siVIM","siCD44")) 

save(ecis.sirna,file = "~/Dropbox/CD44_angiogenesis_paper/data/ecis.sirna.RData")


summary(ecis.sirna)

black_monk_theme <- function(){
  theme_classic() +
  theme(plot.title = element_text(size=10),
        strip.background = element_rect(colour="white", fill="grey80"),
        legend.position="none")
}


# Raw VEGF data ----
ecis.sirna %>%
  filter(GF%in%c("VEGF","bFGF")) %>% 
  Myplot + 
  facet_grid(GF~concGF) +
  ggtitle(bquote(list(Growth~factor~concentration,ng/ml))) +
  ylab(bquote(list(Impedance~"@"~.(Freq)~Hz,Omega))) +
  black_monk_theme()

ecis.sirna %>%
  filter(GF%in%c("FBS_5","FBS_20")) %>% 
  mutate(GF=factor(GF,levels=c("FBS_5","FBS_20"),labels=c("5% FBS","20% FBS"))) %>%
  Myplot + 
  facet_grid(~GF)+
  ylab(bquote(list(Impedance~"@"~.(Freq)~Hz,Omega))) +
  black_monk_theme()

ecis.sirna %>%
  filter(GF%in%c("GDF-2")) %>% 
  Myplot + 
  scale_color_manual(values=colorblind_pal()(8)[-1]) +
  facet_grid(~concGF) +
  ggtitle(bquote(list(Growth~factor~concentration,ng/ml))) +
  ylab(bquote(list(Impedance~"@"~.(Freq)~Hz,Omega))) +
  black_monk_theme()