## @knitr siECIS ---

library(RSQLite)

Param <- "Z"
Freq <- 64000

if(!any(ls()%in%"ecis.sirna")) load("data/ecis.sirna.RData")

if(!Freq==unique(ecis.sirna$Freq)) {
  
  db <- dbConnect(SQLite(), dbname="~/Dropbox/ECIS/ECIS_Project/data/ECIS.sqlite") # open connection
  
  sqlcmd <- paste('SELECT * FROM Data 
                INNER JOIN siRNA using(Date,Well) 
                WHERE Mark = "Release" AND  
                celldensity = 5000 AND 
                Param = "',Param,'" AND
                Freq = "',Freq,'"', sep="")
  
  ecis.sirna <- dbGetQuery(db, sqlcmd)
  
  dbDisconnect(db)
  
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
  }

# Change factor levels order to make it compatible with CellGlo data
ecis.sirna$treatment %<>% factor(.,levels=c("siNTP","siCD44","siVIM","UT"))

ylabeller <- function(x){
  if(x=="C"){out<-paste0("C, Capacitance (F)")}
  if(x=="Z"){out<-paste0("Z, Impedance (Ohm)")}
  if(x=="R"){out<-paste0("R, Resistance (Ohm)")}
  out
}

grid_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="concGF") {value <- paste(value,"ng/ml")}
  return(value)
}


Myplot<-function(x) {
  ggplot(x,aes(timeBin,value,color=treatment)) + 
    facet_grid(Freq~concGF,scales="free") +
    stat_summary(fun.y=mean,geom="line",size=1) +
    stat_summary(fun.data=mean_se,geom="errorbar",size=0.2) +
    scale_color_colorblind() +
    ylab(ylabeller(unique(x$Param))) + 
    xlab("Time after release, h")
}

black_monk_theme <- function(){
  # theme_classic() +
  theme(plot.title = element_text(size=8),
        strip.background = element_rect(colour="white", fill="grey80"),
        legend.position="none")
}

# Raw VEGF data ----
gf.si.ecis <- ecis.sirna %>%
  ungroup %>%
  filter(GF%in%c("VEGF","bFGF")) %>% 
  mutate(GF=factor(GF,levels=c("VEGF","bFGF"),labels=c("VEGF","FGF2"))) %>% 
  Myplot + 
  facet_grid(GF~concGF, labeller = grid_labeller) +
  ylab(bquote(list(Impedance~"@"~.(Freq)~Hz,Omega))) +
  black_monk_theme()

fbs.si.ecis <- ecis.sirna %>%
  ungroup %>%
  filter(GF%in%c("FBS_5","FBS_20")) %>% 
  mutate(GF=factor(GF,levels=c("FBS_5","FBS_20"),labels=c("5% FBS","20% FBS"))) %>%
  Myplot + 
  facet_grid(~GF)+
  ylab(bquote(list(Impedance~"@"~.(Freq)~Hz,Omega))) +
  black_monk_theme()

gdf.si.ecis <- ecis.sirna %>%
  ungroup %>%
  filter(GF%in%c("GDF-2")) %>% 
  Myplot + 
  facet_grid(GF~concGF, labeller = grid_labeller) +
  ylab(bquote(list(Impedance~"@"~.(Freq)~Hz,Omega))) +
  black_monk_theme()