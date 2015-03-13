library(RSQLite)
library(magrittr)
library(plyr);library(dplyr)
library(ggplot2)
library(ggthemes)

Param <- "Z"
Freq <- 16000

db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection
sqlcmd <- paste('SELECT * FROM Data
                  INNER JOIN Treatment using(Date,Well)
                  WHERE Mark IN ("Seed","Starv","Preinc") AND 
                  celldensity = 5000 AND
                  Param = "',Param,'" AND
                  Freq = "',Freq,'"',sep="")
pre <- dbGetQuery(db, sqlcmd)
dbDisconnect(db)

# calculate hourly means
ecispre <- pre %>% # calculate timeBins
  mutate(timeBin=Time%>%cut(., floor(.)%>%unique, labels = FALSE, include.lowest = TRUE)) %>%
  filter(!is.na(timeBin)) %>% 
  group_by(Date) %>% # adjust timeseries
  mutate(timeBin=timeBin-max(timeBin)) %>%  
  filter(timeBin>=-40) %>% 
  group_by(Freq,Well,Param,Date) %>% # normalise values by dividing with last value
  mutate(value = value/mean(value%>%tail(n=3)%>%mean)) %>% 
#   group_by(Freq,Param,Date) %>% # min-max normalise 
#   mutate(value = (value-min(value))/(max(value)-min(value))) %>%
  group_by(Freq,Param,Date,timeBin) %>% # calculate experiment means
  summarise(value=mean(value)) %>%
  group_by(Freq,Param,timeBin) %>% # calculate summary from experiment means
  summarise(Mean=mean(value),
            SD=sd(value),
            N=length(value),
            SE=SD/sqrt(N))

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
Fig3A <- ecispre %>% {
  Fq <- use_series(.,"Freq") %>% as.character %>% as.numeric
  Par <- use_series(.,"Param") %>% unique %>% as.character
  parameter <- switch(Par,Z="impedance",
                      R="resistance",
                      C="capacitance")
  y.pos <- filter(.,timeBin==-17) %>% select(Mean,SD)
  p <- ggplot(.,aes(x=timeBin,y=Mean)) + 
    geom_line(size=1) +
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),size=0.2,alpha=0.75) +
    geom_vline(xintercept = c(-17.5,-1), linetype = "longdash") +
    ylab(bquote(list(Normalised~.(parameter), .(Par)/.(Par)[0]~(.(Fq)~Hz)))) +
    xlab("Time before release, h") +
    annotate("text",x=c(-29,-10),y=y.pos$Mean*1.2,label=c("20% FBS","1% FBS"), size=3) +
    annotate("text",x=1,y=y.pos$Mean*0.9,label="1~h~preincubation%->%release",parse = TRUE,angle=90, size=3)
  p}

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


db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection
sqlcmd <- paste('SELECT * FROM Data 
                  INNER JOIN Treatment using(Date,Well) 
                  WHERE Mark = "Release" AND  
                  celldensity = 5000 AND 
                  Param = "',Param,'" AND
                  Freq = "',Freq,'"',sep="")
imp <- dbGetQuery(db, sqlcmd)
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
  filter(dosestreatment==0|dosestreatment>1000) %>% # throw out small doses with no effect
  filter(!Date=="20131213") %>% # 1. experiment and we had different treatment doses
  mutate(dosestreatment=ceiling(dosestreatment)) # round up to integers

# # diagnostic plots ----
# imp %>%
#   ggplot(aes(x=Date,y=value)) +
#   geom_boxplot() +
#   facet_grid(Freq~Param)
# 
# imp %>% 
#   group_by(Well,Date) %>% # group and normalise to first 3 values each well in each experiment
#   mutate(value = value/(value%>%head(n=3)%>%mean)) %>%
#   group_by(Date,dosestreatment,GF) %>% # min-max normalise 
#   mutate(value = (value-min(value))/(max(value)-min(value))) %>%
#   ggplot(aes(x=Date,y=value)) +
#   geom_boxplot() +
#   facet_grid(~GF)

# calculate hourly means ----
# split dataframe by date, growthfactor and parameter and then 
# simplify multilevel list structure

imp.pairs <- imp %>% 
  group_by(Well,Date) %>% # group and normalise to first 3 values each well in each experiment
  mutate(value = value/(value%>%head(n=3)%>%mean)) %>%
#   group_by(Date,dosestreatment,GF) %>% # min-max normalise 
#   mutate(value = (value-min(value))/(max(value)-min(value))) %>%
  mutate(timeBin=Time%>%cut(., floor(.)%>%unique, labels = FALSE, include.lowest = TRUE)) %>% # calculate hourly means and 
  filter(!is.na(timeBin)) %>% 
  group_by(Freq,Param,Date,concGF,dosestreatment,GF,treatment,timeBin) %>% # summarise each experiment
  summarise(value=mean(value)) %>%
  inset(,"treat2",paste0(.$concGF," ng/ml ",.$GF,"\n+",.$treatment)) %>% # create summary variable treat2
  mutate(treat2=gsub("SB101","3MUT",treat2)) %>%
  dlply(.(GF)) %>% lapply(Pairs) %>% # split data into treated untreated pairs
  lapply(function(x) if (class(x) == "data.frame") list(x) else x)  %>% 
  unlist(recursive=FALSE) %>% # filter out controls from other experiments
  lapply({.%>% {datestouse <- filter(.,dosestreatment>0)%>%
                  use_series(Date)%>%
                  unique%>%
                  droplevels
                filter(.,Date%in%datestouse)}}) %>% # summarise data
  lapply({.%>%group_by(Freq,Param,concGF,dosestreatment,GF,timeBin,treat2) %>% # summarise experiments
            summarise(Mean=mean(value),
                      SD=sd(value),
                      N=length(value),
                      SE=SD/sqrt(N))}) 

Myplot <- function(mydf) mydf %>% {
  Fq <- use_series(.,"Freq") %>% as.character %>% as.numeric
  Par <- use_series(.,"Param") %>% unique %>% as.character
  parameter <- switch(Par,Z="impedance",
                      R="resistance",
                      C="capacitance")
  p <- (.) %>% ungroup %>%
    mutate(dosestreatment=round(dosestreatment/1000,digits = 2)) %>%
    ggplot(.,aes(x=timeBin,y=Mean,color=treat2)) + 
    geom_line(size=1) + # aes(linetype=treat2),
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),alpha=0.25) +
    facet_grid(~dosestreatment) +
    scale_color_manual(values=c("#E69F00","#D55E00","#000000")) +
    ylab(bquote(list(Normalised~.(parameter), .(Par)/.(Par)[0]~(.(Fq)~Hz)))) +
    xlab("Time after release, h")
  p
}

plotlist <- imp.pairs %>% 
  lapply(Myplot)
Fig3B <- plotlist[[6]] + 
  ggtitle(bquote(list(Treatment~conc.,paste(mu,mol)/L))) +
  theme(plot.title = element_text(size=10)) +
  ylab(NULL)

# lets select only VEGF 25 data for plotting
Fig3B <- imp.pairs[[5]] %>% { 
  orig <- (.)
  orig$treat2 <- factor(orig$treat2, c("0 ng/ml VEGF\n+hIgG-Fc","25 ng/ml VEGF\n+hIgG-Fc","25 ng/ml VEGF\n+3MUT-Fc"))
  orig
} %>%
  Myplot %>% {(.) + 
                ggtitle(bquote(list(Treatment~concentration,paste(mu,mol)/L))) +
                theme(plot.title = element_text(size=10)) +
                ylab(NULL) + 
                labs(color=NULL) +
                theme(strip.background = element_rect(colour="white", fill="grey80")) + 
                theme(legend.position = c(0.93, 0.2),
                      legend.background = element_rect(fill=NA))}
  
library(gridExtra)
# ggplot_build %>%
tyhi <- rectGrob(gp = gpar(col = NA))
labels <- lapply(LETTERS[1:4], function(label) textGrob(label, x = unit(0,"npc"), 
                                                        hjust = 0, gp = gpar(fontsize = 8, fontface = 2)))

theme_set(theme_classic(base_size = 10))
Fig3 <- arrangeGrob(arrangeGrob(labels[[1]], Fig3A+ggtitle(""), heights = c(1,12)), 
                                arrangeGrob(labels[[2]], Fig3B, heights = c(1,12)),
                                ncol = 2, widths = c(2,8))
Fig3

# imp.pairs.smooth <- imp %>% 
#   mutate(timeBin=Time%>%cut(., floor(.)%>%unique, labels = FALSE, include.lowest = TRUE)) %>%
#   filter(!is.na(timeBin)) %>% 
#   group_by(Date,Well) %>% 
#   mutate(value = value/value[1]) %>%
#   inset(,"treat2",paste0(.$concGF," ng/ml ",.$GF,"\n+",.$treatment)) %>% 
#   mutate(treat2=gsub("SB101","3MUT",treat2)) %>%
#   group_by(Date,timeBin,Freq,Param,GF,concGF,dosestreatment,treat2) %>% 
#   summarise(Mean=mean(value),
#             SD=sd(value),
#             N=length(value),
#             SE=SD/sqrt(N)) %>%
#   dlply(.(GF)) %>% lapply(Pairs) %>% 
#   lapply(function(x) if (class(x) == "data.frame") list(x) else x)  %>% 
#   unlist(recursive=FALSE) 
# 
# Myplot.smooth <- function(mydf) mydf %>% {
#   Fq <- use_series(.,"Freq") %>% as.character %>% as.numeric
#   Par <- use_series(.,"Param") %>% unique %>% as.character
#   parameter <- switch(Par,Z="impedance",
#                       R="resistance",
#                       C="capacitance")
#   p <- ggplot(.,aes(x=timeBin,y=Mean,color=treat2)) + 
<<<<<<< HEAD
#     stat_summary(fun.data=mean_sdl,mult=1,geom = "smooth") +
=======
#     stat_summary(fun.data=mean_cl_boot, B=1000, geom = "errorbar", width = 0.2,alpha=0.25) +
>>>>>>> 298ba16b2f7e97d07b6bdfc574aad88767a74bf7
#     stat_summary(fun.y=mean, geom = "line", size = 1) +
#     facet_grid(~dosestreatment) +
#     scale_color_manual(values=c("#E69F00","#000000","#0072B2")) +
#     ylab(bquote(list(Normalised~.(parameter), .(Par)/.(Par)[0]~(.(Fq)~Hz)))) +
#     xlab("Time after release (h)")
#   p
# }
# 
# plotlist.smooth <- imp.pairs.smooth %>% 
#   lapply(Myplot.smooth) 
#   
# plotlist.smooth[[5]] + theme(panel.margin.x = unit(0.05, "null"))
