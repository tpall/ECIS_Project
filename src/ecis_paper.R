library(RSQLite)
library(magrittr)
library(plyr);library(dplyr)
library(ggplot2)
library(ggthemes)

Param <- "Z"
Freq <- 16000

# Fig 3B ----

db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection
sqlcmd <- paste('SELECT * FROM Data 
                  INNER JOIN Treatment using(Date,Well) 
                  WHERE Mark = "Release" AND  
                  celldensity = 5000 AND 
                  Param = "',Param,'" AND
                  Freq = "',Freq,'"',sep="")
imp <- dbGetQuery(db, sqlcmd)
dbDisconnect(db)

# Lets save data to 
save(imp,file = "~/Dropbox//CD44_angiogenesis_paper/data/ecis_imp.R")

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
  ungroup %>%
  mutate(treatment=ifelse(dosestreatment>0,as.character(treatment),"untreated")) %>%
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
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),color="black",alpha=0.25) +
    facet_grid(~dosestreatment) +
    scale_color_manual(values=c("#E69F00","#009E73","#D55E00","#000000")) +
    guides(color=guide_legend(ncol=2)) +
    ylab(bquote(list(Normalised~.(parameter), .(Par)/.(Par)[0]~(.(Fq)~Hz)))) +
    xlab("Time after release, h")
  p
}

# plotlist <- imp.pairs %>% 
#   lapply(Myplot)
# Fig3B <- plotlist[[6]] + 
#   ggtitle(bquote(list(Treatment~conc.,paste(mu,mol)/L))) +
#   theme(plot.title = element_text(size=10)) +
#   ylab(NULL)
lapply(imp.pairs,summary)

# lets select only VEGF 25 data for plotting
Fig3B <- imp.pairs[[5]] %>% { 
  orig <- (.) # next line is to reorder legend keys
  orig$treat2 <- factor(orig$treat2, c("0 ng/ml VEGF\n+untreated","25 ng/ml VEGF\n+untreated",
                                       "25 ng/ml VEGF\n+hIgG-Fc","25 ng/ml VEGF\n+3MUT-Fc"))
  orig
} %>%
  Myplot %>% {(.) + 
                ggtitle(bquote(list(Recombinant~protein~concentration,paste(mu,mol)/L))) +
                theme(plot.title = element_text(size=10)) +
                ylab(NULL) + 
                labs(color=NULL) +
                theme(strip.background = element_rect(colour="white", fill="grey80")) + 
                theme(legend.position = c(0.85, 0.13),
                      legend.background = element_rect(fill=NA))}

# Fig3A experimental setup/pretreatments ----
# First, select Dates for Fig3A data
Dates <- imp %>% 
  dlply(.(GF)) %>% 
  lapply(Pairs) %>% 
  lapply(function(x) if (class(x) == "data.frame") list(x) else x)  %>% 
  unlist(recursive=FALSE) %>% 
  lapply({.%>% filter(dosestreatment>0) %>% use_series(Date)  %>% unique})

db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection
sqlcmd <- paste('SELECT * FROM Data
                INNER JOIN Treatment using(Date,Well)
                WHERE Mark IN ("Seed","Starv","Preinc") AND 
                celldensity = 5000 AND
                Param = "',Param,'" AND
                Freq = "',Freq,'"',sep="")
pre <- dbGetQuery(db, sqlcmd)
dbDisconnect(db)

# Lets save data to 
save(pre,file = "~/Dropbox//CD44_angiogenesis_paper/data/ecis_pre.R")

# calculate hourly means
ecispre <- pre %>% # calculate timeBins
  mutate(timeBin=Time%>%cut(., floor(.)%>%unique, labels = FALSE, include.lowest = TRUE)) %>%
  filter(!is.na(timeBin)) %>%
  filter(Date %in% Dates[[5]]) %>% # use only VEGF 25 data presented in Fig 3B
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

# Combine figures ----
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
# #################################################################################
# # Lets have a look at the differences ----
# GF <- "VEGF"
# db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection
# sqlcmd <- paste('SELECT * FROM Data 
#                 INNER JOIN Treatment using(Date,Well) 
#                 WHERE Mark = "Release" AND  
#                 celldensity = 5000 AND
#                 GF = "',GF,'" AND
#                 concGF IN (0,25)', sep="")
# parms <- dbGetQuery(db, sqlcmd)
# dbDisconnect(db)
# 
# # remove columns containing only single value/no variation
# parms %<>% 
#   Filter({. %>% unique %>% length %>% is_greater_than(.,1)}, .)
# 
# # reset Times so that they all start from zero and filter data under 72 hours
# parms %<>% 
#   group_by(Date) %>% 
#   mutate(Time = Time-first(Time)) %>%  
#   filter(Time<=73)
# 
# parms %<>% ungroup %>%
#   mutate(concGF=concGF%>%make.numeric,
#          dosestreatment=dosestreatment%>%make.numeric,
#          Date=Date%>%as.factor,
#          Well=Well%>%as.factor,
#          Freq=Freq%>%as.factor,
#          Param=Param%>%as.factor,
#          GF=GF%>%as.factor,
#          treatment=treatment%>%as.factor)
# 
# # dosestreatment should be binned for each growth factor: but only for summary
# parms %<>% 
#   filter(dosestreatment==0|dosestreatment>1000) %>% # throw out small doses with no effect
#   filter(!Date=="20131213") %>% # 1. experiment and we had different treatment doses
#   mutate(dosestreatment=ceiling(dosestreatment)) # round up to integers
# 
# laglist <- parms %>%
#   mutate(timeBin=Time%>%cut(., floor(.)%>%unique, labels = FALSE, include.lowest = TRUE)) %>%
#   filter(!is.na(timeBin)) %>% 
#   group_by(Freq,Well,Param,Date) %>% 
#   mutate(value = value/last(value),
#          change = value-lag(value,k=-1)) %>%
#   filter(!is.na(change)) %>%
#   group_by(Freq,Param,Date,concGF,dosestreatment,treatment,timeBin) %>% 
#   summarise(change=mean(change)) %>% 
#   group_by(Freq,Param,concGF,dosestreatment,treatment,timeBin) %>% 
#   summarise(Mean=mean(change),
#             SD=sd(change)) %>% 
#   ungroup %>%
#   mutate(treatment=ifelse(dosestreatment>0,as.character(treatment),"untreated")) %>%
#   inset(,"treat2",paste0(.$concGF," ng/ml VEGF\n+",.$treatment)) %>% # create summary variable treat2
#   mutate(treat2=gsub("SB101","3MUT",treat2)) %>%
#   dlply(.(Param))
# 
# laglist %>% names
# 
# laglist[["R"]] %>% { 
#   orig <- (.) # next line is to reorder legend keys
#   orig$treat2 <- factor(orig$treat2, c("0 ng/ml VEGF\n+untreated","25 ng/ml VEGF\n+untreated",
#                                        "25 ng/ml VEGF\n+hIgG-Fc","25 ng/ml VEGF\n+3MUT-Fc"))
#   orig
# } %>%
#   ggplot(aes(x=timeBin,y=Mean,color=treat2)) +
#   geom_line(size=1) +
#   geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),size=0.2) +
#   facet_grid(Freq~dosestreatment, scale="free_y") +
#   scale_color_colorblind()
# 
# 
# 
# laglist[["Z"]] %>% { 
#   orig <- (.) # next line is to reorder legend keys
#   orig$treat2 <- factor(orig$treat2, c("0 ng/ml VEGF\n+untreated","25 ng/ml VEGF\n+untreated",
#                                        "25 ng/ml VEGF\n+hIgG-Fc","25 ng/ml VEGF\n+3MUT-Fc"))
#   orig
# } %>%
#   filter(timeBin<48) %>%
#   ggplot(aes(x=treat2,y=Mean,fill=treat2)) +
#   geom_boxplot() +
#   facet_grid(Freq~dosestreatment, scale="free_y") +
#   scale_fill_colorblind()