## @knitr impedance-fig

# load libraries
library(RSQLite)
library(tidyr) # spread
library(magrittr)
library(plyr);library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)

# rm(list=ls())

# Specify parameters presented
Parameter <- "Z"
Freq.subset <- "16000"
if(!any(ls()=="myalpha"))(myalpha <- 0.3)

# Connect to database
db <- src_sqlite("data/ECIS2.sqlite", create = FALSE)
# src_tbls(db) # list tables in database

# Extract 3MUT-Fc/SB101-Fc experiments
dates <- tbl(db, "Metadata") %>% filter(value=="SB101-Fc") %>% select(Date) %>% 
  distinct() %>% collect() %>% as.data.frame() %>% .[,"Date"]

# Get release marks & filter out 1. experiment @"20131213" dates[-1] bc low treatment doses
marks <- tbl(db, "Marks") %>% filter(Date %in% dates[-1]) %>%
  collect() %>% rename(Mark = Time) %>% group_by(Date) %>%
  summarise(Mark = max(Mark))

# Mean preincubation time 
ecis.preinc <- tbl(db, "Marks") %>% filter(Date %in% dates[-1]) %>%
  collect() %>% rename(Mark = Time) %>% group_by(Date) %>%
  summarise(Preinc = Mark[3]-Mark[2],
            Starv = Mark[3]-Mark[1]) %>% 
  ungroup %>% summarise(Preinc.mean = mean(Preinc),
                        Preinc.se = sd(Preinc)/sqrt(length(Preinc)),
                        Starv.mean = mean(Starv),
                        Starv.se = sd(Starv)/sqrt(length(Starv))) %>% melt

# Split metadata column
wide.metadata <- tbl(db, "Metadata") %>% filter(Date %in% dates[-1]) %>% 
  collect() %>% spread(Metadata, value) %>% 
  mutate(doses_treatment = ceiling(as.numeric(doses_treatment))) %>%
  filter(doses_treatment == 0|doses_treatment > 1000)

# Download data and merge with metadata
imp <- tbl(db, "Data") %>% 
  filter(Param == Parameter, Freq == Freq.subset, Date %in% dates[-1]) %>%
  collect %>% 
  left_join(., wide.metadata) %>% 
  left_join(., marks) %>% rename(Time = `Time (hrs)`) %>% 
  mutate(Time = Time-Mark) %>% 
  group_by(Date) %>%
  mutate(timeBin = cut(Time, 
                       breaks = unique(floor(Time)),
                       labels = unique(floor(Time))[-1],
                       include.lowest=TRUE)) %>%
  ungroup() %>% filter(complete.cases(.)) %>% 
  mutate(conc_GF = as.numeric(as.character(conc_GF)),
         Freq = as.numeric(as.character(Freq)),
         timeBin = as.numeric(as.character(timeBin)))

# split dataframe by date, growthfactor and parameter and then 
# simplify multilevel list structure
# function to split experiments by different uninduced-induced pairs. Pairs is function importante!!!!
Pairs <- . %>% {
  fargs <- use_series(., conc_GF) %>%
    unique %>%
    (function(x) if(length(x) == 1) data.frame(ind = x) else 
      expand.grid(ctrl = x[1], ind = x[-1])) %>% 
    (function(x) split(x, f = x$ind))
  lapply(fargs, function(x) filter(., conc_GF %in% x))
}

imp.pairs <- imp %>% 
  group_by(Date, Well) %>% mutate(value = value/mean(value[timeBin==0], na.rm = T)) %>%
  group_by(Date, conc_GF, doses_treatment, GF, treatment, timeBin) %>% # summarise each experiment
  summarise(value = mean(value, na.rm = T)) %>%
  ungroup %>%
  mutate(treatment = ifelse(doses_treatment > 0, treatment, "untreated"),
         doses_treatment = round(doses_treatment/1000, digits = 2),
         treat2 = paste0(conc_GF," ng/ml ", GF, "\n+", treatment), # create summary variable treat2
         treat2 = gsub("SB101", "3MUT", treat2),
         treat2 = gsub("hIgG", "rhIgG", treat2)) %>% 
  dlply(.(GF)) %>% lapply(Pairs) %>% # split data into treated untreated pairs
  lapply(function(x) if (class(x) == "data.frame") list(x) else x)  %>% 
  unlist(recursive = FALSE) %>% 
  lapply({.%>% {datestouse <- filter(., doses_treatment > 0) %>%
    use_series(Date) %>% unique
  filter(., Date %in% datestouse)}})

imp.pairs[[5]]$Date%>%unique

Myplot <- function(x, myaes, Parameter) {
  parameter <- switch(Parameter, Z = "impedance",
                      R = "resistance",
                      C = "capacitance")
  ggplot(x, myaes) + 
    facet_grid(~ doses_treatment) +
    stat_summary(fun.data = mean_se, geom = "errorbar", alpha = myalpha) +
    stat_summary(fun.y = mean, geom = "line") +
    scale_color_manual(values = colorblind_pal()(8)) + # c("#E69F00","#009E73","#D55E00","#000000")
    guides(color = guide_legend(ncol = 4)) +
    xlab(bquote(list(Time~after~release,h))) +
    ylab(bquote(list(Normalised~.(parameter),.(Parameter)/.(Parameter)[0]~(.(Freq.subset)~Hz))))}

# lets select only VEGF 25 data for plotting
p3b <- imp.pairs[[5]] %>% filter(timeBin >= 0) %>% Myplot(.,aes(timeBin, value, color = treat2), Parameter) 
mytheme <- theme(plot.title = element_text(size = 11),
                 axis.title.y = element_blank(),
                 strip.background = element_rect(colour = "white", fill = "grey80"), 
                 legend.title = element_blank(),
                 legend.justification = c(1,0), legend.position = c(1,0),
                 legend.background = element_rect(fill=NA, colour = NA),
                 legend.key = element_rect(fill=NA, colour = NA))

Fig3B <- p3b + ggtitle(bquote(list(Recombinant~protein~concentration,paste(mu,mol)/L))) + mytheme

# Fig3A experimental setup/pretreatments ----
p3a <- filter(imp.pairs[[5]], timeBin <= 0) %>% 
  group_by(Date, timeBin) %>% # summarise each experiment
  summarise(value = mean(value, na.rm = T)) %>%
  Myplot(aes(timeBin,value), Parameter)

Fig3A <- p3a %+% facet_null() + xlab(bquote(list(Time~before~release,h))) +
  geom_vline(xintercept = -sum(ecis.preinc$value[1:2]), linetype = "longdash") +
  annotate("text", x = c(-35,-mean(c(sum(ecis.preinc$value[1:2]),
                                     sum(ecis.preinc$value[3:4])))),
           y = 1.625, label = c("20%\nFBS", "1%\nFBS")) +
  annotate("text", x = 1.3, y = 1.38, 
           label = "1~h~preincubation%->%release", parse = TRUE, angle = 90) +
  annotate("rect", xmin = -sum(ecis.preinc$value[1:2]), xmax = -sum(ecis.preinc$value[3:4]), 
           ymin = -Inf, ymax = Inf, alpha = myalpha)

