# Check if we already have Bigdata in environment
if(!"Bigdata" %in% unlist(list(ls()))){
  source("src/importxls.R")
}

head(Bigdata)
summary(Bigdata)

sirna <- Bigdata %>% filter(Ind==1) %>%
  group_by(Date) %>%
  mutate(Time=Time-min(Time)) %>% 
  mutate(timeBin=cut(Time, 
                     breaks = unique(floor(Time)),
                     labels = FALSE,
                     include.lowest=TRUE)) %>%
  (function(x) x[complete.cases(x),]) %>% {
    finito <- ddply(.,"timeBin", summarise, N = unique(Date) %>% length) %>%
      filter(N>1) %>% max
    .[.$timeBin<=finito,]
  }  
  
sirna$doses.GF <- as.numeric(as.character(sirna$doses.GF))

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
    facet_grid(Hz~doses.GF, scales="free") +
    stat_summary(fun.data=mean_se, geom="pointrange") +
    scale_color_colorblind(breaks=c("UT","SCR","siCD44","siVIM")) +
    ylab(ylabeller(unique(x$Param))) + xlab("Time (h)")
}

# VEGF data ----
plist <- sirna %>% filter(GF%in%c("VEGF","FBS_5")) %>% 
  dlply(.,"Param", Myplot)
lapply(plist, function(x) 
  ggsave(paste0("graphs/siRNA_VEGF_",
                   regmatches(x$labels$y, regexpr("^.",x$labels$y)),
                   "_", Sys.Date(),
                   ".pdf"), plot = x))
# norm data ---
sirna %>% filter(GF%in%c("VEGF","FBS_5")) %>% 
  ddply(., .(Date,Well,Param,Hz), mutate, value=value-value[1]) %>%
  dlply(.,"Param", Myplot) %>% 
{x; lapply(x, function(y) {
  pam <- regmatches(y$labels$y, regexpr("^.",y$labels$y))
  ggsave(paste0("graphs/siRNA_VEGF_",pam,"_norm_", Sys.Date(),".pdf"), plot = y)
})
}

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
  ddply(., .(Date,Well,Param,Hz), mutate, value=value-value[1]) %>%
  dlply(.,"Param", Myplot) %>% 
{x; lapply(x, function(y) {
  pam <- regmatches(y$labels$y, regexpr("^.",y$labels$y))
  ggsave(paste0("graphs/siRNA_bFGF_",pam,"_norm_", Sys.Date(),".pdf"), plot = y)
})
}

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
  ddply(., .(Date,Well,Param,Hz), mutate, value=value-value[1]) %>%
  dlply(.,"Param", Myplot) %>% 
  lapply(., function(p) p + facet_grid(Hz~GF, scales="free")) %>%
{x; lapply(x, function(y) {
  pam <- regmatches(y$labels$y, regexpr("^.",y$labels$y))
  ggsave(paste0("graphs/siRNA_FBS_",pam,"_norm_", Sys.Date(),".pdf"), plot = y)
})
}