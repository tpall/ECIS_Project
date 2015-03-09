library(ggplot2)
library(ggthemes)
library(magrittr)

sirna %>% filter(GF%in%c("VEGF","FBS_5")) %>% 
  ddply(., .(Date,Well,Param,Hz), mutate, value=value-value[1]) %>%
  dlply(.,"Param", Myplot) %>% 
{x; lapply(x, function(y) {
  pam <- regmatches(y$labels$y, regexpr("^.",y$labels$y))
  ggsave(paste0("graphs/siRNA_VEGF_",pam,"_norm_", Sys.Date(),".pdf"), plot = y)
  })
}


plotfunx <- . %>% 
  ddply(., .(Date,Well,Param,Hz), mutate, value=value/value[1]) %>%
  dlply(.,"Param", function(x) {
    ggplot(x, aes(x=timeBin,y=value,color=factor(Hz))) + 
      facet_grid(treatment~doses.GF) +
      stat_summary(fun.data=mean_se, geom=c("pointrange","line")) +
      scale_color_brewer("Accent") + 
      ylab(paste("Norm.",unique(x$Param)))
  })

Myggsave <- . %>% {
  pam <- gsub(". ", "_",.$labels$y)
  main <- .$labels$title
  ggsave(paste0("graphs/siRNA_", main, "_", pam,"_Spect_", 
                Sys.Date(),".pdf"), plot = .)
}

sirna %>% filter(GF%in%c("VEGF","FBS_5")) %>% plotfunx %>% 
  lapply(.,function(x) {(x+ggtitle("VEGF")) %>% Myggsave})

sirna %>% filter(GF%in%c("bFGF","FBS_5")) %>% plotfunx %>% 
  lapply(.,function(x) {(x+ggtitle("bFGF")) %>% Myggsave})

sirna %>% filter(GF%in%c("GDF-2","FBS_5")) %>% plotfunx %>% 
  lapply(.,function(x) {(x+ggtitle("GDF-2")) %>% Myggsave})

sirna %>% filter(GF%in%c("FBS_20","FBS_5")) %>% plotfunx %>% 
  lapply(., function(x) (x+ggtitle("FBS") + 
                           facet_grid(treatment~GF)) %>% Myggsave)
