Myplot2 <- function(x, Normalisation = c("Raw","Divide","Subtract")) {
  library(plyr)
  library(dplyr)
  library(magrittr)
  library(ggplot2)
  library(ggthemes)
  library(Hmisc)
  
  param <- x$Param %>% unique
  
  if(Normalisation=="Divide"){
    x %<>% 
      group_by(Well,Freq) %>%
      mutate(value = value/value[1])
    Ylabel <- switch(as.character(param[1]), C = bquote(list(Relative~capacitance, C/C[0]~(Mean %+-% SE))),
                     Z = bquote(list(Relative~impedance, Z/Z[0]~(Mean %+-% SE))),
                     R = bquote(list(Relative~resistance, R/R[0]~(Mean %+-% SE))))
  } else if (Normalisation=="Subtract") {
    x %<>% 
      group_by(Well,Freq) %>%
      mutate(value = value-value[1])
    Ylabel <- switch(as.character(param[1]), C = bquote(list(Capacitance~change, C-C[0]~(Mean %+-% SE))),
                     Z = bquote(list(Impedance~change, Z-Z[0]~(Mean %+-% SE))),
                     R = bquote(list(Resistance~change, R-R[0]~(Mean %+-% SE))))
  } else {
    Ylabel <- switch(as.character(param[1]), C = bquote(list(C, Capacitance~(list(F,Mean %+-% SE)))),
                     Z = bquote(list(Z, Impedance~(list(Ohm,Mean %+-% SE)))),
                     R = bquote(list(R, Resistance~(list(Ohm,Mean %+-% SE)))),
                     Alpha = bquote(Modeled~parameter~(Mean %+-% SE)))
  }
  
  
  # generate labels for plot legend
  x$Treatments <- x %>% 
    extract(c("concGF","GF","treatment")) %>% 
    apply(.,1,paste,collapse="-")
  
  GrowthFactor <- x %>% 
    extract(c("GF","concGF")) %>% 
    unique %>% 
    apply(., 1, paste, collapse="-") %>% {
      n <- length(.)
      ifelse(n==1, extract(.,1), extract(.,2))}
  
  ExperimentDate <- x$Date %>% unique %>% strptime(.,"%Y%m%d")
  
  p <- ggplot(x, aes(Time, value, color = Treatments)) + 
    stat_summary(fun.data = mean_se, geom = "pointrange", alpha = 0.2) +
    scale_color_colorblind() +
    ylab(Ylabel) + 
    xlab("Time (h)") +
    ggtitle(paste(GrowthFactor, ExperimentDate)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))
  
  if("Rb"%in%x$Param){
    p <- p + facet_grid(Param~dosestreatment, scales="free")
    Filename <- paste0("graphs/",ExperimentDate,"_",
                       GrowthFactor,"_Model.pdf")
  } else {
    p <- p + facet_grid(Freq~dosestreatment, scales="free")
    if(Normalisation=="Subtract"){
      Filename <- paste0("graphs/",ExperimentDate,"_",
                         GrowthFactor,"_",param,"-Change.pdf")
    } else if (Normalisation=="Divide") {
      Filename <- paste0("graphs/",ExperimentDate,"_",
                         GrowthFactor,"_",param,"-Norm.pdf")
    } else {
      Filename <- paste0("graphs/",ExperimentDate,"_",
                         GrowthFactor,"_",param,"-Raw.pdf")
    }}
  
  p %>% ggsave(Filename,.)
}