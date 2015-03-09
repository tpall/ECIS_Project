library(RSQLite)

db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection

dbListTables(db)
dbListFields(db, "Treatment")
dbListFields(db, "Data")

# load all impedance data for 'Model' 
imp <- dbGetQuery(db, 'SELECT *
                  FROM Data
                  INNER JOIN Treatment
                  using(Date,Well)
                  WHERE Mark = "Release" AND celldensity = 5000')

# load all modeling data for 'Model'
rba <- dbGetQuery(db, 'SELECT *
                  FROM Rbmodel
                  INNER JOIN Treatment
                  using(Date,Well)
                  WHERE celldensity = 5000')
dbDisconnect(db)            # Close connection
rm(db)
library(magrittr)
# library(data.table)

library(plyr);library(dplyr)
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
imp %<>% Filter({. %>% unique %>% length %>% is_greater_than(.,1)}, .)
# reset Times so that they all start from zero and filter out time over 72 hours
imp %<>% group_by(Date) %>% mutate(Time = Time-Time[1]) %>% filter(Time<=72)
# convert cols to numeric and factors
imp %<>% ungroup %>%
  mutate(concGF=concGF%>%make.numeric,
         dosestreatment=dosestreatment%>%make.numeric,
         Date=Date%>%as.factor,
         Well=Well%>%as.factor,
         Param=Param%>%as.factor,
         GF=GF%>%as.factor,
         treatment=treatment%>%as.factor)
imp %>% summary

# dosestreatment should be binned for each growth factor: but only for summary
imp %>% dlply(.(GF,concGF), .%>% use_series(dosestreatment) %>% unique %>% sort) 
imp %<>% 
  filter(dosestreatment==0|dosestreatment>100) %>%
  mutate(doseBin=cut(dosestreatment,breaks=c(0,1,128,402,1267,4001,8001,12641,16001),include.lowest=TRUE))    

## split dataframe by date, growthfactor and parameter and then 
# simplify multilevel list structure
imp.pairs <- imp %>%
  dlply(.,.(Date,GF,Param)) %>% lapply(Pairs) %>% 
  lapply(function(x) if (class(x) == "data.frame") list(x) else x)  %>% 
  unlist(recursive=FALSE)

# Modeling data ----
rba %<>% Filter({. %>% unique %>% length %>% is_greater_than(.,1)}, .)
# reset Times so that they all start from zero and filter out time over 72 hours
rba %<>% ddply(., .(Date), mutate, Time = Time-Time[1]) # %>% filter(Time<=72)
rba %<>% group_by(Date) %>% mutate(Time = Time-Time[1])
# convert cols to numeric and factors
rba %<>% ungroup %>%
  mutate(concGF=concGF%>%make.numeric,
                dosestreatment=dosestreatment%>%make.numeric,
                Date=Date%>%as.factor,
                Well=Well%>%as.factor,
                Param=Param%>%as.factor,
                GF=GF%>%as.factor,
                treatment=treatment%>%as.factor)
rba %>% summary

## For Modeling results split dataframe by date and growthfactor then simplify multilevel list structure
rba.pairs <- rba %>%
  dlply(.,.(Date,GF)) %>% lapply(Pairs) %>% 
  lapply(function(x) if (class(x) == "data.frame") list(x) else x)  %>% 
  unlist(recursive=FALSE)


# Plotting compartement ----
library(ggplot2)
library(ggthemes)
library(Hmisc)

# p[1:2] %>% lapply({.%>%Myplot2(Normalisation="Divide")})

rba.pairs %>% lapply({.%>%Myplot2})
imp.pairs %>% lapply({.%>%Myplot2(Normalisation="Divide")})

# had to repair filename generating code (GrowthFactor) for FBS treated experiments
fbs <- imp.pairs %>% {i <- names(.) %>% grep("FBS",.)
                      .[i]}
fbs %>% lapply({.%>%Myplot2(Normalisation="Divide")})

# function that does it all, just feed in your data and close your eyes
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
