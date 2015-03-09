library(RSQLite)
db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection
dbListFields(db, "Model")
dbListFields(db, "Data")
# load all impedance data for 'Model' 
imp <- dbGetQuery(db, 'SELECT *
                       FROM Data
                       INNER JOIN Model
                       using(Date,Well)
                       WHERE Mark = "Release" AND serum = "FBS"')

# load all modeling data for 'Model'
rba <- dbGetQuery(db, 'SELECT *
                       FROM Rbmodel
                       INNER JOIN Model
                       using(Date,Well)
                       WHERE serum = "FBS"')
dbDisconnect(db)            # Close connection

library(magrittr)
# library(data.table)

# remove columns containing only single value/no variation
imp %<>% Filter({. %>% unique %>% length %>% is_greater_than(.,1)}, .)
rba %<>% Filter({. %>% unique %>% length %>% is_greater_than(.,1)}, .)

head(imp)
imp$celldensity %>% unique
head(rba)
unique(imp$Date)

library(plyr)
# reset Times so that they all start from zero
imp %<>% ddply(., .(Date), mutate, Time = Time-Time[1])
rba %<>% ddply(., .(Date), mutate, Time = Time-Time[1])
rba$concGF %<>% as.character %<>% as.numeric # make numeric for proper faceting
imp$concGF %<>% as.character %<>% as.numeric # make numeric for proper faceting

library(ggplot2)
library(ggthemes)
library(Hmisc)

ylabeller <- function(x){
  if(x=="C"){out <- paste0("C, Capacitance (F)")}
  if(x=="Z"){out <- paste0("Z, Impedance (Ohm)")}
  if(x=="R"){out <- paste0("R, Resistance (Ohm)")}
  if(x=="Alpha"){out <- paste0("Alpha parameter")}
  if(x=="CellMCap"){out <- paste0("Cell Membrane Capacitance")}
  if(x=="Rb"){out <- paste0("Rb parameter")}
  out
}

Myplot <- function(x) {
  GrowthFactor <- x$GF %>% unique
  ExperimentDate <- x$Date %>% unique %>% strptime(.,"%Y%m%d")
  p <- ggplot(x, aes(Time, value, color = celldensity)) + 
    facet_grid(Freq~concGF, scales="free") +
    stat_summary(fun.data = mean_se, geom = "pointrange") +
    scale_color_colorblind() +
    ylab(ylabeller(unique(x$Param))) + 
    xlab("Time (h)") +
    ggtitle(paste(GrowthFactor, ExperimentDate))
  pam <- regmatches(p$labels$y, regexpr("^.", p$labels$y))
  p %>% ggsave(paste0("graphs/",ExperimentDate,"_",
                      GrowthFactor,"_",pam,"_",Sys.Date(),".pdf"),.)
}

imp %>% dlply(.,.(Date, GF, Param), Myplot)
#   ddply(., .(Date, Well, Param, Freq), mutate, value = value/value[1]) %>%
  

# Modeling results plotting functions
PlotModel <- function(x){
  GrowthFactor <- x$GF %>% unique
  ExperimentDate <- x$Date %>% unique %>% strptime(.,"%Y%m%d")
  ggplot(x, aes(Time, value, color = celldensity)) + 
    facet_grid(Param~concGF, scales="free") +
    stat_summary(fun.data = mean_se, geom = "pointrange") +
    scale_color_colorblind() +
    ylab("Modeled Parameter") +
    xlab("Time (h)") +
    ggtitle(paste(GrowthFactor, ExperimentDate))
  ggsave(paste0("graphs/",ExperimentDate,"_",GrowthFactor,"_Modeling_", Sys.Date(),".pdf"), plot = x)
}

# Plot setup experiments modeling results
rba %>% dlply(.,.(Date, GF), PlotModel)