
## @knitr load_sirna_ecis

library(plyr);library(dplyr)
library(RSQLite)
library(tidyr) # spread
library(magrittr)
library(gridExtra);library(grid)
library(ggplot2)
library(ggthemes)

# Raw impedance data ----

# Connect to database
db <- src_sqlite("data/ECIS2.sqlite", create = FALSE)

# Look for table names 
# src_tbls(db) # list tables in database

# Extract siRNA experiments
dates <- tbl(db, "Metadata") %>% filter(value=="siCD44") %>% select(Date) %>% distinct() %>% collect() %>% as.data.frame() %>% .[,"Date"]

# Get release marks
sirna.marks <- tbl(db, "Marks") %>% filter(Date %in% dates) %>%
  collect() %>% rename(Mark = Time) %>% 
  filter(grepl("GF", Label)) %>% select(Mark,Date)

# Split metadata column
wide.metadata <- tbl(db, "Metadata") %>% filter(Date %in% dates) %>% 
  collect() %>% spread(Metadata, value)

# Download data and merge with metadata
ecis.sirna <- tbl(db, "Data") %>% filter(Date %in% dates) %>% 
  collect %>% left_join(., wide.metadata) %>% 
  left_join(., sirna.marks) %>% rename(Time = `Time (hrs)`) %>% 
  group_by(Date) %>% mutate(Time = Time-Mark) %>% 
  mutate(timeBin = cut(Time, 
                       breaks = unique(floor(Time)),
                       labels = unique(floor(Time))[-1],
                       include.lowest=TRUE)) %>%
  ungroup() %>% filter(complete.cases(.)) %>% 
  mutate(conc_GF = as.numeric(as.character(conc_GF)),
         Freq = as.numeric(as.character(Freq)),
         timeBin = as.numeric(as.character(timeBin)))

ecis.sirna$treatment[ecis.sirna$treatment=="SCR"] <- "siNTP"
ecis.sirna$treatment <- factor(ecis.sirna$treatment, levels = c("siNTP", "siCD44", "siVIM", "UT"))
ecis.sirna$Freq <- factor(ecis.sirna$Freq, levels = unique(ecis.sirna$Freq), labels = paste(unique(ecis.sirna$Freq), "Hz"))

## @knitr siECIS 

Parameter <- "Z" 
Freq.subset <- "64000 Hz"
if(!any(ls()=="myalpha"))(myalpha <- 0.3)

# Filter 2 independent experiments per condition
ecis.sirna.filt <- ecis.sirna %>%
  filter(timeBin >= -2, Param %in% Parameter, Freq %in% Freq.subset) %>% {
    finito <- ddply(.,"timeBin", summarise, N = unique(Date) %>% length) %>%
      filter(N>1) %>% max
    .[.$timeBin<=finito,]} %>% 
  mutate(GF = factor(GF, levels = c("FBS_5", "FBS_20", "VEGF", "bFGF", "GDF-2" ), 
                     labels = c("5% FBS","20% FBS", "VEGF", "FGF2", "GDF-2")),
         conc_GF = factor(conc_GF, levels = unique(ecis.sirna$conc_GF), 
                          labels = paste(unique(ecis.sirna$conc_GF), "ng/ml")))

# black_monk_theme <- function(){
#   # theme_classic() +
#   theme(plot.title = element_text(size = 8),
#         strip.background = element_rect(colour = "white", fill = "grey80"),
#         legend.position = "none")
# }

select_N2_summarise <- function(x) {select(x, Date, GF, conc_GF) %>% 
  group_by(GF, conc_GF) %>% summarise(N = length(unique(Date))) %>%
  filter(N>1) %>% select(GF, conc_GF) %>% # filter series with N > 1
  inner_join(x, .) %>% # use only data with N > 1
  group_by(Param, Freq, Date, conc_GF, GF, treatment, timeBin) %>%
  summarise(value = mean(value, na.rm = T)) %>% ungroup}

# siRNA effect on VEGF and FGF2 stimulation ----
gf.si.ecis <- ecis.sirna.filt %>%
  filter(GF %in% c("VEGF","FGF2"), treatment != "UT") %>% 
  select_N2_summarise %>% 
  ggplot(aes(timeBin, value, color = treatment)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", alpha = myalpha) +
  stat_summary(fun.y = mean, geom = "line") +
  xlab(bquote(list(Time~relative~to~release,h))) +
  ylab(bquote(list(Impedance~"@"~.(Freq.subset)~Hz,Omega))) + 
  facet_wrap( ~ GF + conc_GF) + 
  theme(legend.position="none") +
  scale_color_colorblind()

# siRNA effect on FBS stimulation 
fbs.si <- ecis.sirna.filt %>% filter(GF %in% c("5% FBS","20% FBS"), treatment != "UT") %>% select_N2_summarise
fbs.si.ecis <- gf.si.ecis %+% fbs.si %+% facet_grid(~ GF)

# siRNA effect on GDF-2 stimulation, 1 experiment only 
gdf.si <- ecis.sirna.filt %>% filter(GF %in% c("GDF-2"), treatment != "UT") %>%
  group_by(Param, Freq, Date, conc_GF, GF, treatment, timeBin) %>%
  summarise(value = mean(value, na.rm = T))
gdf.si.ecis <- gf.si.ecis %+% gdf.si

