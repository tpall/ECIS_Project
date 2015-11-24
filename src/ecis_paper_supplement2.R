
## @knitr munge_ecis

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
src_tbls(db) # list tables in database

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

# Number of experiments per condition
# ecis.sirna %>% select(Date, GF, conc_GF) %>% 
#   group_by(GF, conc_GF) %>% summarise(N = length(unique(Date)))

# # Migration ----
# allmarks <- tbl(db, "Marks") %>% filter(Date %in% dates) %>%
#   collect() %>% rename(Mark = Time)
# allmarks$Label[2] <- allmarks$Label[4] # make marks consistent
# 
# ecis.sirna %<>% group_by(Date) %>%
#   mutate(Marks = cut(Time, breaks = c(-Inf, allmarks$Mark[allmarks$Date==Date], Inf),
#                      labels = c("20%FBS media",unique(allmarks$Label)),
#                      include.lowest = TRUE))
# 
# # micromot <- ecis.sirna %>% 
# #   group_by(Date,Well,Param,Freq,Marks,GF,conc_GF,treatment) %>%
# #   summarise(Diff = mean(diff(value)),
# #             Diff.SD = sd(diff(value)))
# 
# micromot2 <- ecis.sirna %>% 
#   mutate(Mrk_conc_GF = ifelse(Marks == "20%FBS media", "20% FBS", paste(conc_GF,"ng/ml"))) %>%
#   mutate(Mrk_conc_GF = factor(Mrk_conc_GF, levels = c("20% FBS","0 ng/ml","2.5 ng/ml","8 ng/ml","10 ng/ml","25 ng/ml","79 ng/ml"))) %>%
#   group_by(Date,Well,Param,Freq,Marks,Mrk_conc_GF,GF,conc_GF,treatment) %>%
#   mutate(first.diff = abs(value-lag(value, order_by = Time)),
#          sec.diff = abs(first.diff-lag(first.diff, order_by = Time))) %>%
#   summarise(Diff = mean(sec.diff, na.rm=T))
#   
# micromot2 <- micromot2 %>%
#   group_by(Date,Param,Freq,Mrk_conc_GF,Marks,GF,treatment) %>%
#   summarise(Diff = mean(Diff, na.rm = TRUE))
# 
# # # Anovas ----
# # library(broom)
# # micromot.stats.vegf <- micromot2 %>% 
# #   filter(Param == "Z", GF %in% c("VEGF","FBS_5"), treatment !="UT", Marks != "1%FBS media") %>%
# #   group_by(Freq,Mrk_conc_GF) %>%
# #   do(tidy(TukeyHSD(aov(lm(Diff ~ treatment, data = .)))))
# # 
# # # micromot.stats.vegf %>% data.frame %>% filter(adj.p.value<=0.05) 
# # 
# # micromot.stats.fgf2 <- micromot2 %>% 
# #   filter(Param == "Z", GF %in% c("bFGF","FBS_5"), treatment !="UT", Marks != "1%FBS media") %>%
# #   group_by(Freq,Mrk_conc_GF) %>%
# #   do(tidy(TukeyHSD(aov(lm(Diff ~ treatment, data = .)))))
# # 
# # # micromot.stats.fgf2 %>% filter(adj.p.value<=0.1) %>% data.frame 
# 
# jitdodge <- position_jitterdodge()
# dodge <- position_dodge(width = 1)
# 
# Freq.subset <- c(1000,4000,16000,64000)
# 
# MyMicroPlot <- function(x) {
#   
#   ylabeller <- function(parameter) {
#     if(parameter=="C"){out<-bquote(list(C, Capacitance~group("(",F,")")))}
#     if(parameter=="Z"){out<-bquote(list(Z, Impedance~group("(",Omega,")")))}
#     if(parameter=="R"){out<-bquote(list(R, Resistance~group("(",Omega,")")))}
#     out
#   }
#   
#   ggplot(x,aes(Marks, Diff, color = treatment, fill = treatment)) + 
#     stat_summary(fun.y = mean, geom = "point", shape=4, position = dodge) +
#     stat_summary(fun.data = mean_se, geom = "errorbar", position = dodge) +
#     geom_point(position = jitdodge) +
#     facet_grid(Freq ~ Mrk_conc_GF, scales = "free") +
#     xlab(NULL) + theme(legend.title = element_blank())
# }
# 
# micro.plots.vegf <- micromot2 %>% 
#   filter(GF %in% c("VEGF","FBS_5"), Mrk_conc_GF %in% c("0 ng/ml","8 ng/ml","25 ng/ml","79 ng/ml"), 
#          Marks != "1%FBS media", Freq %in% Freq.subset) %>% 
#   group_by(Param) %>% do(plots = MyMicroPlot(.)) %>% .$plots
# 
# vegf.mpz.Diff <- micro.plots.vegf[[3]] + 
#   ylab("Mean of abs second differences, impedance") + 
#   ggtitle("VEGF") +
#   scale_color_colorblind()
# # vegf.mpz.Diff.mad <- vegf.mpz.Diff %+% aes(Marks, Diff.mad, color = treatment) + ylab("MAD of first differences, impedance")
# 
# micro.plots.fgf2 <- micromot2 %>% 
#   filter(GF %in% c("bFGF","FBS_5"), Mrk_conc_GF %in% c("0 ng/ml","8 ng/ml","25 ng/ml","79 ng/ml"),
#          Marks != "1%FBS media", Freq %in% Freq.subset) %>% 
#   group_by(Param) %>% do(plots = MyMicroPlot(.)) %>% .$plots
# 
# fgf2.mpz.Diff <- micro.plots.fgf2[[3]] + 
#   ggtitle("FGF2") +
#   ylab("Mean of abs second differences, impedance") + scale_color_colorblind()
# # fgf2.mpz.Diff.mad <-fgf2.mpz.Diff %+% aes(Marks, Diff.mad, color = treatment)
# 
# micro.plots.gdf2 <- micromot2 %>% 
#   filter(GF %in% c("GDF-2", "FBS_5"), Mrk_conc_GF %in% c("0 ng/ml","2.5 ng/ml","10 ng/ml"),
#          Marks != "1%FBS media", Freq %in% Freq.subset) %>% 
#   group_by(Param) %>% do(plots = MyMicroPlot(.)) %>% .$plots
# 
# gdf2.mpz.Diff <- micro.plots.gdf2[[3]] + 
#   ggtitle("GDF-2") +
#   ylab("Mean of abs second differences, impedance") + scale_color_colorblind()
# 
# # micro.plots.fbs <- micromot2 %>% 
# #   filter(GF %in% c("FBS_20"), Marks != "1%FBS media", Freq %in% Freq.subset) %>% 
# #   group_by(Param) %>% do(plots = MyMicroPlot(.)) %>% .$plots
# # 
# # fbs.mpz.Diff <- micro.plots.fbs[[3]] + 
# #   ggtitle("FBS") +
# #   ylab("Mean of abs second differences, impedance") + scale_color_colorblind()
# 
# g <- ggplotGrob(vegf.mpz.Diff)$grobs
# legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
# 
# plots <- lapply(list(vegf.mpz.Diff, fgf2.mpz.Diff, gdf2.mpz.Diff), function(x) x + theme(legend.position="none"))
# micro.plot <- arrangeGrob(plots[[1]],
#                         plots[[2]] + theme(axis.title.y = element_blank()),
#                         plots[[3]] + theme(axis.title.y = element_blank()),
#                         legend,
#                         ncol = 4,
#                         widths = c(7,7,7,1))
# grid.draw(micro.plot) 

# Plot functions ----
Myplot <- function(x) {
  
  ylabeller <- function(parameter) {
    if(parameter=="C"){out<-bquote(list(C, Capacitance~group("(",F,")")))}
    if(parameter=="Z"){out<-bquote(list(Z, Impedance~group("(",Omega,")")))}
    if(parameter=="R"){out<-bquote(list(R, Resistance~group("(",Omega,")")))}
    out
  }
  
  ggplot(x,aes(timeBin, value, color=treatment)) + 
    facet_grid(Freq ~ conc_GF, scales = "free") +
    stat_summary(fun.y = mean, geom = "line") +
    stat_summary(fun.data = mean_se,geom = "errorbar") +
    annotate("rect", xmin = -16, xmax = 0, ymin = -Inf, ymax = Inf, alpha = .2) +
    ylab(ylabeller(unique(x$Param))) + 
    xlab(bquote(list(Time~relative~to~release,h)))
}

Freq.subset <- c("1000 Hz","4000 Hz","16000 Hz","64000 Hz")

# Imepedance plots ----
siCD44.VEGF.list <- ecis.sirna %>% 
  filter(!(GF=="bFGF"&conc_GF==8),!(GF=="VEGF"&conc_GF==79)) %>%
  filter(GF%in%c("VEGF","FBS_5"), Freq %in% Freq.subset) %>% 
  group_by(Param) %>% do(plots=Myplot(.)) %>% .$plots

vegf <- siCD44.VEGF.list[[3]] + scale_color_colorblind()

siCD44.FGF2.list <- ecis.sirna %>% 
  filter(!(GF=="bFGF"&conc_GF==8),!(GF=="VEGF"&conc_GF==79)) %>%
  filter(GF%in%c("bFGF","FBS_5"), Freq %in% Freq.subset) %>% 
  group_by(Param) %>% do(plots=Myplot(.)) %>% .$plots

fgf2 <- siCD44.FGF2.list[[3]] + scale_color_colorblind() + theme(axis.title.y = element_blank())

plots <- list(vegf,fgf2)
g <- ggplotGrob(plots[[1]] + theme(legend.title = element_blank(), legend.position="bottom"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

plots <- lapply(plots, function(x) x + theme(legend.position="none"))
supp.imp <- arrangeGrob(arrangeGrob(
  plots[[1]] + ggtitle("VEGF, ng/ml") + theme(plot.title = element_text(size = 11)),
  plots[[2]] + ggtitle("FGF2, ng/ml") + theme(plot.title = element_text(size = 11)),
  ncol = 2),
  legend,
  nrow=2, heights = c(12,1))

# grid.draw(supp.imp)

# Rb modeling ----

rbdata <- tbl(db, "Rbmodel")
sirna.rbdata <- rbdata %>% filter(Date %in% dates)
ecis.rbsirna <- sirna.rbdata %>% collect %>% left_join(., wide.metadata)
colnames(ecis.rbsirna)[1] <- "Time"

# reset time and create time bins
ecis.rbsirna %<>% left_join(., sirna.marks)

rbsirna <- ecis.rbsirna %>% 
  group_by(Date) %>%
  mutate(Time = Time-Mark) %>% 
  mutate(timeBin = cut(Time, 
                       breaks = unique(floor(Time)),
                       labels = unique(floor(Time))[-1],
                       include.lowest=TRUE)) %>%
  ungroup() %>% filter(complete.cases(.)) 

rbsirna %<>% mutate(conc_GF = as.numeric(as.character(conc_GF)),
                    timeBin = as.numeric(as.character(timeBin)))
rbsirna$treatment[rbsirna$treatment=="SCR"] <- "siNTP"
rbsirna$treatment <- factor(rbsirna$treatment, levels = c("siNTP", "siCD44", "siVIM", "UT"))

MyRbplot <- function(x){ 
  ggplot(x, aes(timeBin, value)) +
  geom_point(size = 0.5) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  annotate("rect", xmin = -16, xmax = 0, ymin = -Inf, ymax = Inf, alpha = .2) +
  facet_grid(treatment ~ conc_GF) +
  xlab(bquote(list(Time~relative~to~release,h))) +
  ylab(bquote(list(R[b],Omega%*%cm^2)))}

# Barrier resistance
vegf.Rb <- rbsirna %>% 
  filter(!(GF=="bFGF"&conc_GF==8),!(GF=="VEGF"&conc_GF==79)) %>%
  filter(GF %in% c("VEGF","FBS_5"), Param == "Rb (ohm.cm^2)") %>% 
  # group_by(Date, timeBin, concGF, treatment) %>% summarise(value = mean(value, na.rm = T)) %>%
  MyRbplot

fgf.Rb <- rbsirna %>% 
  filter(!(GF=="bFGF"&conc_GF==8),!(GF=="VEGF"&conc_GF==79)) %>%
  filter(GF %in% c("bFGF","FBS_5"), Param == "Rb (ohm.cm^2)") %>% 
  # group_by(Date, timeBin, concGF, treatment) %>% summarise(value = mean(value, na.rm = T)) %>%
  MyRbplot

supp.Rb <- arrangeGrob(
  vegf.Rb + ggtitle("VEGF, ng/ml") + theme(plot.title = element_text(size = 11)),
  fgf.Rb + ggtitle("FGF2, ng/ml") + theme(axis.title.y = element_blank(), plot.title = element_text(size = 11)),
  ncol = 2,
  widths = c(1,1))

# grid.draw(supp.Rb)

# Letters

labels <- lapply(LETTERS[1:10], 
                 function(letter) textGrob(letter, x=unit(0,"npc"), hjust = 0, gp = gpar(fontface=2)))

supp <- arrangeGrob(arrangeGrob(labels[[1]], supp.imp, heights = c(1,12)),
                    arrangeGrob(labels[[2]], supp.Rb, heights = c(1,12)),
                    nrow = 2,
                    heights = c(5,4))

# grid.draw(supp)
png("graphs/SuppFig4.png", w = 600, h = 800); plot(supp); dev.off()
