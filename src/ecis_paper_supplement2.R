
## @knitr sirna_ecis_supp

# Plot function ----
if(!any(ls()=="myalpha"))(myalpha <- 0.3)

Myplot <- function(x) {
  ylabeller <- function(parameter) {
    if(parameter=="C"){out<-bquote(list(Capacitance,F))}
    if(parameter=="Z"){out<-bquote(list(Impedance,Omega))}
    if(parameter=="R"){out<-bquote(list(Resistance,Omega))}
    out}
  ggplot(x, aes(timeBin, value, color = treatment)) + 
    facet_grid(Freq ~ conc_GF, scales = "free") +
    stat_summary(fun.data = mean_se, geom = "errorbar", alpha = myalpha) +
    stat_summary(fun.y = mean, geom = "line") +
    annotate("rect", xmin = -16, xmax = 0, ymin = -Inf, ymax = Inf, alpha = myalpha) +
    ylab(ylabeller(unique(x$Param))) + 
    xlab(bquote(list(Time~relative~to~release,h)))}

# Filter and summarise ----
Freq.subset <- c("1000 Hz","4000 Hz","16000 Hz","64000 Hz")
ecis.sirna.supp <- ecis.sirna %>% 
  select(Date, GF, conc_GF) %>% 
  group_by(GF, conc_GF) %>% summarise(N = length(unique(Date))) %>%
  filter(N > 1) %>% select(GF, conc_GF) %>% # filter series with N > 1
  inner_join(ecis.sirna, .) %>% # use only data with N > 1
  group_by(Param, Freq, Date, conc_GF, GF, treatment, timeBin) %>%
  summarise(value = mean(value, na.rm = T)) %>% ungroup %>%
  filter(Freq %in% Freq.subset, Param == Parameter, timeBin) %>% {
    finito <- ddply(.,"timeBin", summarise, N = unique(Date) %>% length) %>%
      filter(N>1) %>% max
    .[.$timeBin <= finito,]
  }

# Imepedance plots ----
vegf <- ecis.sirna.supp %>% filter(GF %in% c("VEGF","FBS_5")) %>% Myplot + scale_color_colorblind()
fgf2 <- ecis.sirna.supp %>% filter(GF %in% c("bFGF","FBS_5")) %>% Myplot + theme(axis.title.y = element_blank())

g <- ggplotGrob(vegf + theme(legend.title = element_blank(), legend.position="bottom"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

common.theme <- theme(legend.position="none", plot.title = element_text(size = 11))
supp.imp <- arrangeGrob(arrangeGrob(
  vegf + ggtitle("VEGF, ng/ml") + common.theme,
  fgf2 + ggtitle("FGF2, ng/ml") + common.theme + scale_color_colorblind(),
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
    # geom_point(size = 0.5) +
    stat_summary(fun.y = mean, geom = "line") +
    stat_summary(fun.data = mean_se, geom = "errorbar", alpha = myalpha) +
    annotate("rect", xmin = -16, xmax = 0, ymin = -Inf, ymax = Inf, alpha = .2) +
    facet_grid(treatment ~ conc_GF) +
    xlab(bquote(list(Time~relative~to~release,h))) +
    ylab(bquote(list(R[b],Omega%*%cm^2)))}

rbsirna.filt <- rbsirna %>% select(Date, GF, conc_GF) %>% 
  group_by(GF, conc_GF) %>% summarise(N = length(unique(Date))) %>%
  filter(N > 1) %>% select(GF, conc_GF) %>% # filter series with N > 1
  inner_join(rbsirna, .) %>% # use only data with N > 1
  group_by(Date, Param, GF, conc_GF, treatment, timeBin) %>%
  summarise(value = mean(value, na.rm = T)) %>% ungroup %>% {
    finito <- ddply(.,"timeBin", summarise, N = unique(Date) %>% length) %>%
      filter(N > 1) %>% max
    .[.$timeBin<=finito,]} %>% filter(Param == "Rb (ohm.cm^2)")

# Barrier resistance
vegf.Rb <- rbsirna.filt %>% filter(GF %in% c("VEGF","FBS_5")) %>% MyRbplot
fgf.Rb <- rbsirna.filt %>% filter(GF %in% c("bFGF","FBS_5")) %>% MyRbplot

supp.Rb <- arrangeGrob(
  vegf.Rb + ggtitle("VEGF, ng/ml") + theme(plot.title = element_text(size = 11)),
  fgf.Rb + ggtitle("FGF2, ng/ml") + theme(axis.title.y = element_blank(), plot.title = element_text(size = 11)),
  ncol = 2,
  widths = c(1,1))

# grid.draw(supp.Rb)

# Letters

labels <- lapply(LETTERS[1:10], 
                 function(letter) textGrob(letter, x=unit(0,"npc"), hjust = 0))

supp <- arrangeGrob(arrangeGrob(labels[[1]], supp.imp, heights = c(1,12)),
                    arrangeGrob(labels[[2]], supp.Rb, heights = c(1,12)),
                    nrow = 2,
                    heights = c(5,4))

grid.draw(supp)
