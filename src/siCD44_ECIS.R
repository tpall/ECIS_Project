library(RSQLite)
library(magrittr)
library(plyr);library(dplyr)
library(ggplot2)
library(ggthemes)

# Load siRNA experiment data from database
db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection
# dbListTables(db)
sqlcmd <- paste('SELECT * FROM Data 
                INNER JOIN siRNA using(Date,Well) 
                WHERE Mark = "Release" AND  
                celldensity = 5000',sep="")
ecis.sirna <- dbGetQuery(db, sqlcmd)
dbDisconnect(db)

# Reset time and create time bins
sirna <- ecis.sirna %>% 
  group_by(Date) %>%
  mutate(Time = Time-Time[1]) %>% 
  mutate(timeBin = cut(Time, 
                       breaks = unique(floor(Time)),
                       labels = FALSE,
                       include.lowest=TRUE)) %>%
  ungroup() %>% filter(complete.cases(.)) %>% {
    finito <- group_by(.,timeBin) %>% summarise(N=length(unique(Date))) %>% filter(N>1) %>% max
    filter(.,timeBin<=finito)} %>%
  group_by(Date,GF,concGF,Param,Freq,treatment,timeBin) %>%
  summarise(value = mean(value, na.rm = T)) %>% ungroup

glimpse(sirna)

sirna %<>% mutate(concGF = as.numeric(as.character(concGF)))
sirna$treatment[sirna$treatment=="SCR"] <- "siNTP"
sirna$treatment <- factor(sirna$treatment, levels = c("siNTP", "siCD44", "siVIM", "UT"))
# save(ecis.sirna,file = "~/Dropbox/CD44_angiogenesis_paper/data/ecis.sirna.RData")

# plot stuff -----
library(Hmisc)
library(ggthemes)

ylabeller <- function(x){
  if(x=="C"){out<-bquote(list(C, Capacitance~group("(",F,")")))}
  if(x=="Z"){out<-bquote(list(Z, Impedance~group("(",Omega,")")))}
  if(x=="R"){out<-bquote(list(R, Resistance~group("(",Omega,")")))}
  out
}

Myplot <- function(x) {
  ggplot(x,aes(timeBin, value, color=treatment)) + 
    facet_grid(Freq~concGF, scales = "free") +
    stat_summary(fun.y = mean, geom = "line") +
    stat_summary(fun.data = mean_se,geom = "errorbar", width = 0.2, size = 0.1) +
    ylab(ylabeller(unique(x$Param))) + 
    xlab(bquote(list(Time~after~release,h)))
}

# Raw VEGF data ----
# sirna %>% 
#   filter(GF%in%c("VEGF","FBS_5")) %>% 
#   group_by(Param) %>%
#   do(plots=Myplot(.))%>%
#   .$plots%>%
#   lapply(.%>%{
#     pam <- regmatches(.$labels$y, regexpr("^.",.$labels$y))
#     ggsave(paste0("graphs/siRNA_VEGF_",pam,"_",Sys.Date(),".pdf"),
#            plot=.,width=5,height=7)})
# 
siCD44.VEGF.list <- sirna %>% filter(GF%in%c("VEGF","FBS_5"), Freq >= 1000) %>% 
  group_by(Param) %>% do(plots=Myplot(.)) %>% .$plots

vegf <- siCD44.VEGF.list[[3]] + scale_color_colorblind()

siCD44.FGF2.list <- sirna %>% filter(GF%in%c("bFGF","FBS_5"), Freq>=1000) %>% 
  group_by(Param) %>% do(plots=Myplot(.)) %>% .$plots

fgf2 <- siCD44.FGF2.list[[3]] + scale_color_colorblind() + theme(axis.title.y = element_blank())

library(gridExtra)

plots <- list(vegf,fgf2)
g <- ggplotGrob(plots[[1]] + theme(legend.title = element_blank(), legend.position="right"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

plots <- lapply(plots, function(x) x + theme(legend.position="none"))
supp <- arrangeGrob(plots[[1]] + ggtitle("VEGF"),
                    plots[[2]] + ggtitle("FGF2"),
                    legend,
            ncol = 3,
            widths = c(6,6,2))

grid.draw(supp)
# # norm data ---
# ecis.sirna %>%
#   filter(GF%in%c("VEGF","FBS_5")) %>%
#   group_by(Date,Well,Param,Freq) %>%
#   mutate(value=value/mean(head(value,3))) %>%
#   group_by(Param) %>%
#   do(plots=Myplot(.)) %>% 
#   .$plots %>%
#   lapply(.%>%{
#     pam <- regmatches(.$labels$y, regexpr("^.",.$labels$y))
#     ggsave(paste0("graphs/siRNA_VEGF_",pam,"_norm_",Sys.Date(),".pdf"),
#            plot=.,width=5,height=7)})
# 
# # bFGF data ----
# ecis.sirna %>% 
#   filter(GF%in%c("bFGF","FBS_5")) %>% 
#   group_by(Param) %>%
#   do(plots=Myplot(.))%>%
#   .$plots%>%
#   lapply(.%>%{
#     pam <- regmatches(.$labels$y, regexpr("^.",.$labels$y))
#     ggsave(paste0("graphs/siRNA_FGF2_",pam,"_",Sys.Date(),".pdf"),
#            plot=.,width=5,height=7)})
# 



# # norm data ----
# ecis.sirna %>%
#   filter(GF%in%c("bFGF","FBS_5")) %>%
#   group_by(Date,Well,Param,Freq) %>%
#   mutate(value=value-mean(head(value,3))) %>%
#   group_by(Param) %>%
#   do(plots=Myplot(.)) %>% 
#   .$plots %>%
#   lapply(.%>%{
#     pam <- regmatches(.$labels$y, regexpr("^.",.$labels$y))
#     ggsave(paste0("graphs/siRNA_FGF2_",pam,"_norm_",Sys.Date(),".pdf"),
#            plot=.,width=5,height=7)})
# 
# # FBS data ----
# ecis.sirna %>% 
#   filter(GF%in%c("FBS_20","FBS_5")) %>% 
#   group_by(Param) %>%
#   do(plots=Myplot(.)%>%add(facet_grid(Freq~GF, scales="free")))%>%
#   .$plots%>%
#   lapply(.%>%{
#     pam <- regmatches(.$labels$y, regexpr("^.",.$labels$y))
#     ggsave(paste0("graphs/siRNA_FBS_",pam,"_",Sys.Date(),".pdf"),
#            plot=.,width=2.5,height=7)})
# 
# # norm data ----
# ecis.sirna %>% 
#   filter(GF%in%c("FBS_20","FBS_5")) %>%
#   group_by(Date,Well,Param,Freq) %>%
#   mutate(value=value-mean(head(value,3))) %>%
#   group_by(Param) %>%
#   do(plots=Myplot(.)%>%add(facet_grid(Freq~GF, scales="free")))%>%
#   .$plots%>%
#   lapply(.%>%{
#     pam <- regmatches(.$labels$y, regexpr("^.",.$labels$y))
#     ggsave(paste0("graphs/siRNA_FBS_",pam,"_norm_",Sys.Date(),".pdf"),
#            plot=.,width=2.5,height=7)})

# Rb modeling
db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection
sqlcmd <- paste('SELECT * FROM Rbmodel INNER JOIN siRNA using(Date, Well)')
Rb.sirna <- dbGetQuery(db, sqlcmd)
dbDisconnect(db)

Release <- ecis.sirna %>% group_by(Date) %>% summarise(Release = min(Time))

glimpse(Rb.sirna)

# reset time and create time bins
Rb <- Rb.sirna %>% 
  merge(Release) %>%
  group_by(Date) %>%
  mutate(Time = Time - Release) %>% 
  mutate(timeBin = cut(Time, 
                       breaks = unique(floor(Time)),
                       labels = unique(floor(Time))[-1],
                       include.lowest=TRUE),
         timeBin = as.numeric(as.character(timeBin))) %>%
  ungroup() %>% filter(complete.cases(.))

# %>% {
#     finito <- group_by(.,timeBin) %>% summarise(N=length(unique(Date))) %>% filter(N>1) %>% max
#     filter(.,timeBin<=finito)}

Rb %<>% mutate(concGF = as.numeric(as.character(concGF)))
Rb$treatment[Rb$treatment=="SCR"] <- "siNTP"
Rb$treatment <- factor(Rb$treatment, levels = c("siNTP", "siCD44", "siVIM"))

# Barrier resistance
# Rb %>% filter(GF %in% c("VEGF","FBS_5"), Param == "Rb") %>% 
#   ggplot(aes(timeBin, value)) +
#   geom_point(size = 0.4) +
#   stat_smooth() +
#   facet_grid(concGF~treatment) +
#   xlab(bquote(list(Time~after~release,h))) +
#   ylab(bquote(list(R[b],Omega%*%cm^2)))

Rb %>% filter(GF %in% c("VEGF","FBS_5"), Param == "Rb") %>% 
  # group_by(Date, timeBin, concGF, treatment) %>% summarise(value = mean(value, na.rm = T)) %>%
  ggplot(aes(timeBin, value)) +
  geom_point(size = 0.5) +
  stat_summary(fun.y = mean, geom = "line", size = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", size = 0.25) +
  annotate("rect", xmin = -16, xmax = 0, ymin = 0, ymax = Inf, alpha = .2) +
  facet_grid(concGF~treatment) +
  xlab(bquote(list(Time~relative~to~release,h))) +
  ylab(bquote(list(R[b],Omega%*%cm^2)))

Rb %>% filter(GF %in% c("bFGF","FBS_5"), Param == "Rb") %>% 
  group_by(Date, timeBin, concGF, treatment) %>% summarise(value = mean(value, na.rm = T)) %>%
  ggplot(aes(timeBin, value)) +
  geom_point(size = 0.5) +
  stat_summary(fun.y = mean, geom = "line", size = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", size = 0.25) +
  annotate("rect", xmin = -16, xmax = 0, ymin = 0, ymax = Inf, alpha = .2) +
  facet_grid(concGF~treatment) +
  xlab(bquote(list(Time~relative~to~release,h))) +
  ylab(bquote(list(R[b],Omega%*%cm^2)))

# # Alpha, is a measure of the constraint on current flow beneath the cells
# Rb %>% filter(GF %in% c("VEGF","FBS_5"), treatment!="UT", Param == "Alpha") %>% 
#   group_by(Date, timeBin, concGF, treatment) %>% summarise(value = mean(value, na.rm = T)) %>%
#   ggplot(aes(timeBin, value)) +
#   stat_summary(fun.y = mean, geom = "line", size = 1) +
#   stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, size = 0.25) +
#   facet_grid(concGF~treatment) +
#   xlab(bquote(list(Time~after~release,h))) +
#   ylab(bquote(list(alpha,Omega^0.5%*%cm)))
# 
# # Capacitance of the cell plasma membranes
# Rb %>% filter(GF %in% c("VEGF","FBS_5"), treatment!="UT", Param == "CellMCap") %>% 
#   group_by(Date, timeBin, concGF, treatment) %>% summarise(value = mean(value, na.rm = T)) %>%
#   ggplot(aes(timeBin, value)) +
#   stat_summary(fun.y = mean, geom = "line", size = 1) +
#   stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, size = 0.25) +
#   facet_grid(concGF~treatment) +
#   xlab(bquote(list(Time~after~release,h))) +
#   ylab(bquote(list(Cm,mu*F/cm^2)))
