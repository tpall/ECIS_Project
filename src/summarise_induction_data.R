library('ProjectTemplate')
load.project()

# check if hourly means are already calculated
if(!testObject(datalist.h)) {
  datalist.h <- lapply(datalist.norm, hourlysum)
}

lapply(datalist.h, head)


# first, lets choose 25 ng/ml induced wells and controls ----
# pull 140108, 131218 and 131213 VEGF data off from datalist.h
treatment.exp <- which(unlist(lapply(datalist.h, function(x) "treatment2" %in% names(x))))
sublist <- datalist.h[treatment.exp]
lapply(sublist, head)

require(data.table)
dfa <- rbindlist(sublist)
head(dfa)
levels(factor(dfa$treatment2))
setkey(dfa, "celldensity", "GF")
dfa <- dfa[J("5000","VEGF")]
# drop unnecessary levels
dfa <- droplevels(dfa)

#transform log concentrations back to linear values
dfa$conc <- floor(10^dfa$conc)

#dfa$treatment2 <- paste0(dfa$conc, "-", dfa$GF, "_", dfa$treatment)
# levels(factor(dfa$treatment2))
# # turn Time to numeric
dfa$Time <- as.numeric(as.character(dfa$Time))
class(dfa$Time)

# check if timelines are of same length and truncate if necesary
t <- min(unlist(dlply(dfa, "exp.id", function(x) length(levels(factor(x$Time))))))
dfa <- dfa[dfa$Time <= t, ]


# we want to group by VEGF "conc" 8 vs 25 ng/ml, "treatment2", "dose" and "parameter"
require(dplyr)
gr <- as.quoted(c("exp.id", "Time", "parameter", "doses", "treatment2"))
sb <- dfa %.% regroup(gr) %.% summarise(Mean = mean(value), SD = sd(value)) # NB! not nval anmore

gr <- as.quoted(c("Time", "parameter", "doses", "treatment2"))
sb <- sb %.% regroup(gr) %.% summarise(Mean = mean(Mean), SD = sd(Mean), N = length(Mean), SE = sd(Mean)/sqrt(length(Mean)))

sb <- sb[which(complete.cases(sb)),]
sb$parameter <- factor(sb$parameter, levels = c("Z1k",  "Z2k", "Z4k", "Z8k", "Z16k",  "Z32k", "Z64k"))
pd <- position_dodge(.1)
legendname <- "Treatment"
sb25 <- sb[sb$treatment2 %in% c("0-VEGF_hIgG-Fc", "25-VEGF_hIgG-Fc", "25-VEGF_SB101-Fc"), ] 

ggplot(sb25, aes(x = Time, y = Mean, colour = treatment2, 
                     ymin = Mean - SD, ymax = Mean + SD)) +
  geom_errorbar(width = .1, alpha = .3) +
  geom_line(position = pd, aes(group = treatment2)) +
  facet_grid(parameter ~ doses, scale = "free_y") +
  ylab(expression(paste("Normalised impedance (", mean %+-% SD, ")"))) +
  xlab("Time (hour)") +
  theme(legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  guides(colour = guide_legend(title = legendname))

ggsave("graphs/3MUT-Fc_treatment_summary_VEGF-25.pdf")

levels(factor(sb$treatment2))
sb8 <- sb[sb$treatment2 %in% c("0-VEGF_hIgG-Fc", "8-VEGF_hIgG-Fc", "8-VEGF_SB101-Fc"), ] 

ggplot(sb8, aes(x = Time, y = Mean, colour = treatment2, 
                 ymin = Mean - SD, ymax = Mean + SD)) +
  geom_errorbar(width = .1, alpha = .3) +
  geom_line(position = pd, aes(group = treatment2)) +
  facet_grid(parameter ~ doses, scale = "free_y") +
  ylab(expression(paste("Normalised impedance (", mean %+-% SD, ")"))) +
  xlab("Time (hour)") +
  theme(legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  guides(colour = guide_legend(title = legendname))

ggsave("graphs/3MUT-Fc_treatment_summary_VEGF-7.pdf")



# then, lets analyze VEGF effect on HUVEC growth ----
# pull 131127, 131028 and 131204 VEGF conc dependence data off from datalist.h
# sublist <- datalist.h[which(names(datalist.h) %in% c("131127", "131028", "131204"))]
sublist <- setuplist[which(names(setuplist) %in% c("131127", "131028", "131204"))]
# unlist(llply(sublist, function(x) length(levels(factor(x$Time)))))
# llply(sublist, function(x) levels(factor(x$Time)))
require(data.table)
# 131127 dataframe (here #2) contains non-serum-induced group, we remove those data 
dfa <- sublist[[2]]
dfa <- dfa[dfa$serum=="FBS",]
dfa <- subset(dfa, select = -c(serum) )
sublist[[2]] <- dfa
dfa <- rbindlist(sublist)
# unlist(dlply(dfa, "exp.id", function(x) length(levels(factor(x$Time)))))
# dlply(dfa, "exp.id",function(x) sort(as.numeric(as.character(levels(factor(x$Time))))))
# we select only VEGF induced wells
setkey(dfa, "GF")
dfa <- dfa["VEGF"]
# drop unnecessary levels
dfa <- droplevels(dfa)

#transform log concentrations back to linear values
levels(factor(dfa$conc))
dfa$conc <- floor(10^dfa$conc)
# ok, to pool similar concentrations lets replace these fields manually 
dfa$conc[which(dfa$conc %in% c(1, 2))] <- 2
dfa$conc[which(dfa$conc %in% c(6, 7))] <- 7
dfa$conc[which(dfa$conc %in% c(19, 25))] <- 25

#levels(factor(dfa$conc))
# # turn Time to numeric
dfa$Time <- as.numeric(as.character(dfa$Time))
class(dfa$Time)

# check if timelines are of same length and truncate if necesary
t <- min(unlist(dlply(dfa, "exp.id", function(x) length(levels(factor(x$Time))))))
dfa <- dfa[dfa$Time <= t, ]


# we want to group by "exp.id", VEGF "conc" and "parameter"
require(dplyr)
gr <- as.quoted(c("exp.id", "Time", "parameter", "conc"))
sb <- dfa %>% group_by(gr) %>% summarise(Mean = mean(value), SD=sd(value))
class(sb$Time)

gr <- as.quoted(c("Time", "parameter", "conc"))
sb <- sb %.% group_by(gr) %.% summarise(Mean = mean(Mean), SD=sd(Mean), N = length(Mean), SE = sd(Mean)/sqrt(length(Mean)))

sb <- sb[which(complete.cases(sb)),]
sb$parameter <- factor(sb$parameter, levels = c("Z1k",  "Z2k", "Z4k", "Z8k", "Z16k",  "Z32k", "Z64k"))
pd <- position_dodge(.1)

#sb <- sb[sb$parameter %in% c("Z4k", "Z16k", "Z64k"), ] 

ggplot(sb, aes(x = Time, y = Mean, ymin = Mean - SD, ymax = Mean + SD)) +
  geom_errorbar(width = .1, alpha = .3) +
  geom_line(position = pd) +
  facet_grid(parameter ~ conc, scale = "free_y") +
  ylab(expression(paste("Normalised impedance (", mean %+-% SD, ")"))) +
  xlab("Time (hour)") +
  ggtitle("HUVEC VEGF concentration response\n summary") +
  theme(legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

ggsave("graphs/VEGF_induction_summary.pdf", height = 6.94, width = 4.44)

# NYAS conference poster ----
vegf <- sb[sb$parameter %in% c("Z1k", "Z4k", "Z16k", "Z64k"), ] 
write.csv(vegf, "reports/VEGF_dose-response.csv")
ggplot(vegf, aes(x = Time, y = Mean, ymin = Mean - SD, ymax = Mean + SD)) +
  geom_errorbar(width = .1, alpha = .1) +
  geom_line(position = pd, size = 1) +
  facet_grid(parameter ~ conc, scale = "free_y") +
  ylab(expression(paste("Normalised impedance (", mean %+-% SD, ")"))) +
  xlab("Time (hour)") +
  ggtitle("VEGF dose-response in HUVEC")

ggsave("graphs/VEGF_induction_nyas.pdf", height = 6.94, width = 4.44)

# and now, hands on GDF2 (BMP-9) -----
sublist <- datalist.h[which(names(datalist.h) %in% c("131022", "131127", "131028", "131204"))]
# unlist(llply(sublist, function(x) length(levels(factor(x$Time)))))
# llply(sublist, function(x) levels(factor(x$Time)))
require(data.table)
# 131127 dataframe (here #3) contains non-serum-induced group, we remove those data 
dfa <- sublist[[3]]
  dfa <- dfa[dfa$serum=="FBS",]
  dfa <- subset(dfa, select = -c(serum) )
sublist[[3]] <- dfa
dfa <- rbindlist(sublist)
# unlist(dlply(dfa, "exp.id", function(x) length(levels(factor(x$Time)))))
# dlply(dfa, "exp.id",function(x) sort(as.numeric(as.character(levels(factor(x$Time))))))
# we select only VEGF induced wells
setkey(dfa, "GF")
dfa <- dfa["GDF2"]
# drop unnecessary levels
dfa <- droplevels(dfa)

#transform log concentrations back to linear values
levels(factor(dfa$conc))
dfa$conc <- floor(10^dfa$conc)
# ok, to pool similar concentrations lets replace these fields manually 
dfa$conc[which(dfa$conc %in% c(1, 2))] <- 2
dfa$conc[which(dfa$conc %in% c(6, 7))] <- 7
dfa$conc[which(dfa$conc %in% c(19, 25))] <- 25
dfa$conc[which(dfa$conc %in% c(63, 79))] <- 79
dfa$conc[which(dfa$conc %in% c(251, 316))] <- 316

# # turn Time to numeric
dfa$Time <- as.numeric(as.character(dfa$Time))
class(dfa$Time)

# check if timelines are of same length and truncate if necesary
t <- min(unlist(dlply(dfa, "exp.id", function(x) length(levels(factor(x$Time))))))
dfa <- dfa[dfa$Time <= t, ]


# we want to group by "exp.id", GDF2 "conc" and "parameter"
require(dplyr)
gr <- as.quoted(c("exp.id", "Time", "parameter", "conc", "celldensity"))
sb <- dfa %.% regroup(gr) %.% summarise(Mean = mean(nval), SD=sd(nval))
class(sb$Time)

gr <- as.quoted(c("Time", "parameter", "conc", "celldensity"))
sb <- sb %.% regroup(gr) %.% summarise(Mean = mean(Mean), SD=sd(Mean))

sb <- sb[which(complete.cases(sb)),]
sb$parameter <- factor(sb$parameter, levels = c("Z1k",  "Z2k", "Z4k", "Z8k", "Z16k",  "Z32k", "Z64k"))
pd <- position_dodge(.1)

ggplot(sb, aes(x = Time, y = Mean, ymin = Mean - SD, ymax = Mean + SD, colour = celldensity)) +
  geom_errorbar(width = .1, alpha = .3) +
  geom_line(position = pd) +
  facet_grid(parameter ~ conc, scale = "free_y") +
  ylab(expression(paste("Normalised impedance (", mean %+-% SD, ")"))) +
  xlab("Time (hour)") +
  ggtitle("HUVEC GDF2 concentration response\n summary") +
  theme(legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  guides(colour = guide_legend(title = "Cells/well"))

ggsave("graphs/GDF2_induction_summary.pdf", height = 7, width = 4.6)

# HGF and bFGF stuff -----
sublist <- datalist.h[which(names(datalist.h) %in% c("131022", "131028", "131204"))]
# unlist(llply(sublist, function(x) length(levels(factor(x$Time)))))
# llply(sublist, function(x) levels(factor(x$Time)))
require(data.table)
dfa <- rbindlist(sublist)
# unlist(dlply(dfa, "exp.id", function(x) length(levels(factor(x$Time)))))
# dlply(dfa, "exp.id",function(x) sort(as.numeric(as.character(levels(factor(x$Time))))))
# we select HGF and bFGF induced wells
dfa <- dfa[which(dfa$GF %in% c("HGF", "bFGF")), ]
# drop unnecessary levels
dfa <- droplevels(dfa)

#transform log concentrations back to linear values
levels(factor(dfa$conc))
dfa$conc <- floor(10^dfa$conc)
# ok, to pool similar concentrations lets replace these fields manually 
dfa$conc[which(dfa$conc %in% c(1, 2, 3))] <- 3
dfa$conc[which(dfa$conc %in% c(5, 6, 7))] <- 7
dfa$conc[which(dfa$conc %in% c(12, 15))] <- 15
dfa$conc[which(dfa$conc %in% c(19, 25, 39))] <- 25
dfa$conc[which(dfa$conc %in% c(50, 63, 79))] <- 63
dfa$conc[which(dfa$conc %in% c(251, 316))] <- 316

# # turn Time to numeric
dfa$Time <- as.numeric(as.character(dfa$Time))
class(dfa$Time)

# check if timelines are of same length and truncate if necesary
t <- min(unlist(dlply(dfa, "exp.id", function(x) length(levels(factor(x$Time))))))
dfa <- dfa[dfa$Time <= t, ]


# we want to group by "exp.id", "GF" "conc" and "parameter"
require(dplyr)
gr <- as.quoted(c("exp.id", "Time", "parameter", "GF", "conc", "celldensity"))
sb <- dfa %.% regroup(gr) %.% summarise(Mean = mean(nval), SD=sd(nval))
class(sb$Time)

gr <- as.quoted(c("Time", "parameter", "GF", "conc", "celldensity"))
sb <- sb %.% regroup(gr) %.% summarise(Mean = mean(Mean), SD=sd(Mean))


sb$parameter <- factor(sb$parameter, levels = c("Z1k",  "Z2k", "Z4k", "Z8k", "Z16k",  "Z32k", "Z64k"))
pd <- position_dodge(.1)

sb <- sb[which(complete.cases(sb)),]
# first HGF
hgf <- sb[which(sb$GF == "HGF"), ]
ggplot(hgf, aes(x = Time, y = Mean, ymin = Mean - SD, ymax = Mean + SD, colour = celldensity)) +
  geom_errorbar(width = .1, alpha = .3) +
  geom_line(position = pd) +
  facet_grid(parameter ~ conc, scale = "free_y") +
  ylab(expression(paste("Normalised impedance (", mean %+-% SD, ")"))) +
  xlab("Time (hour)") +
  ggtitle("HUVEC HGF concentration response\n summary") +
  theme(legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  guides(colour = guide_legend(title = "Cells/well"))

ggsave("graphs/HGF_induction_summary.pdf", height = 7, width = 4.6)

fgf <- sb[which(sb$GF == "bFGF"), ]
ggplot(fgf, aes(x = Time, y = Mean, ymin = Mean - SD, ymax = Mean + SD, colour = celldensity)) +
  geom_errorbar(width = .1, alpha = .3) +
  geom_line(position = pd) +
  facet_grid(parameter ~ conc, scale = "free_y") +
  ylab(expression(paste("Normalised impedance (", mean %+-% SD, ")"))) +
  xlab("Time (hour)") +
  ggtitle("HUVEC bFGF concentration response\n summary") +
  theme(legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  guides(colour = guide_legend(title = "Cells/well"))

ggsave("graphs/bFGF_induction_summary.pdf", height = 7, width = 4.6)
