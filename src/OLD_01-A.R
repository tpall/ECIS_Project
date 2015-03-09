# # Example preprocessing script.
# # Libaries
# options(java.parameters = "-Xmx2g")
# library("XLConnect")
# library("magrittr")
# library("reshape2")
# library("plyr");library("dplyr")
# library("data.table")
# 
# # Get directories
# list.dirs <- function(...) {
#   x <- dir(...)
#   x[file_test("-d", file.path(...,x))]
# }
# 
# # dirs <- list.dirs("~/Dropbox/ECIS/SB101HUVEC/data") %>% .[grep("HUVEC_96WE1", .)]
# xlsfiles <- file.path("rawdata", list.files(path = "rawdata", pattern = "_[1,2]{1}.xls"))
# 
# Mungexls <- function(...){
#   options(java.parameters = "-Xmx2g")
#   library("XLConnect")
#   library("magrittr")
#   library("reshape2")
#   library("plyr")
#   library("data.table")
#   
#   sheetFun <- function(x){
#     setdiff(getSheets(x), c("Details","Comments"))
#   }
#   
#   wb <- c(...) %>% loadWorkbook  
#   
#   # Read main dataset
#   All <- wb %>%
# {sheets <- sheetFun(.)
#  readWorksheet(., sheet=sheets)} %>% 
#   lapply(as.data.table) %>% 
#   melt(., id="Time..hrs.")
# names(All)[1] <- "Time"
# 
# # Munge Well and Param.eter info
# All %<>% use_series(variable) %>% 
#   as.character %>% {
#     Well  <- gsub("([A-G0-9]{1,2})[.][ZRC]","\\1",.)
#     Param  <-  {m <- regexpr("[CRZ]$", .) 
#                 regmatches(., m)}
#     data.frame(All,Well,Param)
#   }
# 
# All %<>% use_series(L1) %>% {
#   m <- regexpr("[0-9]+",.)
#   regmatches(.,m) %>% as.numeric    
# } %>% 
#   data.frame(All,Freq=.)
# 
# All$variable <- NULL
# All$L1 <- NULL
# 
# # Add experiment date info
# # All$Date <- c(...) %>% { 
# #   m <- regexpr("[0-9]{6}",.) 
# #   regmatches(.,m)}
# 
# Mark <- wb %>%
#   readWorksheet(., sheet="Comments") %>%
# {i <- !apply(.,1, function(x) !length(grep("<Mark> Time:", x)))
#  .[i,]} %>%
#   gsub("<Mark> Time: ","",.) %>%
#   as.numeric
# 
# if(length(Mark)==2){
#   labels <- c("Seed", "Starv", "Release")
# }else{labels <- c("Seed", "Starv", "Preinc", "Release")}
# 
# All$Mark <- All %>% 
#   use_series(Time) %>% {
#     breaks <- c(0, Mark, max(.))
#     cut(., breaks = breaks,
#         labels = labels)
#   }
# 
# # Metadata
# Metadata <- c(...) %>% { 
#   m <- regexpr("[0-9]{6}",.) 
#   regmatches(.,m)} %>% 
#   paste0("ECIS_", ., "_[Mm]etadata_([[:alpha:]]+)_*([[:alpha:]]*).csv") %>%
#   lapply(function(x) regmatches(list.files(path="data"), regexpr(x, list.files(path="rawdata")))) %>% 
#   unlist %>% 
#   (function(x) sapply(file.path("rawdata",x), read.csv, simplify=FALSE, header=FALSE)) %>%
#   lapply(as.matrix) %>% lapply(c) %>% do.call("cbind",.) %>% data.frame
# 
# Newnames <- Metadata %>% colnames %>% {
#   mat <- regexec("_[Mm]etadata_([[:alpha:]]+)_*([[:alpha:]]*)", .)
#   do.call(rbind, lapply(regmatches(., mat), `[`, c(2L,3L))) } %>%
#   apply(.,1, paste, collapse="")
# colnames(Metadata) <- Newnames 
# 
# Metadata$Well <- c(outer(LETTERS[seq(8)], seq(12), FUN=paste , sep="")) %>% as.factor
# 
# # Match data and metadata
# All <- data.table(All,key="Well")
# Metadata <- data.table(Metadata,key="Well")
# merge(All, Metadata)
# }
# 
# Bigdata <- lapply(xlsfiles, Mungexls)
# names(Bigdata) <- c(xlsfiles) %>% {m <- regexpr("[0-9]{6}",.) 
#                                    regmatches(.,m)}
# # names(Bigdata)
# # lapply(Bigdata,head)
# # Filter out VEGF data from "140505" experiment because of no induction 
# Bigdata$"140505" %<>% setkey(GF) %>% .[!GF=="VEGF"]
# # Sum <- lapply(Bigdata,summary)
# # Sum
# # Bigdata <- rbindlist(Bigdata)
# # head(Biggdata)
