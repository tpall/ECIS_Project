
# Load rawdata from xls file ----
loadData <- function(...) {
  
  library(XLConnect)
  
  wb <- c(...) %>% loadWorkbook
  Date <- readWorksheet(wb, sheet=1)[8,2] %>% gsub("[ -]","",.)
  
  sheetFun <- function(x){
    setdiff(getSheets(x), c("Details","Comments"))
  }
  
  # Read main dataset
  All <- wb %>%
{sheets <- sheetFun(.)
 readWorksheet(., sheet=sheets)} %>% 
  lapply(as.data.table) %>% 
  melt(., id="Time..hrs.")
names(All)[1] <- "Time"

# Munge Well and Param.eter info

All %<>% use_series(L1) %>% {
  m <- regexpr("[0-9]+",.)
  regmatches(.,m) %>% as.numeric    
} %>% 
  data.frame(All,Freq=.)

Mark <- wb %>%
  readWorksheet(., sheet="Comments") %>%
{i <- !apply(.,1, function(x) !length(grep("<Mark> Time:", x)))
 .[i,]} %>%
  gsub("<Mark> Time: ","",.) %>%
  as.numeric

if(length(Mark)==2){
  labels <- c("Seed", "Starv", "Release")
} else {labels <- c("Seed", "Starv", "Preinc", "Release")}

All$Mark <- All %>% 
  use_series(Time) %>% {
    breaks <- c(0, Mark, max(.))
    cut(., breaks = breaks,
        labels = labels)
  }

All %<>% use_series(variable) %>% 
  as.character %>% {
    Well  <- gsub("([A-G0-9]{1,2})[.][ZRC]","\\1",.)
    Param  <-  {m <- regexpr("[CRZ]$", .) 
                regmatches(., m)}
    data.frame(All,Well,Param)
  }

# Add experiment date info
All$Date <- Date
All$variable <- NULL
All$L1 <- NULL
return(All)
}

# First we need to classify our metadata files into three subgroups:
# 1) setup experiments
# 2) SB101-Fc treatment experiments
# 3) siRNA experiments
# Classification is based on experiment variable/column names.

classifyMetaData <- function(...){
  c(...) %>% {m <- regexpr("[0-9]{6}",.) 
              regmatches(.,m)} %>% 
{Date <- .
 tmp <- lapply(.,{. %>% 
                    paste0("ECIS_", ., "_[Mm]etadata_([[:alpha:]]+)_*([[:alpha:]]*).csv") %>%
                    lapply(function(x) regmatches(list.files(path="rawdata"), regexpr(x, list.files(path="rawdata")))) %>% 
                    unlist %>% {
                      mat <- regexec("_[Mm]etadata_([[:alpha:]]+)_*([[:alpha:]]*)", .) 
                      do.call(rbind, lapply(regmatches(., mat), `[`, c(2L,3L))) } %>%
                    apply(.,1, paste, collapse="") %>% t %>% data.frame
 }) %>% rbindlist(., fill=TRUE) 
 tmp %>% mutate(Date = Date)
} %>% gather(Date) %>% {
  i <- complete.cases(.)
  .[i,] %>% select(Date, value)
} %>% table %>% data.frame %>% spread(Date, Freq) %>% 
  select(-value) %>% t %>% dist %>% hclust %>% cutree(., k=3) %>% {
    Date <- data.frame(.) %>% rownames
    data.frame(Date, Type = ., row.names = NULL)
  }
}


# Load metadata from csv files ----
loadMetaData <- function(...){
  
  Metadata <- c(...) %>% { 
    m <- regexpr("[0-9]{6}",.) 
    regmatches(.,m)} %>% 
    paste0("ECIS_", ., "_[Mm]etadata_([[:alpha:]]+)_*([[:alpha:]]*).csv") %>%
    lapply(function(x) regmatches(list.files(path="rawdata"), regexpr(x, list.files(path="rawdata")))) %>% 
    unlist %>% 
    (function(x) sapply(file.path("rawdata",x), read.csv, simplify=FALSE, header=FALSE)) %>%
    lapply(as.matrix) %>% lapply(c) %>% do.call("cbind",.) %>% data.frame
  
  Newnames <- Metadata %>% colnames %>% {
    mat <- regexec("_[Mm]etadata_([[:alpha:]]+)_*([[:alpha:]]*)", .)
    do.call(rbind, lapply(regmatches(., mat), `[`, c(2L,3L))) } %>%
    apply(.,1, paste, collapse="")
  colnames(Metadata) <- Newnames 
  
  Metadata$Well <- c(outer(LETTERS[seq(8)], seq(12), FUN=paste , sep="")) %>% as.factor
  
  #Add experiment date info
  Metadata$Date <- c(...) %>% { 
    m <- regexpr("[0-9]{6}",.) 
    regmatches(.,m)} %>% paste0("20",.)
  return(Metadata)
}

# New function to load metadata to separate tables, 
# takes input classifyMetaData output.
# Example use:
# xlsfiles %>% classifyMetaData %>% dlply(.,"Type",loadMetaData2) %>% 
#   set_names(c("Setup.Metadata","SB101.Metadata","siRNA.Metadata"))
loadMetaData2 <- function(d){
 df <- apply(d, 1, function(x) { x[1] %>% {
    Date <- paste0("20",.)
    paste0("ECIS_", ., "_[Mm]etadata_([[:alpha:]]+)_*([[:alpha:]]*).csv") %>%
      lapply(function(x) regmatches(list.files(path="rawdata"), regexpr(x, list.files(path="rawdata")))) %>% 
      unlist %>% 
      (function(x) sapply(file.path("rawdata",x), read.csv, simplify=FALSE, header=FALSE)) %>%
      lapply(as.matrix) %>% lapply(c) %>% do.call("cbind",.) %>% 
      data.frame %>% {
        Newnames <- colnames(.) %>% {
          mat <- regexec("_[Mm]etadata_([[:alpha:]]+)_*([[:alpha:]]*)", .)
          do.call(rbind, lapply(regmatches(., mat), `[`, c(2L,3L))) } %>%
          apply(.,1, paste, collapse="")
        Well <- c(outer(LETTERS[seq(8)], seq(12), FUN=paste , sep="")) %>% as.factor
        set_names(.,Newnames) %>% mutate(Well=Well)} %>% 
      mutate(Date=Date)} 
  }) %>% rbindlist(., fill=TRUE) %>% 
    as.data.frame 
  
  if("serum" %in% colnames(df)) {
        df$serum[is.na(df$serum)] <- "FBS"
        df
      } else df
}

# Load metadata from csv files ----
loadRbAData <- function(...){
  
  RbAdata <- c(...)  %>% read.table(.,skip=23,header=FALSE,sep=",") %>% {
      Well <- c(outer(LETTERS[seq(8)], seq(12), FUN=paste , sep=""))
      Param <- c("Rb","Alpha","CellMCap","Drift","RMSE")
      ColNames <- c("Time",c(t(outer(Well,Param, FUN=paste))))
      set_names(.,ColNames)
    } %>% 
    melt(., id="Time")
  
  RbAdata <- RbAdata$variable %>% as.character %>% strsplit(.," ") %>% {
    Well <- sapply(.,"[[",1)
    Param <- sapply(.,"[[",2)
    data.frame(Well,Param)
  } %>% cbind(RbAdata, .)
  
  RbAdata$variable <- NULL
  
  #Add experiment date info
  RbAdata$Date <- c(...) %>% { 
    m <- regexpr("[0-9]{6}",.) 
    regmatches(.,m)} %>% paste0("20",.)
  
  RbAdata %>% dcast(., Date+Well+Time~Param, value.var="value") %>%
    melt(., id.vars=c("Date","Well","Time","Drift","RMSE"), variable.name = "Param")
}