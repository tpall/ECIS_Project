# Function to load data from individual ECIS data export xlsx files into 
# sqlite database

library(plyr);library(dplyr)
library(reshape2)
library(tidyr) # separate
library(magrittr)
library(readxl)
library(RSQLite)

# Specify path to xlsx files with raw data
xlsfiles <- file.path("rawdata", list.files(path = "rawdata", pattern = "_[1,2]{1}.xls"))

# Connect to database
db <- dbConnect(SQLite(), dbname = "data/ECIS2.sqlite")

# Function to read xlsx files, and csv files with metadata in plate-shape and RbA modeling data 
loadData2 <- function(xlsfile, db.conn){
  
  # Set path
  path <- xlsfile
  Date <- sub(".+([0-9]{6}).+","\\1", path)
  
  # Get and name sheets
  sheets <- excel_sheets(path)
  wb <- lapply(sheets, read_excel, path = path)
  names(wb) <- sheets
  
  # Find marks
  marks <- wb$Comments %>% use_series("<New Experiment>") %>%
    "["(c(grep("<Mark>",.), grep("<Mark>",.) + 2)) %>%
    matrix(ncol=2, dimnames = list(c(),c("Mark","Label"))) %>%
    data.frame() %>%
    separate(., Mark, into = c("Mark", "Time"), sep = ": ") %>%
    mutate(Time = as.numeric(Time), Date = Date) %>% dplyr::select(-1)
  dbWriteTable(conn = db.conn, name = "Marks", value = marks,
               row.names = FALSE, append = TRUE)
  
  # Some experimental details
  details <- wb$Details %>% "["(1:8,1:2) %>% set_colnames(c("ECIS_Parameters","value"))
  details <- data.frame(t(details$value)) %>% set_colnames(details$ECIS_Parameters)
  dbWriteTable(conn = db.conn, name = "Details", value = details,
               row.names = FALSE, append = TRUE)
  
  # Read data to table
  wbs <- wb[names(wb) %>% setdiff(c("Details", "Comments"))] %>%
    lapply(.%>% melt(id.vars = c("Time (hrs)"))) %>% ldply %>%
    mutate(Well = sub("([A-H][0-9]{1,2}) ([ZRC])", "\\1", variable),
           Param = sub("([A-H][0-9]{1,2}) ([ZRC])", "\\2", variable),
           Freq = sub(".+ ([0-9]+) .+","\\1", .id))
  wbs$Date <- Date
  wbs$.id <- NULL
  wbs$variable <- NULL
  dbWriteTable(conn = db.conn, name = "Data", value = wbs,
               row.names = FALSE, append = TRUE)
  
  # Retrieve metadata
  metadata <- list.files(path = "rawdata", pattern = paste0(Date,"_Metadata"), full.names = T) %>%
    sapply(read.csv, header = F, simplify = F, colClasses = "character") %>% ldply %>% melt(id.vars = ".id") %>%
    group_by(.id, variable) %>%
    mutate(Well = paste0(LETTERS[1:8], sub("V([0-9]+)","\\1",variable)),
           Metadata = sub(".+data_(.+).csv","\\1",.id),
           Date = Date) %>% ungroup %>% dplyr::select(-.id, -variable) %>%
    as.data.frame
  dbWriteTable(conn = db.conn, name = "Metadata", value = metadata,
               row.names = FALSE, append = TRUE)
  
  # Read modelingdata to table
  rba.file <- list.files(path = "rawdata", pattern = paste0(Date,"_MFT_[0-9]_RbA.csv"), full.names = T)
  rbdata <- rba.file %>% read.table(., skip = 23, header = FALSE, sep = ",")
  rb.id <- rba.file %>% read.table(., skip = 19, nrows = 2, header = FALSE,
                                   sep = ",", stringsAsFactors = FALSE, strip.white = TRUE)
  colnames(rbdata) <- c(rb.id[1, 1], paste(rb.id[2, 2:ncol(rbdata)], rb.id[1, 2:ncol(rbdata)]))
  rbdata %<>% melt(id.var = "Time (hrs)") %>%
    mutate(Well = sub("([A-H][0-9]{1,2}) (.+)", "\\1", variable),
           Param = sub("([A-H][0-9]{1,2}) (.+)", "\\2", variable))
  rbdata$Date <- Date
  rbdata$variable <- NULL
  dbWriteTable(conn = db.conn, name = "Rbmodel", value = rbdata,
               row.names = FALSE, append = TRUE)
}

start <- Sys.time()
(sapply(xlsfiles, .%>% loadData2(db.conn = db)))
end <- Sys.time()
end-start

