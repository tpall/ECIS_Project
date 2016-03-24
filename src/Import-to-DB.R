# These scripts load ECIS data exported from xls files in rawdata folder 
# to "Data" table in ECIS.sqlite DB.
# "Metadata" table from csv files and 
# Rb modeling resuls to "Rbmodel" table. 
# Export from xls files takes several minutes
# 
# Load required packages and set the working directory
library(sqldf)      # Loads RSQLite
library(XLConnect)  # To read Excel sheets and workbooks
library(gdata) # To read Excel sheets and workbooks
library(magrittr)
library(data.table)
library(reshape2)
library(tidyr)
library(plyr)
library(dplyr)
source("lib/LoadData.R")

db <- dbConnect(SQLite(), dbname="data/ECIS2.sqlite")

xlsfiles <- file.path("rawdata", list.files(path = "rawdata", pattern = "_[1,2]{1}.xls"))
# if("Data"%in%dbListTables(db)){dbRemoveTable(db, "Data")}



fillDBwithData <- function(xls, conn, name, append = TRUE){
  loadData(xls) %>% dbWriteTable(conn = conn, name = name, value = ., row.names = FALSE, append = append)
}

xlsfiles %>% sapply(., function(x) fillDBwithData(x, db, "Data", append = TRUE))

# if(dbExistsTable(db, "Metadata")){dbRemoveTable(db, "Metadata")}
# Create Metadata table
# OK folks, lets classify our experiments/metadata. Based on experimental 
# variables used.

Metadata <- xlsfiles %>% 
  classifyMetaData %>% 
  dlply(., "Type", loadMetaData2) %>% 
  set_names(c("Model", "Treatments", "siRNA"))

Metadata %>% use_series(Model) %>% dbWriteTable(conn = db, name = "Model", value = ., row.names = FALSE)
Metadata %>% use_series(Treatment) %>% dbWriteTable(conn = db, name = "Treatment", value = ., row.names = FALSE)
Metadata %>% use_series(siRNA) %>% dbWriteTable(conn = db, name = "siRNA", value = ., row.names = FALSE)

# dbDisconnect(conn = db)

# xlsfiles %>% sapply(loadMetaData) %>%
#   rbindlist(., fill=TRUE) %>% {
#   dbWriteTable(conn = db, name = "Metadata", value = ., row.names = FALSE, append=TRUE)
#   }



# Create and insert data to RbA table ----
RbAfiles <- file.path("rawdata", list.files(path = "rawdata", pattern = "_RbA.csv", recursive = F))

fillRBdata <- function(csv, conn){
  loadRbAData(csv) %>%
    dbWriteTable(conn = db, name = "Rbmodel", 
                 value = ., row.names = FALSE, overwrite = TRUE, append = F)
}

RbAfiles %>% sapply(., function(x) fillRBdata(x, db))

# dbListTables(db) 
# dbListFields(db, "Data") 
# dbListFields(db, "Metadata") 
# dbListFields(db, "Rbmodel") 
# data = dbGetQuery(db, 'SELECT *
#                        FROM Data
#                        INNER JOIN Metadata
#                        using(Date,Well)
#                        WHERE Mark = "Release" AND GF="VEGF" ')
# data = dbGetQuery(db, 'SELECT *
#                        FROM Rbmodel
#                        INNER JOIN Metadata
#                        using(Date,Well)
#                        WHERE GF="GDF2" AND Param="Rb"')
# head(data)
# summary(data)
# rm(data)
dbDisconnect(db)            # Close connection

