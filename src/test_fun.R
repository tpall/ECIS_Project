loadRb <- function(xlsfile){
  
  # Set path
  path <- xlsfile
  Date <- sub(".+([0-9]{6}).+","\\1", path)
  
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
  return(rbdata)
}

start <- Sys.time()
(rb.list <- sapply(xlsfiles, .%>% loadRb))
end <- Sys.time()
end-start

sub(".+([0-9]{6}).+","\\1", xlsfiles) %>% (function(x) list.files(path = "rawdata", pattern = paste0(x,"_MFT_[0-9]_RbA.csv"), full.names = T))
sapply(sub(".+([0-9]{6}).+","\\1", xlsfiles), function(x) list.files(path = "rawdata", pattern = paste0(x,"_MFT_[0-9]_RbA.csv"), full.names = T))
