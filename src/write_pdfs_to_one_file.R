library(magrittr)

# The modeling results from setup experiments 
list.files("graphs",full.names = TRUE) %>% 
  "["(grep("Modeling",.)) %>% 
  paste(collapse=" ") %>%
  paste("pdftk",.,"cat output graphs/Models_Setup_20150109.pdf") %>%
  system

# remove single files
list.files("graphs",full.names = TRUE) %>% 
  "["(grep("Modeling",.)) %>% 
  paste(collapse=" ") %>%
  paste("rm",. , collapse=" ") %>%
  system

# The induction results from setup experiments 
list.files("graphs",full.names = TRUE) %>% 
  "["(grep("2015-01-09",.)) %>% 
  paste(collapse=" ") %>%
  paste("pdftk",.,"cat output graphs/Induction_Setup_20150109.pdf") %>%
  system

# remove single files
list.files("graphs",full.names = TRUE) %>% 
  "["(grep("2015-01-09",.)) %>% 
  paste(collapse=" ") %>%
  paste("rm",. , collapse=" ") %>%
  system

## The treatment results 
list.files("graphs",full.names = TRUE) %>% 
  "["(grep("Norm",.)) %>% 
  paste(collapse=" ") %>%
  paste("pdftk",.,"cat output graphs/Treatment-experiments_20150120.pdf") %>%
  system

file.path("graphs","Treatment-experiments_20150120.pdf") %>% 
  paste("gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/screen -dNOPAUSE -dQUIET -dBATCH -sOutputFile=output.pdf", .) %>%
  system


# remove single files
list.files("graphs",full.names = TRUE) %>% 
  "["(grep("Norm",.)) %>% 
  paste(collapse=" ") %>%
  paste("rm",. , collapse=" ") %>%
  system

## The Modeling results from treatment experiments 
list.files("graphs",full.names = TRUE) %>% 
  "["(grep("_Model",.)) %>% 
  paste(collapse=" ") %>%
  paste("pdftk",.,"cat output graphs/Models_Treatments_20150120.pdf") %>%
  system

# remove single files
list.files("graphs",full.names = TRUE) %>% 
  "["(grep("_Model",.)) %>% 
  paste(collapse=" ") %>%
  paste("rm",. , collapse=" ") %>%
  system
