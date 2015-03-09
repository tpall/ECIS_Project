library("ProjectTemplate")
load.project()

library(RSQLite)
db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection

dbListTables(db) 
dbListFields(db, "Data") 
dbListFields(db, "Rbmodel") 
dbListFields(db, "Model") # along with "Treatment" and "siRNA" contain experiment metadata 
 
# load all impedance data for 'Model' 
ecis <- dbGetQuery(db, 'SELECT *
                       FROM Data
                       INNER JOIN Model
                       using(Date,Well)
                       WHERE Mark = "Release"')

# load all modeling data for 'Model'
rba <- dbGetQuery(db, 'SELECT *
                       FROM Rbmodel
                       INNER JOIN Model
                       using(Date,Well)')

class(ecis)

dbDisconnect(db)            # Close connection