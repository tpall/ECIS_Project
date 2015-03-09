# # Preprocessing script.
# library(RSQLite)
# db <- dbConnect(SQLite(), dbname="data/ECIS.sqlite") # open connection
# data <- dbGetQuery(db, 'SELECT *
#                        FROM Data
#                         INNER JOIN Metadata
#                         using(Date,Well)
#                         WHERE Mark = "Release" AND GF="VEGF" ')
# dbDisconnect(db)            # Close connection