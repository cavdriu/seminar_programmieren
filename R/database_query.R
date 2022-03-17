######################
### database query ###
######################


#########
# setup #
#########
library(RSQLite)
library(DBI)
library(dplyr)
library(here)

mtcars_db <- dbReadTable(db, "mtcars")
# You can fetch all results:
res <- dbSendQuery(db, "SELECT * FROM mtcars WHERE cyl = 4")
dbFetch(res)
dbClearResult(res)

# # with dplyr #show_query() / collect()
# mtcars_db2 <- tbl(db, "mtcars")
# mtcars_db2

### disconnect
dbDisconnect(db)
