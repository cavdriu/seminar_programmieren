######################
### database query ###
######################
# source: https://db.rstudio.com/databases/sqlite/
# source: https://db.rstudio.com/r-packages/dplyr/



# setup -------------------------------------------------------------------

library(RSQLite)
library(DBI)
library(here)


# select date -------------------------------------------------------------

### connect to db
db <- dbConnect(drv = SQLite(), dbname = here("data", "social_security.db"))
dbListTables(db)

## readTable into R
socsec <- dbReadTable(db, "socsec")
socsec_dedu <- dbReadTable(db, "socsec_deduction")
socsec_rate <- dbReadTable(db, "socsec_rate")
socsec_remain <- dbReadTable(db, "socsec_remain")

# res <- dbSendQuery(db, "SELECT * FROM socsec WHERE VALUE < 100")
# res <- dbFetch(res)
# dbClearResult(res)

### disconnect
dbDisconnect(db)
