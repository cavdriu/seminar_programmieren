################
### database ###
################
# source: https://db.rstudio.com/databases/sqlite/
# source: https://db.rstudio.com/r-packages/dplyr/


#########
# setup #
#########
library(RSQLite)
library(DBI)
library(here)

#######################
# prepare data for db #
#######################
socsec <- read.csv2(here("data", "ts-x-13.06.02.01.csv"))
socsec_remain <- read.csv2(here("data", "ts-x-13.06.02.04.csv"))
socsec_rate <- read.csv2(here("data", "ts-x-13.06.02.02.csv"))
socsec_deduction <- read.csv2(here("data", "ts-x-13.06.02.03.csv"))

### normalize data for db


##################################
# connect to db and write tables #
##################################
db <- dbConnect(drv = SQLite(), dbname = here("data", "social_security.db"))

dbWriteTable(conn = db, name = "socsec", value = socsec)
dbWriteTable(conn = db, name = "socsec_remain", value = socsec)
dbWriteTable(conn = db, name = "socsec_rate", value = socsec)
dbWriteTable(conn = db, name = "socsec_deduction", value = socsec)

### check db
dbListTables(db)
dbListFields(db, "socsec")

### disconnect
dbDisconnect(db)




