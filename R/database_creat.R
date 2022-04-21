################
### database ###
################
# source: https://db.rstudio.com/databases/sqlite/
# source: https://db.rstudio.com/r-packages/dplyr/



# setup -------------------------------------------------------------------

library(RSQLite)
library(DBI)
library(here)
library(dplyr)


# prepare data for db -----------------------------------------------------

# https://www.bfs.admin.ch/asset/de/ts-x-13.06.02.01
socsec <- read.csv2(here("data", "ts-x-13.06.02.01.csv"))
# https://www.bfs.admin.ch/asset/de/ts-x-13.06.02.02
socsec_rate <- read.csv2(here("data", "ts-x-13.06.02.02.csv"))
# https://www.bfs.admin.ch/asset/de/ts-x-13.06.02.03
socsec_dedu <- read.csv2(here("data", "ts-x-13.06.02.03.csv"))
# https://www.bfs.admin.ch/asset/de/ts-x-13.06.02.04
socsec_remain <- read.csv2(here("data", "ts-x-13.06.02.04.csv"))

### data cleaning for db
glimpse(socsec)

socsec <- socsec %>% 
  rename_with(tolower) %>% 
  rename(year = ï..period, service = component, unit = unit_measure) %>% 
  mutate(unit = if_else(unit == "%", "perc", unit),
         value = round(as.numeric(value), digits = 0),
         service = gsub("_", "+", service))

### socsec_rate
glimpse(socsec_rate)

socsec_rate <- socsec_rate %>% 
  rename_with(tolower) %>% 
  rename(year = ï..period, 
         service = component, 
         pop_group = population_group,
         unit = unit_measure) %>% 
  mutate(unit = if_else(unit == "%", 
                        "perc", 
                        unit),
         value = as.numeric(value))

### socsec_deduction
glimpse(socsec_dedu)

socsec_dedu <- socsec_dedu %>% 
  rename_with(tolower) %>% 
  rename(year = ï..period, 
         service = component, 
         unit = unit_measure, 
         pop_group = population_group) %>% 
  mutate(unit = if_else(unit == "%", 
                        "perc", 
                        unit),
         value = as.numeric(value)) %>% 
  select(!(measure))

# Total reusbringen???

### socsec_remain
glimpse(socsec_remain)

socsec_remain <- socsec_remain %>% 
  rename_with(tolower) %>% 
  rename(year_ref = ï..period_ref, 
         year_obs = period_obs,
         service = component, 
         pop_group = population_group,
         unit = unit_measure) %>% 
  mutate(unit = if_else(unit == "%", 
                      "perc", 
                      unit),
         value = as.numeric(value)) 


# write tables ------------------------------------------

db <- dbConnect(drv = SQLite(), dbname = here("data", "social_security.db"))

dbWriteTable(conn = db, name = "socsec", value = socsec) #overwrite = TRUE
dbWriteTable(conn = db, name = "socsec_remain", value = socsec_remain)
dbWriteTable(conn = db, name = "socsec_rate", value = socsec_rate)
dbWriteTable(conn = db, name = "socsec_dedu", value = socsec_dedu)

### check db
dbListTables(db)
dbListFields(db, "socsec")

### disconnect
dbDisconnect(db)



