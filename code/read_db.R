library(RSQLite)
library(DBI)
library(tidyverse)

filename      <- here::here("study_data.db")
sqlite.driver <- dbDriver("SQLite")
db_con        <- dbConnect(sqlite.driver, dbname = filename)

dbListTables(db_con)

click_events <- dbReadTable(db_con,"click_events")
demographics <- dbReadTable(db_con,"demographics")
responses <- dbReadTable(db_con,"responses")
scroll_events <- dbReadTable(db_con,"scroll_events")

DBI::dbExecute(db_con, "DROP TABLE IF EXISTS click_events")
DBI::dbExecute(db_con, "DROP TABLE IF EXISTS demographics")
DBI::dbExecute(db_con, "DROP TABLE IF EXISTS responses")
DBI::dbExecute(db_con, "DROP TABLE IF EXISTS scroll_events")












# Connect to Database ----------------------------------------------------------
filename      <- here::here("study_data.db")
sqlite.driver <- dbDriver("SQLite")
db_con        <- dbConnect(sqlite.driver, dbname = filename)

# Users ------------------------------------------------------------------------
users <- dbReadTable(db_con,"users")
scroll_events <- dbReadTable(db_con,"scroll_events")
covariates <- dbReadTable(db_con,"covariates")

# setup original data
users <- tibble("prolific_id"       = NA,
                "nick_name"         = NA,
                "study_start_time"  = NA,
                "age"               = NA,
                "gender"            = NA,
                "academic_study"    = NA,
                "stem_related"      = NA,
                "recruited"         = NA
)

# clear participant information
users <- users[0,]

# replace in db
dbRemoveTable(db_con, "users")
dbWriteTable(db_con, "users", users, overwrite = T)

# double check
users <- dbReadTable(db_con,"users")
users

# Disconnect from Database -----------------------------------------------------
dbDisconnect(db_con)
