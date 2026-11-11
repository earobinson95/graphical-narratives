library(RSQLite)
library(DBI)
library(tidyverse)

# Connect to Database ----------------------------------------------------------
filename      <- here::here("study_data.db")
sqlite.driver <- dbDriver("SQLite")
db_con        <- dbConnect(sqlite.driver, dbname = filename)

dbListTables(db_con)

# Users ------------------------------------------------------------------------
users <- dbReadTable(db_con,"users")

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
