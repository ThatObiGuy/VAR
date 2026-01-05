# Testing dbplyer

install.packages(c("DBI", "RPostgres", "dbplyr", "dplyr"))

library(DBI)
library(RPostgres)
library(dbplyr)
library(dplyr)

# Secure credential handling (set these via in the .Renviron)
con <- dbConnect(
  Postgres(),
  host = Sys.getenv("PGHOST"),
  port = 5432,
  dbname = Sys.getenv("PGDATABASE"),
  user = Sys.getenv("PGUSER"),
  password = Sys.getenv("PGPASSWORD")
)

# Use connection to refer to a table, use it like it's loaded in r
odds <- tbl(con, "odds1x2")

opening_odds <- odds %>%
  group_by(event_id) %>%
  slice_min(logged_time, n = 1) %>%
  select(event_id, home_odds, draw_odds, away_odds)

opening_odds  # Prints preview
show_query(opening_odds)  # Shows SQL
