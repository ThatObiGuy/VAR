# Database utilities (pure helpers; no Shiny-specific code)

# Establish a Postgres connection using environment variables
connect_db <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("PGHOST"),
    port = 5432,
    dbname = Sys.getenv("PGDATABASE"),
    user = Sys.getenv("PGUSER"),
    password = Sys.getenv("PGPASSWORD"),
    sslmode = "require",
    channel_binding = "prefer"
  )
}

# Safely disconnect a connection
disconnect_db <- function(con) {
  if (!is.null(con)) {
    try(DBI::dbDisconnect(con), silent = TRUE)
  }
}
