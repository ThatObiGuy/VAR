# Shared resources
# Runs once per R session before ui/server

# Libraries
library(shiny)
library(shinythemes) # Adding themeing package, quick way to make app more appealing with minimal effort
library(DBI)  # For DB interactions
library(RPostgres)  # For DB interactions
library(dbplyr) # To allow us to use the language of dplyr rather than SQL on our tables
library(dplyr) # any server side adjustments
library(DT) # DataTable used to allow users to select matches
library(ggplot2) # Familiar graphing tool
library(plotly) # Interactive plots for Teams view
library(scales)  # Color palettes and scaling utilities

# -------------------------------------------------------------------
# Preflight diagnostics: environment + DNS + DB connectivity (non-fatal)
# Runs once on session start (global.R). Safe to keep even in production,
# but you may remove PGPASSWORD logging after debugging.
# -------------------------------------------------------------------
preflight_diagnostics <- function() {
  safe_run <- function(expr, context) {
    tryCatch(expr, error = function(e) {
      message(sprintf("[preflight] %s ERROR: %s", context, conditionMessage(e)))
      invisible(NULL)
    })
  }
  
  cat("\n===== Preflight diagnostics (startup) =====\n")
  
  # 1) Runtime info
  safe_run({
    ri <- list(
      time_utc = format(Sys.time(), tz = "UTC"),
      sysname = unname(Sys.info()["sysname"]),
      release = unname(Sys.info()["release"]),
      machine = unname(Sys.info()["machine"]),
      user = unname(Sys.info()["user"]),
      r_version = R.version.string,
      shiny = as.character(utils::packageVersion("shiny")),
      DBI = as.character(utils::packageVersion("DBI")),
      RPostgres = as.character(utils::packageVersion("RPostgres"))
    )
    cat("[preflight] Runtime info:\n")
    for (nm in names(ri)) cat(sprintf("  - %s: %s\n", nm, ri[[nm]]))
  }, "runtime info")
  
  # 2) Print DB-related environment variables (including password as requested)
  safe_run({
    envs <- c("PGHOST", "PGPORT", "PGDATABASE", "PGUSER", "PGPASSWORD", "PGSSLMODE")
    cat("[preflight] Environment variables (raw):\n")
    for (k in envs) {
      v <- Sys.getenv(k, unset = "")
      cat(sprintf("  - %s=%s\n", k, v))
    }
  }, "env vars")
  
  # Helper to run a command if present on PATH
  run_cmd <- function(cmd, args) {
    bin <- unname(Sys.which(cmd))
    if (!nzchar(bin)) {
      cat(sprintf("[preflight] '%s' not found on PATH\n", cmd))
      return(invisible(NULL))
    }
    cat(sprintf("[preflight] Running: %s %s\n", cmd, paste(args, collapse = " ")))
    out <- tryCatch(system2(bin, args = args, stdout = TRUE, stderr = TRUE),
                    error = function(e) sprintf("ERROR: %s", conditionMessage(e)))
    if (length(out)) cat(paste0("    ", out, collapse = "\n"), "\n")
    invisible(out)
  }
  
  # 3) DNS resolution attempts for PGHOST
  safe_run({
    host <- Sys.getenv("PGHOST", unset = "")
    if (!nzchar(host)) {
      cat("[preflight] PGHOST is empty; skipping DNS checks.\n")
    } else {
      cat(sprintf("[preflight] DNS checks for host: %s\n", host))
      run_cmd("getent", c("hosts", host))
      run_cmd("host", c(host))
      run_cmd("nslookup", c(host))
    }
  }, "dns checks")
  
  # 4) DB connectivity probe (non-fatal)
  safe_run({
    host <- Sys.getenv("PGHOST"); db <- Sys.getenv("PGDATABASE"); usr <- Sys.getenv("PGUSER"); pwd <- Sys.getenv("PGPASSWORD")
    port <- Sys.getenv("PGPORT", unset = "5432"); ssl <- Sys.getenv("PGSSLMODE", unset = "require")
    port_int <- suppressWarnings(as.integer(port)); if (is.na(port_int)) port_int <- 5432L
    
    cat("[preflight] DBI::dbCanConnect() probe... ")
    can_result <- tryCatch(DBI::dbCanConnect(
      RPostgres::Postgres(),
      host = host, port = port_int, dbname = db, user = usr, password = pwd,
      sslmode = ssl, channel_binding = "prefer"
    ), error = function(e) e)
    
    if (inherits(can_result, "error")) {
      cat("ERROR\n")
      message(sprintf("[preflight] dbCanConnect error: %s", conditionMessage(can_result)))
      need_connect_try <- TRUE
    } else {
      cat(sprintf("%s\n", as.character(can_result)))
      need_connect_try <- isFALSE(can_result)
    }
    
    if (need_connect_try) {
      cat("[preflight] Trying DBI::dbConnect() to capture error...\n")
      con <- NULL
      err <- tryCatch({
        con <<- DBI::dbConnect(
          RPostgres::Postgres(),
          host = host, port = port_int, dbname = db, user = usr, password = pwd,
          sslmode = ssl, channel_binding = "prefer"
        )
        NULL
      }, error = function(e) e)
      
      if (is.null(err)) {
        cat("[preflight] dbConnect succeeded (unexpected if dbCanConnect was FALSE). Disconnecting.\n")
        try(DBI::dbDisconnect(con), silent = TRUE)
      } else {
        message(sprintf("[preflight] dbConnect error: %s", conditionMessage(err)))
      }
    }
  }, "db probe")
  
  cat("===== End preflight diagnostics =====\n\n")
}

# Invoke preflight (non-fatal). Runs once when global.R is sourced.
try(preflight_diagnostics(), silent = TRUE)
