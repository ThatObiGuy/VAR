# Exploratory script: entropy distribution by league with interactive violin + jitter
library(DBI)
library(RPostgres)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(plotly)

# Database connection
con <- dbConnect(
  Postgres(),
  host = Sys.getenv("PGHOST"),
  port = 5432,
  dbname = Sys.getenv("PGDATABASE"),
  user = Sys.getenv("PGUSER"),
  password = Sys.getenv("PGPASSWORD")
)

# Lazy table references
odds <- tbl(con, "odds1x2")
results <- tbl(con, "results1")
entropy <- tbl(con, "entropy")

# Joining entropy with results
match_entropy <- entropy_tbl |>
  select(event_id, entropy) |>
  inner_join(
    results |>
      select(event_id, league_name, starts, home_team, away_team, result),
    by = "event_id"
  ) |>
  # Select columns needed for plotting and tooltips
  select(event_id, league_name, starts, home_team, away_team, result, entropy) |>
  # Bring to memory for ggplot/plotly
  collect()

# Ensure starts is POSIXct (dbplyr may already coerce but enforce for consistency)
match_entropy <- match_entropy |>
  mutate(starts = as.POSIXct(starts, tz = "UTC"))

### PLOT: VIOLIN BY LEAGUE WITH JITTERED POINTS AND QUARTILE LINES ###
# Comparing uncertainty of the different leagues
match_entropy <- match_entropy |>
  mutate(
    tooltip_text = paste0(
      league_name, "\n",
      format(starts, "%Y-%m-%d %H:%M UTC"), "\n",
      home_team, " vs ", away_team, "\n",
      "Outcome: ", result, "\n",
      "Entropy: ", round(entropy, 3), " bits\n",
      "event_id: ", event_id
    )
  )

p_violin <- ggplot(match_entropy, aes(x = league_name, y = entropy)) +
  geom_violin(fill = "steelblue", alpha = 0.4, color = "grey30",
              draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(aes(text = tooltip_text), width = 0.15, height = 0,
              size = 1.5, alpha = 0.5, color = "#2c3e50") +
  theme_minimal() +
  labs(
    title = "Entropy distribution by league",
    x = "League",
    y = "Shannon entropy (bits)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_violinly <- ggplotly(p_violin, tooltip = "text")

print(p_violinly)

try(dbDisconnect(con), silent = TRUE)
