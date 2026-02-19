# Data loading utility for csv's

# Read all CSVs from a directory into a named list of tibbles
# - path: directory containing .csv files
# - na: values to treat as NA
# - guess_max: rows to use for type guessing (readr) / vroom uses own heuristics
load_csv_dir <- function(path = "data", na = c("", "NA", "N/A")) {
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  
  tbls <- lapply(files, function(f) {
    readr::read_csv(
      file = f,
      na = na,
      show_col_types = FALSE,
      progress = FALSE
    ) |>
      tibble::as_tibble()
  })
  names(tbls) <- tools::file_path_sans_ext(basename(files))
  tbls
}
