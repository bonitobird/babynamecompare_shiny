
## Run from Powershell
## & 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe' .\update_babynames.R

build_babynames <- function(output_file = file.path("data", "babynames.csv")) {
  archive_url <- paste0(
    "https://codeload.github.com/bonitobird/babynamecompare_shiny/",
    "zip/refs/heads/main"
  )
  work_dir <- tempfile("babynames-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  old_timeout <- getOption("timeout")
  options(timeout = max(300, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  archive_file <- file.path(work_dir, "repository.zip")
  message("Downloading repository snapshot...")
  download.file(archive_url, archive_file, mode = "wb")
  entries <- unzip(archive_file, list = TRUE)$Name
  yearly_files <- sort(entries[grepl(
    "^[^/]+/raw_data/names/yob[0-9]{4}\\.txt$", entries
  )])
  if (!length(yearly_files)) {
    stop("No raw_data/names/yobYYYY.txt files found in the repository.")
  }

  unzip(archive_file, files = yearly_files, exdir = work_dir)
  yearly_data <- lapply(yearly_files, function(relative_path) {
    filename <- basename(relative_path)
    year <- as.integer(sub("^yob([0-9]{4})\\.txt$", "\\1", filename))
    rows <- read.csv(
      file.path(work_dir, relative_path), header = FALSE,
      col.names = c("name", "sex", "n"),
      colClasses = c("character", "character", "integer"),
      stringsAsFactors = FALSE, na.strings = "", fill = FALSE,
      row.names = NULL, fileEncoding = "UTF-8"
    )
    if (ncol(rows) != 3L || !nrow(rows) || anyNA(rows) ||
        any(!nzchar(trimws(rows$name))) ||
        any(!rows$sex %in% c("M", "F")) || any(rows$n < 0L)) {
      stop("Invalid name, sex, or count data in ", filename)
    }
    rows$year <- year
    rows[c("name", "year", "sex", "n")]
  })
  combined <- do.call(rbind, yearly_data)
  if (anyDuplicated(combined[c("name", "year", "sex")])) {
    stop("Duplicate name/year/sex rows found; output was not written.")
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  write.csv(combined, output_file, row.names = FALSE, fileEncoding = "UTF-8")
  message(sprintf(
    "Saved %s rows from %d files (%d-%d) to %s",
    format(nrow(combined), big.mark = ","), length(yearly_files),
    min(combined$year), max(combined$year),
    normalizePath(output_file, winslash = "/", mustWork = TRUE)
  ))
  invisible(combined)
}

if (sys.nframe() == 0L) {
  arguments <- commandArgs(trailingOnly = TRUE)
  if (length(arguments) > 1L) {
    stop("Usage: Rscript update_babynames.R [output.csv]")
  }
  if (length(arguments)) build_babynames(arguments[[1]]) else build_babynames()
}
