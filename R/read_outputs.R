#' Read an AquaCrop Season Output File
#'
#' Parses an AquaCrop \code{PRMSeason.OUT} file into one or two tidy data frames.
#' Seasonal totals (rows starting with \code{Tot}) are always returned.
#' Intermediate rows (\code{10Day} or \code{Month}) are optionally returned
#' as a second data frame.
#'
#' @param file Character. Path to the \code{PRMSeason.OUT} file.
#' @param intermediate Logical. If \code{TRUE}, returns a named list with two
#'   elements: \code{season} (seasonal totals) and \code{intermediate}
#'   (decadal or monthly rows). If \code{FALSE} (default), returns only the
#'   seasonal totals data frame.
#'
#' @return When \code{intermediate = FALSE}: a \code{data.frame} of seasonal
#'   totals. When \code{intermediate = TRUE}: a named list with:
#'   \describe{
#'     \item{season}{data.frame of seasonal totals (\code{Tot} rows).}
#'     \item{intermediate}{data.frame of decadal or monthly rows
#'       (\code{10Day} or \code{Month} rows), or \code{NULL} if none found.}
#'   }
#'
#' @details
#' Column names are derived from the header line. The \code{RunNr} column
#' is parsed as integer. The \code{prm_file} column contains the station/PRM
#' file name from the last field of each data row. Columns \code{E/Ex},
#' \code{Tr/Trx}, \code{Y(dry)}, \code{Y(fresh)} and \code{Brelative} are
#' renamed to valid R names:
#' \code{E_Ex}, \code{Tr_Trx}, \code{Y_dry}, \code{Y_fresh},
#' \code{Brelative}.
#'
#' @examples
#' \dontrun{
#' # Seasonal totals only
#' season <- read_season_out("LIST/C1PRMSeason.OUT")
#'
#' # With decadal or monthly intermediates
#' out <- read_season_out("LIST/C1PRMSeason.OUT", intermediate = TRUE)
#' out$season
#' out$intermediate
#' }
#'
#' @family AquaCrop readers
#' @export
read_season_out <- function(file, intermediate = FALSE) {
  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }
  if (!grepl("PRMSeason\\.OUT$", basename(file), ignore.case = TRUE)) {
    warning(
      "File does not appear to be a PRMSeason.OUT file: ",
      basename(file),
      call. = FALSE
    )
  }

  lines <- readLines(file, warn = FALSE)

  # ---- Locate header line (contains RunNr) ----------------------------------
  hdr_idx <- grep("RunNr", lines)[1L]
  if (is.na(hdr_idx)) {
    stop("Cannot find header line in: ", file, call. = FALSE)
  }

  # ---- Parse column names from header --------------------------------------
  raw_cols <- strsplit(trimws(lines[hdr_idx]), "\\s+")[[1L]]
  # Append prm_file for the last unnamed column
  col_names <- c(raw_cols, "prm_file")
  # Sanitise problematic names
  col_names <- gsub("E/Ex", "E_Ex", col_names, fixed = TRUE)
  col_names <- gsub("Tr/Trx", "Tr_Trx", col_names, fixed = TRUE)
  col_names <- gsub("Y(dry)", "Y_dry", col_names, fixed = TRUE)
  col_names <- gsub("Y(fresh)", "Y_fresh", col_names, fixed = TRUE)
  col_names <- gsub("Brelative", "Brelative", col_names, fixed = TRUE)

  # ---- Identify data lines --------------------------------------------------
  # Data lines start with Tot(N), 10Day(N), or Month(N)
  tot_idx <- grep("^\\s*Tot\\(", lines)
  day_idx <- grep("^\\s*Day", lines)
  tenday_idx <- grep("^\\s*10Day", lines)
  month_idx <- grep("^\\s*Month", lines)
  inter_idx <- sort(c(day_idx, tenday_idx, month_idx))

  # ---- Parser: fixed-width split then prm_file -----------------------------
  .parse_lines <- function(idx) {
    if (length(idx) == 0L) {
      return(NULL)
    }

    rows <- lapply(idx, function(i) {
      ln <- lines[i]
      # Split on whitespace; last token is prm_file (filename with no spaces)
      toks <- strsplit(trimws(ln), "\\s+")[[1L]]
      # Expect length(col_names) tokens; pad or trim if needed
      n_expected <- length(col_names)
      if (length(toks) < n_expected) {
        toks <- c(toks, rep(NA_character_, n_expected - length(toks)))
      } else if (length(toks) > n_expected) {
        # Extra tokens: collapse excess into prm_file
        extra <- paste(toks[n_expected:length(toks)], collapse = " ")
        toks <- c(toks[seq_len(n_expected - 1L)], extra)
      }
      toks
    })

    df <- as.data.frame(
      do.call(rbind, rows),
      stringsAsFactors = FALSE
    )
    names(df) <- col_names

    # ---- Type conversions ---------------------------------------------------
    # RunNr: strip parentheses, coerce to integer
    df$RunNr <- as.integer(gsub("[^0-9]", "", df$RunNr))

    # All numeric columns except RunNr and prm_file
    num_cols <- setdiff(col_names, c("RunNr", "prm_file"))
    df[num_cols] <- lapply(df[num_cols], function(x) {
      suppressWarnings(as.numeric(x))
    })

    tibble::as_tibble(df)
  }

  season_df <- .parse_lines(tot_idx)
  inter_df <- if (length(inter_idx) > 0L) .parse_lines(inter_idx) else NULL

  season_df <- tibble::as_tibble(season_df[, 2:length(season_df)])

  inter_df <- tibble::as_tibble(inter_df[, 2:length(inter_df)])

  if (!intermediate) {
    return(season_df)
  }

  if (is.null(inter_df)) {
    warning(
      "intermediate = TRUE but no decadal (10Day) or monthly (Month) rows ",
      "found in: ",
      file,
      " -- returning NULL for $intermediate.",
      call. = FALSE
    )
  }

  list(season = season_df, intermediate = inter_df)
}
