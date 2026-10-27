# Clean raw station attributes into an analysis-ready table.
#
# Issues handled (all observed in the source data):
#   - The text "NA" is used instead of a true null.
#   - Some category values carry trailing spaces (e.g. "James River ").
#   - Sample dates and counts are stored as text.
#   - SURVEY_PROGS packs several programs into one field, separated by "<br>".

#' Turn "NA" / "" placeholders into real missing values and trim whitespace
normalize_text <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "null")] <- NA_character_
  x
}

#' Parse ArcGIS ISO timestamps such as "2026-03-23T17:00:00Z" to Date
parse_iso_date <- function(x) {
  x <- normalize_text(x)
  as.Date(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}

#' Split the "<br>"-separated program field into a list of program names
split_programs <- function(x) {
  lapply(strsplit(ifelse(is.na(x), "", x), "<br>", fixed = TRUE), function(p) {
    p <- trimws(p)
    p[p != "" & p != "NA"]
  })
}

#' Clean the raw attribute table
#'
#' @param raw Data frame returned by fetch_stations().
#' @return A data frame with one row per station and tidy columns.
clean_stations <- function(raw) {
  required <- c("STATION_ID", "LATITUDE", "LONGITUDE", "DEQ_Region", "Basin",
                "SURVEY_PROGS", "FIRST_SAMPLE_DATE", "LAST_SAMPLE_DATE", "SAMPLE_COUNT")
  missing_cols <- setdiff(required, names(raw))
  if (length(missing_cols) > 0) {
    stop("Source is missing expected columns: ", paste(missing_cols, collapse = ", "))
  }

  programs <- split_programs(raw$SURVEY_PROGS)

  data.frame(
    station_id   = normalize_text(raw$STATION_ID),
    latitude     = as.numeric(raw$LATITUDE),
    longitude    = as.numeric(raw$LONGITUDE),
    region       = normalize_text(raw$DEQ_Region),
    basin        = normalize_text(raw$Basin),
    subbasin     = normalize_text(raw$Subbasin %||% NA),
    county       = normalize_text(raw$COUNTY %||% NA),
    stream_name  = normalize_text(raw$STREAM_NAME %||% NA),
    description  = normalize_text(raw$STATION_DESCRIPTION %||% NA),
    programs     = vapply(programs, paste, character(1), collapse = "; "),
    n_programs   = lengths(programs),
    first_sample = parse_iso_date(raw$FIRST_SAMPLE_DATE),
    last_sample  = parse_iso_date(raw$LAST_SAMPLE_DATE),
    sample_count = suppressWarnings(as.integer(normalize_text(raw$SAMPLE_COUNT))),
    stringsAsFactors = FALSE
  )
}

#' One row per station-program pair, for filtering by program
station_programs <- function(stations) {
  progs <- strsplit(stations$programs, "; ", fixed = TRUE)
  data.frame(
    station_id = rep(stations$station_id, lengths(progs)),
    program    = unlist(progs),
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

