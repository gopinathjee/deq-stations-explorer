# Data quality checks run on every refresh.
#
# Each check returns a row: what was checked, how many records it flagged,
# and whether that is a failure (blocks the refresh), a warning (shown to
# users), or informational.

# Approximate bounding box for Virginia, with a small buffer.
VA_BBOX <- list(lat = c(36.5, 39.5), lon = c(-83.7, -75.2))

check_row <- function(check, flagged, severity, detail) {
  status <- if (flagged == 0) "Pass" else severity
  data.frame(check = check, flagged = as.integer(flagged), status = status,
             detail = detail, stringsAsFactors = FALSE)
}

#' Run all checks
#'
#' @param raw The raw attributes (before cleaning), to detect source issues.
#' @param stations The cleaned table.
#' @param server_count Record count reported by the service, or NA if unknown.
validate_stations <- function(raw, stations, server_count = NA_integer_) {
  text_cols <- names(raw)[vapply(raw, is.character, logical(1))]
  trailing <- sum(vapply(text_cols, function(col) {
    v <- raw[[col]]
    sum(!is.na(v) & v != trimws(v))
  }, numeric(1)))
  na_text <- sum(vapply(text_cols, function(col) sum(raw[[col]] %in% "NA"), numeric(1)))

  outside_va <- with(stations,
    !is.na(latitude) & !is.na(longitude) &
    (latitude < VA_BBOX$lat[1] | latitude > VA_BBOX$lat[2] |
     longitude < VA_BBOX$lon[1] | longitude > VA_BBOX$lon[2]))

  rows <- list(
    check_row("Record count matches the service",
              if (is.na(server_count)) 0 else abs(nrow(stations) - server_count),
              "Fail",
              if (is.na(server_count)) "Server count not checked"
              else sprintf("Pulled %d of %d records", nrow(stations), server_count)),
    check_row("Station ID present and unique",
              sum(is.na(stations$station_id)) + sum(duplicated(stations$station_id, incomparables = NA)),
              "Fail", "Missing or duplicated STATION_ID"),
    check_row("Coordinates present",
              sum(is.na(stations$latitude) | is.na(stations$longitude)),
              "Warning", "Stations that cannot be mapped"),
    check_row("Coordinates inside Virginia",
              sum(outside_va), "Warning", "Points outside a Virginia bounding box"),
    check_row("Region assigned",
              sum(is.na(stations$region)), "Warning", "Missing DEQ_Region"),
    check_row("Basin assigned",
              sum(is.na(stations$basin)), "Warning", "Missing or 'NA' Basin"),
    check_row("Sample history available",
              sum(is.na(stations$sample_count)), "Info",
              "No prior samples recorded (likely new stations)"),
    check_row("First sample on or before last sample",
              sum(!is.na(stations$first_sample) & !is.na(stations$last_sample) &
                    stations$first_sample > stations$last_sample),
              "Warning", "Date order reversed"),
    check_row("Text values with trailing spaces (fixed in cleaning)",
              trailing, "Info", "Trimmed so filters group correctly"),
    check_row("'NA' text used instead of nulls (fixed in cleaning)",
              na_text, "Info", "Converted to true missing values")
  )
  do.call(rbind, rows)
}

#' TRUE if any check failed (used to stop a scheduled refresh)
has_failures <- function(report) any(report$status == "Fail")
