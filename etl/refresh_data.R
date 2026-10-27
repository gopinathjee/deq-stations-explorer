# Refresh job: fetch -> clean -> validate -> save.
#
# Run from the project folder:   Rscript etl/refresh_data.R
# On Posit Connect this can be deployed as a scheduled script or Quarto
# document. It exits with an error (and leaves the previous snapshot in
# place) if a validation check fails, so the app never shows a bad pull.

source("R/fetch.R")
source("R/clean.R")
source("R/validate.R")

dir.create("data", showWarnings = FALSE)

message("Fetching stations from DEQ ArcGIS service...")
raw          <- fetch_stations()
server_count <- fetch_server_count()
stations     <- clean_stations(raw)
report       <- validate_stations(raw, stations, server_count)

print(report[, c("check", "flagged", "status")], row.names = FALSE)

if (has_failures(report)) {
  stop("Validation failed; previous snapshot kept. See checks above.")
}

snapshot <- list(
  stations   = stations,
  validation = report,
  refreshed  = Sys.time(),
  source     = STATIONS_LAYER_URL
)
tmp <- tempfile(fileext = ".rds", tmpdir = "data")
saveRDS(snapshot, tmp)
file.rename(tmp, "data/stations_snapshot.rds")   # atomic swap
message(sprintf("Saved %d stations to data/stations_snapshot.rds", nrow(stations)))

# CSV exports for the Tableau version (same cleaned data as the app).
utils::write.csv(stations, "data/stations_clean.csv", row.names = FALSE, na = "")
utils::write.csv(station_programs(stations), "data/station_programs.csv", row.names = FALSE)
utils::write.csv(report, "data/validation_report.csv", row.names = FALSE)
message("Wrote CSV exports for Tableau to data/")
