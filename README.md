# 2026 Monitoring Plan Stations Explorer (R Shiny)

An interactive explorer for Virginia DEQ's 2026 Water Quality Monitoring Plan stations: where they are, which programs sample them, and which have no sample history yet. Built as a work sample by Gopinath Jeereddy, using only public data.

**Data:** DEQ public ArcGIS REST service, layer *2026 Monitoring Plan Stations*
(`gisdata.deq.virginia.gov/arcgis/rest/services/public/EDMA/MapServer/131`), 1,351 stations at the time of writing.

## What it does

- Filter by DEQ region, basin, monitoring program, and "no sample history yet."
- Summary counts: stations, stations in more than one program, stations with no prior samples.
- Map of stations colored by region, with popups (Leaflet).
- Stations per basin chart, searchable station table, CSV download.
- A **Data quality** tab showing the checks run on the last refresh.

## How it's built

```
etl/refresh_data.R     fetch -> clean -> validate -> save snapshot (run on a schedule)
R/fetch.R              pages through the ArcGIS REST service (1,000 records per request)
R/clean.R              fixes source issues: "NA" text, trailing spaces, text dates, <br>-packed programs
R/validate.R           10 data quality checks; a failed check stops the refresh
app.R                  Shiny UI and server; reads the validated snapshot only
tests/testthat/        23 unit tests on fetch, clean and validate (no network needed)
```

The app never queries the service directly. A refresh job writes a validated snapshot, and the app reads it. If a check fails (for example, a short pull or duplicate station IDs), the job stops and the app keeps the last good snapshot.

## Run it

```r
install.packages(c("shiny", "bslib", "DT", "ggplot2", "leaflet", "jsonlite", "testthat"))
source("etl/refresh_data.R")   # first run: pulls the data (about 1,351 rows)
shiny::runApp()
```

Run the tests:

```r
testthat::test_dir("tests/testthat")
```

## Deploying to Posit Connect

1. Publish the app with `rsconnect::deployApp()` or git-backed publishing (`rsconnect::writeManifest()`).
2. Publish `etl/refresh_data.R` (or a Quarto wrapper) as a scheduled job that writes the snapshot to a shared location (a `pins` board or database table) the app reads.
3. Set an email alert on job failure.

## Known limits

- The service stores dates and counts as text and uses the text "NA" for missing values; `clean.R` handles these, and the Data quality tab reports how many were fixed.
- The Virginia bounding-box check is approximate.
- Data is for reference only; DEQ does not guarantee its accuracy.
