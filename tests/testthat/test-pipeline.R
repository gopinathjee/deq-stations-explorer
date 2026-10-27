# Fixture: the first three rows are real records from the DEQ service;
# rows TEST-MULTI and TEST-BAD are synthetic edge cases for the checks.

root <- file.path("..", "..")
source(file.path(root, "R", "fetch.R"))
source(file.path(root, "R", "clean.R"))
source(file.path(root, "R", "validate.R"))

fixture <- jsonlite::fromJSON(test_path("fixtures", "page.json"))
raw <- fixture$features$attributes
stations <- clean_stations(raw)

# ---- fetch -----------------------------------------------------------------
test_that("fetch_stations pages until a short page is returned", {
  calls <- character()
  fake <- function(url) {
    calls <<- c(calls, url)
    offset <- as.integer(sub(".*resultOffset=([0-9]+).*", "\\1", url))
    rows <- if (offset < nrow(raw)) raw[(offset + 1):min(offset + 2, nrow(raw)), ] else raw[0, ]
    list(features = list(attributes = rows), exceededTransferLimit = offset + 2 < nrow(raw))
  }
  out <- fetch_stations(base_url = "https://example/query", page_size = 2, get_json = fake)
  expect_equal(nrow(out), nrow(raw))
  expect_length(calls, 3)                       # offsets 0, 2, 4
  expect_match(calls[2], "resultOffset=2")
})

test_that("fetch_stations surfaces service errors", {
  fake <- function(url) list(error = list(message = "Invalid query"))
  expect_error(fetch_stations(get_json = fake), "Invalid query")
})

# ---- clean -----------------------------------------------------------------
test_that("'NA' text becomes a real missing value and spaces are trimmed", {
  expect_true(is.na(stations$stream_name[1]))
  expect_equal(stations$basin[1], "Ches. Bay and Small Coastal")
  expect_true(is.na(stations$basin[5]))
})

test_that("dates and counts are parsed from text", {
  expect_equal(stations$first_sample[3], as.Date("2026-03-23"))
  expect_identical(stations$sample_count[3], 1L)
  expect_true(is.na(stations$sample_count[1]))
})

test_that("multi-program field is split on <br>", {
  expect_equal(stations$programs[4], "Ambient; Fish Tissue")
  expect_equal(stations$n_programs[4], 2L)
  expect_equal(stations$n_programs[1], 1L)
  long <- station_programs(stations)
  expect_equal(nrow(long), sum(stations$n_programs))
})

test_that("clean_stations stops if expected columns disappear", {
  expect_error(clean_stations(raw[, setdiff(names(raw), "Basin")]), "Basin")
})

# ---- validate --------------------------------------------------------------
report <- validate_stations(raw, stations, server_count = nrow(raw))
flag <- function(check) report$flagged[report$check == check]

test_that("checks flag the synthetic bad row", {
  expect_equal(flag("Coordinates inside Virginia"), 1)
  expect_equal(flag("Basin assigned"), 1)
  expect_equal(flag("Region assigned"), 1)
  expect_equal(flag("First sample on or before last sample"), 1)
  expect_equal(flag("Sample history available"), 2)
  expect_false(has_failures(report))
})

test_that("a short pull or duplicate IDs fail the refresh", {
  expect_true(has_failures(validate_stations(raw, stations, server_count = 1351L)))
  dup <- rbind(stations, stations[1, ])
  expect_true(has_failures(validate_stations(raw, dup, server_count = nrow(dup))))
})
