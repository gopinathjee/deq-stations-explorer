# Fetch DEQ's 2026 Monitoring Plan Stations from the public ArcGIS REST service.
#
# Layer: "2026 Monitoring Plan Stations" (EDMA MapServer, layer 131)
# The service returns at most 1,000 records per request, so we page with
# resultOffset until a page comes back short.

STATIONS_LAYER_URL <- paste0(
  "https://gisdata.deq.virginia.gov/arcgis/rest/services/public/",
  "EDMA/MapServer/131/query"
)

#' Build a query URL for one page of records
build_page_url <- function(base_url, offset, page_size) {
  params <- c(
    where = "1=1",
    outFields = "*",
    returnGeometry = "false",
    orderByFields = "OBJECTID",
    resultOffset = offset,
    resultRecordCount = page_size,
    f = "json"
  )
  paste0(base_url, "?", paste(names(params),
                              vapply(params, utils::URLencode, character(1), reserved = TRUE),
                              sep = "=", collapse = "&"))
}

#' Ask the service how many records it holds (used to validate the pull)
fetch_server_count <- function(base_url = STATIONS_LAYER_URL,
                               get_json = function(u) jsonlite::fromJSON(u)) {
  url <- paste0(base_url, "?where=1%3D1&returnCountOnly=true&f=json")
  res <- get_json(url)
  if (!is.null(res$error)) stop("ArcGIS error: ", res$error$message)
  as.integer(res$count)
}

#' Fetch all station records as a data frame of raw attributes
#'
#' @param get_json Function that takes a URL and returns parsed JSON.
#'   Injected so tests can run without network access.
fetch_stations <- function(base_url = STATIONS_LAYER_URL, page_size = 1000,
                           get_json = function(u) jsonlite::fromJSON(u),
                           max_pages = 50) {
  pages <- list()
  offset <- 0
  for (i in seq_len(max_pages)) {
    res <- get_json(build_page_url(base_url, offset, page_size))
    if (!is.null(res$error)) stop("ArcGIS error: ", res$error$message)
    attrs <- res$features$attributes
    n <- if (is.null(attrs)) 0 else nrow(attrs)
    if (n > 0) pages[[length(pages) + 1]] <- attrs
    more <- isTRUE(res$exceededTransferLimit) || n == page_size
    if (n == 0 || !more) break
    offset <- offset + n
  }
  if (length(pages) == 0) stop("The service returned no records.")
  out <- do.call(rbind, pages)
  rownames(out) <- NULL
  out
}

