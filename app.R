# Monitoring Stations Explorer
#
# Explore DEQ's 2026 Water Quality Monitoring Plan stations by region,
# basin and monitoring program, with a built-in data quality report.
#
# Data: Virginia DEQ public ArcGIS service, "2026 Monitoring Plan Stations".
# The app reads a validated snapshot written by etl/refresh_data.R; if no
# snapshot exists yet, it runs the refresh once at startup.

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(leaflet)

source("R/fetch.R")
source("R/clean.R")
source("R/validate.R")

snapshot_path <- "data/stations_snapshot.rds"
if (!file.exists(snapshot_path)) source("etl/refresh_data.R", local = new.env())
snap       <- readRDS(snapshot_path)
stations   <- snap$stations
prog_long  <- station_programs(stations)

regions  <- sort(unique(stats::na.omit(stations$region)))
basins   <- sort(unique(stats::na.omit(stations$basin)))
programs <- sort(unique(prog_long$program))

region_pal <- colorFactor(
  c("#1F6F5C", "#B23A48", "#3B5BA5", "#C77D12", "#6B4E9B", "#2C8C99", "#7A7A7A"),
  domain = regions
)

# ---- UI --------------------------------------------------------------------
ui <- page_sidebar(
  title = "2026 Monitoring Plan Stations Explorer",
  theme = bs_theme(version = 5, primary = "#1F6F5C"),

  sidebar = sidebar(
    width = 300,
    selectizeInput("region", "DEQ region", choices = regions, multiple = TRUE,
                   options = list(placeholder = "All regions")),
    selectizeInput("basin", "Basin", choices = basins, multiple = TRUE,
                   options = list(placeholder = "All basins")),
    selectizeInput("program", "Monitoring program", choices = programs, multiple = TRUE,
                   options = list(placeholder = "All programs")),
    checkboxInput("new_only", "Only stations with no sample history", FALSE),
    downloadButton("download", "Download filtered stations (CSV)"),
    hr(),
    p(class = "text-muted small",
      "Source: Virginia DEQ public ArcGIS service, 2026 Monitoring Plan Stations.",
      "Snapshot refreshed ", format(snap$refreshed, "%b %d, %Y %H:%M"), ".")
  ),

  layout_columns(
    fill = FALSE,
    value_box("Stations", textOutput("n_stations"), theme = "primary"),
    value_box("In more than one program", textOutput("n_multi"), theme = "secondary"),
    value_box("No sample history yet", textOutput("n_new"), theme = "warning")
  ),

  navset_card_tab(
    nav_panel("Map", leafletOutput("map", height = 520)),
    nav_panel("By basin", plotOutput("basin_plot", height = 420)),
    nav_panel("Stations", DTOutput("table")),
    nav_panel("Data quality",
              p("Checks run on every refresh. A failed check stops the refresh,",
                "so this app keeps showing the last good snapshot."),
              DTOutput("quality"))
  )
)

# ---- Server ----------------------------------------------------------------
server <- function(input, output, session) {

  filtered <- reactive({
    df <- stations
    if (length(input$region))  df <- df[df$region %in% input$region, ]
    if (length(input$basin))   df <- df[df$basin  %in% input$basin, ]
    if (length(input$program)) {
      ids <- prog_long$station_id[prog_long$program %in% input$program]
      df <- df[df$station_id %in% ids, ]
    }
    if (isTRUE(input$new_only)) df <- df[is.na(df$sample_count), ]
    df
  })

  output$n_stations <- renderText(format(nrow(filtered()), big.mark = ","))
  output$n_multi    <- renderText(sum(filtered()$n_programs > 1))
  output$n_new      <- renderText(sum(is.na(filtered()$sample_count)))

  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      fitBounds(VA_BBOX$lon[1], VA_BBOX$lat[1], VA_BBOX$lon[2], VA_BBOX$lat[2]) |>
      addLegend("bottomright", pal = region_pal, values = regions, title = "DEQ region")
  })

  # Update markers without redrawing the whole map when filters change.
  observe({
    df <- filtered()
    df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
    leafletProxy("map", data = df) |>
      clearMarkers() |>
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude, radius = 5, stroke = FALSE,
        fillOpacity = 0.8, fillColor = ~region_pal(region),
        popup = ~sprintf(
          "<b>%s</b><br>%s<br>%s, %s<br>Programs: %s<br>Samples to date: %s",
          htmltools::htmlEscape(station_id),
          htmltools::htmlEscape(ifelse(is.na(description), "", description)),
          htmltools::htmlEscape(ifelse(is.na(county), "", county)),
          htmltools::htmlEscape(ifelse(is.na(region), "", region)),
          htmltools::htmlEscape(programs),
          ifelse(is.na(sample_count), "none recorded", sample_count))
      )
  })

  output$basin_plot <- renderPlot({
    df <- filtered()
    validate(need(nrow(df) > 0, "No stations match the current filters."))
    counts <- as.data.frame(table(basin = ifelse(is.na(df$basin), "(not assigned)", df$basin)))
    ggplot(counts, aes(x = Freq, y = reorder(basin, Freq))) +
      geom_col(fill = "#1F6F5C") +
      geom_text(aes(label = Freq), hjust = -0.2, size = 3.8) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Planned stations", y = NULL) +
      theme_minimal(base_size = 13)
  })

  output$table <- renderDT({
    df <- filtered()[, c("station_id", "region", "basin", "county", "stream_name",
                         "programs", "sample_count", "last_sample")]
    names(df) <- c("Station", "Region", "Basin", "County", "Stream",
                   "Programs", "Samples to date", "Last sample")
    datatable(df, rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE))
  })

  output$quality <- renderDT({
    datatable(snap$validation, rownames = FALSE,
              colnames = c("Check", "Records flagged", "Status", "Detail"),
              options = list(dom = "t", ordering = FALSE, pageLength = 20))
  })

  output$download <- downloadHandler(
    filename = function() paste0("deq_2026_monitoring_stations_", Sys.Date(), ".csv"),
    content  = function(file) utils::write.csv(filtered(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
