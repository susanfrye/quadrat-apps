# =============================================================================
# App E: Species at Risk — Site Screening Tool
# Real data via the rgbif package (GBIF.org public API)
#
# A first-pass screening tool: enter (or click) a site location and a search
# radius, and the app returns every at-risk species record nearby — with a map,
# a summary table by species/recency, and a downloadable CSV. Mirrors the kind
# of preliminary SAR screening done early in an environmental impact assessment.
#
# SETUP (run once in console before launching):
#   install.packages(c("shiny","bslib","leaflet","plotly","rgbif",
#                      "dplyr","tibble","DT","geosphere"))
#
# Data is downloaded on first run and cached locally as cache_gbif_sar_screen.rds
# =============================================================================

library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(rgbif)
library(dplyr)
library(DT)
library(geosphere)
library(tibble)

# ── Ontario SAR species (cross-taxa) ──────────────────────────────────────────
# Each: scientific name, common name, taxon group, status (illustrative).
sar_meta <- tibble::tribble(
  ~sci,                        ~common,                  ~group,      ~status,
  "Danaus plexippus",          "Monarch",                "Insect",    "Special Concern",
  "Bombus affinis",            "Rusty-patched Bumble Bee","Insect",   "Endangered",
  "Dolichonyx oryzivorus",     "Bobolink",               "Bird",      "Threatened",
  "Sturnella magna",           "Eastern Meadowlark",     "Bird",      "Threatened",
  "Hirundo rustica",           "Barn Swallow",           "Bird",      "Special Concern",
  "Chaetura pelagica",         "Chimney Swift",          "Bird",      "Threatened",
  "Antrostomus vociferus",     "Eastern Whip-poor-will", "Bird",      "Threatened",
  "Emydoidea blandingii",      "Blanding's Turtle",      "Reptile",   "Threatened",
  "Chelydra serpentina",       "Snapping Turtle",        "Reptile",   "Special Concern",
  "Ambystoma jeffersonianum",  "Jefferson Salamander",   "Amphibian", "Endangered",
  "Juglans cinerea",           "Butternut",              "Plant",     "Endangered",
  "Panax quinquefolius",       "American Ginseng",       "Plant",     "Endangered",
  "Myotis lucifugus",          "Little Brown Myotis",    "Mammal",    "Endangered"
)

# ── Download & cache ──────────────────────────────────────────────────────────
cache_file <- "cache_gbif_sar_screen.rds"

if (!file.exists(cache_file)) {
  message("Downloading GBIF occurrence data — takes a minute the first time...")

  raw <- lapply(seq_len(nrow(sar_meta)), function(i) {
    row <- sar_meta[i, ]
    tryCatch({
      res <- occ_search(
        scientificName = row$sci,
        country        = "CA",
        stateProvince  = "Ontario",
        year           = "2000,2024",
        limit          = 1000,
        hasCoordinate  = TRUE,
        fields         = c("gbifID", "decimalLatitude", "decimalLongitude",
                           "year", "month", "basisOfRecord")
      )$data
      if (!is.null(res) && nrow(res) > 0) {
        res$common <- row$common
        res$group  <- row$group
        res$status <- row$status
      }
      res
    }, error = function(e) { message("Failed: ", row$common); NULL })
  })

  occs <- bind_rows(Filter(Negate(is.null), raw)) |>
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(year)) |>
    rename(lat = decimalLatitude, lon = decimalLongitude)

  saveRDS(occs, cache_file)
  message("Cached ", nrow(occs), " records to ", cache_file)
}

occs <- readRDS(cache_file)

status_levels <- c("Endangered", "Threatened", "Special Concern")
status_pal <- colorFactor(c("#D55E00", "#E69F00", "#009E73"),  # Okabe-Ito (colourblind-safe)
                          levels = status_levels)

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Species at Risk — Site Screening Tool",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#0d9488",
                   heading_font = font_google("Space Grotesk"),
                   base_font = font_google("Inter")) |>
    bs_add_variables("border-radius" = "0", "border-radius-lg" = "0",
                     "border-radius-sm" = "0"),
  sidebar = sidebar(
    width = 320,
    helpText("Enter a site location, or click the map to drop a point."),
    numericInput("lat", "Latitude",  value = 44.30, min = 41.6, max = 56.9, step = 0.01),
    numericInput("lon", "Longitude", value = -78.30, min = -95.2, max = -74.3, step = 0.01),
    sliderInput("radius", "Search radius (km)", min = 1, max = 25, value = 5, step = 1),
    checkboxGroupInput("groups", "Taxon groups",
                       choices  = sort(unique(occs$group)),
                       selected = sort(unique(occs$group))),
    hr(),
    downloadButton("dl", "Download results (CSV)", class = "btn-primary btn-sm"),
    hr(),
    helpText("Records: GBIF.org, Ontario, 2000–2024. A screening aid only —",
             "absence of records is not evidence of absence.")
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(full_screen = TRUE, card_header("Records within radius"),
         leafletOutput("map", height = 460)),
    card(card_header("Summary by species"),
         value_box("Records found", textOutput("n_found"), theme = "primary"),
         DTOutput("tbl"))
  )
)

# ── Server ──────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Reactive: occurrences within the radius of the chosen point
  hits <- reactive({
    req(input$lat, input$lon, input$radius)
    d <- occs |> filter(group %in% input$groups)
    if (nrow(d) == 0) return(d[0, ] |> mutate(dist_km = numeric(0)))
    dist_m <- distHaversine(cbind(d$lon, d$lat), c(input$lon, input$lat))
    d |>
      mutate(dist_km = round(dist_m / 1000, 2)) |>
      filter(dist_km <= input$radius)
  })

  # Per-species summary
  summ <- reactive({
    h <- hits()
    if (nrow(h) == 0)
      return(tibble(Species = character(), Status = character(),
                    Records = integer(), `Most recent` = integer(),
                    `Closest (km)` = numeric()))
    h |>
      group_by(Species = common, Status = status) |>
      summarise(Records = n(),
                `Most recent` = max(year),
                `Closest (km)` = min(dist_km),
                .groups = "drop") |>
      arrange(match(Status, status_levels), `Closest (km)`)
  })

  output$n_found <- renderText(format(nrow(hits()), big.mark = ","))

  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -80, lat = 45, zoom = 6)
  })

  observe({
    h <- hits()
    pal <- status_pal
    m <- leafletProxy("map") |>
      clearMarkers() |> clearShapes() |> clearControls() |>
      addCircles(lng = input$lon, lat = input$lat, radius = input$radius * 1000,
                 color = "#0a0a0a", weight = 1.5, fillColor = "#0d9488",
                 fillOpacity = 0.08) |>
      addAwesomeMarkers(lng = input$lon, lat = input$lat,
                        icon = awesomeIcons(icon = "crosshairs", library = "fa",
                                            markerColor = "darkblue"),
                        label = "Site")
    if (nrow(h) > 0) {
      m |>
        addCircleMarkers(data = h, lng = ~lon, lat = ~lat,
                         radius = 5, stroke = FALSE, fillOpacity = 0.8,
                         color = ~pal(status),
                         popup = ~paste0("<b>", common, "</b><br>", status,
                                         "<br>", year, " · ", dist_km, " km")) |>
        addLegend(position = "bottomright", pal = pal, values = status_levels,
                  title = "Status", opacity = 0.9)
    } else m
  })

  # Click map to set the point
  observeEvent(input$map_click, {
    click <- input$map_click
    updateNumericInput(session, "lat", value = round(click$lat, 4))
    updateNumericInput(session, "lon", value = round(click$lng, 4))
  })

  output$tbl <- renderDT({
    datatable(summ(), rownames = FALSE, options = list(dom = "tp", pageLength = 8))
  })

  output$dl <- downloadHandler(
    filename = function() paste0("sar_screening_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(hits() |>
                  select(common, status, group, year, month, lat, lon, dist_km) |>
                  arrange(dist_km),
                file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
