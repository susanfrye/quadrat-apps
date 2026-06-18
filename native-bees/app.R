# =============================================================================
# App F: Ontario Native Bees — Bumble Bee Tracker
# Real data via the rgbif package (GBIF.org — aggregates iNaturalist,
# museum records, Bumble Bee Watch, and more)
#
# Maps wild bumble bee (Bombus) records across Ontario, with species-richness
# mapping, flight-period phenology, and at-risk status — including the
# Endangered Rusty-patched Bumble Bee. Built around native-bee monitoring,
# the focus of my doctoral research.
#
# SETUP (run once in console before launching):
#   install.packages(c("shiny","bslib","leaflet","leaflet.extras","plotly",
#                      "rgbif","dplyr","tibble"))
#
# Data is downloaded on first run and cached locally as cache_gbif_bees.rds
# =============================================================================

library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(rgbif)
library(dplyr)
library(tibble)

# ── Ontario bumble bee species ────────────────────────────────────────────────
bee_meta <- tibble::tribble(
  ~sci,                    ~common,                    ~status,
  "Bombus affinis",        "Rusty-patched Bumble Bee", "Endangered",
  "Bombus terricola",      "Yellow-banded Bumble Bee", "Special Concern",
  "Bombus pensylvanicus",  "American Bumble Bee",      "Special Concern",
  "Bombus impatiens",      "Common Eastern Bumble Bee","Secure",
  "Bombus ternarius",      "Tricoloured Bumble Bee",   "Secure",
  "Bombus vagans",         "Half-black Bumble Bee",    "Secure",
  "Bombus borealis",       "Northern Amber Bumble Bee","Secure",
  "Bombus griseocollis",   "Brown-belted Bumble Bee",  "Secure",
  "Bombus fervidus",       "Yellow Bumble Bee",        "Special Concern",
  "Bombus rufocinctus",    "Red-belted Bumble Bee",    "Secure"
)

# ── Download & cache ──────────────────────────────────────────────────────────
cache_file <- "cache_gbif_bees.rds"

if (!file.exists(cache_file)) {
  message("Downloading GBIF bumble bee data — takes a minute the first time...")

  raw <- lapply(seq_len(nrow(bee_meta)), function(i) {
    row <- bee_meta[i, ]
    tryCatch({
      res <- occ_search(
        scientificName = row$sci,
        country        = "CA",
        stateProvince  = "Ontario",
        year           = "2000,2024",
        limit          = 1500,
        hasCoordinate  = TRUE,
        fields         = c("gbifID", "decimalLatitude", "decimalLongitude",
                           "year", "month")
      )$data
      if (!is.null(res) && nrow(res) > 0) {
        res$common <- row$common
        res$status <- row$status
      }
      res
    }, error = function(e) { message("Failed: ", row$common); NULL })
  })

  bees <- bind_rows(Filter(Negate(is.null), raw)) |>
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(year)) |>
    rename(lat = decimalLatitude, lon = decimalLongitude) |>
    mutate(decade = paste0(floor(year / 10) * 10, "s"))

  saveRDS(bees, cache_file)
  message("Cached ", nrow(bees), " records to ", cache_file)
}

bees <- readRDS(cache_file)
status_levels <- c("Endangered", "Special Concern", "Secure")
# Status palette — switch by un/commenting one line (Okabe-Ito active):
status_pal <- colorFactor(c("#D55E00", "#E69F00", "#009E73"), levels = status_levels)  # Okabe-Ito (colourblind-safe)
# status_pal <- colorFactor(c("#a4133c", "#64748b", "#94a3b8"), levels = status_levels)  # Crimson + slate
# status_pal <- colorFactor(c("#6d28d9", "#0d9488", "#6b8f71"), levels = status_levels)  # Cool, no red
sp_choices <- bee_meta$common

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Ontario Native Bees — Bumble Bee Tracker",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#0d9488",
                   heading_font = font_google("Space Grotesk"),
                   base_font = font_google("Inter")) |>
    bs_add_variables("border-radius" = "0", "border-radius-lg" = "0",
                     "border-radius-sm" = "0"),
  sidebar = sidebar(
    width = 300,
    selectizeInput("species", "Species", choices = sp_choices,
                   selected = sp_choices, multiple = TRUE),
    sliderInput("years", "Year range",
                min = 2000, max = 2024, value = c(2000, 2024), sep = ""),
    checkboxInput("heat", "Show density heatmap", value = FALSE),
    hr(),
    helpText("Records: GBIF.org (incl. iNaturalist & Bumble Bee Watch),",
             "Ontario, 2000–2024.")
  ),
  layout_columns(
    col_widths = c(12),
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box("Records", textOutput("n_rec"), theme = "primary"),
      value_box("Species shown", textOutput("n_sp"),
                theme = value_box_theme(bg = "#f8fafb", fg = "#0a0a0a")),
      value_box("At-risk records", textOutput("n_risk"),
                theme = value_box_theme(bg = "#D55E00", fg = "#ffffff"))
    )
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(full_screen = TRUE, card_header("Occurrence map"),
         leafletOutput("map", height = 460)),
    card(card_header("Flight-period phenology"),
         plotlyOutput("phen", height = 210),
         card_header("Records by decade"),
         plotlyOutput("trend", height = 200))
  )
)

# ── Server ──────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  dat <- reactive({
    req(input$species)
    bees |>
      filter(common %in% input$species,
             year >= input$years[1], year <= input$years[2])
  })

  output$n_rec  <- renderText(format(nrow(dat()), big.mark = ","))
  output$n_sp   <- renderText(length(unique(dat()$common)))
  output$n_risk <- renderText({
    format(sum(dat()$status %in% c("Endangered", "Special Concern")), big.mark = ",")
  })

  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -80, lat = 45.5, zoom = 6)
  })

  observe({
    d <- dat()
    m <- leafletProxy("map") |> clearGroup("pts") |> clearControls()
    if (nrow(d) == 0) return(m)
    if (isTRUE(input$heat)) {
      m |> addHeatmap(data = d, lng = ~lon, lat = ~lat, group = "pts",
                      blur = 20, max = 0.6, radius = 12)
    } else {
      m |>
        addCircleMarkers(data = d, lng = ~lon, lat = ~lat, group = "pts",
                         radius = 4, stroke = FALSE, fillOpacity = 0.7,
                         color = ~status_pal(status),
                         popup = ~paste0("<b>", common, "</b><br>", status,
                                         "<br>", year)) |>
        addLegend("bottomright", pal = status_pal, values = status_levels,
                  title = "Status", opacity = 0.9)
    }
  })

  # Phenology: records by month
  output$phen <- renderPlotly({
    d <- dat() |> filter(!is.na(month)) |> count(month)
    if (nrow(d) == 0) return(plotly_empty())
    d <- d |> right_join(tibble(month = 1:12), by = "month") |>
      mutate(n = ifelse(is.na(n), 0, n))
    plot_ly(d, x = ~factor(month.abb[month], levels = month.abb), y = ~n,
            type = "bar", marker = list(color = "#0d9488")) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Records"),
             margin = list(t = 10))
  })

  # Trend: records by decade
  output$trend <- renderPlotly({
    d <- dat() |> count(decade)
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, x = ~decade, y = ~n, type = "bar",
            marker = list(color = "#3a7d44")) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Records"),
             margin = list(t = 10))
  })
}

shinyApp(ui, server)
