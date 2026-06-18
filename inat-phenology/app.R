# =============================================================================
# App D: iNaturalist Ontario — Phenology & Biodiversity Hotspot Dashboard
# Real data via the rinat package (iNaturalist public API)
#
# SETUP (run once in the console before launching the app):
#   install.packages(c("shiny","bslib","leaflet","leaflet.extras",
#                      "plotly","rinat","dplyr","ggplot2","lubridate"))
#
# Data is downloaded on first run and cached as cache_inat_ontario.rds (~5 MB)
# Downloads research-grade observations from Ontario for 4 taxa groups.
# iNaturalist rate-limits at ~100 req/min; the download takes ~2–3 min.
# =============================================================================

library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(rinat)
library(dplyr)
library(ggplot2)
library(lubridate)

# ── Taxa groups to pull ───────────────────────────────────────────────────────
# iNaturalist taxon IDs (iconic_taxa string used by rinat)
taxa_groups <- list(
  "Birds"       = "Aves",
  "Butterflies" = "Lepidoptera",
  "Plants"      = "Plantae",
  "Herptiles"   = "Reptilia"   # includes Amphibia via iNat grouping
)

# Ontario bounding box: swlat, swlng, nelat, nelng
ON_BBOX <- c(41.9, -95.2, 56.9, -74.3)

# ── Download & cache ──────────────────────────────────────────────────────────
cache_file <- "cache_inat_ontario.rds"

if (!file.exists(cache_file)) {
  message("Downloading iNaturalist Ontario data (~2–3 min, one-time)...")

  all_obs <- lapply(names(taxa_groups), function(grp) {
    taxon <- taxa_groups[[grp]]
    message("  Pulling: ", grp, " (", taxon, ")...")

    result <- tryCatch(
      get_inat_obs(
        taxon_name  = taxon,
        place_id    = 6183,    # Ontario place ID on iNaturalist
        quality     = "research",
        geo         = TRUE,
        maxresults  = 2500,
        year        = 2023
      ),
      error = function(e) {
        message("  Falling back to bbox for: ", grp)
        tryCatch(
          get_inat_obs(
            taxon_name = taxon,
            bounds     = ON_BBOX,
            quality    = "research",
            geo        = TRUE,
            maxresults = 2000,
            year       = 2023
          ),
          error = function(e2) NULL
        )
      }
    )

    if (!is.null(result) && nrow(result) > 0) {
      result$taxa_group <- grp
    }
    result
  })

  obs <- bind_rows(Filter(Negate(is.null), all_obs)) |>
    filter(
      !is.na(latitude), !is.na(longitude),
      latitude  > 41.9, latitude  < 57,
      longitude > -95.5, longitude < -74
    ) |>
    mutate(
      observed_on = as.Date(observed_on),
      year        = year(observed_on),
      month       = month(observed_on),
      doy         = yday(observed_on),
      month_abbr  = factor(month.abb[month], levels = month.abb)
    ) |>
    select(
      id, taxa_group, common_name, scientific_name = scientific_name,
      observed_on, year, month, month_abbr, doy,
      lat = latitude, lon = longitude, quality_grade,
      place_guess
    )

  saveRDS(obs, cache_file)
  message("Cached ", nrow(obs), " research-grade observations.")
}

obs <- readRDS(cache_file)

# ── Colour helpers ────────────────────────────────────────────────────────────
taxa_names   <- sort(unique(obs$taxa_group))
taxa_colors  <- setNames(
  c("#0072B2", "#E69F00", "#009E73", "#CC79A7")[seq_along(taxa_names)],  # Okabe-Ito (colourblind-safe)
  taxa_names
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "iNaturalist Ontario — Phenology & Biodiversity Hotspots",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#0d9488",
                   heading_font = font_google("Space Grotesk"),
                   base_font = font_google("Inter")) |>
    bs_add_variables("border-radius" = "0", "border-radius-lg" = "0",
                     "border-radius-sm" = "0"),

  sidebar = sidebar(
    width = 240,
    h6("Filters", class = "text-muted fw-bold mb-2"),
    checkboxGroupInput(
      "sel_taxa", "Taxa group",
      choices  = taxa_names,
      selected = taxa_names
    ),
    sliderInput(
      "sel_months", "Month range",
      min = 1, max = 12,
      value = c(1, 12),
      step = 1,
      ticks = FALSE
    ),
    uiOutput("month_label"),
    hr(),
    uiOutput("obs_summary"),
    hr(),
    p(class = "small text-muted",
      "Source: iNaturalist — research-grade observations from Ontario, 2023.",
      tags$br(),
      tags$a(href = "https://www.inaturalist.org/places/ontario",
             target = "_blank", "iNaturalist Ontario →"))
  ),

  layout_columns(
    col_widths = c(6, 6),
    card(
      full_screen = TRUE,
      card_header("Observation Density Map"),
      leafletOutput("hotspot_map", height = 380)
    ),
    card(
      full_screen = TRUE,
      card_header("Phenology — Day of Year Distribution"),
      plotlyOutput("phenology_chart", height = 380)
    ),
    card(
      col_span = 2,
      full_screen = TRUE,
      card_header("Monthly Observations by Taxa"),
      plotlyOutput("monthly_bar", height = 260)
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  output$month_label <- renderUI({
    m1 <- month.abb[input$sel_months[1]]
    m2 <- month.abb[input$sel_months[2]]
    p(class = "small text-muted mt-n2", paste(m1, "→", m2))
  })

  filtered <- reactive({
    req(input$sel_taxa, input$sel_months)
    obs |>
      filter(
        taxa_group %in% input$sel_taxa,
        month >= input$sel_months[1],
        month <= input$sel_months[2]
      )
  })

  output$obs_summary <- renderUI({
    d <- filtered()
    tagList(
      div(class = "d-flex justify-content-between small",
          span("Observations:"), strong(format(nrow(d), big.mark = ","))),
      div(class = "d-flex justify-content-between small mt-1",
          span("Species:"), strong(n_distinct(d$scientific_name))),
      div(class = "d-flex justify-content-between small mt-1",
          span("Observers:"), strong("research-grade"))
    )
  })

  # Base leaflet map
  output$hotspot_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -84, lat = 48, zoom = 5)
  })

  # Update heatmap
  observe({
    d <- filtered()
    leafletProxy("hotspot_map") |> clearGroup("pts")

    if (nrow(d) == 0) return()

    if (nrow(d) > 500) {
      leafletProxy("hotspot_map") |>
        addHeatmap(
          data      = d,
          group     = "pts",
          lat       = ~lat,
          lng       = ~lon,
          radius    = 10,
          blur      = 15,
          max       = 0.04,
          gradient  = c("0" = "#ccfbf1", "0.5" = "#14b8a6", "1" = "#0f766e")
        )
    } else {
      sp_pal <- colorFactor(palette = unname(taxa_colors), domain = taxa_names)
      leafletProxy("hotspot_map") |>
        addCircleMarkers(
          data        = d,
          group       = "pts",
          lat         = ~lat, lng = ~lon,
          radius      = 5,
          fillColor   = ~sp_pal(taxa_group),
          fillOpacity = 0.75,
          color       = "#fff",
          weight      = 0.8,
          popup = ~paste0("<b>", common_name, "</b><br>",
                          "<i>", scientific_name, "</i><br>",
                          taxa_group, " &nbsp;|&nbsp; ",
                          format(observed_on, "%b %d"))
        )
    }
  })

  # Phenology: smoothed density by DOY
  output$phenology_chart <- renderPlotly({
    d <- filtered()
    if (nrow(d) < 10) return(plotly_empty())

    p <- ggplot(d, aes(x = doy, fill = taxa_group, color = taxa_group)) +
      geom_density(alpha = 0.3, adjust = 1.5, linewidth = 0.9) +
      scale_fill_manual(values  = taxa_colors) +
      scale_color_manual(values = taxa_colors) +
      scale_x_continuous(
        breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
        labels = month.abb,
        limits = c(1, 366)
      ) +
      labs(x = NULL, y = "Observation density", fill = NULL, color = NULL) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank())

    ggplotly(p) |>
      layout(legend = list(orientation = "h", y = -0.25))
  })

  # Monthly stacked bar
  output$monthly_bar <- renderPlotly({
    d <- filtered() |>
      count(month_abbr, taxa_group) |>
      filter(!is.na(month_abbr))

    if (nrow(d) == 0) return(plotly_empty())

    p <- ggplot(d, aes(x = month_abbr, y = n, fill = taxa_group,
                       text = paste0(taxa_group, " — ", month_abbr, ": ",
                                     format(n, big.mark = ","), " obs"))) +
      geom_col(position = "stack", width = 0.75) +
      scale_fill_manual(values = taxa_colors) +
      labs(x = NULL, y = "Observations", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.x = element_blank(),
            legend.position = "bottom")

    ggplotly(p, tooltip = "text") |>
      layout(legend = list(orientation = "h", y = -0.25))
  })
}

shinyApp(ui, server)
