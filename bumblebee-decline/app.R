# =============================================================================
# App H: Ontario At-Risk Bumble Bee — Decline & Range-Shift Dashboard
# Real data via the rgbif package (GBIF.org — aggregates iNaturalist,
# museum records, Bumble Bee Watch, and more)
#
# Built to address two findings Wildlife Preservation Canada has reported from
# its Bumble Bee Recovery field work:
#   1. Yellow-banded Bumble Bee (Bombus terricola) appears to be shifting NORTH.
#   2. Despite intensive searches since 2012, the Endangered Rusty-patched
#      Bumble Bee (Bombus affinis) has not been relocated.
#
# GBIF occurrence counts are EFFORT-BIASED (records rise as more people record),
# so this tool deliberately avoids naive count-trends. It uses effort-robust
# metrics instead:
#   • Proportion of all Bombus records that are at-risk species (controls effort)
#   • Mean-latitude (range centroid) shift by decade (relatively effort-robust)
#   • A survey-effort grid that flags where at-risk bees are historic-only or
#     absent despite Bombus being recorded there — i.e. where to survey next.
#
# SETUP (run once in console before launching):
#   install.packages(c("shiny","bslib","leaflet","leaflet.extras","plotly",
#                      "rgbif","dplyr","tibble"))
#
# Data is downloaded on first run and cached as cache_gbif_bombus_decline.rds
# =============================================================================

library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(rgbif)
library(dplyr)
library(tibble)

# ── Config: focal Ontario bumble bees and conservation status ─────────────────
# `hero` = the northward-shift story; `baseline` = secure species used as an
# effort control. Edit this table to re-scope the dashboard.
bee_meta <- tibble::tribble(
  ~sci,                    ~common,                     ~status,            ~role,
  "Bombus affinis",        "Rusty-patched Bumble Bee",  "Endangered",       "focal",
  "Bombus terricola",      "Yellow-banded Bumble Bee",  "Special Concern",  "hero",
  "Bombus pensylvanicus",  "American Bumble Bee",       "Special Concern",  "focal",
  "Bombus fervidus",       "Yellow Bumble Bee",         "Special Concern",  "focal",
  "Bombus impatiens",      "Common Eastern Bumble Bee", "Secure",           "baseline"
)
at_risk_status <- c("Endangered", "Special Concern")

YEAR_MIN <- 2000
YEAR_MAX <- 2024
GRID_DEG <- 0.5    # survey-effort grid cell size (degrees)
RECENT_FROM <- 2015  # "recent" window for the survey-gap classification

# ── Download & cache ──────────────────────────────────────────────────────────
# Three cheap, robust pulls (no slow pagination):
#   1. denom_year — EXACT records-per-year for ALL Ontario Bombus, via one GBIF
#      facet call. This is the denominator for the proportion metric.
#   2. sp_year    — EXACT records-per-year for each focal species, via one facet
#      call each, so the decline numerator is accurate even for the very common
#      baseline species.
#   3. focal      — actual coordinates for the focal species (drives the centroid,
#      the maps, and the survey-effort grid).
# Cached together as a named list so the app loads instantly.
cache_file <- "cache_gbif_bombus_decline.rds"

facet_year <- function(sci = NULL, key = NULL) {
  args <- list(country = "CA", stateProvince = "Ontario",
               year = paste0(YEAR_MIN, ",", YEAR_MAX), hasCoordinate = TRUE,
               limit = 0, facet = "year", facetLimit = 200)
  if (!is.null(sci)) args$scientificName <- sci
  if (!is.null(key)) args$taxonKey <- key
  f <- do.call(occ_search, args)$facets$year
  if (is.null(f) || nrow(f) == 0) return(tibble(year = integer(), n = integer()))
  tibble(year = as.integer(f$name), n = as.integer(f$count))
}

if (!file.exists(cache_file)) {
  message("Downloading GBIF Bombus data for Ontario — a few seconds the first time...")
  bombus_key <- rgbif::name_backbone("Bombus")$usageKey

  denom_year <- facet_year(key = bombus_key) |> rename(total = n)

  sp_year <- bind_rows(lapply(seq_len(nrow(bee_meta)), function(i) {
    row <- bee_meta[i, ]
    fy <- facet_year(sci = row$sci)
    if (nrow(fy) == 0) return(NULL)
    fy$common <- row$common; fy$status <- row$status; fy$role <- row$role; fy
  }))

  focal <- bind_rows(lapply(seq_len(nrow(bee_meta)), function(i) {
    row <- bee_meta[i, ]
    res <- tryCatch(
      occ_search(scientificName = row$sci, country = "CA", stateProvince = "Ontario",
                 year = paste0(YEAR_MIN, ",", YEAR_MAX), hasCoordinate = TRUE,
                 limit = 5000,
                 fields = c("decimalLatitude", "decimalLongitude", "year", "month"))$data,
      error = function(e) NULL)
    if (is.null(res) || nrow(res) == 0) return(NULL)
    res$common <- row$common; res$status <- row$status; res$role <- row$role
    res
  })) |>
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(year)) |>
    rename(lat = decimalLatitude, lon = decimalLongitude) |>
    mutate(decade = paste0(floor(year / 10) * 10, "s"),
           at_risk = status %in% at_risk_status)

  saveRDS(list(denom_year = denom_year, sp_year = sp_year, focal = focal), cache_file)
  message("Cached: ", sum(denom_year$total), " total Bombus / ",
          nrow(focal), " focal records")
}

dat_all     <- readRDS(cache_file)
denom_year  <- dat_all$denom_year
sp_year     <- dat_all$sp_year |> mutate(at_risk = status %in% at_risk_status)
focal       <- dat_all$focal

# Okabe-Ito (colourblind-safe) status palette — alternatives commented below.
status_levels <- c("Endangered", "Special Concern", "Secure")
status_pal <- colorFactor(c("#D55E00", "#E69F00", "#009E73"), levels = status_levels)  # Okabe-Ito
# status_pal <- colorFactor(c("#a4133c", "#64748b", "#94a3b8"), levels = status_levels) # Crimson + slate
# status_pal <- colorFactor(c("#6d28d9", "#0d9488", "#6b8f71"), levels = status_levels) # Cool, no red

# Per-species palette (Okabe-Ito, colourblind-safe), one colour per focal species.
focal_choices <- bee_meta$common
species_pal <- colorFactor(
  c("#D55E00", "#E69F00", "#0072B2", "#CC79A7", "#009E73"),
  levels = bee_meta$common)
TEAL <- "#0d9488"
KM_PER_DEG_LAT <- 111  # approx km per degree of latitude

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = "Ontario At-Risk Bumble Bees — Decline & Range Shift",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = TEAL,
                   heading_font = font_google("Space Grotesk"),
                   base_font = font_google("Inter")) |>
    bs_add_variables("border-radius" = "0", "border-radius-lg" = "0",
                     "border-radius-sm" = "0"),
  sidebar = sidebar(
    width = 310,
    selectizeInput("species", "Focal species", choices = focal_choices,
                   selected = focal_choices, multiple = TRUE),
    sliderInput("years", "Year range", min = YEAR_MIN, max = YEAR_MAX,
                value = c(YEAR_MIN, YEAR_MAX), sep = ""),
    hr(),
    helpText(
      "Records: GBIF.org (incl. iNaturalist & Bumble Bee Watch), Ontario, ",
      YEAR_MIN, "–", YEAR_MAX, ". Metrics are effort-robust by design — see the",
      strong(" Methods & caveats"), "tab."
    )
  ),

  # ── Tab 1: Range shift ──
  nav_panel(
    "Northern range",
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box("Hero species", textOutput("hero_name"), theme = "primary"),
      value_box("North of the common bee", textOutput("shift_km"),
                theme = value_box_theme(bg = "#E69F00", fg = "#0a0a0a")),
      value_box("Records analysed", textOutput("n_hero"),
                theme = value_box_theme(bg = "#f8fafb", fg = "#0a0a0a"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Latitude of each species vs the common baseline bee"),
           plotlyOutput("centroid", height = 380),
           card_footer(class = "small text-muted",
             "Comparing species recorded by the SAME observers controls for where ",
             "people look. At-risk bees sitting north of the dashed baseline are ",
             "more northern in range — a signal robust to the southern recording boom. ",
             "(A clean temporal trend, by contrast, can't be separated from observer ",
             "shifts in opportunistic data — see Methods.)")),
      card(full_screen = TRUE,
           card_header("Where the selected species are recorded",
                       checkboxInput("map_era", "Split early vs recent", FALSE)),
           leafletOutput("shift_map", height = 380))
    )
  ),

  # ── Tab 2: Decline signal ──
  nav_panel(
    "Decline signal",
    card(
      card_header("At-risk records as a share of ALL Ontario Bombus records"),
      plotlyOutput("proportion", height = 420),
      card_footer(class = "small text-muted",
        "Dividing by total Bombus effort each year controls for the fact that ",
        "everyone is recording more bees over time. A falling share is a decline ",
        "signal that a rising raw count would hide.")
    )
  ),

  # ── Tab 3: Survey effort & gaps ──
  nav_panel(
    "Survey effort & gaps",
    layout_columns(
      col_widths = c(8, 4),
      card(full_screen = TRUE,
           card_header("Survey-effort grid — where to look next"),
           leafletOutput("gap_map", height = 520)),
      card(card_header("Reading the map"),
           uiOutput("gap_legend"),
           hr(),
           card_header("Priority cells"),
           textOutput("gap_summary"))
    )
  ),

  # ── Tab 4: Species map ──
  nav_panel(
    "Species map",
    card(full_screen = TRUE,
         card_header("Every record, coloured by species",
                     checkboxInput("sp_cluster", "Cluster dense points", FALSE)),
         leafletOutput("species_map", height = 560),
         card_footer(class = "small text-muted",
           "All selected focal species, one colour each. Use the sidebar to add or ",
           "remove species; tick clustering when the common bee crowds the map."))
  ),

  # ── Tab 5: Methods & caveats ──
  nav_panel(
    "Methods & caveats",
    card(card_body(
      htmltools::HTML(
        "<h4>What this tool does</h4>
         <p>It summarises open occurrence records for Ontario bumble bees
         (<em>Bombus</em>) from <a href='https://www.gbif.org'>GBIF.org</a>, which
         aggregates iNaturalist, Bumble Bee Watch, museum, and research records.
         It is built around two patterns reported by
         <a href='https://wildlifepreservation.ca/bumble-bee-recovery/'>Wildlife
         Preservation Canada</a>: the Yellow-banded Bumble Bee turning up further
         north, and the continued absence of the Endangered Rusty-patched Bumble Bee.</p>

         <h4>Why it avoids raw count trends</h4>
         <p>Occurrence records reflect <strong>observer effort</strong> as much as
         true abundance. Records of nearly every species rise over time simply
         because more people are out recording — and that recording boom is
         concentrated in the populous south. A naive &ldquo;records per year&rdquo;
         or even a raw mean-latitude trend would therefore be misleading
         (in this data the Yellow-banded centroid actually drifts <em>south</em>,
         pulled by southern observers). This dashboard uses metrics chosen to be
         robust to that bias:</p>
         <ul>
           <li><strong>Latitude vs a same-period baseline.</strong> Comparing an
           at-risk species with the common bee recorded by the <em>same</em>
           observers cancels out where people look. A species sitting well north of
           that baseline is genuinely more northern in range.</li>
           <li><strong>Proportion of records.</strong> Expressing at-risk species
           as a share of <em>all</em> Bombus records each year divides out the
           year-to-year change in total effort.</li>
           <li><strong>Survey-effort grid.</strong> Rather than claiming absence,
           it flags cells where Bombus <em>are</em> recorded but at-risk species are
           historic-only or absent — i.e. promising places to direct surveys.</li>
         </ul>

         <h4>Caveats</h4>
         <ul>
           <li>Absence of records is <strong>not</strong> evidence of absence,
           especially in under-surveyed northern Ontario.</li>
           <li>Species-level identifications from photographs carry error.</li>
           <li><strong>A clean temporal range shift cannot be read from this
           opportunistic data</strong> — observer distribution changes too much over
           time. Establishing one is exactly what structured field programs like
           WPC's are for; this screen is built to <em>prioritise</em> that effort,
           not replace it.</li>
           <li>This is open-data screening. A defensible recovery analysis would
           model detection effort explicitly and incorporate targeted field data.</li>
         </ul>
         <p class='text-muted'>Prepared with Quadrat Studio · Susan Frye ·
         <a href='https://quadratstudio.ca'>quadratstudio.ca</a></p>"
      )
    ))
  ),

  nav_spacer(),
  nav_item(tags$a("quadratstudio.ca", href = "https://quadratstudio.ca", target = "_blank"))
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  hero_common <- bee_meta$common[bee_meta$role == "hero"]
  hero_sci    <- bee_meta$sci[bee_meta$role == "hero"]

  # Focal-species coordinate records, year-filtered (drives centroid/maps/grid)
  sel <- reactive({
    req(input$species)
    focal |> filter(common %in% input$species,
                    year >= input$years[1], year <= input$years[2])
  })

  # Hero species records (drives the range-shift tab)
  hero_dat <- reactive({
    focal |> filter(common == hero_common,
                    year >= input$years[1], year <= input$years[2])
  })

  # Baseline (the secure common species) mean latitude — the effort reference.
  baseline_common <- bee_meta$common[bee_meta$role == "baseline"]
  baseline_lat <- reactive({
    b <- focal |> filter(common == baseline_common,
                         year >= input$years[1], year <= input$years[2])
    if (nrow(b) == 0) return(NA_real_)
    mean(b$lat)
  })

  # ── Northern-range value boxes ──
  output$hero_name <- renderText(hero_common)
  output$n_hero    <- renderText(format(nrow(hero_dat()), big.mark = ","))
  output$shift_km  <- renderText({
    bl <- baseline_lat(); d <- hero_dat()
    if (is.na(bl) || nrow(d) == 0) return("n/a")
    delta <- (mean(d$lat) - bl) * KM_PER_DEG_LAT
    paste0(ifelse(delta >= 0, "+", "−"), round(abs(delta)), " km")
  })

  # ── Mean latitude per species, with the baseline as a dashed reference ──
  output$centroid <- renderPlotly({
    d <- sel() |>
      group_by(common) |>
      summarise(lat = mean(lat), sd = sd(lat), n = n(), .groups = "drop") |>
      arrange(lat)
    if (nrow(d) == 0) return(plotly_empty())
    d$err <- ifelse(d$n >= 5 & !is.na(d$sd), d$sd, 0)
    bl <- baseline_lat()
    shapes <- list(); annos <- list()
    if (!is.na(bl)) {
      shapes <- list(list(type = "line", x0 = bl, x1 = bl, yref = "paper",
                          y0 = 0, y1 = 1,
                          line = list(dash = "dash", color = "#0a0a0a", width = 1.5)))
      annos <- list(list(x = bl, y = 1, yref = "paper", yanchor = "bottom",
                         text = paste0("common-bee baseline ", round(bl, 1), "°N"),
                         showarrow = FALSE, font = list(size = 11, color = "#0a0a0a")))
    }
    plot_ly(d, x = ~lat, y = ~factor(common, levels = common), type = "scatter",
            mode = "markers", color = ~common,
            error_x = list(array = ~err, color = "#9ca3af"),
            marker = list(size = 11),
            hovertemplate = "%{y}<br>mean %{x:.2f}°N<extra></extra>") |>
      layout(xaxis = list(title = "Mean latitude (°N)"), yaxis = list(title = ""),
             showlegend = FALSE, margin = list(t = 20),
             shapes = shapes, annotations = annos)
  })

  # ── Map of the selected species — by status, or split early vs recent ──
  output$shift_map <- renderLeaflet({
    d <- sel()
    base <- leaflet() |> addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -82, lat = 47, zoom = 5)
    if (nrow(d) == 0) return(base)
    if (isTRUE(input$map_era)) {
      mid <- floor((input$years[1] + input$years[2]) / 2)
      d <- d |> mutate(period = ifelse(year <= mid,
                                       paste0(input$years[1], "–", mid),
                                       paste0(mid + 1, "–", input$years[2])))
      per_pal <- colorFactor(c("#9ca3af", "#D55E00"), domain = sort(unique(d$period)))
      base |>
        addCircleMarkers(data = d, ~lon, ~lat, radius = 4, stroke = FALSE,
                         fillOpacity = 0.65, color = ~per_pal(period),
                         popup = ~paste0("<b>", common, "</b><br>", year)) |>
        addLegend("bottomright", pal = per_pal, values = d$period,
                  title = "Period", opacity = 0.9)
    } else {
      base |>
        addCircleMarkers(data = d, ~lon, ~lat, radius = 4, stroke = FALSE,
                         fillOpacity = 0.65, color = ~status_pal(status),
                         popup = ~paste0("<b>", common, "</b><br>", status, "<br>", year)) |>
        addLegend("bottomright", pal = status_pal, values = status_levels,
                  title = "Status", opacity = 0.9)
    }
  })

  # ── Proportion-of-records by year (exact facet counts / exact totals) ──
  output$proportion <- renderPlotly({
    req(input$species)
    d <- sp_year |>
      filter(common %in% input$species,
             year >= input$years[1], year <= input$years[2]) |>
      left_join(denom_year, by = "year") |>
      mutate(pct = 100 * n / total) |>
      filter(total >= 20)   # suppress years with too little effort to be meaningful
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, x = ~year, y = ~pct, color = ~common, type = "scatter",
            mode = "lines+markers",
            hovertemplate = "%{fullData.name}<br>%{x}: %{y:.1f}% of Bombus records<extra></extra>") |>
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Share of all Bombus records (%)"),
             legend = list(orientation = "h", y = -0.2), margin = list(t = 10))
  })

  # ── Survey-effort grid ──
  # Effort proxy = all focal-species records (the common baseline dominates and
  # tracks where people actually record bumble bees); at-risk = the focal SAR set.
  grid <- reactive({
    d <- focal |> filter(year >= input$years[1], year <= input$years[2])
    if (nrow(d) == 0) return(NULL)
    d <- d |>
      mutate(gx = floor(lon / GRID_DEG) * GRID_DEG,
             gy = floor(lat / GRID_DEG) * GRID_DEG)
    d |>
      group_by(gx, gy) |>
      summarise(
        total      = n(),
        risk_any   = sum(at_risk),
        risk_recent= sum(at_risk & year >= RECENT_FROM),
        .groups = "drop") |>
      mutate(class = case_when(
        risk_recent > 0            ~ "At-risk present (recent)",
        risk_any   > 0             ~ "At-risk historic only",
        TRUE                       ~ "Bombus recorded, no at-risk"
      ))
  })

  gap_pal <- colorFactor(
    c("#009E73", "#E69F00", "#56B4E9"),
    levels = c("At-risk present (recent)", "At-risk historic only",
               "Bombus recorded, no at-risk"))

  output$gap_map <- renderLeaflet({
    g <- grid()
    if (is.null(g) || nrow(g) == 0)
      return(leaflet() |> addProviderTiles(providers$CartoDB.Positron) |>
               setView(-82, 47, 5))
    leaflet(g) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(-82, 47, 5) |>
      addRectangles(
        lng1 = ~gx, lat1 = ~gy, lng2 = ~gx + GRID_DEG, lat2 = ~gy + GRID_DEG,
        weight = 0.5, color = "#ffffff", fillColor = ~gap_pal(class),
        fillOpacity = ~pmin(0.85, 0.25 + total / max(g$total)),
        popup = ~paste0("<b>", class, "</b><br>",
                        total, " Bombus records<br>",
                        risk_any, " at-risk (", risk_recent, " since ", RECENT_FROM, ")")) |>
      addLegend("bottomright", pal = gap_pal,
                values = c("At-risk present (recent)", "At-risk historic only",
                           "Bombus recorded, no at-risk"),
                title = paste0(GRID_DEG, "° cells"), opacity = 0.9)
  })

  output$gap_legend <- renderUI({
    htmltools::HTML(paste0(
      "<p class='small'>Each cell is a ", GRID_DEG, "°&nbsp;&times;&nbsp;", GRID_DEG,
      "° block of Ontario. Colour shows the at-risk status of bumble bees recorded ",
      "there; opacity scales with how many Bombus records the cell holds ",
      "(darker = better surveyed).</p>",
      "<ul class='small'>",
      "<li><span style='color:#009E73'>&#9632;</span> At-risk bee seen recently.</li>",
      "<li><span style='color:#E69F00'>&#9632;</span> <strong>Historic only</strong> — ",
      "at-risk bee recorded here once, but not since ", RECENT_FROM,
      ". Prime candidates for a return visit.</li>",
      "<li><span style='color:#56B4E9'>&#9632;</span> Bombus are recorded here, but no ",
      "at-risk species ever has been — worth a targeted look.</li>",
      "</ul>"))
  })

  output$gap_summary <- renderText({
    g <- grid(); if (is.null(g)) return("")
    hist_only <- sum(g$class == "At-risk historic only")
    none      <- sum(g$class == "Bombus recorded, no at-risk")
    paste0(hist_only, " cells have at-risk bees on record but none since ", RECENT_FROM,
           "; a further ", none, " well-surveyed cells have never recorded an at-risk ",
           "bumble bee. These are the highest-value places to direct survey effort.")
  })

  # ── Species map — one colour per species ──
  output$species_map <- renderLeaflet({
    d <- sel()
    base <- leaflet() |> addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -82, lat = 47, zoom = 5)
    if (nrow(d) == 0) return(base)
    clust <- if (isTRUE(input$sp_cluster)) markerClusterOptions() else NULL
    base |>
      addCircleMarkers(data = d, ~lon, ~lat, radius = 4, stroke = FALSE,
                       fillOpacity = 0.7, color = ~species_pal(common),
                       clusterOptions = clust,
                       popup = ~paste0("<b>", common, "</b><br>", status, "<br>", year)) |>
      addLegend("bottomright", pal = species_pal, values = focal_choices,
                title = "Species", opacity = 0.9)
  })
}

shinyApp(ui, server)
