# =============================================================================
# App G: Monitoring Design — Trend Detection Power Tool
#
# How much monitoring is enough? This tool simulates a long-term monitoring
# program and estimates the statistical power to detect a declining trend,
# given the number of plots, number of survey years, the size of the decline,
# and how variable the sites are. It answers the question clients actually ask:
# "How many plots and years do we need to be confident we'd catch a real
# decline?" — the core of defensible monitoring design.
#
# Pure simulation — no external data, no API calls. Uses only base R + light
# packages, so it deploys as a static, in-browser app via shinylive
# (no server, no cold starts, no app-count limits):
#   install.packages("shinylive")
#   shinylive::export(".", "site_shinylive")   # then host the static output
#
# SETUP (run once in console before launching locally):
#   install.packages(c("shiny","bslib","ggplot2"))
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)

# ── Core simulation ───────────────────────────────────────────────────────────
# Simulate counts at `n_plots` plots over `n_years`, with a multiplicative
# annual change (decline), lognormal among-plot variability, and Poisson
# observation noise. Fit a log-linear trend and test for a negative slope.
# Returns power = proportion of simulations detecting a significant decline.
sim_power <- function(n_plots, n_years, pct_change, baseline_mean,
                      among_cv, alpha = 0.05, n_sim = 300) {
  # Annual multiplicative rate that yields the target % change over the period
  total_ratio <- 1 + pct_change / 100                 # e.g. -30% -> 0.70
  annual_rate <- total_ratio^(1 / (n_years - 1))
  years <- seq_len(n_years) - 1
  sd_log <- sqrt(log(1 + among_cv^2))                 # lognormal sd for given CV

  detect <- replicate(n_sim, {
    # Per-plot baseline multipliers (among-plot variability)
    plot_mult <- rlnorm(n_plots, meanlog = -0.5 * sd_log^2, sdlog = sd_log)
    df <- expand.grid(plot = seq_len(n_plots), year = years)
    lambda <- baseline_mean * plot_mult[df$plot] * annual_rate^df$year
    df$count <- rpois(nrow(df), lambda = pmax(lambda, 1e-6))
    # Log-linear trend test
    fit <- try(glm(count ~ year, family = poisson(), data = df), silent = TRUE)
    if (inherits(fit, "try-error")) return(FALSE)
    co <- summary(fit)$coefficients
    if (!"year" %in% rownames(co)) return(FALSE)
    slope <- co["year", "Estimate"]
    p     <- co["year", "Pr(>|z|)"]
    (p < alpha) && (slope < 0)
  })
  mean(detect)
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Monitoring Design — Trend Detection Power",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#0d9488",
                   heading_font = font_google("Space Grotesk"),
                   base_font = font_google("Inter")) |>
    bs_add_variables("border-radius" = "0", "border-radius-lg" = "0",
                     "border-radius-sm" = "0"),
  sidebar = sidebar(
    width = 330,
    sliderInput("pct", "Decline to detect (% over study)",
                min = -60, max = -5, value = -30, step = 5),
    sliderInput("years", "Number of survey years",
                min = 3, max = 20, value = 10, step = 1),
    sliderInput("baseline", "Typical count per plot (baseline)",
                min = 2, max = 50, value = 12, step = 1),
    sliderInput("cv", "Among-plot variability (CV)",
                min = 0.2, max = 2.0, value = 0.8, step = 0.1),
    sliderInput("alpha", "Significance level (alpha)",
                min = 0.01, max = 0.10, value = 0.05, step = 0.01),
    hr(),
    helpText("Power is estimated by simulation. The curve shows how detection",
             "power rises with the number of plots; 80% is the usual target.")
  ),
  layout_columns(
    col_widths = c(12),
    layout_columns(
      col_widths = c(6, 6),
      value_box("Plots for 80% power", textOutput("n_needed"), theme = "primary"),
      value_box("Power at selected plots", textOutput("power_at"),
                theme = value_box_theme(bg = "#f8fafb", fg = "#0a0a0a"))
    )
  ),
  card(full_screen = TRUE,
       card_header("Power vs. number of monitoring plots"),
       plotOutput("curve", height = 380)),
  card(card_header("Read it at a glance"),
       sliderInput("plots_focus", "Plots in your design",
                   min = 5, max = 100, value = 30, step = 5),
       textOutput("interpretation"))
)

# ── Server ──────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  plot_grid <- reactive(seq(5, 100, by = 5))

  power_curve <- reactive({
    g <- plot_grid()
    pw <- vapply(g, function(np) {
      sim_power(n_plots = np, n_years = input$years, pct_change = input$pct,
                baseline_mean = input$baseline, among_cv = input$cv,
                alpha = input$alpha, n_sim = 250)
    }, numeric(1))
    data.frame(plots = g, power = pw)
  })

  output$curve <- renderPlot({
    d <- power_curve()
    hit <- d$plots[which(d$power >= 0.8)[1]]
    ggplot(d, aes(plots, power)) +
      geom_hline(yintercept = 0.8, linetype = "dashed", colour = "#6b7280") +
      geom_line(colour = "#0d9488", linewidth = 1.2) +
      geom_point(colour = "#0d9488", size = 2) +
      { if (!is.na(hit)) geom_vline(xintercept = hit, colour = "#0a0a0a",
                                    linetype = "dotted") } +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(x = "Number of plots", y = "Power to detect the decline",
           caption = "Dashed line = 80% power target") +
      theme_minimal(base_size = 14)
  })

  output$n_needed <- renderText({
    d <- power_curve()
    hit <- d$plots[which(d$power >= 0.8)[1]]
    if (is.na(hit)) ">100" else as.character(hit)
  })

  output$power_at <- renderText({
    p <- sim_power(n_plots = input$plots_focus, n_years = input$years,
                   pct_change = input$pct, baseline_mean = input$baseline,
                   among_cv = input$cv, alpha = input$alpha, n_sim = 300)
    paste0(round(p * 100), "%")
  })

  output$interpretation <- renderText({
    p <- sim_power(n_plots = input$plots_focus, n_years = input$years,
                   pct_change = input$pct, baseline_mean = input$baseline,
                   among_cv = input$cv, alpha = input$alpha, n_sim = 300)
    verdict <- if (p >= 0.8) "well-powered" else
               if (p >= 0.6) "borderline — consider more plots or years" else
               "underpowered — a real decline could easily go undetected"
    paste0("With ", input$plots_focus, " plots surveyed over ", input$years,
           " years, you have about ", round(p * 100),
           "% power to detect a ", abs(input$pct),
           "% decline. This design is ", verdict, ".")
  })
}

shinyApp(ui, server)
