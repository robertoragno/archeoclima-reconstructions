# ==============================================================================
# 05_figures_model.R
#
# Purpose: Visualise centennial climate reconstruction results.
#          Source this file to load plot_model_results(), then call it with
#          the output of run_model() from 03_model_prep.R.
#
# Functions exported:
#   plot_model_results(results, climate_var)
# ==============================================================================

library(ggdist)
library(tidybayes)
library(tidyverse)

#' Plot centennial climate reconstruction results.
#'
#' Produces three ggplot objects:
#'   $main  — posterior reconstruction vs CHELSA prior
#'   $shift — documentary shift away from CHELSA prior
#'   $rho   — AR(1) persistence parameter posterior vs prior
#'
#' @param results      List returned by run_model().
#' @param climate_var  `"temperature"` or `"precipitation"`.
#' @return Named list of ggplot objects.

plot_model_results <- function(results, climate_var = "temperature") {

  # Variable-specific labels and colours
  if (climate_var == "temperature") {
    y_lab         <- "Temperature anomaly (\u00B0C)"
    shift_y_lab   <- "Shift (\u00B0C)"
    title         <- "Reconstruction of temperature anomalies"
    shift_title   <- "Shift from CHELSA estimates (temperature)"
    shift_caption <- paste0(
      "Positive = sources suggest warmer conditions than CHELSA\n",
      "Negative = sources suggest colder conditions than CHELSA\n",
      "Hollow points = centuries without documentary evidence"
    )
    posterior_col <- "#E69F00"
  } else {
    y_lab         <- "Precipitation anomaly (%)"
    shift_y_lab   <- "Shift (%)"
    title         <- "Reconstruction of precipitation anomalies"
    shift_title   <- "Shift from CHELSA estimates (precipitation)"
    shift_caption <- paste0(
      "Positive = sources suggest wetter conditions than CHELSA\n",
      "Negative = sources suggest drier conditions than CHELSA\n",
      "Hollow points = centuries without documentary evidence"
    )
    posterior_col <- "skyblue4"
  }

  # Shared x-axis theme
  x_theme <- list(
    scale_x_continuous(breaks = seq(1000, 1800, by = 100)),
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )

  # ── Plot 1: Main reconstruction ────────────────────────────────────────────

  recon <- results$reconstruction |>
    rename(.value = posterior_median, .lower = q025, .upper = q975) |>
    mutate(has_events = n_events > 0L)

  p_main <- ggplot(recon, aes(x = century, y = .value)) +
    # CHELSA prior
    geom_line(aes(y = chelsa_prior),
              colour = "grey50", linetype = "dashed", linewidth = 1) +
    geom_point(aes(y = chelsa_prior), colour = "grey50", size = 3) +
    # Posterior ribbon + line
    geom_lineribbon(aes(ymin = .lower, ymax = .upper),
                    fill = posterior_col, alpha = 0.25,
                    colour = posterior_col, linewidth = 1.1) +
    # Points: filled if documentary evidence exists, hollow otherwise
    geom_point(data = filter(recon,  has_events), aes(size = n_events), colour = posterior_col) +
    geom_point(data = filter(recon, !has_events), shape = 21, size = 3,
               colour = posterior_col, fill = "white", stroke = 1.2) +
    geom_text(aes(label = n_events), nudge_y = 0.25, size = 3, colour = posterior_col) +
    geom_hline(yintercept = 0, colour = "black", alpha = 0.3) +
    x_theme +
    scale_size_continuous(range = c(1, 5), name = "Documentary\nevents") +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    labs(
      title   = title,
      x       = "Century",
      y       = y_lab,
      caption = paste0(
        "Grey dashed: CHELSA-TraCE prior | Coloured: posterior (updated by documentary sources)\n",
        "Numbers indicate documentary events per century"
      )
    ) +
    theme_tidybayes() +
    theme(
      plot.title      = element_text(face = "bold", size = 14),
      legend.position = "none",
      plot.caption    = element_text(size = 8, hjust = 0),
      plot.margin     = margin(10, 10, 20, 10)
    )

  # ── Plot 2: Shift from CHELSA ──────────────────────────────────────────────

  shift <- results$reconstruction |>
    rename(.value = shift_from_chelsa, .lower = shift_q025, .upper = shift_q975) |>
    mutate(has_events = n_events > 0L)

  shift_col <- "#bf4342"

  p_shift <- ggplot(shift, aes(x = century, y = .value)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_lineribbon(aes(ymin = .lower, ymax = .upper),
                    fill = shift_col, alpha = 0.3,
                    colour = shift_col, linewidth = 1.1) +
    geom_point(data = filter(shift,  has_events), aes(size = n_events), colour = shift_col) +
    geom_point(data = filter(shift, !has_events), shape = 21, size = 3,
               colour = shift_col, fill = "white", stroke = 1.2) +
    geom_text(aes(label = n_events), nudge_y = 0.25, size = 3, colour = shift_col) +
    x_theme +
    scale_size_continuous(range = c(2, 6), guide = "none") +
    labs(
      title    = shift_title,
      subtitle = "Informational contribution of documentary sources relative to the prior",
      x        = "Century",
      y        = shift_y_lab,
      caption  = shift_caption
    ) +
    theme_tidybayes() +
    theme(
      plot.title   = element_text(face = "bold", size = 13),
      plot.margin  = margin(10, 10, 20, 10),
      plot.caption = element_text(size = 8, hjust = 0)
    )

  # ── Plot 3: rho posterior vs prior ────────────────────────────────────────

  p_rho <- NULL
  if (!is.null(results$rho_posterior)) {
    rho_df    <- tibble(rho = results$rho_posterior)
    prior_df  <- tibble(rho = seq(0, 1, length.out = 300),
                        density = dbeta(rho, 3, 2))

    p_rho <- ggplot(rho_df, aes(x = rho)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 40, fill = "steelblue", alpha = 0.6) +
      geom_line(data = prior_df, aes(x = rho, y = density),
                colour = "grey30", linetype = "dashed", linewidth = 1) +
      labs(
        title    = expression(paste("Posterior of ", rho, " (AR(1) persistence)")),
        subtitle = "Dashed line: Beta(3, 2) prior",
        x        = expression(rho),
        y        = "Density"
      ) +
      theme_tidybayes() +
      theme(plot.title = element_text(face = "bold", size = 13))
  }

  list(main = p_main, shift = p_shift, rho = p_rho)
}
