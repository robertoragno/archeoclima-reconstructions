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
    posterior_col <- "#E69F00"
  } else {
    y_lab         <- "Precipitation anomaly (%)"
    shift_y_lab   <- "Shift (%)"
    title         <- "Reconstruction of precipitation anomalies"
    shift_title   <- "Shift from CHELSA estimates (precipitation)"
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

  p_main <- ggplot(recon, aes(x = century)) +
    geom_hline(yintercept = 0, colour = "black", alpha = 0.3) +
    # Posterior ribbon mapped to fill for legend entry
    geom_ribbon(aes(ymin = .lower, ymax = .upper,
                    fill = "Posterior median (shading: 95% CI)"),
                alpha = 0.25, colour = NA) +
    # CHELSA prior mapped to colour for legend entry
    geom_line(aes(y = chelsa_prior, colour = "CHELSA-TraCE21k prior"),
              linetype = "dashed", linewidth = 1) +
    geom_point(aes(y = chelsa_prior, colour = "CHELSA-TraCE21k prior"), size = 3) +
    # Posterior line mapped to colour
    geom_line(aes(y = .value, colour = "Posterior median (shading: 95% CI)"),
              linewidth = 1.1) +
    # Points sized by n_events (fixed colour, no legend)
    geom_point(data = filter(recon,  has_events),
               aes(y = .value, size = n_events), colour = posterior_col) +
    geom_point(data = filter(recon, !has_events),
               aes(y = .value), shape = 21, size = 3,
               colour = posterior_col, fill = "white", stroke = 1.2) +
    geom_text(data = filter(recon, has_events),
              aes(y = .value, label = n_events),
              nudge_y = 0.25, size = 3, colour = posterior_col) +
    x_theme +
    scale_colour_manual(
      name   = NULL,
      values = c("CHELSA-TraCE21k prior" = "grey50",
                 "Posterior median (shading: 95% CI)" = posterior_col)
    ) +
    scale_fill_manual(
      name   = NULL,
      values = c("Posterior median (shading: 95% CI)" = posterior_col)
    ) +
    scale_size_continuous(range = c(1, 5), guide = "none") +
    guides(
      colour = guide_legend(
        nrow = 1,
        override.aes = list(
          linetype  = c("dashed", "solid"),
          linewidth = c(1, 1.1),
          shape     = c(16, NA),
          fill      = c(NA, NA)
        )
      ),
      fill = "none"
    ) +
    labs(title = title, x = "Century", y = y_lab, tag = "A") +
    theme_tidybayes() +
    theme(
      text            = element_text(size = 14),
      plot.title      = element_text(face = "bold", size = 18),
      plot.tag        = element_text(face = "bold", size = 16),
      legend.position = "bottom",
      plot.margin     = margin(10, 10, 10, 10)
    )

  # ── Plot 2: Shift from CHELSA ──────────────────────────────────────────────

  shift <- results$reconstruction |>
    rename(.value = shift_from_chelsa, .lower = shift_q025, .upper = shift_q975) |>
    mutate(has_events = n_events > 0L)

  shift_col <- "#bf4342"

  p_shift <- ggplot(shift, aes(x = century)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    # Ribbon mapped to fill for legend entry
    geom_ribbon(aes(ymin = .lower, ymax = .upper,
                    fill = "Posterior shift (shading: 95% CI)"),
                alpha = 0.3, colour = NA) +
    geom_line(aes(y = .value, colour = "Posterior shift (shading: 95% CI)"),
              linewidth = 1.1) +
    geom_point(data = filter(shift,  has_events),
               aes(y = .value, size = n_events), colour = shift_col) +
    geom_point(data = filter(shift, !has_events),
               aes(y = .value), shape = 21, size = 3,
               colour = shift_col, fill = "white", stroke = 1.2) +
    geom_text(data = filter(shift, has_events),
              aes(y = .value, label = n_events),
              nudge_y = 0.25, size = 3, colour = shift_col) +
    x_theme +
    scale_colour_manual(name = NULL, values = c("Posterior shift (shading: 95% CI)" = shift_col)) +
    scale_fill_manual(  name = NULL, values = c("Posterior shift (shading: 95% CI)" = shift_col)) +
    scale_size_continuous(range = c(2, 6), guide = "none") +
    guides(
      colour = guide_legend(
        nrow = 1,
        override.aes = list(
          fill      = NA,
          linewidth = 1.1,
          shape     = NA
        )
      ),
      fill = "none"
    ) +
    labs(
      title    = shift_title,
      subtitle = "Informational contribution of documentary sources relative to the prior",
      x        = "Century",
      y        = shift_y_lab,
      tag      = "B"
    ) +
    theme_tidybayes() +
    theme(
      text            = element_text(size = 14),
      plot.title      = element_text(face = "bold", size = 18),
      plot.tag        = element_text(face = "bold", size = 16),
      legend.position = "bottom",
      plot.margin     = margin(10, 10, 10, 10)
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
      theme_tidybayes() + theme(text = element_text(size = 13)) +
      theme(plot.title = element_text(face = "bold", size = 13))
  }

  list(main = p_main, shift = p_shift, rho = p_rho)
}
