# ==============================================================================
# 06b_sensitivity_figures.R
#
# Figure-only companion to 06_sensitivity.R.
# Reads the per-run CSVs saved by 06_sensitivity.R and regenerates
# outputs/figures/sensitivity_comparison.png without re-running Stan.
#
# Run this when you want to tweak the figure without waiting ~20 min for Stan.
# ==============================================================================

library(here)
library(tidyverse)
library(ggdist)
library(tidybayes)
library(patchwork)

out_tables  <- here("outputs", "tables")
out_figures <- here("outputs", "figures")

grid <- list(
  temperature   = c(0.25, 0.50, 1.00),
  precipitation = c(2.5,  5.0,  10.0)
)

# ── Load saved per-run CSVs ──────────────────────────────────────────────────

load_sensitivity_csvs <- function(cv, sd_vals) {
  purrr::map_dfr(sd_vals, function(sd_val) {
    label <- sprintf("%s_sd%s", cv,
                     gsub("\\.", "", formatC(sd_val, format = "f", digits = 2)))
    path  <- file.path(out_tables, paste0("sensitivity_", label, ".csv"))
    readr::read_csv(path, show_col_types = FALSE)
  })
}

recon_all <- bind_rows(
  load_sensitivity_csvs("temperature",   grid$temperature),
  load_sensitivity_csvs("precipitation", grid$precipitation)
)

# ── Figure ───────────────────────────────────────────────────────────────────

# Each variable gets its canonical color; SD levels are separated into facets
# so panels are uncluttered and directly comparable across uncertainty levels.
make_sens_plot <- function(cv, post_col) {

  dat <- recon_all |>
    filter(climate_var == cv) |>
    mutate(sd_label = factor(
      paste0("s.d. = ", chelsa_sd_value,
             if (cv == "temperature") " \u00B0C" else "%"),
      levels = paste0("s.d. = ", grid[[cv]],
                      if (cv == "temperature") " \u00B0C" else "%")
    ))

  # chelsa_ref has no sd_label column -> ggplot repeats it in every facet
  chelsa_ref <- dat |> distinct(century, chelsa_prior)

  y_lab <- if (cv == "temperature") "Temperature anomaly (\u00B0C)"
           else "Precipitation anomaly (%)"
  title <- if (cv == "temperature") "Temperature: sensitivity to assumed CHELSA uncertainty"
           else "Precipitation: sensitivity to assumed CHELSA uncertainty"

  ggplot(dat, aes(x = century, y = posterior_median)) +
    geom_hline(yintercept = 0, colour = "black", alpha = 0.3) +
    geom_line(data = chelsa_ref, aes(x = century, y = chelsa_prior),
              colour = "grey50", linetype = "dashed", linewidth = 1,
              inherit.aes = FALSE) +
    geom_point(data = chelsa_ref, aes(x = century, y = chelsa_prior),
               colour = "grey50", size = 2.5, inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = q025, ymax = q975),
                fill = post_col, alpha = 0.22, colour = NA) +
    geom_line(colour = post_col, linewidth = 1.0) +
    geom_point(colour = post_col, size = 2.5) +
    facet_wrap(~ sd_label, nrow = 1) +
    scale_x_continuous(breaks = seq(1000, 1800, by = 200)) +
    scale_y_continuous(
      breaks = if (cv == "temperature")
        function(x) seq(floor(min(x) * 2) / 2, ceiling(max(x) * 2) / 2, by = 0.5)
      else
        scales::pretty_breaks(n = 6)
    ) +
    labs(x = "Century", y = y_lab, title = title,
         caption = paste0(
           "Grey dashed: CHELSA-TraCE21k prior. Shading: 95% credible interval.\n",
           "Each facet shows the posterior under a different assumed simulation uncertainty."
         )) +
    theme_tidybayes() +
    theme(
      text            = element_text(size = 13),
      plot.title      = element_text(face = "bold", size = 14),
      strip.text      = element_text(size = 12, face = "bold"),
      axis.text.x     = element_text(angle = 45, hjust = 1),
      plot.caption    = element_text(size = 8, hjust = 0),
      plot.margin     = margin(10, 10, 10, 10)
    )
}

p_temp   <- make_sens_plot("temperature",   post_col = "#E69F00")
p_precip <- make_sens_plot("precipitation", post_col = "skyblue4")

p_combined <- (p_temp / p_precip) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 14))

ggsave(file.path(out_figures, "sensitivity_comparison.png"),
       p_combined, width = 13, height = 14, dpi = 300, bg = "white")

message("Saved: outputs/figures/sensitivity_comparison.png")
