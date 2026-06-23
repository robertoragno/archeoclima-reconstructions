# Per-century decomposition of theta[t] into CHELSA baseline, AR(1) persistence,
# and stochastic innovation. Uses existing fits; no resampling needed.

library(here)
library(tidyverse)
library(ggdist)
library(patchwork)

out_tables  <- here("outputs", "tables")
out_figures <- here("outputs", "figures")

# --- Decompose one fit -------------------------------------------------------

decompose_fit <- function(results, climate_var) {

  fit        <- results$fit
  chelsa_mean <- results$stan_input$chelsa_full$chelsa_mean
  centuries   <- results$stan_input$all_centuries
  N           <- length(centuries)

  # Extract posterior draws
  theta_draws    <- rstan::extract(fit, pars = "theta")$theta            # [S x N]
  tc_draws       <- rstan::extract(fit, pars = "theta_centered")$theta_centered  # [S x N]
  rho_draws      <- rstan::extract(fit, pars = "rho")$rho                # [S]
  sigma_draws    <- rstan::extract(fit, pars = "sigma_innovation")$sigma_innovation  # [S]

  S <- length(rho_draws)

  # Compute the three components per draw per century
  # t=1: no persistence term
  baseline_mat    <- matrix(rep(chelsa_mean, each = S), nrow = S, ncol = N)
  innovation_mat  <- sweep(tc_draws, 1, sigma_draws, `*`)

  # Persistence: rho * (theta[t-1] - chelsa_mean[t-1]), zero for t=1
  persistence_mat <- matrix(0, nrow = S, ncol = N)
  for (t in 2:N) {
    persistence_mat[, t] <- rho_draws * (theta_draws[, t-1] - chelsa_mean[t-1])
  }

  # Verification: components should sum to theta (up to floating point)
  recon_check <- baseline_mat + persistence_mat + innovation_mat
  max_err <- max(abs(recon_check - theta_draws))
  message(sprintf("[%s] Max decomposition error: %.2e %s",
                  climate_var, max_err,
                  if (max_err < 1e-8) "[OK]" else "[CHECK]"))

  # Summarise each component per century
  summarise_component <- function(mat, name) {
    tibble(
      century          = centuries,
      component        = name,
      posterior_median = apply(mat, 2, median),
      q025             = apply(mat, 2, quantile, 0.025),
      q975             = apply(mat, 2, quantile, 0.975)
    )
  }

  bind_rows(
    summarise_component(baseline_mat,    "CHELSA baseline"),
    summarise_component(persistence_mat, "AR(1) persistence"),
    summarise_component(innovation_mat,  "Innovation")
  ) |>
    mutate(climate_var = climate_var)
}

# --- Run for both variables --------------------------------------------------

results_temp  <- readRDS(here("data", "processed", "results_temp.rds"))
results_precip <- readRDS(here("data", "processed", "results_precip.rds"))

decomp_temp   <- decompose_fit(results_temp,  "temperature")
decomp_precip <- decompose_fit(results_precip, "precipitation")

readr::write_csv(decomp_temp,
  file.path(out_tables, "decomposition_temperature.csv"))
readr::write_csv(decomp_precip,
  file.path(out_tables, "decomposition_precipitation.csv"))

# --- Wide summary table ------------------------------------------------------

make_wide_table <- function(decomp) {
  decomp |>
    select(century, component, posterior_median) |>
    pivot_wider(names_from = component,
                values_from = posterior_median,
                names_repair = "universal") |>
    rename_with(~ gsub("\\.", "_", .x)) |>
    rename_with(~ gsub(" ", "_", .x))
}

wide_temp   <- make_wide_table(decomp_temp)
wide_precip <- make_wide_table(decomp_precip)

readr::write_csv(wide_temp,
  file.path(out_tables, "decomposition_temperature_wide.csv"))
readr::write_csv(wide_precip,
  file.path(out_tables, "decomposition_precipitation_wide.csv"))

# --- Reconstruction subplot (panels A, C) ------------------------------------

make_recon_subplot <- function(results, cv) {
  recon    <- results$reconstruction
  post_col <- if (cv == "temperature") "#E69F00" else "skyblue4"
  y_lab    <- if (cv == "temperature") "Temperature anomaly (\u00B0C)"
               else "Precipitation anomaly (%)"
  title    <- if (cv == "temperature") "Temperature reconstruction"
               else "Precipitation reconstruction"

  ggplot(recon, aes(x = century)) +
    geom_hline(yintercept = 0, colour = "black", alpha = 0.3) +
    # Ribbon mapped to fill scale so it appears in legend
    geom_ribbon(aes(ymin = q025, ymax = q975,
                    fill = "Posterior median (shading: 95% CI)"),
                alpha = 0.22, colour = NA) +
    # CHELSA prior mapped to colour scale for legend entry
    geom_line(aes(y = chelsa_prior, colour = "CHELSA prior"),
              linetype = "dashed", linewidth = 0.9) +
    geom_point(aes(y = chelsa_prior, colour = "CHELSA prior"), size = 2.5) +
    # Posterior median mapped to colour scale
    geom_line(aes(y = posterior_median, colour = "Posterior median (shading: 95% CI)"),
              linewidth = 1.1) +
    # Filled circles sized by number of documentary sources (fixed colour, no legend)
    geom_point(data = ~ filter(.x, n_events > 0L),
               aes(y = posterior_median, size = n_events),
               colour = post_col) +
    # Hollow circles for data-sparse centuries (fixed colour, no legend)
    geom_point(data = ~ filter(.x, n_events == 0L),
               aes(y = posterior_median),
               shape = 21, size = 2.5,
               colour = post_col, fill = "white", stroke = 1.2) +
    # n_events labels \u2014 explained in figure caption, not plot.caption
    geom_text(data = ~ filter(.x, n_events > 0L),
              aes(y = posterior_median, label = n_events),
              nudge_y = if (cv == "temperature") 0.15 else 0.4,
              size = 2.8, colour = post_col) +
    scale_colour_manual(
      name   = NULL,
      values = c("CHELSA prior" = "grey50",
                 "Posterior median (shading: 95% CI)" = post_col)
    ) +
    scale_fill_manual(
      name   = NULL,
      values = c("Posterior median (shading: 95% CI)" = post_col)
    ) +
    scale_size_continuous(range = c(2, 5), guide = "none") +
    scale_x_continuous(breaks = seq(1000, 1800, by = 100)) +
    guides(
      colour = guide_legend(
        nrow = 1,
        override.aes = list(
          linetype  = c("dashed", "solid"),
          linewidth = c(0.9, 1.1),
          shape     = c(16, NA),
          fill      = c(NA, NA)
        )
      ),
      fill = "none"
    ) +
    labs(x = "Century", y = y_lab, title = title) +
    theme_tidybayes() +
    theme(
      text            = element_text(size = 14),
      plot.title      = element_text(face = "bold", size = 18),
      axis.text.x     = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.margin     = margin(8, 8, 8, 8)
    )
}

# --- Decomposition subplot (panels B, D) -------------------------------------

make_decomp_plot <- function(decomp, cv) {

  unit  <- if (cv == "temperature") "\u00B0C" else "%"
  y_lab <- if (cv == "temperature") "Temperature anomaly (\u00B0C)"
           else "Precipitation anomaly (%)"
  title <- if (cv == "temperature") "Temperature decomposition"
           else "Precipitation decomposition"

  ggplot(decomp, aes(x = century, y = posterior_median,
                     colour = component, fill = component)) +
    geom_hline(yintercept = 0, colour = "black", alpha = 0.3) +
    geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.18, colour = NA) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.5) +
    scale_x_continuous(breaks = seq(1000, 1800, by = 100)) +
    scale_colour_manual(values = c("CHELSA baseline"   = "#AAAAAA",
                                   "AR(1) persistence" = "#0072B2",
                                   "Innovation"        = "#D55E00")) +
    scale_fill_manual(  values = c("CHELSA baseline"   = "#AAAAAA",
                                   "AR(1) persistence" = "#0072B2",
                                   "Innovation"        = "#D55E00")) +
    labs(x = "Century", y = y_lab, title = title,
         colour = NULL, fill = NULL,
         subtitle = "Components sum to \u03b8 at floating-point precision.") +
    guides(
      colour = guide_legend(
        nrow = 1,
        override.aes = list(
          fill      = NA,
          linewidth = 1.0,
          size      = 3
        )
      ),
      fill = "none"
    ) +
    theme_tidybayes() +
    theme(
      plot.title      = element_text(face = "bold", size = 18),
      plot.subtitle   = element_text(size = 11, colour = "grey45"),
      legend.position = "bottom",
      axis.text.x     = element_text(angle = 45, hjust = 1),
      text            = element_text(size = 14),
      plot.margin     = margin(10, 10, 10, 10)
    )
}

p_temp_decomp   <- make_decomp_plot(decomp_temp,   "temperature")
p_precip_decomp <- make_decomp_plot(decomp_precip, "precipitation")

p_recon_t <- make_recon_subplot(results_temp,   "temperature")
p_recon_p <- make_recon_subplot(results_precip, "precipitation")

# 2x2: (reconstruction | decomposition) for each climate variable
p_decomp_combined <- ((p_recon_t | p_temp_decomp) / (p_recon_p | p_precip_decomp)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 14))

ggsave(file.path(out_figures, "decomposition_combined.png"),
       p_decomp_combined, width = 18, height = 14, dpi = 300, bg = "white")

message("\nDecomposition tables saved: outputs/tables/decomposition_*.csv")
message("Decomposition figures saved: outputs/figures/decomposition_*.png")

# --- Print for inspection ----------------------------------------------------

message("\nTemperature decomposition (posterior medians):")
print(wide_temp, digits = 3)

message("\nPrecipitation decomposition (posterior medians):")
print(wide_precip, digits = 3)
