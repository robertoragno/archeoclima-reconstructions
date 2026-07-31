# Sensitivity analysis: how posteriors change as assumed CHELSA uncertainty
# (chelsa_sd) varies over the reviewer-requested grid. Uses
# stan/centennial_model_sens.stan; centennial_model.stan is not touched.

library(here)
library(rstan)
library(tidyverse)
library(ggdist)
library(patchwork)

source(here("R", "03_model_prep.R"))  # prepare_chelsa, prepare_documentary, build_stan_data

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# --- Grid --------------------------------------------------------------------

grid <- list(
  temperature   = c(0.25, 0.50, 1.00),
  precipitation = c(2.5,  5.0,  10.0)
)

chains <- 4L
iter   <- 2000L

sens_stan <- here("stan", "centennial_model_sens.stan")
pfister_csv <- here("data", "processed", "pfister_coded.csv")
chelsa_csv  <- here("data", "processed", "chelsa_climate.csv")
out_tables  <- here("outputs", "tables")
out_figures <- here("outputs", "figures")

climate_data <- readr::read_csv(chelsa_csv, show_col_types = FALSE)
pfister_data <- readr::read_csv(pfister_csv, show_col_types = FALSE)

# --- Run one grid cell -------------------------------------------------------

run_sens_cell <- function(climate_var, chelsa_sd_value) {

  label <- sprintf("%s_sd%s",
                   climate_var,
                   gsub("\\.", "", formatC(chelsa_sd_value, format = "f", digits = 2)))

  message("\n", strrep("=", 60))
  message(sprintf("  Sensitivity: %s | chelsa_sd = %.2f", climate_var, chelsa_sd_value))
  message(strrep("=", 60))

  chelsa_prep <- prepare_chelsa(climate_data, climate_var) |>
    mutate(chelsa_sd = chelsa_sd_value)   # override hard-coded sd

  doc_prep    <- prepare_documentary(pfister_data, climate_var)
  stan_input  <- build_stan_data(chelsa_prep, doc_prep)

  fit <- stan(
    file    = sens_stan,
    data    = stan_input$stan_data,
    chains  = chains,
    iter    = iter,
    warmup  = iter %/% 2L,
    seed    = 20260317L,
    control = list(adapt_delta = 0.95),
    verbose = FALSE
  )

  # diagnostics
  rhat_max <- max(summary(fit)$summary[, "Rhat"], na.rm = TRUE)
  message(sprintf("  Max Rhat: %.4f %s", rhat_max, if (rhat_max < 1.05) "[OK]" else "[WARNING]"))

  # summaries only — full fit not saved
  theta_samp  <- rstan::extract(fit, pars = "theta")$theta
  sigma_samp  <- rstan::extract(fit, pars = "sigma_innovation")$sigma_innovation

  recon <- tibble(
    climate_var      = climate_var,
    chelsa_sd_value  = chelsa_sd_value,
    century          = stan_input$all_centuries,
    chelsa_prior     = stan_input$chelsa_full$chelsa_mean,
    posterior_median = apply(theta_samp, 2, median),
    posterior_mean   = apply(theta_samp, 2, mean),
    posterior_sd     = apply(theta_samp, 2, sd),
    q025             = apply(theta_samp, 2, quantile, 0.025),
    q975             = apply(theta_samp, 2, quantile, 0.975)
  )

  sigma_summary <- tibble(
    climate_var      = climate_var,
    chelsa_sd_value  = chelsa_sd_value,
    sigma_median     = median(sigma_samp),
    sigma_q025       = quantile(sigma_samp, 0.025),
    sigma_q975       = quantile(sigma_samp, 0.975)
  )

  readr::write_csv(recon,
    file.path(out_tables, paste0("sensitivity_", label, ".csv")))

  list(recon = recon, sigma = sigma_summary)
}

# --- Run all grid cells ------------------------------------------------------

all_recon  <- list()
all_sigma  <- list()

for (cv in names(grid)) {
  for (sd_val in grid[[cv]]) {
    result <- run_sens_cell(cv, sd_val)
    key    <- paste(cv, sd_val, sep = "_")
    all_recon[[key]] <- result$recon
    all_sigma[[key]] <- result$sigma
  }
}

recon_all  <- bind_rows(all_recon)
sigma_all  <- bind_rows(all_sigma)

readr::write_csv(sigma_all,
  file.path(out_tables, "sensitivity_sigma_innovation.csv"))

# --- Comparison figure -------------------------------------------------------

library(ggdist)
library(tidybayes)

make_sens_plot <- function(cv) {

  dat <- recon_all |>
    filter(climate_var == cv) |>
    mutate(sd_label = factor(
      paste0(chelsa_sd_value, if (cv == "temperature") " \u00B0C" else "%"),
      levels = paste0(grid[[cv]], if (cv == "temperature") " \u00B0C" else "%")
    ))

  chelsa_ref <- dat |> distinct(century, chelsa_prior)

  y_lab  <- if (cv == "temperature") "Temperature anomaly (\u00B0C)"
            else "Precipitation anomaly (%)"
  title  <- if (cv == "temperature") "Temperature sensitivity"
            else "Precipitation sensitivity"

  ggplot(dat, aes(x = century, y = posterior_median,
                  colour = sd_label, fill = sd_label)) +
    geom_hline(yintercept = 0, colour = "black", alpha = 0.3) +
    geom_line(data = chelsa_ref, aes(x = century, y = chelsa_prior),
              colour = "grey50", linetype = "dashed", linewidth = 1,
              inherit.aes = FALSE) +
    geom_point(data = chelsa_ref, aes(x = century, y = chelsa_prior),
               colour = "grey50", size = 3, inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.18, colour = NA) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.5) +
    scale_x_continuous(breaks = seq(1000, 1800, by = 100)) +
    scale_colour_manual(values = c("#2166ac", "#f4a582", "#b2182b"),
                        name = "Assumed CHELSA s.d.") +
    scale_fill_manual(  values = c("#2166ac", "#f4a582", "#b2182b"),
                        name = "Assumed CHELSA s.d.") +
    labs(x = "Century", y = y_lab, title = title,
         caption = "Grey dashed: CHELSA-TraCE prior. Shading: 95% credible intervals.") +
    guides(colour = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
    theme_tidybayes() +
    theme(
      plot.title      = element_text(face = "bold", size = 14),
      legend.position = "bottom",
      axis.text.x     = element_text(angle = 45, hjust = 1),
      plot.caption    = element_text(size = 8, hjust = 0),
      plot.margin     = margin(10, 10, 10, 10)
    )
}

p_temp   <- make_sens_plot("temperature")
p_precip <- make_sens_plot("precipitation")

p_combined <- (p_temp / p_precip) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 14))

ggsave(file.path(out_figures, "sensitivity_comparison.png"),
       p_combined, width = 10, height = 16, dpi = 300, bg = "white")

message("\nSensitivity figures saved: outputs/figures/sensitivity_comparison.png")

# --- Summary table -----------------------------------------------------------

sens_table <- recon_all |>
  select(climate_var, chelsa_sd_value, century, posterior_median, q025, q975) |>
  left_join(sigma_all, by = c("climate_var", "chelsa_sd_value"))

readr::write_csv(sens_table,
  file.path(out_tables, "sensitivity_summary.csv"))

message("\nSensitivity table saved: outputs/tables/sensitivity_summary.csv")
message("\nIdentification check (sigma_innovation across grid):")
print(sigma_all, digits = 3)
