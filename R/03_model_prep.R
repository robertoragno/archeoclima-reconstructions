# ==============================================================================
# 03_model_prep.R
#
# Purpose: Define helper functions for preparing input data and fitting the
#          centennial climate reconstruction model (Stan).
#
# The Stan model itself lives in stan/centennial_model.stan.
# Call run_model() from run_all.R; do not source this file directly.
# ==============================================================================

required_packages <- c(
  "here",
  "rstan",
  "tidyverse",
  "ggdist",
  "tidybayes"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    renv::install(pkg)
  }
}

library(here)
library(rstan)
library(tidyverse)
library(ggdist)
library(tidybayes)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# ==============================================================================
# 1. Prepare CHELSA-TraCE21k data for the model
# ==============================================================================

#' Reformat CHELSA centennial climate data for Stan.
#'
#' Converts midpoint years to century start years (e.g. 1050 → 1000) and
#' computes anomalies relative to the 1000–1800 CE mean.
#'
#' @param climate_data  Data frame with columns `year`, `temperature_C`,
#'                      `precipitation_mm` (as produced by 02_rasters.R).
#' @param climate_var   `"temperature"` or `"precipitation"`.
#' @return A data frame with columns `century`, `chelsa_mean` (anomaly),
#'         `chelsa_sd`.

prepare_chelsa <- function(climate_data, climate_var = "temperature") {

  # Convert midpoint years back to century start years
  chelsa <- climate_data |>
    mutate(century = floor((year - 50) / 100) * 100)

  if (climate_var == "temperature") {
    chelsa |>
      select(century, chelsa_raw = temperature_C) |>
      mutate(
        chelsa_mean = chelsa_raw - mean(chelsa_raw),  # anomaly relative to period mean
        chelsa_sd   = 0.5    # °C; reflects GCM output uncertainty
      )
  } else {
    chelsa |>
      select(century, chelsa_raw = precipitation_mm) |>
      mutate(
        chelsa_mean = (chelsa_raw - mean(chelsa_raw)) / mean(chelsa_raw) * 100,  # % anomaly
        chelsa_sd   = 5.0   # %; reflects GCM output uncertainty
      )
  }
}

# ==============================================================================
# 2. Aggregate documentary evidence by century
# ==============================================================================

#' Aggregate Pfister-coded documentary events to century level.
#'
#' @param pfister_data  Data frame as produced by 01_pfister_prep.R.
#' @param climate_var   `"temperature"` or `"precipitation"`.
#' @return A data frame with one row per observed century, containing
#'         `century`, `n_events`, `pfister_codes` (list-column), and
#'         `mean_pfister`.

prepare_documentary <- function(pfister_data, climate_var = "temperature") {

  pfister_col <- if (climate_var == "temperature") "Pfister_Cold_Warm" else "Pfister_Wet_Dry"

  pfister_data |>
    filter(!is.na(.data[[pfister_col]])) |>
    mutate(
      pfister_value = .data[[pfister_col]],
      pfister_code  = pfister_value + 4L,  # recode -3:3 → 1:7
      century       = floor(Start_date / 100) * 100
    ) |>
    group_by(century) |>
    summarise(
      n_events     = n(),
      pfister_codes = list(pfister_code),
      mean_pfister  = mean(pfister_value),
      .groups = "drop"
    )
}

# ==============================================================================
# 3. Build the Stan data list
# ==============================================================================

#' Combine CHELSA and documentary data into a Stan-ready list.
#'
#' @param chelsa_data       Output of `prepare_chelsa()`.
#' @param documentary_data  Output of `prepare_documentary()`.
#' @return A named list for passing to `rstan::stan()`, plus auxiliary tables.

build_stan_data <- function(chelsa_data, documentary_data) {

  all_centuries <- seq(1000, 1800, by = 100)  # 9 centuries

  chelsa_full <- left_join(
    tibble(century = all_centuries),
    chelsa_data,
    by = "century"
  )

  doc <- documentary_data |>
    mutate(century_idx = match(century, all_centuries)) |>
    filter(!is.na(century_idx)) |>
    arrange(century_idx)

  all_pfister <- unlist(doc$pfister_codes)

  stan_data <- list(
    N_centuries      = length(all_centuries),
    N_obs_centuries  = nrow(doc),
    century_idx      = doc$century_idx,
    total_events     = length(all_pfister),
    pfister_codes    = as.integer(all_pfister),
    events_count     = doc$n_events,
    chelsa_mean      = chelsa_full$chelsa_mean,
    chelsa_sd        = chelsa_full$chelsa_sd
  )

  message(sprintf(
    "Stan data: %d centuries | %d with observations | %d total events (mean %.1f/century)",
    stan_data$N_centuries, stan_data$N_obs_centuries,
    stan_data$total_events, mean(doc$n_events)
  ))

  list(
    stan_data     = stan_data,
    all_centuries = all_centuries,
    chelsa_full   = chelsa_full,
    documentary   = doc
  )
}

# ==============================================================================
# 4. Fit model and extract results
# ==============================================================================

#' Run the full centennial climate reconstruction pipeline.
#'
#' @param climate_data  Data frame from 02_rasters.R.
#' @param pfister_file  Path to the Pfister-coded CSV (01_pfister_prep.R output).
#' @param climate_var   `"temperature"` or `"precipitation"`.
#' @param chains        Number of MCMC chains (default 4).
#' @param iter          Total iterations per chain including warmup (default 2000).
#' @return A named list: `fit`, `reconstruction`, `cutpoints`, `ppc_coverage`,
#'         `rho_posterior`, `stan_input`, `documentary`.

run_model <- function(climate_data,
                      pfister_file,
                      climate_var = "temperature",
                      chains = 4L,
                      iter   = 2000L) {

  cli_h1 <- function(x) message("\n", strrep("=", 60), "\n  ", x, "\n", strrep("=", 60))
  cli_h2 <- function(x) message("\n-- ", x, " ", strrep("-", max(0, 55 - nchar(x))))

  cli_h1("CENTENNIAL CLIMATE RECONSTRUCTION")
  message("  Variable: ", climate_var)
  message("  Chains:   ", chains, " × ", iter, " iter (", iter / 2, " warmup)")

  cli_h2("Step 1: CHELSA-TraCE21k data")
  chelsa_prep <- prepare_chelsa(climate_data, climate_var)
  print(chelsa_prep)

  cli_h2("Step 2: Documentary evidence")
  doc_prep <- read_csv(pfister_file, show_col_types = FALSE) |>
    prepare_documentary(climate_var)
  print(doc_prep)

  cli_h2("Step 3: Assembling Stan data")
  stan_input <- build_stan_data(chelsa_prep, doc_prep)

  cli_h2("Step 4: Fitting Stan model (~5–10 min)")
  fit <- stan(
    file    = here("stan", "centennial_model.stan"),
    data    = stan_input$stan_data,
    chains  = chains,
    iter    = iter,
    warmup  = iter %/% 2L,
    control = list(adapt_delta = 0.95),
    verbose = FALSE
  )

  cli_h2("Step 5: Extracting posterior summaries")

  theta_samples  <- rstan::extract(fit, pars = "theta")$theta
  shift_samples  <- rstan::extract(fit, pars = "shift_from_chelsa")$shift_from_chelsa
  rho_samples    <- rstan::extract(fit, pars = "rho")$rho
  pfister_pred   <- rstan::extract(fit, pars = "pfister_pred")$pfister_pred
  cp_samples     <- rstan::extract(fit, pars = "cutpoints")$cutpoints

  reconstruction <- tibble(
    century          = stan_input$all_centuries,
    chelsa_prior     = stan_input$chelsa_full$chelsa_mean,
    posterior_median = apply(theta_samples, 2, median),
    posterior_mean   = apply(theta_samples, 2, mean),
    posterior_sd     = apply(theta_samples, 2, sd),
    q025             = apply(theta_samples, 2, quantile, 0.025),
    q975             = apply(theta_samples, 2, quantile, 0.975),
    shift_from_chelsa = apply(shift_samples, 2, median),
    shift_q025       = apply(shift_samples, 2, quantile, 0.025),
    shift_q975       = apply(shift_samples, 2, quantile, 0.975),
    n_events         = 0L
  )

  # Merge event counts
  reconstruction <- reconstruction |>
    left_join(select(doc_prep, century, n_events_doc = n_events), by = "century") |>
    mutate(n_events = coalesce(n_events_doc, 0L)) |>
    select(-n_events_doc)

  # ── Diagnostics ─────────────────────────────────────────────────────────────

  cli_h2("Diagnostics")

  fit_summary <- summary(fit)$summary
  rhat_max    <- max(fit_summary[, "Rhat"], na.rm = TRUE)
  message(sprintf("  Max Rhat: %.4f %s", rhat_max, if (rhat_max < 1.05) "[OK]" else "[WARNING]"))

  message(sprintf("  rho posterior: median %.3f [%.3f, %.3f]",
                  median(rho_samples),
                  quantile(rho_samples, 0.025),
                  quantile(rho_samples, 0.975)))
  message("  rho prior: Beta(3, 2) — mean 0.60, mode 0.67")

  # Posterior predictive coverage
  all_obs  <- unlist(doc_prep$pfister_codes)
  coverage <- vapply(seq_along(all_obs), function(i) {
    ci <- quantile(pfister_pred[, i], c(0.025, 0.975))
    all_obs[i] >= ci[1] & all_obs[i] <= ci[2]
  }, logical(1))
  ppc_coverage <- mean(coverage) * 100
  message(sprintf("  PPC 95%% coverage: %.1f%% %s",
                  ppc_coverage,
                  if (ppc_coverage >= 90 & ppc_coverage <= 98) "[OK]" else "[CHECK]"))

  cutpoints_df <- tibble(
    boundary = c("-3/-2", "-2/-1", "-1/0", "0/+1", "+1/+2", "+2/+3"),
    mean  = apply(cp_samples, 2, mean),
    sd    = apply(cp_samples, 2, sd),
    q025  = apply(cp_samples, 2, quantile, 0.025),
    q975  = apply(cp_samples, 2, quantile, 0.975)
  )

  message("\nPfister cutpoints (posterior):")
  print(cutpoints_df, digits = 2)

  list(
    fit            = fit,
    reconstruction = reconstruction,
    cutpoints      = cutpoints_df,
    ppc_coverage   = ppc_coverage,
    rho_posterior  = rho_samples,
    stan_input     = stan_input,
    documentary    = doc_prep
  )
}

