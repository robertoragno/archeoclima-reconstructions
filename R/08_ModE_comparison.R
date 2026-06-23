# Compare the Bayesian posterior against ModE-RA and ModE-Sim (Valler et al. 2022).
# All series re-baselined to their own 1500-1800 mean before comparison.
# Sourcing helpers/mode_processing.R loads (or rebuilds) the centennial means.

library(here)
library(rstan)
library(tidyverse)
library(ggdist)
library(tidybayes)
library(patchwork)

source(here("R", "helpers", "mode_processing.R"))

out_tables  <- here("outputs", "tables")
out_figures <- here("outputs", "figures")

REF_CENTS <- c(1500L, 1600L, 1700L, 1800L)

# --- Load posterior draws and CHELSA -----------------------------------------

results_t <- readRDS(here("data", "processed", "results_temp.rds"))
results_p <- readRDS(here("data", "processed", "results_precip.rds"))

theta_t <- rstan::extract(results_t$fit, pars = "theta")$theta   # [4000 x 9]
theta_p <- rstan::extract(results_p$fit, pars = "theta")$theta

centuries    <- results_t$reconstruction$century
chelsa_t_nom <- results_t$reconstruction$chelsa_prior    # deg C vs 1000-1800
chelsa_p_nom <- results_p$reconstruction$chelsa_prior    # % vs 1000-1800

ref_idx <- which(centuries %in% REF_CENTS)

# --- Re-baseline temperature (all -> deg C vs 1500-1800) ---------------------

ra_ref_t   <- mean(ra_temp$value_cent[ra_temp$century %in% REF_CENTS])
ra_temp_rb <- ra_temp |>
  left_join(ra_tstd |> select(century, std_cent), by = "century") |>
  mutate(value_rb = value_cent - ra_ref_t,
         lower_rb = value_rb - std_cent,
         upper_rb = value_rb + std_cent)

sim_temp_c  <- sim_temp |> mutate(value_c = value_cent - 273.15)
sim_ref_t   <- mean(sim_temp_c$value_c[sim_temp_c$century %in% REF_CENTS])
sim_temp_rb <- sim_temp_c |>
  left_join(sim_tstd |> select(century, std_cent), by = "century") |>
  mutate(value_rb = value_c - sim_ref_t,
         lower_rb = value_rb - std_cent,
         upper_rb = value_rb + std_cent)

chelsa_ref_t <- mean(chelsa_t_nom[ref_idx])
chelsa_t_rb  <- chelsa_t_nom - chelsa_ref_t

post_ref_t  <- rowMeans(theta_t[, ref_idx])
theta_t_rb  <- theta_t - post_ref_t
post_t_med  <- apply(theta_t_rb, 2, median)
post_t_q025 <- apply(theta_t_rb, 2, quantile, 0.025)
post_t_q975 <- apply(theta_t_rb, 2, quantile, 0.975)

# --- Re-baseline precipitation (all -> % vs 1500-1800) -----------------------
# ModE-Sim absolute mean is the common denominator for % conversion.

sim_ref_p_abs <- mean(sim_prec$value_cent[sim_prec$century %in% REF_CENTS])
message(sprintf("ModE-Sim 1500-1800 precip reference: %.1f mm/yr", sim_ref_p_abs))

ra_ref_p   <- mean(ra_prec$value_cent[ra_prec$century %in% REF_CENTS])
ra_prec_rb <- ra_prec |>
  left_join(ra_pstd |> select(century, std_cent), by = "century") |>
  mutate(value_rb = (value_cent - ra_ref_p) / sim_ref_p_abs * 100,
         std_rb   = std_cent / sim_ref_p_abs * 100,
         lower_rb = value_rb - std_rb,
         upper_rb = value_rb + std_rb)

sim_prec_rb <- sim_prec |>
  left_join(sim_pstd |> select(century, std_cent), by = "century") |>
  mutate(value_rb = (value_cent - sim_ref_p_abs) / sim_ref_p_abs * 100,
         std_rb   = std_cent / sim_ref_p_abs * 100,
         lower_rb = value_rb - std_rb,
         upper_rb = value_rb + std_rb)

chelsa_ref_p <- mean(chelsa_p_nom[ref_idx])
chelsa_p_rb  <- chelsa_p_nom - chelsa_ref_p

post_ref_p  <- rowMeans(theta_p[, ref_idx])
theta_p_rb  <- theta_p - post_ref_p
post_p_med  <- apply(theta_p_rb, 2, median)
post_p_q025 <- apply(theta_p_rb, 2, quantile, 0.025)
post_p_q975 <- apply(theta_p_rb, 2, quantile, 0.975)

# --- RA vs Sim gap (small gap -> few regional proxies assimilated into RA) ----

ra_sim_gap <- inner_join(
  ra_temp_rb |> select(century, ra = value_rb),
  sim_temp_rb |> select(century, sim = value_rb),
  by = "century"
) |> mutate(gap_t = ra - sim) |>
  left_join(
    inner_join(
      ra_prec_rb |> select(century, ra_p = value_rb),
      sim_prec_rb |> select(century, sim_p = value_rb),
      by = "century"
    ) |> mutate(gap_p = ra_p - sim_p) |> select(century, gap_p),
    by = "century"
  )

message("\nModE-RA minus ModE-Sim gap:")
print(ra_sim_gap, digits = 3)

# --- Assemble long-format table ----------------------------------------------

mk_series <- function(cent, med, lo, hi, label, var) {
  tibble(century = cent, series = label, variable = var,
         median = med, lower = lo, upper = hi)
}

comp_temp <- bind_rows(
  mk_series(centuries, post_t_med, post_t_q025, post_t_q975, "Posterior", "temperature"),
  mk_series(centuries, chelsa_t_rb, NA_real_, NA_real_,       "CHELSA",   "temperature"),
  mk_series(ra_temp_rb$century, ra_temp_rb$value_rb,
            ra_temp_rb$lower_rb, ra_temp_rb$upper_rb,         "ModE-RA",  "temperature"),
  mk_series(sim_temp_rb$century, sim_temp_rb$value_rb,
            sim_temp_rb$lower_rb, sim_temp_rb$upper_rb,       "ModE-Sim", "temperature")
)

prec_cents <- centuries[centuries <= 1800L]
ref_p_idx  <- which(centuries <= 1800L)

comp_prec <- bind_rows(
  mk_series(prec_cents, post_p_med[ref_p_idx], post_p_q025[ref_p_idx],
            post_p_q975[ref_p_idx],                            "Posterior", "precipitation"),
  mk_series(prec_cents, chelsa_p_rb[ref_p_idx], NA_real_, NA_real_,
                                                               "CHELSA",    "precipitation"),
  ra_prec_rb |> filter(century <= 1800L) |>
    transmute(century, series = "ModE-RA", variable = "precipitation",
              median = value_rb, lower = lower_rb, upper = upper_rb),
  sim_prec_rb |> filter(century <= 1800L) |>
    transmute(century, series = "ModE-Sim", variable = "precipitation",
              median = value_rb, lower = lower_rb, upper = upper_rb)
)

comparison_all <- bind_rows(comp_temp, comp_prec)
readr::write_csv(comparison_all, file.path(out_tables, "ModE_comparison_long.csv"))

# --- Wide tables with per-century differences --------------------------------

make_wide <- function(comp, partial_flag_cents) {
  pivot_wider(comp |> select(century, series, median),
              names_from = series, values_from = median) |>
    mutate(partial_sim     = century %in% partial_flag_cents,
           diff_post_ra    = Posterior - `ModE-RA`,
           diff_post_sim   = Posterior - `ModE-Sim`,
           diff_chelsa_sim = CHELSA    - `ModE-Sim`)
}

wide_temp <- make_wide(comp_temp, sim_temp_rb$century[sim_temp_rb$partial])
wide_prec <- make_wide(comp_prec, sim_prec_rb$century[sim_prec_rb$partial])

readr::write_csv(wide_temp,   file.path(out_tables, "ModE_comparison_temperature_wide.csv"))
readr::write_csv(wide_prec,   file.path(out_tables, "ModE_comparison_precipitation_wide.csv"))
readr::write_csv(ra_sim_gap,  file.path(out_tables, "ModE_ra_sim_gap.csv"))

# --- Agreement metrics -------------------------------------------------------

agree <- function(x, y, label, var) {
  ok <- complete.cases(x, y)
  tibble(variable = var, comparison = label, n = sum(ok),
         pearson_r = cor(x[ok], y[ok]),
         mean_abs_diff = mean(abs(x[ok] - y[ok])))
}

metrics_all <- bind_rows(
  agree(wide_temp$Posterior, wide_temp$`ModE-RA`,  "Posterior vs ModE-RA",  "temperature (\u00B0C)"),
  agree(wide_temp$Posterior, wide_temp$`ModE-Sim`, "Posterior vs ModE-Sim", "temperature (\u00B0C)"),
  agree(wide_temp$CHELSA,    wide_temp$`ModE-Sim`, "CHELSA vs ModE-Sim",    "temperature (\u00B0C)"),
  agree(wide_prec$Posterior, wide_prec$`ModE-RA`,  "Posterior vs ModE-RA",  "precipitation (%)"),
  agree(wide_prec$Posterior, wide_prec$`ModE-Sim`, "Posterior vs ModE-Sim", "precipitation (%)"),
  agree(wide_prec$CHELSA,    wide_prec$`ModE-Sim`, "CHELSA vs ModE-Sim",    "precipitation (%)")
)

message("\nAgreement metrics (descriptive, n=5 overlap centuries):")
print(metrics_all, digits = 3)
readr::write_csv(metrics_all, file.path(out_tables, "ModE_comparison_metrics.csv"))

# --- Figure ------------------------------------------------------------------

MODE_COL_RA  <- "grey30"
MODE_COL_SIM <- "grey65"
CHELSA_COL   <- "grey50"

make_comp_plot <- function(cv, y_lab, title, post_col) {
  dat        <- comparison_all |> filter(variable == cv)
  dat_chelsa <- dat |> filter(series == "CHELSA")
  dat_other  <- dat |> filter(series != "CHELSA")

  ggplot(dat, aes(x = century, y = median, colour = series, fill = series)) +
    geom_hline(yintercept = 0, colour = "black", alpha = 0.3) +
    geom_ribbon(data = filter(dat_other, !is.na(lower)),
                aes(ymin = lower, ymax = upper), alpha = 0.12, colour = NA) +
    geom_line(data = dat_other, linewidth = 0.9) +
    geom_point(data = dat_other, size = 2.5) +
    geom_line(data = dat_chelsa, linetype = "dashed", linewidth = 0.8) +
    geom_point(data = dat_chelsa, shape = 21, size = 2, fill = "white", stroke = 1.0) +
    scale_x_continuous(breaks = seq(1000, 1800, by = 100)) +
    scale_colour_manual(values = c("Posterior" = post_col, "CHELSA" = CHELSA_COL,
                                   "ModE-RA" = MODE_COL_RA, "ModE-Sim" = MODE_COL_SIM),
                        name = NULL) +
    scale_fill_manual(  values = c("Posterior" = post_col, "CHELSA" = CHELSA_COL,
                                   "ModE-RA" = MODE_COL_RA, "ModE-Sim" = MODE_COL_SIM),
                        name = NULL) +
    labs(x = "Century", y = y_lab, title = title) +
    guides(colour = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
    theme_tidybayes() +
    theme(text = element_text(size = 14), plot.title = element_text(face = "bold", size = 18),
          legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin = margin(10, 10, 10, 10))
}

p_t <- make_comp_plot("temperature", "Temperature anomaly (\u00B0C)",
                      "Temperature anomalies: posterior vs. ModE-RA and ModE-Sim",
                      post_col = "#E69F00")
p_p <- make_comp_plot("precipitation", "Precipitation anomaly (%)",
                      "Precipitation anomalies: posterior vs. ModE-RA and ModE-Sim",
                      post_col = "skyblue4")

p_combined <- (p_t + p_p) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 16))

ggsave(file.path(out_figures, "ModE_comparison.png"),
       p_combined, width = 16, height = 8, dpi = 300, bg = "white")

message("Saved: outputs/figures/ModE_comparison.png")
message("Saved: outputs/tables/ModE_comparison_*.csv, ModE_ra_sim_gap.csv")

message("\nTemperature comparison (deg C, re-baselined):")
print(wide_temp, digits = 3)
message("\nPrecipitation comparison (%, re-baselined):")
print(wide_prec, digits = 3)
