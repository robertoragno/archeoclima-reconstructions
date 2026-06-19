# ==============================================================================
# 05b_model_figures_only.R
#
# Figure-only companion to the run_all.R pipeline.
# Loads existing RDS fits and regenerates the four main model figures
# without re-running Stan.
#
# Run this after editing R/05_figures_model.R (colours, font sizes, labels).
# ==============================================================================

library(here)
source(here("R", "05_figures_model.R"))

fig_dir <- here("outputs", "figures")

save_model_figures <- function(results, climate_var) {
  plots <- plot_model_results(results, climate_var)
  ggsave(file.path(fig_dir, paste0(climate_var, "_reconstruction.png")),
         plots$main,  width = 10, height = 8, dpi = 300, bg = "white")
  ggsave(file.path(fig_dir, paste0(climate_var, "_chelsa_shift.png")),
         plots$shift, width = 10, height = 8, dpi = 300, bg = "white")
  if (!is.null(plots$rho)) {
    ggsave(file.path(fig_dir, paste0(climate_var, "_rho_posterior.png")),
           plots$rho,  width = 8,  height = 5, dpi = 300, bg = "white")
  }
  message("Saved: ", climate_var, " figures")
}

results_temp   <- readRDS(here("data", "processed", "results_temp.rds"))
results_precip <- readRDS(here("data", "processed", "results_precip.rds"))

save_model_figures(results_temp,   "temperature")
save_model_figures(results_precip, "precipitation")

message("Done. Check outputs/figures/")
