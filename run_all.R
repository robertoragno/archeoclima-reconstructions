# ==============================================================================
# run_all.R
#
# Master script: runs the full ArcheoClima reconstruction pipeline.
#
# Prerequisites:
#   1. renv::restore() to install all required packages
#   2. CHELSA-TraCE21k rasters downloaded and path set in R/02_rasters.R
#   3. data/raw/Tbl_Principale.csv present
#
# Outputs: data/processed/, outputs/figures/, outputs/tables/
# ==============================================================================

required_packages <- c(
  "here"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    renv::install(pkg)
  }
}

library(here)

# Ensure output directories exist
for (d in c(
  here("data", "processed"),
  here("outputs", "figures"),
  here("outputs", "tables")
)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ==============================================================================
# Step 1: Pfister coding of documentary events
# ==============================================================================

message("\n[1/5] Coding documentary events on the Pfister scale...")
source(here("R", "01_pfister_prep.R"))

# ==============================================================================
# Step 2: CHELSA raster processing
#
# Skip if pre-processed file already exists (rasters are large and slow).
# ==============================================================================

chelsa_csv <- here("data", "processed", "chelsa_climate.csv")

if (file.exists(chelsa_csv)) {
  message("\n[2/5] Loading pre-processed CHELSA data from file...")
  climate_data <- readr::read_csv(chelsa_csv, show_col_types = FALSE)
} else {
  message("\n[2/5] Processing CHELSA rasters (this may take a few minutes)...")
  source(here("R", "02_rasters.R"))
  readr::write_csv(climate_data, chelsa_csv)
  message("Saved: ", chelsa_csv)
}

# ==============================================================================
# Step 3: Load model functions
# ==============================================================================

message("\n[3/5] Loading model helper functions...")
source(here("R", "03_model_prep.R"))
source(here("R", "05_figures_model.R"))

# ==============================================================================
# Step 4: Fit models
# ==============================================================================

pfister_csv <- here("data", "processed", "pfister_coded.csv")

# ── Temperature ───────────────────────────────────────────────────────────────

message("\n[4/5] Fitting temperature reconstruction...")
results_temp <- run_model(
  climate_data = climate_data,
  pfister_file = pfister_csv,
  climate_var  = "temperature",
  chains = 4L,
  iter   = 2000L
)
saveRDS(results_temp, here("data", "processed", "results_temp.rds"))

# ── Precipitation ─────────────────────────────────────────────────────────────

message("\nFitting precipitation reconstruction...")
results_precip <- run_model(
  climate_data = climate_data,
  pfister_file = pfister_csv,
  climate_var  = "precipitation",
  chains = 4L,
  iter   = 2000L
)
saveRDS(results_precip, here("data", "processed", "results_precip.rds"))

# ==============================================================================
# Step 5: Figures and tables
# ==============================================================================

message("\n[5/5] Generating figures...")

# ── Database figures ───────────────────────────────────────────────────────────
source(here("R", "04_figures_database.R"))

# ── Model figures ─────────────────────────────────────────────────────────────

save_model_figures <- function(results, climate_var) {
  plots   <- plot_model_results(results, climate_var)
  fig_dir <- here("outputs", "figures")

  ggsave(file.path(fig_dir, paste0(climate_var, "_reconstruction.png")),
         plots$main,  width = 10, height = 8, dpi = 300, bg = "white")
  ggsave(file.path(fig_dir, paste0(climate_var, "_chelsa_shift.png")),
         plots$shift, width = 10, height = 8, dpi = 300, bg = "white")
  if (!is.null(plots$rho)) {
    ggsave(file.path(fig_dir, paste0(climate_var, "_rho_posterior.png")),
           plots$rho, width = 8, height = 5, dpi = 300, bg = "white")
  }

  readr::write_csv(
    results$reconstruction,
    here("outputs", "tables", paste0(climate_var, "_reconstruction.csv"))
  )
}

save_model_figures(results_temp,  "temperature")
save_model_figures(results_precip, "precipitation")

# ==============================================================================
# Summary: rho comparison across models
# ==============================================================================

message("\n", strrep("=", 60))
message("  rho comparison: temperature vs precipitation")
message(strrep("=", 60))

for (nm in c("temperature", "precipitation")) {
  res <- if (nm == "temperature") results_temp else results_precip
  rho <- res$rho_posterior
  message(sprintf("\n%s model:", stringr::str_to_title(nm)))
  message(sprintf("  rho median: %.3f [%.3f, %.3f]",
                  median(rho), quantile(rho, 0.025), quantile(rho, 0.975)))
}

message("\nPrior: Beta(3, 2) — mean 0.60, mode 0.67")
message("Note: posteriors close to the prior indicate that 9 time points",
        "\nprovide limited information about rho. Report prior and posterior together.")

message("\n", strrep("=", 60))
message("  Pipeline complete. Outputs in outputs/")
message(strrep("=", 60))
