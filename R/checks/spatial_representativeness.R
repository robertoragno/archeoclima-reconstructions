# Compare CHELSA-TraCE21k anomalies for the full study box vs the Puglia
# sub-region (R1 Major Comment 8). Uses pre-cropped rasters in
# data/processed/CHELSA_pre_cropped/. Output: outputs/tables/spatial_check.csv

library(here)
library(terra)
library(tidyverse)

raster_dir <- here("data", "processed", "CHELSA_pre_cropped")
out_dir    <- here("outputs", "tables")

ext_full   <- ext(13, 19, 39.5, 42)      # full study box
ext_puglia <- ext(15, 18.5, 39.8, 41.8) # Puglia sub-region

raster_mean <- function(path, extent) {
  r <- rast(path) |> crop(extent)
  global(r, fun = "mean", na.rm = TRUE)[1, 1]
}

time_id_to_century <- function(id) 1000L + (id - 11L) * 100L  # IDs 11-19 -> 1000-1800


bio01_files <- sort(list.files(raster_dir, pattern = "bio01.*\\.tif$", full.names = TRUE))
bio12_files <- sort(list.files(raster_dir, pattern = "bio12.*\\.tif$", full.names = TRUE))

stopifnot("Need 9 bio01 files" = length(bio01_files) == 9L,
          "Need 9 bio12 files" = length(bio12_files) == 9L)

time_ids  <- as.integer(sub(".*bio01_(\\d+)_V.*", "\\1", basename(bio01_files)))
centuries <- time_id_to_century(time_ids)

message("Extracting spatial means for full box and Puglia sub-region ...")

results <- tibble(
  century          = centuries,
  temp_full_C      = sapply(bio01_files, raster_mean, extent = ext_full),
  temp_puglia_C    = sapply(bio01_files, raster_mean, extent = ext_puglia),
  precip_full_mm   = sapply(bio12_files, raster_mean, extent = ext_full),
  precip_puglia_mm = sapply(bio12_files, raster_mean, extent = ext_puglia)
) |>
  mutate(
    temp_anom_full    = temp_full_C    - mean(temp_full_C),
    temp_anom_puglia  = temp_puglia_C  - mean(temp_puglia_C),
    prec_anom_full    = (precip_full_mm   - mean(precip_full_mm))   / mean(precip_full_mm)   * 100,
    prec_anom_puglia  = (precip_puglia_mm - mean(precip_puglia_mm)) / mean(precip_puglia_mm) * 100,
    delta_temp_abs   = temp_puglia_C    - temp_full_C,
    delta_precip_abs = precip_puglia_mm - precip_full_mm,
    delta_temp_anom  = temp_anom_puglia - temp_anom_full,
    delta_prec_anom  = prec_anom_puglia - prec_anom_full
  )

cat("\n=== Absolute offset (Puglia minus full box) ===\n")
cat(sprintf("Temperature:   %.2f to %.2f deg C\n",
            min(results$delta_temp_abs), max(results$delta_temp_abs)))
cat(sprintf("Precipitation: %.1f to %.1f mm/yr\n",
            min(results$delta_precip_abs), max(results$delta_precip_abs)))

cat("\n=== Anomaly differences (Puglia minus full box) ===\n")
cat(sprintf("Max |delta T anomaly|: %.2f deg C\n", max(abs(results$delta_temp_anom))))
cat(sprintf("Max |delta P anomaly|: %.1f %%\n",   max(abs(results$delta_prec_anom))))

cat("\nPer-century breakdown:\n")
print(results |>
  select(century, delta_temp_abs, delta_precip_abs,
         delta_temp_anom, delta_prec_anom) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))),
  n = 20)

readr::write_csv(
  results |> select(century,
                    temp_full_C, temp_puglia_C, delta_temp_abs, delta_temp_anom,
                    precip_full_mm, precip_puglia_mm, delta_precip_abs, delta_prec_anom),
  file.path(out_dir, "spatial_check.csv")
)
message("Saved: outputs/tables/spatial_check.csv")
