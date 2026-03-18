# ==============================================================================
# 02_rasters.R
#
# Purpose: Load CHELSA-TraCE21k rasters, crop to the study area, and extract
#          centennial mean temperature and precipitation values.
#
# Input:   CHELSA-TraCE21k raster files (bio01: temperature, bio12: precipitation)
#          → set `chelsa_dir` below to the folder containing your raster files
#
# Output:  data/processed/chelsa_climate.csv
#
# Notes:   The CHELSA-TraCE21k data must be downloaded separately from
#          https://chelsa-climate.org/chelsa-trace21k/
#          Download the bio01 and bio12 variables for TraCE21k IDs 11–19
#          (corresponding to 1000–1800 CE at centennial resolution).
#          If chelsa_climate.csv already exists, this script can be skipped.
#          I have also included the cropped files for convenience in data/raw
# ==============================================================================

required_packages <- c(
  "here",
  "terra",
  "tidyverse"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    renv::install(pkg)
  }
}

library(here)
library(terra)
library(tidyverse)

# ------------------------------------------------------------------------------
# 1. Configure paths
# ------------------------------------------------------------------------------

# Set this to the folder containing your CHELSA-TraCE21k .tif files.
# Sub-folders "temperature/" and "precipitation/" are expected.
chelsa_dir <- "/path/to/your/chelsa/files"   # <-- edit this


temp_files   <- list.files(file.path(chelsa_dir, "temperature"),
                            pattern = "bio01.*\\.tif$", full.names = TRUE)
precip_files <- list.files(file.path(chelsa_dir, "precipitation"),
                            pattern = "bio12.*\\.tif$", full.names = TRUE)

if (length(temp_files) == 0 || length(precip_files) == 0) {
  stop(
    "No CHELSA raster files found in '", chelsa_dir, "'.\n",
    "Please download bio01 and bio12 from https://chelsa-climate.org/chelsa-trace21k/\n",
    "and update the `chelsa_dir` variable in R/02_rasters.R."
  )
}

message("Temperature files found:  ", length(temp_files))
message("Precipitation files found: ", length(precip_files))

# ------------------------------------------------------------------------------
# 2. Define study area
# ------------------------------------------------------------------------------

# Central–southern Italy: longitude 13–19°E, latitude 39.5–42°N
study_area <- ext(13, 19, 39.5, 42)

# ------------------------------------------------------------------------------
# 3. Crop rasters and extract spatial means
# ------------------------------------------------------------------------------

crop_and_mean <- function(filepath, extent) {
  r <- rast(filepath)
  r_cropped <- crop(r, extent)
  global(r_cropped, fun = "mean", na.rm = TRUE)[1, 1]
}

message("Processing temperature rasters...")
temp_means <- sapply(temp_files, crop_and_mean, extent = study_area)

message("Processing precipitation rasters...")
precip_means <- sapply(precip_files, crop_and_mean, extent = study_area)

# ------------------------------------------------------------------------------
# 4. Map CHELSA time IDs to calendar years
#
# CHELSA-TraCE21k filenames encode a time ID (e.g., _11_ = 11th centennial
# slice). We convert to the midpoint year of each century for reference,
# but the model uses century start years (see 03_model_prep.R).
# ------------------------------------------------------------------------------

extract_time_id <- function(filename) {
  as.integer(sub(".*_(\\d+)_V.*", "\\1", basename(filename)))
}

time_id_to_midyear <- function(time_id) {
  start_year <- 1000 + (time_id - 11) * 100
  start_year + 50  # midpoint
}

temp_ids <- sapply(temp_files, extract_time_id)
years    <- time_id_to_midyear(temp_ids)

# ------------------------------------------------------------------------------
# 5. Assemble and save
# ------------------------------------------------------------------------------

climate_data <- tibble(
  year             = years,
  temperature_C    = temp_means,
  precipitation_mm = precip_means
)

message("\nCHELSA centennial summary:")
print(climate_data)

# Quick diagnostic plots
old_par <- par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))

plot(climate_data$year, climate_data$temperature_C,
     type = "b", pch = 19, col = "firebrick",
     xlab = "Year CE", ylab = "Temperature (°C)",
     main = "Mean annual temperature – Central-southern Italy")

plot(climate_data$year, climate_data$precipitation_mm,
     type = "b", pch = 19, col = "steelblue",
     xlab = "Year CE", ylab = "Precipitation (mm/year)",
     main = "Annual precipitation – Central-southern Italy")

par(old_par)

write_csv(climate_data, here("data", "processed", "chelsa_climate.csv"))
message("Saved: data/processed/chelsa_climate.csv")
