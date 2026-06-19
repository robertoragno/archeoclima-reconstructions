# ModE-RA / ModE-Sim: NetCDF -> centennial means
#
# Sourced by R/08_ModE_comparison.R. Populates eight data frames in the
# calling environment: ra_temp, ra_tstd, ra_prec, ra_pstd (ModE-RA) and
# sim_temp, sim_tstd, sim_prec, sim_pstd (ModE-Sim).
#
# If data/processed/mode_cache/ already contains all 8 CSVs the raw NetCDFs
# are not touched. If any CSV is missing the script tries to rebuild from the
# raw files. If those are also missing it stops with download instructions.
#
# --- Downloading the raw files ------------------------------------------------
# ModE-RA and ModE-Sim Set 1420-3 are available from the WDC Climate Data portal
# https://www.wdc-climate.de/ui/q?hierarchy_steps_ss=ModE-RA_s14203-18501&entry_type_s=Dataset
# and: 
# https://www.wdc-climate.de/ui/q?query=ModE-Sim&entry_type_s=Dataset&page=0&rows=15
# If the links above do not work, search for "ModE-RA" and 
# "ModE-Sim" on the WDC Climate Data portal.
# Place the two unpacked folders under data/raw/ModE/:
#   data/raw/ModE/ModE-RA_s14203-18501_ensanom_1-28/
#   data/raw/ModE/ModE-Sim_s14203_ensabs_1-28/
# Then re-run R/08_ModE_comparison.R; this script builds the cache and the raw
# folders can be deleted again.
# ------------------------------------------------------------------------------

library(here)
library(ncdf4)
library(tidyverse)

STUDY_LON <- c(13, 19)
STUDY_LAT <- c(39.5, 42)

cache_dir <- here("data", "processed", "mode_cache")
ra_dir    <- here("data", "raw", "ModE", "ModE-RA_s14203-18501_ensanom_1-28")
sim_dir   <- here("data", "raw", "ModE", "ModE-Sim_s14203_ensabs_1-28")

cache_names <- c("ra_temp_mean", "ra_temp_std", "ra_prec_mean", "ra_prec_std",
                 "sim_temp_mean", "sim_temp_std", "sim_prec_mean", "sim_prec_std")
cache_paths <- file.path(cache_dir, paste0(cache_names, ".csv"))
cache_complete <- all(file.exists(cache_paths))

# ---- NetCDF helpers (only defined if we actually need them) ------------------

if (!cache_complete) {

  ra_files <- list(
    temp_mean = file.path(ra_dir, "ModE-RA_ensmean_temp2_anom_wrt_1901-2000_1421-2008_mon.nc"),
    temp_std  = file.path(ra_dir, "ModE-RA_ensstd_temp2_anom_wrt_1901-2000_1421-2008_mon.nc"),
    prec_mean = file.path(ra_dir, "ModE-RA_ensmean_totprec_anom_wrt_1901-2000_1421-2008_mon.nc"),
    prec_std  = file.path(ra_dir, "ModE-RA_ensstd_totprec_anom_wrt_1901-2000_1421-2008_mon.nc")
  )
  sim_files <- list(
    temp_mean = file.path(sim_dir, "ModE-Sim_set_1420-3_ensmean_temp2_abs_1420-1849_mon.nc"),
    temp_std  = file.path(sim_dir, "ModE-Sim_set_1420-3_ensstd_temp2_abs_1420-1849_mon.nc"),
    prec_mean = file.path(sim_dir, "ModE-Sim_set_1420-3_ensmean_totprec_abs_1420-1849_mon.nc"),
    prec_std  = file.path(sim_dir, "ModE-Sim_set_1420-3_ensstd_totprec_abs_1420-1849_mon.nc")
  )

  missing_raw <- !sapply(c(ra_files, sim_files), file.exists)
  if (any(missing_raw)) {
    stop(
      "Mode cache is incomplete and raw NetCDF files are missing.\n",
      "See the download instructions at the top of R/helpers/mode_processing.R.\n",
      "Missing: ", paste(names(missing_raw)[missing_raw], collapse = ", ")
    )
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  extract_monthly_ts <- function(nc_path, var_name, cache_name) {
    cache_path <- file.path(cache_dir, paste0(cache_name, ".csv"))
    if (file.exists(cache_path)) {
      message("  (cache) ", cache_name)
      return(readr::read_csv(cache_path, show_col_types = FALSE))
    }
    message("  processing ", cache_name, " ...")
    nc <- nc_open(nc_path)
    on.exit(nc_close(nc))

    lon_name <- if ("longitude" %in% names(nc$dim)) "longitude" else "lon"
    lat_name <- if ("latitude"  %in% names(nc$dim)) "latitude"  else "lat"

    lons <- nc$dim[[lon_name]]$vals
    lats <- nc$dim[[lat_name]]$vals
    time_raw <- nc$dim[["time"]]$vals

    lon_idx <- which(lons >= STUDY_LON[1] & lons <= STUDY_LON[2])
    lat_idx <- which(lats >= STUDY_LAT[1] & lats <= STUDY_LAT[2])

    lat_w <- cos(lats[lat_idx] * pi / 180)
    lat_w <- lat_w / sum(lat_w)

    origin <- as.POSIXct("1400-01-01 00:00:00", tz = "UTC")
    dates  <- origin + as.numeric(time_raw) * 3600
    years  <- as.integer(format(dates, "%Y"))
    months <- as.integer(format(dates, "%m"))

    vals <- ncvar_get(nc, var_name,
                      start = c(min(lon_idx), min(lat_idx), 1),
                      count = c(length(lon_idx), length(lat_idx), length(time_raw)))

    area_mean <- vapply(seq_along(time_raw), function(i) {
      slice    <- vals[, , i]
      lon_mean <- colMeans(slice, na.rm = TRUE)
      sum(lon_mean * lat_w, na.rm = TRUE)
    }, numeric(1))

    result <- tibble(year = years, month = months, value = area_mean)
    readr::write_csv(result, cache_path)
    result
  }

  to_annual_temp <- function(df) {
    df |> group_by(year) |> filter(n() == 12L) |>
      summarise(value_annual = mean(value), .groups = "drop")
  }

  to_annual_precip <- function(df) {
    month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    df |>
      mutate(mm_month = value * month_days[month] * 86400) |>
      group_by(year) |> filter(n() == 12L) |>
      summarise(value_annual = sum(mm_month), .groups = "drop")
  }

  to_centennial <- function(df, min_years = 50L) {
    df |>
      mutate(century = as.integer(floor(year / 100) * 100)) |>
      group_by(century) |>
      summarise(value_cent = mean(value_annual), n_years = n(),
                partial = n() < 100L, .groups = "drop") |>
      filter(n_years >= min_years, century >= 1000L, century <= 1800L)
  }

  message("--- Building ModE cache from raw NetCDFs ---")
  message("ModE-RA:")
  ra_temp <- to_centennial(to_annual_temp(extract_monthly_ts(ra_files$temp_mean, "temp2",   "ra_temp_mean")))
  ra_tstd <- to_centennial(to_annual_temp(extract_monthly_ts(ra_files$temp_std,  "temp2",   "ra_temp_std")))  |> rename(std_cent = value_cent)
  ra_prec <- to_centennial(to_annual_precip(extract_monthly_ts(ra_files$prec_mean, "totprec", "ra_prec_mean")))
  ra_pstd <- to_centennial(to_annual_precip(extract_monthly_ts(ra_files$prec_std,  "totprec", "ra_prec_std")))  |> rename(std_cent = value_cent)

  message("ModE-Sim:")
  sim_temp <- to_centennial(to_annual_temp(extract_monthly_ts(sim_files$temp_mean, "temp2",   "sim_temp_mean")))
  sim_tstd <- to_centennial(to_annual_temp(extract_monthly_ts(sim_files$temp_std,  "temp2",   "sim_temp_std")))  |> rename(std_cent = value_cent)
  sim_prec <- to_centennial(to_annual_precip(extract_monthly_ts(sim_files$prec_mean, "totprec", "sim_prec_mean")))
  sim_pstd <- to_centennial(to_annual_precip(extract_monthly_ts(sim_files$prec_std,  "totprec", "sim_prec_std")))  |> rename(std_cent = value_cent)

  message("Cache written to data/processed/mode_cache/")

} else {

  message("Loading ModE cache ...")
  load_cent <- function(name) readr::read_csv(file.path(cache_dir, paste0(name, ".csv")), show_col_types = FALSE)

  ra_temp <- load_cent("ra_temp_mean")
  ra_tstd <- load_cent("ra_temp_std")  |> rename(std_cent = value_cent)
  ra_prec <- load_cent("ra_prec_mean")
  ra_pstd <- load_cent("ra_prec_std")  |> rename(std_cent = value_cent)
  sim_temp <- load_cent("sim_temp_mean")
  sim_tstd <- load_cent("sim_temp_std") |> rename(std_cent = value_cent)
  sim_prec <- load_cent("sim_prec_mean")
  sim_pstd <- load_cent("sim_prec_std") |> rename(std_cent = value_cent)

}
