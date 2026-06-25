# ArcheoClima – Integrating historical sources with climate simulations: temperature and precipitation anomalies in southern Italy (1000–1800 CE): 

Replication code for:

> Ragno, R. & Goffredo, R. (*under review*). "Integrating historical sources with climate simulations: temperature and precipitation anomalies in southern Italy (1000–1800 CE)." 

The model integrates CHELSA-TraCE21k palaeoclimate simulations with Pfister-coded documentary sources from the ArcheoClima database, using a Bayesian state-space model with an AR(1) process fit in Stan.

---

## Repository structure

```
.
├── R/
│   ├── 01_pfister_prep.R         # Pfister coding of documentary events
│   ├── 02_rasters.R              # CHELSA-TraCE21k raster processing
│   ├── 03_model_prep.R           # Stan model helper functions (prepare_chelsa, build_stan_data)
│   ├── 04_figures_database.R     # Descriptive figures for the ArcheoClima database
│   ├── 05_figures_model.R        # Main model output figures
│   ├── 05b_model_figures_only.R  # Regenerate model figures from saved RDS (no Stan re-run)
│   ├── 06_sensitivity.R          # Sensitivity analysis: posterior under varying CHELSA s.d. grid
│   ├── 06b_sensitivity_figures.R # Regenerate sensitivity figure from saved CSVs (no Stan re-run)
│   ├── 07_decomposition.R        # AR(1) decomposition of theta into CHELSA / persistence / innovation
│   ├── 08_ModE_comparison.R      # Compare posterior against ModE-RA and ModE-Sim
│   ├── checks/
│   │   ├── spatial_representativeness.R      # Full-box vs. Puglia sub-region CHELSA comparison
│   │   └── plot_spatial_representativeness.R # Plots spatial_check.csv as a three-panel figure
│   └── helpers/
│       └── mode_processing.R     # NetCDF → centennial means for ModE-RA / ModE-Sim (cached)
├── stan/
│   ├── centennial_model.stan     # Main Stan model (AR(1) + ordered logistic)
│   └── centennial_model_sens.stan  # Variant with free chelsa_sd for sensitivity analysis
├── data/
│   ├── raw/
│   │   ├── Dataset_ITA.csv       # ArcheoClima database (Italian)
│   │   ├── Dataset_EN.csv        # ArcheoClima database (English)
│   │   └── Database_Structure/   # Original XLSX tables and schema diagram
│   └── processed/
│       ├── CHELSA_pre_cropped/   # Pre-cropped CHELSA rasters (bio01 + bio12, IDs 11–19)
│       ├── mode_cache/           # Centennial means from ModE-RA / ModE-Sim (auto-built)
│       ├── chelsa_climate.csv    # Processed CHELSA anomalies
│       └── pfister_coded.csv     # Pfister-coded documentary events
├── outputs/
│   ├── figures/                  # Saved plots (.png)
│   └── tables/                   # Saved tables (.csv)
├── run_all.R                     # Master script to run the core pipeline (steps 1–5)
└── renv.lock                     # Package snapshot for reproducibility
```

---

## Requirements

### R packages

This project uses [`renv`](https://rstudio.github.io/renv/) for reproducibility. To restore the exact package environment:

```r
install.packages("renv")
renv::restore()
```

Key packages: `tidyverse`, `terra`, `rstan` (≥ 2.21), `ggdist`, `tidybayes`, `patchwork`, `maps`, `here`, `scales`, `stringr`.

The ModE-RA / ModE-Sim comparison script (`R/08_ModE_comparison.R`) additionally requires `ncdf4`, which is not part of the core pipeline and must be installed separately if you want to replicate that analysis:

```r
install.packages("ncdf4")
```

Stan requires a working C++ toolchain. See the [RStan getting started guide](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).

### CHELSA-TraCE21k raster data

The CHELSA-TraCE21k rasters are not included in this repository due to their size (~GB per variable). They must be downloaded separately from:

> **<https://chelsa-climate.org/chelsa-trace21k/>**

Download the **bio01** (mean annual temperature) and **bio12** (annual precipitation) variables for the time slices corresponding to 1000–1800 CE (TraCE21k IDs 11–19). Place the files in a directory and set the path in `R/02_rasters.R`:

```r
chelsa_dir <- "/path/to/your/chelsa/files"
```

The processed output (`climate_data`) is saved to `data/processed/chelsa_climate.csv` after the first run, so you only need the rasters once. Pre-cropped rasters (totalling ~8 MB) and the processed CSV are already included in this repository, so this step can be skipped.

### ModE-RA / ModE-Sim data (optional)

`R/08_ModE_comparison.R` compares the posterior against the ModE-RA reanalysis and ModE-Sim ensemble (Valler et al. 2022, Hand et al. 2023). These NetCDF files are not distributed here. Download them from the WDC Climate Data portal:

- ModE-RA: <https://www.wdc-climate.de/ui/q?hierarchy_steps_ss=ModE-RA_s14203-18501&entry_type_s=Dataset>
- ModE-Sim: <https://www.wdc-climate.de/ui/q?query=ModE-Sim&entry_type_s=Dataset>

Place the unpacked folders under `data/raw/ModE/`:

```
data/raw/ModE/ModE-RA_s14203-18501_ensanom_1-28/
data/raw/ModE/ModE-Sim_s14203_ensabs_1-28/
```

Once the centennial means are computed, they are cached in `data/processed/mode_cache/` as lightweight CSVs. A pre-built cache is already included in this repository, so the raw NetCDFs are not strictly needed unless you want to regenerate the cache from scratch.

---

## Reproducing the analysis

Run the core pipeline from the project root:

```r
source("run_all.R")
```

Or step through the scripts in order:

| Step | Script | Output |
|------|--------|--------|
| 1 | `R/01_pfister_prep.R` | `data/processed/pfister_coded.csv` |
| 2 | `R/02_rasters.R` | `data/processed/chelsa_climate.csv` |
| 3–4 | `run_all.R` (model) | `data/processed/results_temp.rds`, `results_precip.rds` |
| 5 | `R/04_figures_database.R` | `outputs/figures/` (database panels) |
| 6 | `R/05_figures_model.R` | `outputs/figures/` (reconstruction plots) |
| 7 | `R/06_sensitivity.R` | `outputs/tables/sensitivity_*.csv`, `outputs/figures/sensitivity_comparison.png` |
| 8 | `R/07_decomposition.R` | `outputs/tables/decomposition_*.csv`, `outputs/figures/decomposition_combined.png` |
| 9 | `R/08_ModE_comparison.R` | `outputs/tables/ModE_comparison_*.csv`, `outputs/figures/ModE_comparison.png` |
| – | `R/checks/spatial_representativeness.R` | `outputs/tables/spatial_check.csv` |
| – | `R/checks/plot_spatial_representativeness.R` | `outputs/figures/spatial_representativeness.png` |

**Figure-only shortcuts** (skip Stan): after the core pipeline has been run once, use `R/05b_model_figures_only.R` or `R/06b_sensitivity_figures.R` to regenerate figures from saved results without re-running Stan.

Expected runtime: ~2–3 minutes per model (4 chains × 2000 iterations on a modern laptop). The sensitivity grid (6 cells × 2 variables) adds ~20 minutes.

---

## Data

The ArcheoClima database (`Dataset_EN.csv`) is available in this repository and on Zenodo. It contains 526 documented climate events from southern Italy (1000–1850 CE) drawn from 130 historical sources, coded for event type, strength, and geographic impact. Not all sources were used for this analysis.

![The structure of the database showing the relations to the main tables.](/data/raw/Database_Structure/Database_Structure.png)

For more information on the structure of the database please refer to [https://doi.org/10.5281/zenodo.19102067](https://doi.org/10.5281/zenodo.19102067)

---

## Model

The centennial climate reconstruction uses:

- **Prior**: CHELSA-TraCE21k centennial means (temperature anomaly in °C; precipitation anomaly in %)
- **Likelihood**: Pfister-coded documentary events modelled as ordered logistic observations
- **Process**: AR(1) deviations from the CHELSA prior, with ρ ~ Beta(3, 2) (mode ≈ 0.67)

See `stan/centennial_model.stan` and `R/03_model_prep.R` for full details.

### Sensitivity analysis

`R/06_sensitivity.R` reruns the model using `stan/centennial_model_sens.stan` (a variant that accepts `chelsa_sd` as a free parameter) across a grid of assumed CHELSA uncertainties:

- Temperature: s.d. = 0.25, 0.50, 1.00 °C
- Precipitation: s.d. = 2.5, 5.0, 10.0 %

Results are saved as per-cell CSVs and combined in `outputs/tables/sensitivity_summary.csv`.

### AR(1) decomposition

`R/07_decomposition.R` decomposes each posterior θ[t] into three additive components using the fitted draws: CHELSA baseline, AR(1) persistence, and stochastic innovation. The components sum to θ at floating-point precision.

### ModE-RA / ModE-Sim comparison

`R/08_ModE_comparison.R` re-baselines all series to their 1500–1800 means and computes Pearson *r* and mean absolute difference between the posterior and the two independent model products. The RA–Sim gap (small gap indicating limited regional proxy assimilation) is saved to `outputs/tables/ModE_ra_sim_gap.csv`.

---

## Citation

If you use this code or the ArcheoClima database, please cite the paper (still under review) above and the dataset DOI: https://doi.org/10.5281/zenodo.19087807

[![DOI](https://zenodo.org/badge/1185147777.svg)](https://doi.org/10.5281/zenodo.19087807)

---

## Disclaimer

Given the high volume of work at the moment, the R scripts were refined using an AI tool (Github Copilot) to make sure they are readable and well-commented.
I have fully tested the scripts after refinement, but if you encounter any issues please let me know by opening an issue in this repository.
