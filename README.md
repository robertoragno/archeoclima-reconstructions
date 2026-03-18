# ArcheoClima – Temperature and precipitation anomalies in southern Italy (1000–1800 CE): integrating historical sources with climate simulations

Replication code for:

> Ragno, R. & Goffredo, R. (*to be submitted*). "Temperature and precipitation anomalies in southern Italy (1000–1800 CE): integrating historical sources with climate simulations." 

The model integrates CHELSA-TraCE21k palaeoclimate simulations with Pfister-coded documentary sources from the ArcheoClima database, using a Bayesian state-space model with an AR(1) process fit in Stan.

---

## Repository structure

```
.
├── R/
│   ├── 01_pfister_prep.R       # Pfister coding of documentary events
│   ├── 02_rasters.R            # CHELSA-TraCE21k raster processing
│   ├── 03_model_prep.R         # Stan model definition and helper functions
│   ├── 04_figures_database.R   # Descriptive figures for the ArcheoClima database
│   └── 05_figures_model.R      # Model output figures
├── stan/
│   └── centennial_model.stan   # Stan model (AR(1) + ordered logistic)
├── data/
│   ├── raw/
│   │   └── Dataset_ITA.csv     # ArcheoClima database (see Data section)
│   │   └── Dataset_EN.csv      # ArcheoClima database (see Data section)
│   │   └── Database_Structure  # ArcheoClima database original XSLX tables
│   └── processed/              # Intermediate files written by the scripts
│   │   └── Chelsa_pre_Cropped  # Chelsa Rasters pre-cropped, so that the dataset does not have to be necessarily downloaded
├── outputs/
│   ├── figures/                # Saved plots (.png)
│   └── tables/                 # Saved tables (.csv)
├── run_all.R                   # Master script to run the full pipeline
└── renv.lock                   # Package snapshot for reproducibility
```

---

## Requirements

### R packages

This project uses [`renv`](https://rstudio.github.io/renv/) for reproducibility. To restore the exact package environment used in the paper:

```r
install.packages("renv")
renv::restore()
```

Key packages: `tidyverse`, `terra`, `rstan` (≥ 2.21), `ggdist`, `tidybayes`, `patchwork`, `here`, `scales`.

Stan requires a working C++ toolchain. See the [RStan getting started guide](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).

### CHELSA-TraCE21k raster data

The CHELSA-TraCE21k rasters are not included in this repository due to their size (~GB per variable). They must be downloaded separately from:

> **<https://chelsa-climate.org/chelsa-trace21k/>**

Download the **bio01** (mean annual temperature) and **bio12** (annual precipitation) variables for the time slices corresponding to 1000–1800 CE (TraCE21k IDs 11–19). Place the files in a directory and set the path in `R/02_rasters.R`:

```r
chelsa_dir <- "/path/to/your/chelsa/files"
```

The processed output (`climate_data`) is saved to `data/processed/chelsa_climate.csv` after the first run, so you only need the rasters once. However, I have included in this directory the pre-cropped rasters (which total a size of 8MB) and the processed CSV, so this step can be avoided if needed. 

---

## Reproducing the analysis

Run the full pipeline from the project root:

```r
source("run_all.R")
```

Or step through the scripts in order:

| Step | Script | Output |
|------|--------|--------|
| 1 | `R/01_pfister_prep.R` | `data/processed/pfister_coded.csv` |
| 2 | `R/02_rasters.R` | `data/processed/chelsa_climate.csv` |
| 3–4 | `run_all.R` (model) | `data/processed/results_temp.rds`, `results_precip.rds` |
| 5 | `R/04_figures_database.R` | `outputs/figures/` |
| 6 | `R/05_figures_model.R` | `outputs/figures/` |

Expected runtime: ~2-3 minutes per model (4 chains × 2000 iterations on a modern laptop; I am using a Macboook Pro from 2021).

---

## Data

The ArcheoClima database (`Database_EN.csv`) is available in this Github repository and on Zenodo. It contains 526 documented climate events from southern Italy (1000–1850 CE) drawn from 130 historical sources, coded for event type, strength, and geographic impact. Not all sources were used for this script.

![The structure of the database showing the relations to the main tables.](/data/raw/Database_Structure/Database_Structure.png)

---

## Model

The centennial climate reconstruction uses:

- **Prior**: CHELSA-TraCE21k centennial means (temperature anomaly in °C; precipitation anomaly in %)
- **Likelihood**: Pfister-coded documentary events modelled as ordered logistic observations
- **Process**: AR(1) deviations from the CHELSA prior, with ρ ~ Beta(3, 2) (mode ≈ 0.67)

See `stan/centennial_model.stan` and `R/03_model_prep.R` for full details.

---

## Citation

If you use this code or the ArcheoClima database, please cite the paper (still under review) above and the dataset DOI
[![DOI](https://zenodo.org/badge/1185147777.svg)](https://doi.org/10.5281/zenodo.19087807)


---

## Disclaimer

Given the high volume of work at the moment, the R scripts were refined using an AI tool to make sure it is readable and well-commented.
I have fully tested the scripts after refinement, but if you encounter any issues please let me know by opening an issue in this repository.
