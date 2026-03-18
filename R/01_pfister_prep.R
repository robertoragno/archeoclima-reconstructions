# ==============================================================================
# 01_pfister_prep.R
#
# Purpose: Filter the ArcheoClima database to Pfister-relevant event types
#          and convert event strength to Pfister wet/dry and cold/warm indices.
#
# Input:   data/raw/Tbl_Principale.csv
# Output:  data/processed/pfister_coded.csv
# ==============================================================================

required_packages <- c("here", "tidyverse")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(here)
library(tidyverse)

# ------------------------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------------------------

data <- read_csv(
  here("data", "raw", "Dataset_ITA.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------------------------
# 2. Filter to Pfister-relevant event types
#
# Type 1:  Drought             → DRY
# Type 3:  Flood               → WET
# Type 4:  Hail/Snow           → WET / COLD
# Type 5:  Precipitation       → WET
# Type 8:  Hot/Arid Oscillation       → WARM / DRY
# Type 9:  Cold/Humid Oscillation       → COLD / WET
# ------------------------------------------------------------------------------

pfister_types <- c(1, 3, 4, 5, 8, 9)

data <- data |>
  filter(Event_Type %in% pfister_types)

# ------------------------------------------------------------------------------
# 3. Convert Event_Strength (1–3) to signed Pfister indices
#
# Wet/Dry index:   negative = dry,  positive = wet
# Cold/Warm index: negative = cold, positive = warm
# ------------------------------------------------------------------------------

data <- data |>
  mutate(
    Pfister_Wet_Dry = case_when(
      Event_Type %in% c(1, 8) ~ -1L * Event_Strength,  # Drought / Hot-arid
      Event_Type %in% c(3, 4, 5, 9) ~  1L * Event_Strength,  # Flood / Hail / Precip / Cold-humid
      .default = NA_integer_
    ),
    Pfister_Cold_Warm = case_when(
      Event_Type %in% c(4, 9) ~ -1L * Event_Strength,  # Hail/Snow / Cold-humid
      Event_Type == 8          ~  1L * Event_Strength,  # Hot-arid
      Event_Type %in% c(1, 3, 5) ~ NA_integer_,        # Pure precip/drought: no temp signal
      .default = NA_integer_
    )
  )

# ------------------------------------------------------------------------------
# 4. Summary distributions (for inspection)
# ------------------------------------------------------------------------------

pfister_wetdry_dist <- data |>
  count(Pfister_Wet_Dry) |>
  arrange(Pfister_Wet_Dry) |>
  mutate(
    direction  = case_when(Pfister_Wet_Dry < 0 ~ "DRY", Pfister_Wet_Dry > 0 ~ "WET", .default = "Neutral"),
    percentage = round(100 * n / sum(n), 1)
  )

pfister_coldwarm_dist <- data |>
  filter(!is.na(Pfister_Cold_Warm)) |>
  count(Pfister_Cold_Warm) |>
  arrange(Pfister_Cold_Warm) |>
  mutate(
    direction  = case_when(Pfister_Cold_Warm < 0 ~ "COLD", Pfister_Cold_Warm > 0 ~ "WARM", .default = "Neutral"),
    percentage = round(100 * n / sum(n), 1)
  )

message("Wet/Dry distribution:")
print(pfister_wetdry_dist)

message("\nCold/Warm distribution:")
print(pfister_coldwarm_dist)

# ------------------------------------------------------------------------------
# 5. Save
# ------------------------------------------------------------------------------

write_csv(data, here("data", "processed", "pfister_coded.csv"))
message("Saved: data/processed/pfister_coded.csv")
