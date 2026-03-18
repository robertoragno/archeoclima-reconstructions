# ==============================================================================
# 04_figures_database.R
#
# Purpose: Descriptive figures for the ArcheoClima database.
#          Produces five individual plots and one combined panel figure.
#
# Input:   data/raw/Tbl_Principale.csv
# Output:  outputs/figures/plot1_event_frequency.png
#          outputs/figures/plot2_temporal_distribution.png
#          outputs/figures/plot3_macrocategories.png
#          outputs/figures/plot4_heatmap.png
#          outputs/figures/plot5_piechart.png
#          outputs/figures/combined_database_panel.png
#          outputs/tables/summary_events.csv
# ==============================================================================

required_packages <- c(
  "here",
  "tidyverse",
  "scales",
  "patchwork"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    renv::install(pkg)
  }
}

library(here)
library(tidyverse)
library(scales)
library(patchwork)

# ------------------------------------------------------------------------------
# 1. Event type metadata
# ------------------------------------------------------------------------------

event_colors <- c(
  "1"  = "#D2691E",  "2"  = "#DAA520",  "3"  = "#1E90FF",
  "4"  = "#B0C4DE",  "5"  = "#4682B4",  "6"  = "#00008B",
  "7"  = "#6B8E23",  "8"  = "#FF6347",  "9"  = "#483D8B",
  "10" = "#708090",  "11" = "#2F4F4F",  "12" = "#8B4513",
  "13" = "#A9A9A9",  "14" = "#8B0000",  "15" = "#191970",
  "16" = "#9ACD32"
)

event_labels <- c(
  "1"  = "Drought",           "2"  = "Famine",
  "3"  = "Flood",             "4"  = "Hailstorm",
  "5"  = "Precipitation",     "6"  = "Tsunami",
  "7"  = "Locusts",           "8"  = "Hot-arid osc.",
  "9"  = "Cold-humid osc.",   "10" = "Wind",
  "11" = "Eclipse",           "12" = "Livestock loss",
  "13" = "Unspec. calamity",  "14" = "Earthquake",
  "15" = "Shipwreck",         "16" = "Fireflies",
  "17" = "Rodents",           "18" = "Resin fall"
)

# ------------------------------------------------------------------------------
# 2. Load and prepare data
# ------------------------------------------------------------------------------

raw_data <- read_csv(
  here("data", "raw", "Dataset_EN.csv"),
  show_col_types = FALSE
)

# Parse Start_date to a numeric year and assign to century
data <- raw_data |>
  filter(!Event_Type %in% c(11, 14)) |>
  mutate(
    Event_Type_factor = factor(Event_Type, levels = 1:18, labels = event_labels),
    Start_date_numeric = as.integer(
      case_when(
        str_detect(Start_date, "^ante") ~ str_extract(Start_date, "\\d{3,4}"),
        str_detect(Start_date, "/")     ~ str_extract(Start_date, "^\\d{3,4}"),
        .default = Start_date
      )
    ),
    Century = floor(Start_date_numeric / 100) * 100
  ) |>
  droplevels()  # removes Eclipse and Earthquake from the factor after filtering

# ------------------------------------------------------------------------------
# 3. Plot 1: Bar chart — frequency by event type
# ------------------------------------------------------------------------------

event_counts <- data |>
  count(Event_Type, Event_Type_factor) |>
  arrange(desc(n))

p1 <- ggplot(event_counts,
             aes(x = reorder(Event_Type_factor, n), y = n,
                 fill = as.character(Event_Type))) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = event_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Climate event reports in the dataset",
    subtitle = "Documentary sources, 2nd millennium CE",
    x = NULL,
    y = "Number of events"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11, colour = "grey30"),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

# ------------------------------------------------------------------------------
# 4. Plot 2: Bar chart — temporal distribution by century
# ------------------------------------------------------------------------------

century_counts  <- data |> count(Century, Event_Type, Event_Type_factor)
century_totals  <- data |> count(Century, name = "total")

p2 <- ggplot(century_counts, aes(x = Century, y = n)) +
  geom_col(width = 80, fill = "#4A7BB7", alpha = 0.85) +
  geom_text(
    data  = century_totals,
    aes(x = Century, y = total, label = total),
    vjust = -0.4, size = 3.8, fontface = "bold", colour = "grey20"
  ) +
  scale_x_continuous(
    breaks = seq(600, 1900, 100),
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Temporal distribution of events",
    x     = "Century",
    y     = "Number of events"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 16),
    axis.title.x       = element_text(margin = margin(t = 8)),
    axis.title.y       = element_text(margin = margin(r = 8)),
    axis.text          = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(12, 12, 12, 12)
  )

# ------------------------------------------------------------------------------
# 5. Plot 3: Stacked bar — events by macro-category
# ------------------------------------------------------------------------------

data_categorised <- data |>
  mutate(
    Macrocategory = case_when(
      Event_Type %in% c(3, 4, 5, 6)     ~ "Hydrological",
      Event_Type %in% c(1, 2, 8)        ~ "Drought / Heat",
      Event_Type == 9                    ~ "Cold / Humid",
      Event_Type == 10                   ~ "Wind",
      Event_Type %in% c(7, 12, 15, 16)  ~ "Biological",
      .default                           = "Other"
    )
  )

macro_counts <- data_categorised |>
  count(Macrocategory, Event_Type_factor, Event_Type) |>
  arrange(desc(n))

p3 <- ggplot(macro_counts,
             aes(x = reorder(Macrocategory, n, sum), y = n,
                 fill = as.character(Event_Type))) +
  geom_col(position = "stack") +
  scale_fill_manual(values = event_colors, guide = "none") +
  coord_flip() +
  labs(title = "Events by macro-category", x = NULL, y = "Number of events") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank()
  )

# ------------------------------------------------------------------------------
# 6. Plot 4: Heatmap — event type × century
# ------------------------------------------------------------------------------

heatmap_data <- data |>
  count(Century, Event_Type_factor)

p4 <- ggplot(heatmap_data,
             aes(x = Century, y = Event_Type_factor, fill = n)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(n > 0, n, "")), size = 3, colour = "grey20") +
  scale_fill_gradient(
    low = "white", high = "#1E90FF",
    name = "Events", na.value = "grey95"
  ) +
  scale_x_continuous(breaks = seq(1000, 1800, 100), expand = c(0, 0)) +
  labs(
    title    = "Event frequency by type and century",
    subtitle = "Heatmap: 1000–1800 CE",
    x = "Century",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    axis.text.y      = element_text(size = 9),
    panel.grid       = element_blank(),
    legend.position  = "right"
  )

# ------------------------------------------------------------------------------
# 7. Save individual figures
# ------------------------------------------------------------------------------

fig_dir <- here("outputs", "figures")

ggsave(file.path(fig_dir, "plot1_event_frequency.png"),   p1, width = 10, height = 5,  dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "plot2_temporal_distribution.png"), p2, width = 14, height = 8, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "plot3_macrocategories.png"),   p3, width = 10, height = 6,  dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "plot4_heatmap.png"),           p4, width = 12, height = 8,  dpi = 300, bg = "white")

# ------------------------------------------------------------------------------
# 9. Combined panel figure (for paper)
# ------------------------------------------------------------------------------

combined <- (
  (p1 + labs(title = "A) Frequency by type",       subtitle = NULL, caption = NULL) +
             theme(plot.title = element_text(size = 11, face = "bold"))) |
  (p2 + labs(title = "B) Temporal distribution",   subtitle = NULL) +
             theme(plot.title = element_text(size = 11, face = "bold"), legend.position = "none"))
) / (
  (p3 + labs(title = "C) Macro-categories") +
             theme(plot.title = element_text(size = 11, face = "bold"))) |
  (p4 + labs(title = "D) Temporal heatmap",        subtitle = NULL) +
             theme(plot.title = element_text(size = 11, face = "bold"), axis.text.y = element_text(size = 7)))
) +
  plot_annotation(
    title    = "Documented Climate Events \u2014 ArcheoClima Database",
    subtitle = "Central-southern Italy, 1000\u20131850 CE",
    theme = theme(
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, colour = "grey30"),
      plot.caption  = element_text(hjust = 0,  colour = "grey50")
    )
  )

ggsave(file.path(fig_dir, "combined_database_panel.png"),
       combined, width = 14, height = 10, dpi = 300, bg = "white")

# ------------------------------------------------------------------------------
# 10. Summary statistics and export
# ------------------------------------------------------------------------------

message("\n=== ArcheoClima dataset summary ===")
message("Total events:       ", nrow(data))
message("Period:             ", min(data$Start_date_numeric, na.rm = TRUE), "\u2013",
                                max(data$Start_date_numeric, na.rm = TRUE), " CE")
message("Distinct types:     ", n_distinct(data$Event_Type))

summary_table <- event_counts |>
  left_join(
    data |>
      group_by(Event_Type) |>
      summarise(
        mean_strength = mean(Event_Strength,      na.rm = TRUE),
        mean_impact   = mean(Geographic_Impact,   na.rm = TRUE),
        .groups = "drop"
      ),
    by = "Event_Type"
  ) |>
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    colour     = event_colors[as.character(Event_Type)]
  ) |>
  select(
    Type               = Event_Type_factor,
    Frequency          = n,
    `Percentage (%)`   = percentage,
    `Mean strength`    = mean_strength,
    `Mean geo. impact` = mean_impact,
    Colour             = colour
  )

write_csv(summary_table, here("outputs", "tables", "summary_events.csv"))
message("Saved: outputs/tables/summary_events.csv")
message("Saved: outputs/figures/ (6 figures)")

