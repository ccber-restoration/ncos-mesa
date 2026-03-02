## Script for combined graph of November and December water potentials for MU, ML, and SP reference sites ##

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)
library(scales)

## Process data sets ##
process_wp <- function(file_path, date_label) {
  wp <- read_csv(file_path) %>%
    separate_wider_delim(
      Sp_sample,
      delim = "_",
      names = c("species", "sample")
    ) %>%
    mutate(
      species = case_when(
        species == "Bapi" ~ "BAPI",
        species == "Arca" ~ "ARCA",
        TRUE ~ species
      ),
      date = date_label
    ) %>%
    group_by(species, Location, sample, date) %>%
    summarize(
      mean_MD = mean(MPa, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  return(wp)
}

### Select and Process data sets ##

wp_2025_12_03 <- process_wp(
  "/Users/emileedoering/Desktop/NCOS_Water_Potentials  copy/NewWater_potentials_2025-12-03 2.csv",
  "2025-12-03"
)

wp_2025_11_07 <- process_wp(
  "/Users/emileedoering/Desktop/NCOS_Water_Potentials  copy/Water Potentials 11_7 - Sheet1.csv",
  "2025-11-07"
)

## Combine data sets ## 

wp_combined <- bind_rows(wp_2025_11_07, wp_2025_12_03)

## Plot side by side ##

wp_combined_fig <- ggplot(
  wp_combined,
  aes(
    x = Location,
    y = mean_MD,
    color = date,               # differentiate dates
    shape = species,            # differentiate species
    group = interaction(species, date)  # ensures dodge works
  )
) +
  geom_jitter(
    position = position_dodge(width = 0.4), # side-by-side points
    size = 2
  ) +
  facet_wrap(vars(species)) +
  theme_cowplot() +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = breaks_width(0.5)
  ) +
  labs(
    y = "Water potential (-MPa)",
    x = "Location",
    color = "Date",
    shape = "Species"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Display plot ##

wp_combined_fig

## Save to folder ##

ggsave(
  wp_combined_fig,
  filename = "/Users/emileedoering/Desktop/NCOS_Water_Potentials  copy/waterpotentials_11_7_vs_12_03_sidebyside.png",
  width = 220,
  height = 120,
  units = "mm",
  bg = "white"
)

