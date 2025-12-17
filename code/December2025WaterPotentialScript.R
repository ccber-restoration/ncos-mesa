### Script for 12/3/2025 water potentials from upper &lower mesa and sp reference site ##
# Load packages
library(scales)
library(readr)
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)

## Read csv data ##
wp_2025_12_03 <- read.csv(
  "/Users/emileedoering/Desktop/NCOS_Water_Potentials  copy/Water_potentials_2025-12-03.csv",
  skip = 1,
  fill = TRUE
)

## Separate species and sample, make species names standard ##

wp_2025_12_03 <- wp_2025_12_03 %>%
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
    )
  )

### Summarize data ###

wp_summarized <- wp_2025_12_03 %>% 
  group_by(species, Location, sample) %>% 
  summarize(
    mean_MD = mean(MPa, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

### Plot data ###

wp_fig_2025_12_03 <- ggplot(
  data = wp_summarized,
  aes(
    x = Location,
    y = mean_MD,
    fill = species,
    color = species,
    group = species
  )
) +
  geom_jitter(height = 0, width = 0.4) +
  theme_cowplot() +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = breaks_width(0.5)
  ) +
  facet_wrap(vars(species)) +
  ylab("Water potential (-MPa)")

## Show Plot ##

wp_fig_2025_12_03

## Save plot to NCOS folder ##

ggsave(
  wp_fig_2025_12_03,
  filename = "/Users/emileedoering/Desktop/NCOS_Water_Potentials  copy/waterpotentials_20251203.png",
  bg = "white",
  width = 140,
  height = 100,
  units = "mm"
)






