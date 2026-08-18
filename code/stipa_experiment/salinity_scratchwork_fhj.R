# The purpose of this script is to plot soil EC values against the saline solution EC values
# to see how effective the protocol was at creating the intended soil EC values

# setup

library(tidyverse)
library(janitor)
library(cowplot)

# get regression equation
source("code/mS_per_cm_to_dS_per_m.R")

# read in data
soil_ec <- read_csv("data/stipa_salinity_experiment/stipa_sample_harvested_2026-07-10.csv") %>% 
  mutate(soil_EC_ds_m = uS_per_cm_to_dS_per_m(EC_microS_cm),
         soil_EC_ds_m_simple = EC_microS_cm*5/1000)

# figure
fig_soil_EC_simple <- ggplot(data = soil_ec, aes(x = EC_water, y = soil_EC_ds_m_simple, color = watering_treatment)) +
  geom_point() +
  geom_line(linetype = "dashed") +
  scale_x_continuous(limits = c(0,NA), breaks = c(0, 0.8, 4, 7.5, 11)) +
  scale_y_continuous(limits = c(0,NA), breaks = c(0,2,4,6,8,10,12,14)) +
  theme_cowplot() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  # Calflora lists the maximum salinity as "moderately saline) (8.2 dS/m)
  # https://www.calflora.org/entry/compare.html?crn=12067
  # geom_hline(yintercept = 8.2, linetype = "dashed") +
  xlab("EC of water (dS/m)") +
  ylab("EC of soil (dS/m)") +
  labs(color = "Watering Treatment",
       title = "Simple conversion (no regression)") +
  annotate(
    "text", 
    x = 10, y = 9,            # Position slightly above the line midpoint
    label = "1:1 Line",                  # Matches the slope angle visually
    color = "gray50", 
    size = 4
  ) +
  coord_equal()
  

fig_soil_EC_simple 

ggsave("figures/Stipa_experiment_soil_EC_simple.png",
       width = 7,
       height = 7,
       bg = "white")


