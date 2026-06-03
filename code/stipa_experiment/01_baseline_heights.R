library(tidyverse)
library(readxl)
library(cowplot)

baseline_heights <- read_xlsx(path = "data/stipa_salinity_experiment/Stipa_salinity_experiment_data_2026-04-24.xlsx") %>% 
  filter(is.na(notes))


median_height <- median(baseline_heights$height_cm)

height_summary <- baseline_heights %>% 
  group_by(water, salinity_treatment) %>% 
  summarize(mean_height = mean(height_cm),
            n = n())

fig_baseline_ht <- ggplot(data = baseline_heights, aes(x = water, y = height_cm, color = water)) +
  geom_point(alpha = 0.5) +
  #geom_point(position = position_dodge(width = 2)) +
  geom_point( data = height_summary, aes(x = water, y = mean_height, color = water), size = 5) +
  xlab("EC (dS/m)") +
  ylab("Height (cm)") +
  facet_wrap(vars(salinity_treatment), nrow = 1) +
  #scale_y_continuous(limits = c(0,NA)) +
  #scale_x_continuous(limits = c(0,NA), breaks = c(0.8, 4, 7.5, 11)) +
  theme_cowplot() +
  geom_hline(yintercept = median_height, linetype = "dashed")

fig_baseline_ht
