#script for preliminary visualization of soil moisture and salinity

library(tidyverse)
library(janitor)
library(cowplot)

soils_2026_winter <- read_csv("data/soil_samples/2026-winter-cores_2026-02-04.csv") %>% 
  clean_names() %>% 
  mutate(subplot = as.factor(subplot))


fig_moisture <- ggplot(data = soils_2026_winter, aes(x = depth, y = percent_soil_moisture, color= subplot)) +
  geom_point() +
  theme_cowplot() +
  xlab("Depth bin (cm)") +
  ylab("% moisture") +
  scale_y_continuous(limits = c(0,NA)) +
  labs(title = "January 2026 West Plot")

fig_moisture

fig_conductivity <- ggplot(data = soils_2026_winter, aes(x = depth, y = e_c_u_s_cm, color = subplot)) +
  geom_point() +
  theme_cowplot() +
  xlab("Depth bin (cm)") +
  ylab("Conductivity (microSiemens/cm)") +
  scale_y_continuous(limits = c(0,NA))

fig_conductivity

fig_WP_winter <- plot_grid(fig_moisture, fig_conductivity,
                           nrow = 2)

fig_WP_winter

ggsave(filename = "figures/Winter_2026_West_Plot_moisture_conductivity_DRAFT.pdf")
