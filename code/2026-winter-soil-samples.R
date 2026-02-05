#script for preliminary visualization of soil moisture and salinity

library(tidyverse)
library(janitor)
library(cowplot)

soils_2026_winter <- read_csv("data/soil_samples/2026-winter-cores_2026-02-04.csv") %>% 
  clean_names() %>% 
  mutate(subplot = as.factor(subplot),
         depth_midpoint = -(depth_top + depth_bottom)/2
         ) 
 

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

# re-do figures as depth profiles

fig_moisture_profile <- ggplot(data = soils_2026_winter, aes(y = depth_midpoint, x = percent_soil_moisture, color = subplot)) +
  geom_point() +
  geom_path() +
  ylab("Sample depth (cm)") +
  xlab("% moisture") +
  theme_cowplot() +
  scale_y_continuous(limits = c(NA,0)) +
  scale_x_continuous(limits = c(0,NA))

fig_moisture_profile

fig_conductivity_profile <- ggplot(data = soils_2026_winter, aes(y = depth_midpoint, x = e_c_u_s_cm, color = subplot)) +
  geom_point() +
  geom_path() +
  theme_cowplot() +
  ylab("Sample depth (cm)") +
  xlab("Conductivity (microSiemens/cm)") +
  scale_y_continuous(limits = c(NA,0)) +
  scale_x_continuous(limits = c(0,NA))

fig_conductivity_profile

#assemble 2-panel figure

fig_WP_winter_profile <- plot_grid(fig_moisture_profile, fig_conductivity_profile,
                           nrow = 1)

fig_WP_winter_profile

ggsave(plot = fig_WP_winter_profile, filename = "figures/Winter_2026_West_Plot_depth_profiles_DRAFT.pdf")



