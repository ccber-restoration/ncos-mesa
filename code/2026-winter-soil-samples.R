#script for preliminary visualization of soil moisture and salinity


# setup ----
library(tidyverse)
library(janitor)
library(cowplot)

#packages specifically for soil science

library(soiltexture)

#algorithms for quantitative pedology
library(aqp)

#load custom function for estimating salinity in more standard units
#estimate dS per m from 5:1 method ()

source("code/mS_per_cm_to_dS_per_m.R")

#read in data, get depth midpoints, and estimate EC in dS/m

soils_2026_winter <- read_csv("data/soil_samples/2026-winter-cores_2026-02-17.csv") %>% 
  clean_names() %>% 
  mutate(subplot = as.factor(subplot),
         depth_midpoint = -(depth_top + depth_bottom)/2,
         ec_ds_per_m = uS_per_cm_to_dS_per_m(e_c_u_s_cm),
         ec_converted = (e_c_u_s_cm/1000)*5,
         # use lapply to apply the conversion function to the vector
         ssc = lapply(texture_code, texcl_to_ssc),
         #subset ssc to the corresponding columns
         sand = sapply(ssc, `[[`, "sand"),
         silt = sapply(ssc, `[[`, "silt"),
         clay = sapply(ssc, `[[`, "clay")
  ) %>%
  select(-ssc) %>%  # Remove temporary column
  #move quantitative columns next to texture
  relocate(sand:clay, .after = texture_code)


# compare estimates
ggplot(data = soils_2026_winter, aes(x = ec_converted, y = ec_ds_per_m)) +
  geom_point() +
  #add 1:1 line
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(0,NA)) +
  scale_y_continuous(limits = c(0, NA))
  

# calculate mean ----

soils_2026_winter_mean <- soils_2026_winter %>% 
  group_by(depth_midpoint) %>% 
  summarize(mean_moisture = mean(percent_soil_moisture),
            mean_conductivity = mean(ec_ds_per_m)) %>% 
  ungroup()


# revised version using coord_flip ----
#based on this blog: https://rdoodles.rbind.io/2018/09/a-simple-ggplot-of-some-measure-against-depth/#:~:text=Second%20%E2%80%93%20make%20ggplot,TAGGED%20IN

fig_moisture_flip <- ggplot(data = soils_2026_winter, aes(x = depth_midpoint, y = percent_soil_moisture, color = subplot)) +
  geom_point() +
  geom_path() +
  geom_path(data = soils_2026_winter_mean, aes(x = depth_midpoint, y = mean_moisture), color = "black", linewidth = 2) +
  xlab("Sample depth (cm)") +
  ylab("% moisture") +
  coord_flip() +
  theme_cowplot() +
  scale_x_continuous(limits = c(NA,0)) +
  scale_y_continuous(
    limits = c(0,NA), 
                     position = "right") +
  labs(title = "NCOS Mesa West Plot, Jan-Feb 2026")

fig_moisture_flip

fig_EC_flip <- ggplot(data = soils_2026_winter, aes(x = depth_midpoint, y = ec_ds_per_m, color = subplot)) +
  geom_point() +
  geom_path() +
  
  geom_path(data = soils_2026_winter_mean, aes(x = depth_midpoint, y = mean_conductivity), color = "black", linewidth = 2) +
  theme_cowplot() +
  coord_flip() +
  xlab("Sample depth (cm)") +
  ylab("Estimated conductivity (dS/m)") +
  scale_x_continuous(limits = c(NA,0)) +
  scale_y_continuous(limits = c(0,NA), position = "right") +
  labs(title = " ")
  
fig_EC_flip

#assemble 2-panel figure
fig_west_plot_winter_2026 <- plot_grid(fig_moisture_flip + theme(legend.position = "none"), 
                                       fig_EC_flip + theme(legend.position = c(0.6, 0.8)),
                                   nrow = 1)

fig_west_plot_winter_2026

ggsave(plot = fig_west_plot_winter_2026, filename = "figures/Winter_2026_West_Plot_depth_profiles_DRAFT_2026-02-16.pdf")

# plot soil profiles for cores 7 & 8

fig_texture <- ggplot(data = soils_2026_winter, aes(x = depth_midpoint, y = sand, color = subplot)) +
  geom_point() +
  geom_path() +
  theme_cowplot() +
  coord_flip() +
  #ylab("Sample depth (cm)") +
  #xlab("% Sand (estimated)") +
  scale_x_continuous(limits = c(NA,0)) +
  scale_y_continuous(limits = c(0,NA), position = "right") +
  labs(title = "West Plot", y = "Sand (%)", x = "Depth (cm)")
  
  fig_texture
  

  # ~~~~~~~~~~~~~~~~~~~~~ ----
# ~~~~~~~~~~~~~~~~~~~~~ ----
# old code ----

# first draft ----

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

#ggsave(filename = "figures/Winter_2026_West_Plot_moisture_conductivity_DRAFT.pdf")

# second draft- re-do figures as depth profiles ----

fig_moisture_profile <- ggplot(data = soils_2026_winter, aes(y = depth_midpoint, x = percent_soil_moisture, color = subplot)) +
  geom_point() +
  geom_path() +
  ylab("Sample depth (cm)") +
  xlab("% moisture") +
  theme_cowplot() +
  scale_y_continuous(limits = c(NA,0)) +
  scale_x_continuous(limits = c(0,NA)) +
  labs(title = "West Plot, Jan 2026")

fig_moisture_profile

fig_conductivity_profile <- ggplot(data = soils_2026_winter, aes(y = depth_midpoint, x = ec_ds_per_m, color = subplot)) +
  geom_point() +
  geom_path() +
  theme_cowplot() +
  ylab("Sample depth (cm)") +
  xlab("Estimated conductivity (dS/m)") +
  scale_y_continuous(limits = c(NA,0)) +
  scale_x_continuous(limits = c(0,NA))

fig_conductivity_profile

#assemble 2-panel figure

fig_WP_winter_profile <- plot_grid(fig_moisture_profile, fig_conductivity_profile,
                                   nrow = 1)

fig_WP_winter_profile

#ggsave(plot = fig_WP_winter_profile, filename = "figures/Winter_2026_West_Plot_depth_profiles_DRAFT.pdf")


