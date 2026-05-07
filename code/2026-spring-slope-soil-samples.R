#script for graphing soil moisture, salinity, and texture of 
#soil samples collected by Miles Clark from the Mesa Slopoe in spring quarter 2026

# setup ----
library(tidyverse)
library(janitor)

#plotting
library(cowplot)
library(ggthemes) # to use Paul Tol color palettes

#packages specifically for soil science
library(aqp) # algorithms for quantitative pedology
#library(soiltexture)

#load custom function for estimating salinity in more standard units
#estimate dS per m from 5:1 method ()

source("code/mS_per_cm_to_dS_per_m.R")

#read in data, get depth midpoints, and estimate EC in dS/m
soils_2026_spring <- read_csv("data/soil_samples/NCOS_Mesa_Slope_Spring_2026_cores.csv") %>% 
  clean_names() %>% 
  mutate(
         depth_midpoint = -(depth_top + depth_bottom)/2,
         ec_ds_per_m = uS_per_cm_to_dS_per_m(ec_u_s_cm),
         ec_converted = (ec_u_s_cm/1000)*5,
         # use lapply to apply the conversion function to the vector
         ssc = lapply(texture_code, texcl_to_ssc),
         percent_soil_moisture = percent_soil_moisture*100,
         #subset ssc to the corresponding columns
         sand = sapply(ssc, `[[`, "sand"),
         silt = sapply(ssc, `[[`, "silt"),
         clay = sapply(ssc, `[[`, "clay"),
         year = year(sample_date)
  ) %>%
  select(-ssc) %>%  # Remove temporary column
  #move quantitative columns next to texture
  relocate(sand:clay, .after = texture_code)

# calculate means by depth ----

#calculate mean values for both plots combined
means_2026_spring <- soils_2026_spring %>% 
  group_by(depth_midpoint) %>% 
  summarize(mean_ec = mean(ec_ds_per_m, na.rm = TRUE),
            mean_moisture = mean(percent_soil_moisture),
            mean_sand = mean(sand),
            mean_clay = mean(clay),
            n = n()) %>% 
  ungroup()


# ~~~~~~~~~~~~~~~~~~~~~ ----

# Texture  ----

##   Sand  ----
fig_2026_spring_sand <- ggplot(data = soils_2026_spring, aes(x = depth_midpoint, y = sand)) +
  geom_point(aes(color = zone, group = zone)) +
  #geom_jitter() +
  geom_path(aes(color = zone, group = zone)) +
  geom_path(data = means_2026_spring, aes(x = depth_midpoint, y = mean_sand), color = "black", linewidth = 2) +
  theme_cowplot() +
  coord_flip() +
  #ylab("Sample depth (cm)") +
  #xlab("% Sand (estimated)") +
  scale_x_continuous(limits = c(-75,0), breaks = seq(-75, 0, by =15)) +
  scale_y_continuous(limits = c(0,100), position = "right") +
  scale_color_ptol() +
  labs(title = "Mesa Slope, April 2026", y = "Sand (%)", x = "Depth (cm)")

fig_2026_spring_sand

## Clay  ----
fig_2026_spring_clay <- ggplot(data = soils_2026_spring, aes(x = depth_midpoint, y = clay)) +
  geom_point(aes(color = zone, group = zone)) +
  #geom_jitter() +
  geom_path(aes(color = zone, group = zone)) +
  geom_path(data = means_2026_spring, aes(x = depth_midpoint, y = mean_clay), color = "black", linewidth = 2) +
  theme_cowplot() +
  coord_flip() +
  #ylab("Sample depth (cm)") +
  #xlab("% Sand (estimated)") +
  scale_x_continuous(limits = c(-75,0), breaks = seq(-75, 0, by =15)) +
  scale_y_continuous(limits = c(0,100), position = "right") +
  scale_color_ptol() +
  labs(title = "Mesa Slope, April 2026", y = "Clay (%)", x = "Depth (cm)")

fig_2026_spring_clay

## combined plot ----
fig_slope_texture <- plot_grid(fig_2026_spring_sand, fig_2026_spring_clay,
                            nrow = 1)
fig_slope_texture

#save to file
ggsave(filename = "figures/soil_cores/Spring_2026_Mesa_Slope_texture.pdf", 
       plot = fig_slope_texture,
       width = 8,
       height = 5,
       units = "in")



# EC  ----
fig_2026_spring_ec <- ggplot(data = soils_2026_spring, aes(x = depth_midpoint, y = ec_ds_per_m, color = zone)) +
  geom_point() +
  geom_path() +
  geom_path(data = means_2026_spring, aes(x = depth_midpoint, y = mean_ec), color = "black", linewidth = 2) +
  xlab("Sample depth (cm)") +
  ylab("EC") +
  coord_flip() +
  theme_cowplot() +
  scale_x_continuous(limits = c(-75,0), breaks = seq(-75, 0, by =15)) +
  scale_y_continuous(limits = c(0,NA), position = "right") +
  labs(y = "EC (dS/m)", x = "Depth (cm)", title = "Mesa Slope cores, April 2026") +
  scale_color_ptol()

fig_2026_spring_ec

#save to file
ggsave(filename = "figures/soil_cores/Spring_2026_Mesa_Slope_EC.pdf",
       fig_2026_spring_ec,
       width = 6,
       height = 4,
       units = "in",
       bg = "white")

#  moisture plot ---- 
fig_2026_spring_moisture <- ggplot(data = soils_2026_spring, aes(x = depth_midpoint, y = percent_soil_moisture, color = zone)) +
  geom_point() +
  geom_path() +
  geom_path(data = means_2026_spring, aes(x = depth_midpoint, y = mean_moisture), color = "black", linewidth = 2) +
  xlab("Sample depth (cm)") +
  ylab("EC") +
  coord_flip() +
  theme_cowplot() +
  scale_x_continuous(limits = c(-75,0), breaks = seq(-75, 0, by =15)) +
  scale_y_continuous(limits = c(0,NA), position = "right") +
  labs(y = "% moisture", x = "Depth (cm)", title = "Mesa Slope, April 2026") +
  scale_color_ptol()

fig_2026_spring_moisture

ggsave(filename = "figures/soil_cores/Spring_2026_Mesa_Slope_moisture.pdf",
       fig_2026_spring_moisture,
       width = 6,
       height = 4,
       units = "in",
       bg = "white")
