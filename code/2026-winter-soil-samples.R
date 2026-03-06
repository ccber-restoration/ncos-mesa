#script for preliminary visualization of soil moisture and salinity from 
#soil cores collected in West and Central NCOS Mesa plots in winter 2026


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
soils_2026_winter <- read_csv("data/soil_samples/2026-winter-cores_2026-03-06.csv") %>% 
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
         clay = sapply(ssc, `[[`, "clay"),
         year = year(sample_date)
  ) %>%
  select(-ssc) %>%  # Remove temporary column
  #move quantitative columns next to texture
  relocate(sand:clay, .after = texture_code) %>% 
  mutate(plot = relevel(as.factor(plot), "West Plot", .))


# compare estimates
ggplot(data = soils_2026_winter, aes(x = ec_converted, y = ec_ds_per_m)) +
  geom_point() +
  #add 1:1 line
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(0,NA)) +
  scale_y_continuous(limits = c(0, NA))
  
# West plot subset ----
wp_soils_2026_winter <- soils_2026_winter %>% 
  filter(plot == "West Plot")

## calculate mean ----
wp_soils_2026_winter_mean <- wp_soils_2026_winter %>% 
  group_by(depth_midpoint) %>% 
  summarize(mean_moisture = mean(percent_soil_moisture),
            mean_conductivity = mean(ec_ds_per_m)) %>% 
  ungroup()


## graph moisture & EC ----
# use coord_flip(), based on this blog: https://rdoodles.rbind.io/2018/09/a-simple-ggplot-of-some-measure-against-depth/#:~:text=Second%20%E2%80%93%20make%20ggplot,TAGGED%20IN

fig_wp_moisture <- ggplot(data = wp_soils_2026_winter, aes(x = depth_midpoint, y = percent_soil_moisture, color = subplot)) +
  geom_point() +
  geom_path() +
  geom_path(data = wp_soils_2026_winter_mean, aes(x = depth_midpoint, y = mean_moisture), color = "black", linewidth = 2) +
  xlab("Sample depth (cm)") +
  ylab("% moisture") +
  coord_flip() +
  theme_cowplot() +
  scale_x_continuous(limits = c(-90,0), breaks = seq(-90, 0, by = 15)) +
  scale_y_continuous(
    limits = c(0,NA), 
                     position = "right") +
  #scale_color_manual(values = colors) +
  scale_colour_ptol() +
  labs(title = "West Plot, Jan-Feb 2026")

fig_wp_moisture

fig_wp_EC <- ggplot(data = wp_soils_2026_winter, aes(x = depth_midpoint, y = ec_ds_per_m, color = subplot)) +
  geom_point() +
  geom_path() +
  #plot mean values as black line
  geom_path(data = wp_soils_2026_winter_mean, aes(x = depth_midpoint, y = mean_conductivity), color = "black", linewidth = 2) +
  theme_cowplot() +
  coord_flip() +
  xlab("Sample depth (cm)") +
  ylab("Estimated EC (dS/m)") +
  scale_x_continuous(limits = c(-90,0), breaks = seq(-90, 0, by = 15) ) +
  scale_y_continuous(limits = c(0,NA), position = "right") +
  #scale_color_manual(values = colors) +
  scale_colour_ptol() +
  labs(title = " ")
  
fig_wp_EC

#assemble 2-panel figure
fig_west_plot_winter_2026 <- plot_grid(fig_wp_moisture + theme(legend.position = "none"), 
                                       fig_wp_EC + theme(legend.position = c(0.8, 0.7) 
                                                           #legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
                                                           #legend.box.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
                                                           ),
                                   nrow = 1)

fig_west_plot_winter_2026

ggsave(plot = fig_west_plot_winter_2026, 
       filename = "figures/soil_cores/Winter_2026_West_Plot_depth_profiles_2026-03-06.pdf",
       width = 8,
       height = 5,
       units = "in")

## west plot texture ----

# plot soil texture profiles for WP cores 1,2, 7 & 8
fig_wp_sand <- ggplot(data = wp_soils_2026_winter, aes(x = depth_midpoint, y = sand, color = subplot)) +
  geom_point() +
  #geom_jitter() +
  geom_path() +
  theme_cowplot() +
  coord_flip() +
  #ylab("Sample depth (cm)") +
  #xlab("% Sand (estimated)") +
  scale_x_continuous(limits = c(-90,0), breaks = seq(-90, 0, by =15)) +
  scale_y_continuous(limits = c(0,100), position = "right") +
  scale_color_ptol() +
  labs(title = "West Plot", y = "Sand (%)", x = "Depth (cm)")
  
fig_wp_sand

# save to file
ggsave("figures/soil_cores/wp_2026_sand.png",
       plot = fig_wp_sand)

fig_wp_clay <- ggplot(data = wp_soils_2026_winter, aes(x = depth_midpoint, y = clay, color = subplot)) +
  geom_point() +
  #geom_jitter() +
  geom_path() +
  theme_cowplot() +
  #facet_wrap(vars(subplot)) +
  coord_flip() +
  #ylab("Sample depth (cm)") +
  #xlab("% Sand (estimated)") +
  scale_x_continuous(limits = c(-90,0), breaks = seq(-90, 0, by =15)) +
  scale_y_continuous(limits = c(0,100), position = "right") +
  scale_color_ptol() +
  labs(title = "West Plot", y = "Clay (%)", x = "Depth (cm)")

fig_wp_clay

# save to file
ggsave("figures/soil_cores/wp_2026_clay.png",
       plot = fig_wp_clay)

# Central plot ----
cp_soils_2026_winter <- soils_2026_winter %>% 
  filter(plot == "Central Plot")

## calculate mean ----
cp_soils_2026_winter_mean <- cp_soils_2026_winter %>% 
  group_by(depth_midpoint) %>% 
  summarize(mean_moisture = mean(percent_soil_moisture),
            mean_conductivity = mean(ec_ds_per_m, na.rm = TRUE),
            n = n()) %>% 
  ungroup()

## graph moisture & EC ----
fig_cp_moisture <- ggplot(data = cp_soils_2026_winter, aes(x = depth_midpoint, y = percent_soil_moisture, color = subplot)) +
  geom_point() +
  geom_path() +
  geom_path(data = cp_soils_2026_winter_mean, aes(x = depth_midpoint, y = mean_moisture), color = "black", linewidth = 2) +
  xlab("Sample depth (cm)") +
  ylab("% moisture") +
  coord_flip() +
  theme_cowplot() +
  scale_x_continuous(limits = c(-90,0), breaks = seq(-90, 0, by = 15)) +
  scale_y_continuous(
    limits = c(0,NA), 
    position = "right") +
  #scale_color_manual(values = colors) +
  scale_colour_ptol() +
  labs(title = "Central Plot, Feb-March 2026")

fig_cp_moisture

fig_cp_EC <- ggplot(data = cp_soils_2026_winter, aes(x = depth_midpoint, y = ec_ds_per_m, color = subplot)) +
  geom_point() +
  geom_path() +
  
  geom_path(data = cp_soils_2026_winter_mean, aes(x = depth_midpoint, y = mean_conductivity), color = "black", linewidth = 2) +
  theme_cowplot() +
  coord_flip() +
  xlab("Sample depth (cm)") +
  ylab("Estimated EC (dS/m)") +
  scale_x_continuous(limits = c(-90,0), breaks = seq(-90, 0, by = 15) ) +
  scale_y_continuous(limits = c(0,NA), position = "right") +
  #scale_color_manual(values = colors) +
  scale_color_ptol() +
  labs(title = " ")

fig_cp_EC

#assemble 2-panel figure
fig_cp_plot_winter_2026 <- plot_grid(fig_cp_moisture + theme(legend.position = "none"), 
                                       fig_cp_EC + theme(legend.position = c(0.8, 0.7) 
                                                           #legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
                                                           #legend.box.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
                                       ),
                                       nrow = 1)

fig_cp_plot_winter_2026

ggsave(plot = fig_cp_plot_winter_2026, 
       filename = "figures/soil_cores/Winter_2026_Central_Plot_depth_profiles_2026-03-05.pdf",
       width = 8,
       height = 5,
       units = "in")

## graph central plot texture ----

#TODO


# ~~~~~~~~~~~~~~~~~~~~~ ----

# Faceted EC plot ----

#calculate means for both plots combined
means_2026 <- soils_2026_winter %>% 
  group_by(plot, depth_midpoint) %>% 
  summarize(mean_ec = mean(ec_ds_per_m, na.rm = TRUE),
            mean_moisture = mean(percent_soil_moisture),
            n = n()) %>% 
  ungroup()


fig_2026_ec <- ggplot(data = soils_2026_winter, aes(x = depth_midpoint, y = ec_ds_per_m, color = subplot)) +
  geom_path(data = means_2026, aes(x = depth_midpoint, y = mean_ec), color = "black", linewidth = 2) +
  geom_point() +
  geom_path() +
  xlab("Sample depth (cm)") +
  ylab("EC") +
  coord_flip() +
  theme_cowplot() +
  facet_wrap(facets = vars(plot)) +
  scale_x_continuous(limits = c(-90,0), breaks = seq(-90, 0, by =15)) +
  scale_y_continuous(limits = c(0,NA), position = "right") +
  labs(y = "Estimated EC (dS/m)", x = "Depth (bin midpoint, cm)", title = "Jan-Mar 2026") +
  scale_color_ptol()

fig_2026_ec

ggsave(filename = "figures/soil_cores/winter_2026_EC_faceted.png",
       fig_2026_ec,
       width = 6,
       height = 4,
       units = "in",
       bg = "white")

## faceted moisture plot ---- 
fig_2026_moisture <- ggplot(data = soils_2026_winter, aes(x = depth_midpoint, y = percent_soil_moisture, color = subplot)) +
  geom_path(data = means_2026, aes(x = depth_midpoint, y = mean_moisture), color = "black", linewidth = 2) +
  geom_point() +
  geom_path() +
  xlab("Sample depth (cm)") +
  ylab("EC") +
  coord_flip() +
  theme_cowplot() +
  facet_wrap(facets = vars(plot)) +
  scale_x_continuous(limits = c(-90,0), breaks = seq(-90, 0, by =15)) +
  scale_y_continuous(limits = c(0,NA), position = "right") +
  labs(y = "% moisture", x = "Depth (bin midpoint, cm)", title = "Jan-Mar 2026") +
  scale_color_ptol()

fig_2026_moisture

ggsave(filename = "figures/soil_cores/winter_2026_moisture_faceted.png",
       fig_2026_moisture,
       width = 6,
       height = 4,
       units = "in",
       bg = "white")


# ~~~~~~~~~~~~~~~~~~~~~ ----
# old code ----

# devtools::install_github("an-bui/calecopal")
# 
calecopal::cal_palette("arbutus",
                       n = 8,
                       type = "continuous")

colors <- c("#DFE3CE", "#C7D38F", "#AEC366", "#96B07D", "#9C9D82", "#C18976", "#B47565", "#976153")


# first draft ----

fig_moisture <- ggplot(data = soils_2026_winter, aes(x = depth, y = percent_soil_moisture, color= subplot)) +
  geom_point() +
  theme_cowplot() +
  xlab("Depth bin (cm)") +
  ylab("% moisture") +
  scale_y_continuous(limits = c(0,NA)) +
  scale_color_manual(values = colors) +
  labs(title = "January 2026 West Plot")

fig_moisture

fig_conductivity <- ggplot(data = soils_2026_winter, aes(x = depth, y = e_c_u_s_cm, color = subplot)) +
  geom_point() +
  theme_cowplot() +
  xlab("Depth bin (cm)") +
  ylab("Conductivity (microSiemens/cm)") +
  scale_color_manual(values = colors) +
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


