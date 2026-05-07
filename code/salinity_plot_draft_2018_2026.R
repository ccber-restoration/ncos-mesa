#use soils_2018 and soils_2026_winter (from 2026-winter-soil-samples.R) as inputs

#select subset of columns that match with 2026 data and do some renaming
ec_2018 <- soils_2018 %>% 
  mutate(year = year(sample_date)) %>% 
  select(c(plot, year, subplot_number, depth_midpoint, ec_ds_m)) %>% 
  rename(subplot = subplot_number)

ec_2026 <- soils_2026_winter %>% 
  select(plot, year, subplot, depth_midpoint, ec_ds_per_m) %>% 
  rename(ec_ds_m = ec_ds_per_m)

ec_combined <- bind_rows(ec_2018, ec_2026) %>% 
  mutate(year = as.factor(year)) %>% 
  #fix a few non-standard depth bins
  mutate(depth_midpoint = case_when(
    depth_midpoint %in% c(-78.0, -77.5, -77.5) ~ -82.5,
    .default = depth_midpoint
  ))

ec_combined_mean <- ec_combined %>%
  group_by(plot, year, depth_midpoint) %>% 
  summarize(mean_ec = mean(ec_ds_m, na.rm = TRUE),
            n = n()) %>% 
  ungroup()


  
fig_ec_combined <- ggplot(data = ec_combined, aes(x = depth_midpoint, y = ec_ds_m, group = subplot)) +
  #geom_path(aes(color = year), alpha = 0.5) +
  geom_path(data = ec_combined_mean, aes(x = depth_midpoint, y = mean_ec, group = year, color = year), linewidth = 2) +
  #geom_point() +
  #geom_path() +
  xlab("Sample depth (cm)") +
  ylab("EC") +
  coord_flip() +
  theme_cowplot() +
  facet_wrap(facets = vars(plot)) +
  scale_x_continuous(limits = c(-90,0), breaks = seq(-90, 0, by =15)) +
  scale_y_continuous(limits = c(0,NA), position = "left") +
  labs(y = "EC (dS/m)", x = "Depth (cm)", title = "2018 & 2026") +
  scale_color_ptol()

fig_ec_combined 

#ggplotly(fig_ec_combined)

ggsave("figures/soil_cores/sercal/2018_2026_comparison_lines.png", fig_ec_combined,
       width = 7.46,
       height = 5.45,
       units = "in",
       bg = "white")

# for SERCAL, just Central plot

ec_combined_cp <- ec_combined %>% 
  filter(plot == "Central Plot")

ec_combined_mean_cp <- ec_combined_cp %>%
  group_by(plot, year, depth_midpoint) %>% 
  summarize(mean_ec = mean(ec_ds_m, na.rm = TRUE),
            n = n()) %>% 
  ungroup()


fig_ec_combined_cp <- ggplot(data = ec_combined_cp, aes(x = depth_midpoint, y = ec_ds_m, group = subplot)) +
  #geom_path(aes(color = year), alpha = 0.5) +
  geom_path(data = ec_combined_mean_cp, aes(x = depth_midpoint, y = mean_ec, group = year, color = year), linewidth = 2) +
  #geom_point() +
  #geom_path() +
  xlab("Sample depth (cm)") +
  ylab("EC") +
  coord_flip() +
  theme_cowplot() +
  #facet_wrap(facets = vars(plot)) +
  scale_x_continuous(limits = c(-90,0), breaks = seq(-90, 0, by =15)) +
  scale_y_continuous(limits = c(0,NA), position = "left") +
  labs(y = "EC (dS/m)", x = "Depth (cm)", title = "Central plot: 2018 vs. 2026") +
  scale_color_ptol()

fig_ec_combined_cp 

ggsave("figures/soil_cores/sercal/Central_Plot_EC_2018_2026_comparison_lines.png", fig_ec_combined,
       width = 7.46,
       height = 5.45,
       units = "in",
       bg = "white")



  