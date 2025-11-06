#subsetting and updating code from mesa_soils.R

# Gravimetric water content ----
# According to Kellen sample point M1 is close to the Upper array of
# sensors and M2 is close to the lower array of sensors, so I guess we
# should plot the soil moisture (gravimetric) by date with M1 data from
# surface and sub-surface (same depths of 6 and 18 inches) on the Upper
# array and M2 on the lower array!

#read in data

mesa_gwc <- read_csv("data/mesa_slope_gravimetric_soil_data_2025-09-04.csv") %>% 
  separate_wider_delim(sample_id, 
                       names = c("site", "sample", "depth_in"), 
                       delim = "_", cols_remove = FALSE) %>%
  #get rid of 2-day dry mass
  select(-dry_weight_2d) %>% 
  #calculate water content
  mutate(soil_moisture = ((wet_weight_g - tin_weight_g) -(dry_weight_4d - tin_weight_g))/(dry_weight_4d - tin_weight_g),
         gwc_percent = round((soil_moisture*100),digits = 3))

#salinity depths
salinity_sample_depths <- c("18", "6", "7")

mesa_gwc_wrangled <- mesa_gwc %>%  
  #don't filter to just two sample points
  #filter(site == "M" & sample <3) %>% 
  #filter(depth_in %in% c("6", "18")) %>% 
  unite("sample_point", site, sample, sep = "_", remove = TRUE) %>% 
  mutate(
    slope_position = case_when(
      sample_point == "M_1" ~ "upper",
      sample_point == "M_2" ~ "lower"),
    depth = case_when(
      depth_in == "6" ~ "shallow",
      depth_in == "18" ~ "deep"
    )
  ) %>% 
  filter(depth_in %in% salinity_sample_depths) %>% 
  mutate(depth_in = as.numeric(depth_in)) %>% 
  filter(!is.na(eC_uS_cm))

  

fig_ec <- ggplot(data = mesa_gwc_wrangled, aes(x = sample_date, y = eC_uS_cm, color = sample_point, group = sample_point
                                               )) +
  geom_point() +
  #geom_boxplot() +
  geom_line(linetype = "dashed") +
  theme_cowplot() +
  xlab("Date") +
  ylab("EC 1:5 Ratio (µS/cm)") + 
  facet_wrap(vars(depth_in))
 
fig_ec

ggsave(filename = "figures/Salinity_uS_cm_2025_timeseries.pdf", plot = fig_ec)
