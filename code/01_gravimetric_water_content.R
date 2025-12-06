# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----

# Gravimetric water content ----
# According to Kellen sample point M1 is close to the Upper array of
# sensors and M2 is close to the lower array of sensors, so I guess we
# should plot the soil moisture (gravimetric) by date with M1 data from
# surface and sub-surface (same depths of 6 and 18 inches) on the Upper
# array and M2 on the lower array!

#read in data

mesa_gwc <- read_csv("data/soil_samples/mesa_slope_gravimetric_soil_data_2025-08-04.csv") %>% 
  separate_wider_delim(sample_id, 
                       names = c("site", "sample", "depth_in"), 
                       delim = "_", cols_remove = FALSE) %>%
  #get rid of 2-day dry mass
  select(-dry_weight_2d) %>% 
  #calculate water content
  mutate(soil_moisture = ((wet_weight_g - tin_weight_g) -(dry_weight_4d - tin_weight_g))/(dry_weight_4d - tin_weight_g),
         gwc_percent = round((soil_moisture*100),digits = 3))

mesa_gwc_filter <- mesa_gwc %>%  
  #filter to just M1 & M2 sample points
  filter(site == "M" & sample <3) %>% 
  filter(depth_in %in% c("6", "18")) %>% 
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
  clean_names()

#plot over time

fig_gwc <- ggplot(data = mesa_gwc_filter, aes(x = sample_date, y = soil_moisture, group = sample_id, color = depth_in )) +
  geom_point() +
  geom_line() +
  LA_theme +
  vwc_scale +
  xlab("Date") +
  ylab("Gravimetric water content") +
  facet_wrap(vars(slope_position))

fig_gwc