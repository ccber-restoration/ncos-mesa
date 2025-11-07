# set up ----
source("code/00_setup.R")

#Volumetric water content ----

## upper array ----
# load in upper VWC sensor data
upper <- read_csv("data/volumetric_water_content/z6B01375 27Jul25-1901upperall.csv", na = c("NA", "", "#N/A")) %>% 
  #clean up names
  clean_names() %>% 
  #reformat timestamp as a proper date-time
  mutate(timestamp = mdy_hm(timestamp))

upper_long <- upper %>% 
  #remove battery columns
  select(-bat_percent,-m_v_battery_voltage) %>% 
  #now pivot long
  pivot_longer(names_to = "port", cols = c(2:7), values_to = "vwc") %>% 
  #add in depth column
  mutate(depth = case_when(
    port %in% c("port1_vwc", "port3_vwc", "port5_vwc") ~ "shallow",
    port %in% c("port2_vwc", "port4_vwc", "port6_vwc") ~ "deep",
    .default = NA
  )) %>% 
  mutate(depth = fct_relevel(depth, "shallow", "deep"))

## lower array data ----
# load in lower VWC sensor data
lower <- read_csv("data/volumetric_water_content/z6B01383 27Jul25-1904lowerall.csv", na = c("NA", "", "#N/A")) %>% 
  #clean up names
  clean_names() %>% 
  #reformat timestamp as a proper date-time
  mutate(timestamp = mdy_hm(timestamp))

lower_long <- lower %>% 
  #remove battery columns
  select(-bat_percent,-m_v_battery_voltage) %>% 
  #now pivot long
  pivot_longer(names_to = "port", cols = c(2:7), values_to = "vwc") %>% 
  #add in depth column
  mutate(depth = case_when(
    port %in% c("port1_vwc", "port3_vwc", "port5_vwc") ~ "shallow",
    port %in% c("port2_vwc", "port4_vwc", "port6_vwc") ~ "deep",
    .default = NA
  ))

## visualize VWC ----

#create y axis scale
vwc_scale <- scale_y_continuous(limits = c(0,0.7))


#plot1 with upper lable and colored by depth
plot_upper1 <- ggplot(upper_long, aes(x=timestamp, y=vwc, col=depth)) + 
  geom_line(aes(group = port)) +
  ggtitle("Upper") +
  scale_x_datetime(date_breaks = "2 month",
                   date_labels = "%b") +
  LA_theme +
  vwc_scale

plot_upper1

#plot2 without title, different color for each port 
plot_upper2 <- ggplot(upper_long, aes(x=timestamp, y=vwc, col=port)) + 
  geom_line() +
  scale_x_datetime(date_breaks = "2 month",
                   date_labels = "%b") +
  LA_theme +
  vwc_scale

plot_upper2

# lower plots 
plot_lower1 <- ggplot(lower_long, aes(x=timestamp, y=vwc, col=depth)) + 
  geom_line(aes(group = port)) +
  ggtitle("Lower") +
  scale_x_datetime(date_breaks = "2 month",
                   date_labels = "%b") +
  LA_theme +
  vwc_scale

plot_lower1

#second lower plot, by port
plot_lower2 <- ggplot(lower_long, aes(x=timestamp, y=vwc, col=port)) + 
  geom_line() +
  scale_x_datetime(date_breaks = "2 month",
                   date_labels = "%b") +
  LA_theme +
  vwc_scale

plot_lower2

fig_vwc <- plot_grid(plot_upper1, plot_lower1, plot_upper2, plot_lower2,
                     nrow = 2) 
 

fig_vwc

ggsave("figures/Mesa_Slope_VWC_2025-07-27.pdf",
       fig_vwc)


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

  
# ~~~~~~~~~~~~~~~~~~~~~~~~~ ----

#Combine data sets ----

#get list of gwc sampling dates
sample_dates <- unique(mesa_gwc_filter$sample_date)

#filter vwc to sample dates, then summarize by date
upper_vwc_filt <- upper_long %>% 
  mutate(date = date(timestamp)) %>% 
  filter(date %in% sample_dates) %>% 
  group_by(date, port, depth) %>% 
  summarize(mean_vwc = mean(vwc)) %>% 
  ungroup() %>% 
  mutate(slope_position = "upper")

lower_vwc_filt <- lower_long %>% 
  mutate(date = date(timestamp)) %>% 
  filter(date %in% sample_dates) %>% 
  group_by(date, port, depth) %>% 
  summarize(mean_vwc = mean(vwc)) %>% 
  ungroup() %>% 
  mutate(slope_position = "lower")

vwc_filt_combined <- rbind(upper_vwc_filt, lower_vwc_filt) %>% 
  #group by date, array, and depth
  group_by(date, slope_position, depth) %>% 
  summarize(vwc = mean(mean_vwc)) %>% 
  ungroup()

wc_combined <- mesa_gwc_filter %>%
  left_join(vwc_filt_combined, by =join_by(slope_position, depth, sample_date == date)) %>% 
  unite("point", slope_position, depth, sep = "_", remove = FALSE) %>% 
  mutate(timestamp = as.POSIXct(sample_date)) %>% 
  mutate(depth = fct_relevel(depth, "shallow", "deep")) %>% 
  mutate(slope_position = fct_relevel(slope_position, "upper", "lower"))

# scatterplot ----
fig_wc_comparison <- ggplot(data = wc_combined, aes(x = soil_moisture, y = vwc, color = depth)) +
  geom_point() +
  xlab("Gravimetric water content") +
  ylab("Volumetric water content") + 
  geom_abline(slope = 1, intercept = 0) +
  theme_cowplot() +
  scale_x_continuous(limits = c(0,NA), expand = c(0,NA)) +
  scale_y_continuous(limits = c(0,NA),expand = c(0,NA)) +
  facet_wrap(vars(slope_position), nrow = 1) +
  coord_fixed()
 
fig_wc_comparison

ggsave("figures/mesa_slope_wc_scatterplot_2025-08-01.pdf")

#wc time series ----

gwc_upper <- wc_combined %>% 
  filter(slope_position == "upper")

gwc_lower <- wc_combined %>% 
  filter(slope_position == "lower")

fig_upper_combined <- plot_upper1 +
  #add dotted lines for sample dates
  geom_vline(xintercept = as.numeric(as.POSIXct("2025-02-28")), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2025-04-07")), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2025-04-23")), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2025-05-07")), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2025-06-01")), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2025-07-17")), linetype = "dotted") +
  geom_point(data = gwc_upper,
             aes(x = timestamp, y = soil_moisture, fill = depth)) +
  ylab("Water content") +
  xlab("Date")

fig_upper_combined 

fig_lower_combined <- plot_lower1 +
  #geom_vline(xintercept = as.numeric(as.POSIXct("2025-02-28")), linetype = "dotted") +
  #geom_vline(xintercept = as.numeric(as.POSIXct("2025-04-07")), linetype = "dotted") +
  #geom_vline(xintercept = as.numeric(as.POSIXct("2025-04-23")), linetype = "dotted") +
  #geom_vline(xintercept = as.numeric(as.POSIXct("2025-05-07")), linetype = "dotted") +
  #geom_vline(xintercept = as.numeric(as.POSIXct("2025-06-01")), linetype = "dotted") +
  #geom_vline(xintercept = as.numeric(as.POSIXct("2025-07-17")), linetype = "dotted") +
  geom_point(data = gwc_lower, 
             aes(x = timestamp, y = soil_moisture, fill = depth)) +
  ylab("Water content") +
  xlab("Date")
  
fig_lower_combined


legend <- get_legend(fig_upper_combined)

fig_combined <- plot_grid(fig_upper_combined + 
                            theme(legend.position = "none") + 
                            ggtitle("Upper slope"), 
                          fig_lower_combined + 
                            theme(legend.position = "none") +
                            ggtitle("Lower slope")
                          ,
                          legend,
                          rel_widths = c(1,1,0.3),
                          nrow = 1)

fig_combined

ggsave("figures/mesa_slope_water_content_comparison_2025-08-04.pdf",
       fig_combined,
       width = 180,
       units = "mm")


