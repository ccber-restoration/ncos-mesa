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

## lower array ----
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


#plot1 with upper label and colored by depth
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