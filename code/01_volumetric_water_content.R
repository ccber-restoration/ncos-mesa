# set up ----
source("code/00_setup.R")

#Volumetric water content ----

## upper array ----
# load in upper VWC sensor data

upper <- readxl::read_xlsx("data/volumetric_water_content/raw_sensor_data/z6B01375 11Nov25-1146_NO_ROWS_SINCE_9-9-25.xlsx", 
                           sheet = "Processed Data Config 1",
                           col_names = c("Timestamp",	"Port1_VWC",	"Port2_VWC",	"Port3_VWC",	"Port4_VWC",	"Port5_VWC",	"Port6_VWC",	"Bat_percent",	"mV.Battery.Voltage"),
                           col_types = c("date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), 
                           skip=3) %>%
  #clean up names
  clean_names() %>% 
  #Looks like the battery came back to life briefly after 2025-09-09, but date was reset...
  #filter to data after Jan 1 2025
  filter(timestamp > as.POSIXct("2025-01-01"))
                
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

#note, seems data logger reverted to default time sometime after 2025-10-29 02:00:00
#time was reset on "2025-11-07 14:00:00"?

lower <- readxl::read_xlsx("data/volumetric_water_content/raw_sensor_data/z6B01383 05Dec25-1318_Lower_All_Rows.xlsx", 
                           sheet = "Processed Data Config 1",
                           col_names = c("Timestamp",	"Port1_VWC",	"Port2_VWC",	"Port3_VWC",	"Port4_VWC",	"Port5_VWC",	"Port6_VWC",	"Bat_percent",	"mV.Battery.Voltage"),
                           col_types = c("date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), 
                           skip=3) %>% 
  clean_names() %>% 
  filter(timestamp > as.POSIXct("2025-01-01"))

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
  )) %>% 
  mutate(depth = fct_relevel(depth, "shallow", "deep"))

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

#rename output file
#ggsave("figures/Mesa_Slope_VWC_2025-12-06.pdf",
#       fig_vwc)


# two-panel version

date_scale <- scale_x_datetime(date_breaks = "1 month", 
                               date_labels = "%b",
                               limits = c(as.POSIXct("2025-01-15"), as.POSIXct("2025-12-15")))

fig_vwc_2_panel <- plot_grid(plot_upper1 + date_scale + xlab("Timestamp") + ylab("VWC"), 
                             plot_lower1 + date_scale + xlab("Timestamp") + ylab("VWC"),
                             nrow = 2,
                             align = "v")

fig_vwc_2_panel


ggsave("figures/vwc/Mesa_Slope_VWC_2025-12-06.pdf")
