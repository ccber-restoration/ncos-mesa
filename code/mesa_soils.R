source("code/00_setup.R")

# load in upper VWC sensor data
upper <- read_csv("data/z6B01375 27Jul25-1901upperall.csv", na = c("NA", "", "#N/A")) %>% 
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
  ))

# load in lower VWC sensor data
lower <- read_csv("data/z6B01383 27Jul25-1904lowerall.csv", na = c("NA", "", "#N/A")) %>% 
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


