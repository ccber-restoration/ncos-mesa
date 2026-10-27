
# load tidyverse packages
library(tidyverse)
library(janitor)

# read in data
ncos_hydrosense_vwc_2026_10_27 <- read_csv("data/volumetric_water_content/handheld_hydrosense/ncos_hydrosense_vwc_2026-10-27.csv")  |> 
  mutate(date_time = (mdy_hms(TimeStamp))) |> 
  clean_names()


#check mean by zone ---

zone_mean <- ncos_hydrosense_vwc_2026_10_27 |> 
  group_by(zone) |> 
  summarize(mean_moisture = mean(moisture_percent),
            n_measurements = n())

write_csv(ncos_hydrosense_vwc_2026_10_27, "data/volumetric_water_content/handheld_hydrosense/processed_dat_2026-10-27.csv")