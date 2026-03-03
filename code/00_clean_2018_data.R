# setup ----

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(readxl)
library(janitor)
library(cowplot)
library(ggthemes) # to use Paul Tol color palettes
library(plotly)


#load regression equation
source("code/mS_per_cm_to_dS_per_m.R")


# read & clean data ----
#read in data from Box Biochar experiment folder
soils_2018 <- readxl::read_xlsx(path = "data/from_Box_NCOS_Biochar_Project/NCOS_Biochar_EC_pH_DataMASTER.xlsx",
                                       sheet = "Best_Data") %>% 
  clean_names(replace = janitor:::mu_to_u) %>% 
  #start separating the sample_id into its constituent parts
  separate_wider_delim(
    cols = sample_id,
    names = c("subplot", "auger_hole", "depth_top", "depth_bottom"),
    delim = "_",
    cols_remove = FALSE) %>% 
  #make depth columns numeric
  mutate(depth_top = as.numeric(depth_top),
         depth_bottom = as.numeric(depth_bottom)) %>% 
  #separate plot and subplot values
  separate_wider_position(cols = subplot,
                          widths = c(plot = 2, subplot_number = 1),
                          cols_remove = FALSE) %>% 
  filter(!is.na(ec_1_5_ratio_u_s)) %>%
#drops to 126 observations
  #assign a date (arbitrary date within 2018-02)
  mutate(sample_date = as.Date("2018-02-14"),
         depth_midpoint = -(depth_top + depth_bottom)/2) %>% 
  select(-(fgl_p_h:p_h_oct2019)) %>% 
  #filter to just first auger hole
  filter(auger_hole == "AH1") %>% 
  #drops to 79 observations
  mutate(plot = case_match(plot,
    "CP" ~ "Central Plot",
    "WP" ~ "West Plot"
  )) %>% 
  mutate(plot = fct_relevel(plot, "West Plot"),
         ec_ds_m = uS_per_cm_to_dS_per_m(ec_1_5_ratio_u_s)) 

# Note that the depths for the deepest cores (below 75 cm) were not standardized... 
# A couple central plot cores only went to 80 (instead of 90)... makes the mean line be weird

# calculate mean values ----
mean_ec_2018 <- soils_2018 %>% 
  group_by(plot, depth_midpoint) %>% 
  summarize(mean_ec = mean(ec_ds_m),
            n = n()) %>% 
  ungroup() 


# quick plot ----

fig_2018_ec <- ggplot(data = soils_2018, aes(x = depth_midpoint, y = ec_ds_m, color = subplot_number)) +
  geom_path(data = mean_ec_2018, aes(x = depth_midpoint, y = mean_ec), color = "black", linewidth = 2) +
  geom_point() +
  #geom_path() +
  xlab("Sample depth (cm)") +
  ylab("EC") +
  coord_flip() +
  theme_cowplot() +
  facet_wrap(facets = vars(plot)) +
  scale_x_continuous(limits = c(-90,0), breaks = seq(-90, 0, by =15)) +
  scale_y_continuous(limits = c(0,NA), position = "left") +
  labs(y = "Estimated EC (dS/m)", x = "Depth (bin midpoint, cm)", title = "Feb 2018") +
  scale_color_ptol()

  fig_2018_ec  



                           
