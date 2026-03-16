# load packages ----
library(tidyverse)
library(lubridate)
library(calecopal)
library(janitor)
library(cowplot)
library(scales)

water_potentials <- read_csv("data/water_potentials/water_potentials_2026-03-11.csv")

#averaged by sampled plant

water_potentials_mean <- water_potentials %>% 
  group_by(Date, species, Location, plant_id) %>% 
  summarize(mean_mpa = mean(MPa, na.rm = TRUE),
            n_stems = n()) %>% 
  mutate(location_new = case_when(
    Location == "ML" ~ "Mesa",
    Location == "MU" ~ "Mesa",
    Location == "Mesa" ~ "Mesa",
    Location == "Ref" ~ "South Parcel",
    Location == "SP" ~ "South Parcel"
    
  ))


#filter to species with multiple timepoints

water_potentials_temporal <- water_potentials_mean %>% 
  filter(species %in% c("Arca", "Bapi"))

fig_arca_bapi <- ggplot(data = water_potentials_temporal, aes(x = Date, y = mean_mpa)) +
  geom_point(aes(color = species)) +
  facet_wrap(vars(location_new)) +
  theme_cowplot() +
  theme(panel.border = element_rect(color = "black", fill = NA)) +
  scale_y_continuous(limits = c(0,NA)) +
  ylab("Mean water potential (-MPa)")
  
  
fig_arca_bapi

ggsave(filename = "figures/water_potentials/wp_arca_bapi_time_series.png",
       width = 7,
       height = 4,
       units = "in")

# Encelia figure ----

encelia <- water_potentials %>% 
  filter(species == "Enca") %>% 
  group_by(Location, wilted_status, plant_id) %>% 
  summarize(mean_mpa = mean(MPa),
            n_stems = n()) %>%
  ungroup()
  


fig_encelia <- ggplot(data = encelia, aes(x = Location, y = mean_mpa, color = wilted_status)) +
  geom_point() +
  theme_cowplot() +
  ylab("Mean water potential (-MPa)") +
  labs(title = "Encelia californica on 2026-02-26") +
  scale_y_continuous(limits = c(0,NA))

fig_encelia

ggsave(filename = "figures/water_potentials/wp_encelia_2026-02-26.png")




# remaining species

wp_others <- water_potentials %>% 
  filter(species %in% c("Lonic", "Roca", "Rhin", "Sale")) %>% 
  group_by(Date, species, Location, plant_id) %>% 
  summarize(mean_mpa = mean(MPa, na.rm = TRUE),
            n_stems = n()) %>% 
  ungroup() %>% 
  mutate(location_new = case_when(
    Location == "ML" ~ "Mesa",
    Location == "MU" ~ "Mesa",
    Location == "Mesa" ~ "Mesa",
    Location == "Ref" ~ "South Parcel",
    Location == "SP" ~ "South Parcel"
  ))


fig_wp_other_sp <- ggplot(data = wp_others, aes(x = location_new, y = mean_mpa)) +
  geom_point() + 
  facet_wrap(vars(species), nrow = 1) +
  theme_cowplot() +
  theme() +
  theme(panel.border = element_rect(color = "black", fill = NA)) +
  scale_y_continuous(limits = c(0,NA)) +
  ylab("Mean water potential (-MPa)") +
  xlab("Location") +
  labs(title = "Other species sampled 2026-01-14")
  
  

fig_wp_other_sp  

ggsave(filename = "figures/water_potentials/wp_other_species_2026-01-14.png", fig_wp_other_sp)
  
# -MPa on y axis
# date on x axis?
#facet by species
# color/symbol by site

fig_water_potentials_mean <- ggplot(data = water_potentials_mean, aes(x = Date, y = mean_mpa)) +
  geom_point(aes(color = Location)) +
  facet_wrap(vars(species)) +
  theme_cowplot() +
  ylab("Water potential (-MPa)") +
  

fig_water_potentials_mean

