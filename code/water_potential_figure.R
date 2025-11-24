# 2025-11-24 this script is for graphing the plant water potential data collected on 
#7 November 2025 by Emilee Doering

source("code/00_setup.R")

library(scales)

wp_2025_11_07 <- read_csv("data/water_potentials/water_potentials_2025-11-07.csv") %>% 
  separate_wider_delim(Sp_sample, delim = "_", names = c("species", "sample")) %>% 
  mutate(species = case_when(
    species == "Bapi" ~ "BAPI",
    species == "Arca" ~ "ARCA"
  ))

wp_summarized <- wp_2025_11_07 %>% 
  group_by(species, Location, sample) %>% 
  summarize(mean_MD = mean(MPa),
            n = n())


#plot by location and species

wp_fig_2025_11_07 <- ggplot(data = wp_summarized, aes(x = Location, y = mean_MD, fill = species, color = species, group = species)) +
                             geom_jitter(height = 0, width = 0.2) +
  theme_cowplot() +
  scale_y_continuous(limits = c(0,NA), breaks = breaks_width(0.5)) +
  facet_wrap(vars(species)) +
  ylab("Water potential (-MPa)")

wp_fig_2025_11_07   

ggsave(wp_fig_2025_11_07, filename = "figures/waterpotentials_20251107.png",
       bg = "white",
       width = 140,
       height = 100,
       units = "mm")


