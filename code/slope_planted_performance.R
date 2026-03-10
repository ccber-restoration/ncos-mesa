library(tidyverse)
library(readxl)
library(cowplot)
library(calecopal)


planted_manual <- read_xlsx("data/planted_species/planted_survival_manual_2025_monitoring.xlsx") %>% 
  mutate(Species = fct_reorder(Species, percent_survival))

fig_planted_performance <- ggplot(data = planted_manual, aes(x = percent_survival, y = Species, fill = counted_alive)) +
  geom_col() +
  scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
  theme_cowplot() +
  xlab("Percent of number planted (%)") +
  labs(fill = "Live individuals") +
  scale_fill

fig_planted_performance

ggsave(filename = "figures/2025_pseudosurvival.png", plot = fig_planted_performance)
