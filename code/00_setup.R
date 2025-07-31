# load packages ----
library(tidyverse)
library(lubridate)
library(calecopal)
library(janitor)
library(cowplot)

# Lea's chosen color palette

mypaldark <- rep(cal_palette("kelp1",n=6, type = "discrete"), each=2)
opacities <- rep(c("","66"), times=3)

mypal <- paste0(mypaldark, opacities)

palette(mypal)

# Lee's theme

LA_theme <- theme(panel.background = element_rect(fill='white', colour='black'), # Make background white and border black
      panel.grid.major = element_blank(),  # Hide major gridlines
      panel.grid.minor = element_blank()
      )  # Hide minor gridlines
