# load packages ----
library(tidyverse)
library(lubridate)
library(calecopal)
library(janitor)

# Lea's chosen color palette

mypaldark <- rep(cal_palette("kelp1",n=6, type = "discrete"), each=2)
opacities <- rep(c("","66"), times=3)

mypal <- paste0(mypaldark, opacities)

palette(mypal)
