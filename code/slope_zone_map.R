library(sf)
library(mapview)
library(tidyverse)
library(janitor)
library(leafem)

# filepaths for reading in shapefile:

#directy where all the shapefiles are
data_source_dir <- "data/slope_zones/NCOS_Mesa_Slope_Zones"

#specific filename (with no file extension)
layer_name <- "NCOS_Mesa_Slope_Zones" # no extension

#transects

dir_transects <- "data/transects/NCOS_Vegetation_Monitoring_Transects"

layer_name_transects <- "NCOS_VegMonitoring_Transects"

transects_css <- st_read(dir_transects, layer = layer_name_transects) %>% 
  filter(str_starts(Transect_I, "CSS"))

#zones to exclude based on guidance from Lisa:
zones_excluded <- c("D1", "B2", "B4", "H3", "I2", "B3", "C2", "F4", "C5", "H2")


mapview(transects_css, map.types = "Esri.WorldImagery")

#read in zones layer
slope_zones <- st_read(data_source_dir, layer = layer_name) %>% 
  select(-(EPCA:JUOC))

mapview(slope_zones, map.types = "Esri.WorldImagery")

# read in zone summary data
shrubs_by_zone <- read_csv("data/shrubs/zone_shrub_density.csv") %>% 
  clean_names() %>% 
  #divide zone area in square feet by 13.8 square feet per plant (density for 4-ft spacing)
  mutate(desired_plant_count = round(sq_ft/13.8),0) %>% 
  mutate(proportion_of_desired = total_plant_count_2025_extract/desired_plant_count,
         percent_of_desired = round(proportion_of_desired*100, 1)) %>% 
  rename(Zone_Label = zone)

sum_desired = sum(shrubs_by_zone$desired_plant_count)
#63,496

sum_actual = sum(shrubs_by_zone$total_plant_count_2025_extract)

zone_performance <- slope_zones %>% 
  #exclude zones
  filter(!(Zone_Label %in% zones_excluded)) %>% 
  left_join(shrubs_by_zone)

#for colors see: https://blog.r-project.org/2019/04/01/hcl-based-color-palettes-in-grdevices/

#currently set to show static labels for zones (transect layer turned off)
mapview(zone_performance, zcol = "percent_of_desired", 
        col.regions = rev(hcl.colors(10, palette = "Emrld")),
        layer.name = "Percent of desired plant count",
        map.types = "Esri.WorldImagery") %>% 
  addStaticLabels(label = zone_performance$Zone_Label) #+
  #mapview(transects_css) 
 


