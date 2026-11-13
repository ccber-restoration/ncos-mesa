library(tidyverse)
library(sf)
library(mapview)
library(janitor)
library(RColorBrewer)

hydrosense_gl <- read_csv("data/volumetric_water_content/handheld_hydrosense/hydrosense_ncos_grassland_transects_2026-11-09.csv") |> 
  clean_names()

hydrosense_gl_sf <- st_as_sf(hydrosense_gl, 
                             coords = c("longitude", "latitude"),
                             crs = 4326) |>
  mutate(zone = case_when(
    record %in% c(346, 347, 348) ~ "113",
    record %in% c(349, 350, 351) ~ "114",
    record %in% c(352, 353, 354) ~ "115",
    record %in% c(355, 356, 357) ~ "116",
    record %in% c(358, 359, 360) ~ "117",
    record %in% c(361, 362, 363) ~ "118",
    record %in% c(364, 365, 366) ~ "119",
    record %in% c(367, 368) ~ "120",
    .default = zone
  )) |> 
  # change 999999900.00 to NA
  mutate(moisture_percent = case_when(
    moisture_percent == 999999900.00 ~ NA,
    .default = moisture_percent
  )) |> 
  mutate(transect = case_when(
    record >=288 & record <= 298 ~ "GL-12",
    record >=299 & record <= 310 ~ "GL-02",
    record >=311 & record <= 319 ~ "GL-14",
    record >=320 & record <= 329  ~ "GL-15",
    record >=330 & record <= 339  ~ "GL-09",
    record >=340 & record <= 351  ~ "GL-10",
    record >=352 & record <= 360 ~ "GL-07",
    record >=361 & record <= 368  ~ "GL-11"
  ))


mapview(hydrosense_gl_sf, map.types = "Esri.WorldImagery", zcol = "transect")

# average by distance (Zone)
distance_mean <- hydrosense_gl_sf |> 
  group_by(zone) |> 
  summarize(moisture_mean = mean(moisture_percent, na.rm = TRUE),
            perm_mean = mean(permittivity, na.rm = TRUE),
            n_measurements = n(),
            non_na_percentages = sum(!is.na(moisture_percent))
  )


mapview(distance_mean, map.types = "Esri.WorldImagery", zcol = "zone")


# average by transect
transect_mean <- hydrosense_gl_sf |> 
  group_by(transect) |> 
  summarize(moisture_mean = mean(moisture_percent, na.rm = TRUE),
            perm_mean = mean(permittivity, na.rm = TRUE),
            n_measurements = n(),
            non_na_percentages = sum(!is.na(moisture_percent))
  )


# Create a continuous blue color ramp (e.g., 100 levels from light blue to dark blue)
blue_ramp <- colorRampPalette(brewer.pal(9, "Blues"))(10)


mapview(transect_mean, map.types = "Esri.WorldImagery", 
        zcol = "moisture_mean",
        col.regions = blue_ramp)

# map centroid point for each transect
centroid_pt <- st_centroid(transect_mean)

mapview(centroid_pt,  map.types = "Esri.WorldImagery", 
        zcol = "moisture_mean",
        col.regions = blue_ramp)
