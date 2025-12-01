# Overview

This repository is for soil and plant research research on the "Mesa Slope" area in North Campus Open Space, specifically to explore why the native plant community has not successfully established as hoped.

# Data

Note that this repository generally uses .csv copies of data files, but the original/raw/working versions of some data sets are on Google Drive or on ArcGIS Online.

## Soil

-  Volumetric water content is continuously collected by a sensor array starting 2025-01-29.

-  Gravimetric soil data was collected by Jacob Pike, Kellen Pierce, Zephyr Moss, and Sydney Beckett. They used a shared Google Drive folder: "CCBER Soils Team Data". https://drive.google.com/drive/u/2/folders/1KuaGsWqHGZcds0SsivSqpVOM9jRhxu1n
    -  Soil samples were collected on 2025-02-25, 2025-04-07, 2025-04-23,
2025-05-07, 2025-06-01, and... **UPDATE**
-  Lara Roelofs continued work on soil salinity and moisture during summer 2025: Grassland_salinity_2025 (1).xlsx
-  Emilee Doering continued work on soil salinity, moisture, texture and veg cover in fall 2025: NCOS Mesa Soil Data 10_20_2025.xlsx

- See "NCOS_Mesa_Slopes_Soil_Samples" AGOL feature layer

## Vegetation

-  **NCOS_Mesa_Slope_Veg_Monitoring**: AGOL feature layer used for monitoring shrubs and grasses on the Mesa Slope in 2025.
-  **NCOS_Mesa_Slope_Zones**: AGOL feature layer with mesa slope planting zones (and palettes?)
-  Also see separate GitHub repository **ncos_planting_log**: https://github.com/ccber-restoration/ncos_planting_log (contact is Claire W-H)

## Plant physiology
- Pre-dawn and midday water potentials for BAPI & ARCA were collected by Lee et al. on 2020-02-20.
- Midday water potentials were collected for BAPI & ARCA by Emilee Doering on 2025-11-07.
- **UPDATE** Repeat midday water potential data collection planned for week of 2025-12-01.  

# Code

Note that scripts that were not originally written within this repository are stored in code/as_received.

FHJ modified some of them to run within this repository.

** NEEDS UPDATING**

-  00_setup.R does some set up for plotting volumetric water content data (Jan-July 2025)
-  mesa_soils.R plots volumetric water content data and gravimetric water content together (Jan-July 2025)
-  sep2025_salinity_moisture_texture_cover.R was written by Lara Roelofs for working with salinity, moisture, soil texture, and veg cover (summer 2025). Produces the following figures:
    -    soil texture.png
    -    Veg cover.png
    -    Soil moisture.png
    -    Salinity µm.png
    -    Salinity dS.png       
-water_potential_figure.R was written by FHJ to plot water potentials from 2025-11-07; can be updated to graph additional data.


