library(googlesheets4)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

#getting file - I am using direct link to a google drive file - see "Grassland_salinity_2025" - set working directory here. 

gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets.readonly", 
                    "https://www.googleapis.com/auth/drive.readonly"))
sheet_url <- "https://docs.google.com/spreadsheets/d/1rknqV6rXtNB68L4lmfr8dASTrtkOZi3Yc2POJlmXjJQ/edit?gid=378930411#gid=378930411" 
raw_data <- read_sheet(sheet_url, sheet= "All data - 2025 (M & ER), 2019 (C, W)")
veg_data_no_class <- read_sheet(sheet_url, sheet= "2025 veg cover", skip = 1)
veg_classes <- read_sheet(sheet_url, sheet= "vegetation_classes", skip = 1)

#salinity
  ##filter 2019 data to only include equivelent M plots (C in 2019 data) 2, 4, 6, 8 at the same depth bins
filtered_data <- raw_data %>%
  filter(plot %in% c("M4", "M2", "M6", "M8", "CP2", "CP4", "CP6", "CP8", "0")) %>% 
  filter(!(depth %in% c("75_80", "75_81"))) %>% 
  mutate(plot_num = str_extract(plot, "\\d+")) 

filtered_data$year <- as.character(unlist(filtered_data$year))

filtered_data$depth <- factor(filtered_data$depth, levels = sort(unique(filtered_data$depth)))
filtered_data$year <- factor(filtered_data$year, levels = c("2019", "2025", "Reference"))


  ##figures
    ### 1:5 EC ratio in µS/m
ggplot(filtered_data, aes(x = depth, y = `EC_1:5_ratio_µS`, fill = year, group = interaction(year, depth))) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_point(aes(color = year, shape = factor(plot_num)),
             position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0),
             alpha = 1, size = 2) +
  scale_fill_manual(values = c("2019" = "#c9b79c80", "2025" = "#00236680", "Reference" = "gray")) +
  scale_color_manual(values = c("2019" = "#c9b79c", "2025" = "#002366", "Reference" = "gray")) +
  scale_x_discrete(labels = c("0_15" = "0-15", "15_30" = "15-30", "30_45" = "30-45", 
                              "45_60" = "45-60", "60_75" = "60-75", "75_90" = "75-90")) +
  labs(x = "Depth Bin", y = "EC 1:5 Ratio (µS/cm)", fill = "Year", color = "Year", shape = "Plot Number") +
  scale_y_continuous(breaks = seq(0, 4100, by = 500)) +
  theme_minimal()

    ### 1:1 EC ratio in dS/m - APPROXIMATED using 2018 FGL and CCBER linear regression equation
ggplot(filtered_data, aes(x = depth, y = `FGL equivalent from 2018 equation`, fill = year, group = interaction(year, depth))) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_point(aes(color = year, shape = factor(plot_num)),
             position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0),
             alpha = 1, size = 2) +
  scale_fill_manual(values = c("2019" = "#c9b79c80", "2025" = "#00236680")) +
  scale_color_manual(values = c("2019" = "#c9b79c", "2025" = "#002366")) +
  scale_x_discrete(labels = c("0_15" = "0-15", "15_30" = "15-30", "30_45" = "30-45", 
                              "45_60" = "45-60", "60_75" = "60-75", "75_90" = "75-90")) +
  labs(x = "Depth Bin", y = "EC 1:1 (dS/m)", fill = "Year", color = "Year", shape = "Plot Number") +
  scale_y_continuous(breaks = seq(0, 30, by = 2.5)) +
  theme_minimal()



#moisture
  ## this is 2025 NCOS only
mesa_filtered_data <- filtered_data %>%
  filter(year %in% c("2025"))
  
ggplot(mesa_filtered_data, aes(x = factor(depth), y = soil_moisture, fill = year)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "#00236680") +  # semi-transparent blue fill
  geom_point(aes(shape = factor(plot_num)),
             color = "#002366",  # solid dark blue for all points
             position = position_jitter(width = 0, height = 0),  # no jitter, vertically aligned
             alpha = 1, size = 2) +
  labs(x = "Depth Bin", y = "Soil Moisture (%)", shape = "Plot Number") +
  scale_x_discrete(labels = c("0_15" = "0-15", "15_30" = "15-30", "30_45" = "30-45", 
                              "45_60" = "45-60", "60_75" = "60-75", "75_90" = "75-90")) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  theme_minimal()

  ## both
m_filtered_data <- filtered_data
m_filtered_data <- m_filtered_data %>%
  filter(year %in% c("2025", "Reference"))
m_filtered_data$year <- factor(m_filtered_data$year, levels = c("Reference", "2025"))
m_filtered_data <- m_filtered_data[!is.na(m_filtered_data$year), ]

ggplot(m_filtered_data, aes(x = depth, y = `soil_moisture`, fill = year, group = interaction(year, depth))) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_point(aes(color = year, shape = factor(plot_num)),
             position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0),
             alpha = 1, size = 2) +
  scale_fill_manual(values = c("Reference" = "gray", "2025" = "#00236680")) +
  scale_color_manual(values = c("Reference" = "gray", "2025" = "#002366")) +
  scale_x_discrete(labels = c("0_15" = "0-15", "15_30" = "15-30", "30_45" = "30-45", 
                              "45_60" = "45-60", "60_75" = "60-75", "75_90" = "75-90")) +
  labs(x = "Depth Bin", y = "Soil moisture (%)", fill = "Year", color = "Year", shape = "Plot Number") +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  theme_minimal()



# moisture v. salinity....
  ##Filter for 2025 data only
data_2025 <- filtered_data %>% filter(year == "2025")

library(ggpubr)

ggplot(data_2025, aes(x = soil_moisture, y = `FGL equivalent from 2018 equation`)) +
  geom_point(alpha = 0.7, color = "#002366") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  stat_regline_equation(
    aes(label =  ..eq.label..), 
    label.x.npc = "center", label.y.npc = 0.8, size = 5
  ) +
  stat_regline_equation(
    aes(label =  ..rr.label..), 
    label.x.npc = "center", label.y.npc = 0.7, size = 5
  ) +
  labs(
    x = "Soil Moisture (%)",
    y = "EC 1:1 (dS/m)"
  ) +
  theme_minimal()


#soil texture
texture_data <- m_filtered_data %>%
  separate(depth, into = c("top_depth", "bottom_depth"), sep = "_", convert = TRUE)

texture_data$`Hand soil texture class` <- factor(texture_data$`Hand soil texture class`)

texture_data <- texture_data %>%
  mutate(plot_num = str_extract(sample_id, "^[A-Za-z]+[0-9]*"))

soil_colors <- c(
  "clay" = "#993300",   
  "silty clay" = "#663333",   
  "sandy clay" = "#CC6633",
  "clay loam" = "#A57D00",
  "loam" = "#993",
  "loamy sand" = "#FF9933",
  "sand" = "#FFCC66", 
  "sandy clay loam" = "#CC9900"
)

ggplot(texture_data, aes(x = factor(plot_num), fill = `Hand soil texture class`)) +
  geom_rect(aes(
    ymin = top_depth,
    ymax = bottom_depth,
    xmin = as.numeric(factor(plot_num)) - 0.4,
    xmax = as.numeric(factor(plot_num)) + 0.4)) +
  scale_fill_manual(values = soil_colors) +
  scale_y_reverse(breaks = seq(0, 90, by = 15)) +  
  labs(x = "Plot", y = "Depth (cm)", fill = "Soil Type") +
  theme_minimal()


#vegetation data
veg_data <- veg_data_no_class %>%
  left_join(veg_classes, by = "species/bare")


sample_cover <- veg_data %>%
  group_by(location, plot_type, Class) %>%
  summarise(class_cover = sum(percent_cover, na.rm = TRUE), .groups = "drop")
sample_totals <- sample_cover %>%
  group_by(location) %>%
  summarise(sample_total = sum(class_cover), .groups = "drop")
sample_props <- sample_cover %>%
  left_join(sample_totals, by = "location") %>%
  mutate(prop = (class_cover / sample_total) * 100)


veg_class_colors <- c(
  "bare ground" = "#663300",
  "check" = "#999999",
  "native forb" = "#FFFC00",
  "native grass" = "#66CC00",
  "non-native forb" = "#CC6633",
  "non-native grasses" = "#cc9966"
)


ggplot(sample_props, aes(x = location, y = prop, fill = Class)) +
  geom_col() +
  facet_wrap(~plot_type, scales = "free_x") +
  labs(
    x = NULL, 
    y = "Percent Cover (%)",
    fill = "Vegetation Class"
  ) +
  theme_minimal() +
  scale_fill_manual(values = veg_class_colors) +
  theme(axis.text.x = element_blank(),    
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 14, face = "bold"))


# other random things/figrues....
ggplot(m_filtered_data, aes(x = `Hand soil texture class`, y = `EC_1:5_ratio_µS`)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Salinity Levels (EC_1:5_ratio_µS) by Soil Texture Class",
    x = "Soil Texture Class",
    y = "Salinity (µS)"
  )
ggplot(mesa_filtered_data, aes(x = `Hand soil texture class`, y = `EC_1:5_ratio_µS`)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Salinity Levels (EC_1:5_ratio_µS) by Soil Texture Class",
    x = "Soil Texture Class",
    y = "Salinity (µS)"
  )

