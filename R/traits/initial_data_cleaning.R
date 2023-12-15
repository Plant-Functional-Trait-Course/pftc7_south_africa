# Initial data cleaning

# Loading packs
library(readxl)
library(tidyverse)
library(janitor)
library(ggplot2)

remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)

get_file(node = "hk2cy",
         file = "PFTC7_SA_raw_traits_2023.xlsx",
         path = "raw_data/traits",
         remote_path = "raw_data/raw_trait_data")

# importing dataset
raw_data <- read_excel(path = "raw_data/PFTC7_SA_raw_traits_2023.xlsx", sheet = "Gradient") %>%
  clean_names() %>%
  filter(!is.na(id)) #removing empty rows


raw_data %>%
  ggplot(aes(x = site_id, y = elevation_m_asl, label = id)) +
  geom_jitter() +
  geom_text_repel(nudge_y = 0.5, segment.size = 0.2)

# fixing site_id and elevation_m_asl so it matches. looking at vegetation cover data and checking with which days we were collecting leaves from each site

raw_data <- raw_data %>%
  mutate(site_id = ifelse(id == "DMG7207", 5, site_id)) %>%  # checked with vegetation cover
  mutate(elevation_m_asl = ifelse(id == "ITM0809", 2400, elevation_m_asl)) %>%  # checked with vegetation cover data
  mutate(elevation_m_asl = ifelse(id == "EEY3042", 2600, elevation_m_asl)) %>% # checked with vegetation cover data
  mutate(elevation_m_asl = ifelse(id == "HXU4345", 2600, elevation_m_asl)) %>% # checked with vegetation cover data
  mutate(elevation_m_asl = ifelse(id == "IHJ2692", 2600, elevation_m_asl)) %>% # checked with vegetation cover data
  mutate(site_id = ifelse(id == "IJH2283", 3, site_id)) %>%  #checked with date we were in field
  mutate(site_id = ifelse(id == "INS9410", 3, site_id)) %>%  #checked with date we were in field
  mutate(site_id = ifelse(id == "IKX1011", 4, site_id)) %>%
  mutate(elevation_m_asl = ifelse(id == "IKX1011", 2600, elevation_m_asl)) %>%  #checked with vegetation cover data and cross checked with day we were in the field. both site and elevation was wrong
  mutate(site_id = ifelse(id == "ESS5610", 1, site_id)) %>%
  mutate(site_id = ifelse(id == "DPP0906", 2, site_id)) %>%
  mutate(site_id = ifelse(id == "IJT0675", 3, site_id)) %>%
  mutate(site_id = ifelse(id == "ICE5231", 3, site_id)) %>%
  mutate(site_id = ifelse(id == "IJQ3983", 3, site_id)) %>%
  mutate(site_id = ifelse(id == "IET3917", 3, site_id))

# 4 datapoints are still not fixed, have not enough information to fix this yet. HHC7973, DEH6145, IFG4764, INM3250


# making commas to points
raw_data <- raw_data %>%
  mutate(across(c(wet_mass_g, leaf_thickness_1_mm, leaf_thickness_2_mm, leaf_thickness_3_mm, veg_height_cm, rep_height_cm, dry_mass_g), as.numeric))


# fixing missing ASPECT


ggplot(aes(x = aspect, y = site_id, label = id), data = raw_data) +
  geom_jitter() +
  geom_text_repel(data = subset(raw_data, is.na(aspect)), nudge_y = 0.5, segment.size = 0.2)

raw_data <- raw_data %>%
  mutate(aspect = case_when(id %in% c("EBC2849", "EMJ1959", "ETC7447", "EPX9304", "CYT0765", "GHC4651", "IRE6770", "ISD4620", "DVZ3020", "DVI0509", "IVX4766", "HTI1269", "DIH9486", "HXS1079", "HCL1010", "HZF7684") ~ "east",
                            id %in% c("EAP2076", "DXL0983", "ILV9642", "DND2812", "DCV2133", "DCM1884", "HOL9126", "HYV0206", "DIM6158", "IGO9419", "HLX8263", "IEA0066")  ~ "west",
                            TRUE ~ aspect)) %>%   # Keep the original value for other IDs
  mutate(plant_id = case_when(id %in% c("DXL0983")  ~ 3,
                              TRUE ~ plant_id))

#plotting
ggplot(aes(x = leaf_thickness_1_mm, y = leaf_thickness_2_mm), data = raw_data) +
  geom_point()
ggplot(aes(x = species, y = leaf_thickness_2_mm), data = raw_data) +
  geom_point()


raw_data %>%
  filter(site_id == 1, plot_id == 2) %>%
  pivot_longer(cols = starts_with("leaf_thickness"), names_to = "measurement", values_to = "thickness") %>%
  ggplot(aes(x = measurement, y = thickness, fill = measurement)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ species, scales = "free") +
  labs(title = "Leaf Thickness Distribution by Species (Site = 1, Plot = 1)", x = "Measurement", y = "Thickness") +
  scale_fill_manual(values = c("blue", "red", "green")) +
  theme_minimal()

