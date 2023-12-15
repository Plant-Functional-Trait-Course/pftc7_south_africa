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



