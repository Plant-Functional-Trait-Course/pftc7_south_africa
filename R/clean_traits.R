# clean trait data
# TRAIT DATA CLEANING

# load libraries
#remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)
library(tidyverse)
library(tidylog)
library(readxl)
library(janitor)
library(PFTCFunctions)

# download trait data from OSF
get_file(node = "hk2cy",
         file = "PFTC7_SA_raw_traits_2023.xlsx",
         path = "raw_data",
         remote_path = "raw_data/raw_trait_data")


# import data
raw_traits <- read_excel(path = "raw_data/PFTC7_SA_raw_traits_2023.xlsx",
                         sheet = "Gradient")

# check IDs
# valid_codes <- PFTCFunctions::get_PFTC_envelope_codes(seed = 202312)
# dd |> anti_join(valid_codes, by = c("id" = "hashcode")) # zero

dd <- raw_traits |>
  clean_names() |>
  # remove dry mass columns, will come from different dataset
  select(-dry_mass_g, -dry_wet_mass_ratio, -remark_dry_mass) |>
  # remove empty rows and duplicates
  filter(!is.na(id)) |>
  distinct() |>

  # remove wrong duplicates
  filter(!c(id == "HCQ9074" & is.na(plot_id))) |>
  mutate(id = if_else(id == "ebh1122", "EBH1122", id)) |>

  # fixing site_id and elevation_m_asl so it matches.
  # looking at vegetation cover data and checking with which days we were collecting leaves from each site
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
  mutate(site_id = ifelse(id == "IET3917", 3, site_id)) |>


  # fixing missing ASPECT
  mutate(aspect = case_when(id %in% c("EBC2849", "EMJ1959", "ETC7447", "EPX9304", "CYT0765", "GHC4651", "IRE6770", "ISD4620", "DVZ3020", "DVI0509", "IVX4766", "HTI1269", "DIH9486", "HXS1079", "HCL1010", "HZF7684") ~ "east",
                            id %in% c("EAP2076", "DXL0983", "ILV9642", "DND2812", "DCV2133", "DCM1884", "HOL9126", "HYV0206", "DIM6158", "IGO9419", "HLX8263", "IEA0066")  ~ "west",
                            TRUE ~ aspect)) %>%   # Keep the original value for other IDs

### STILL TO FIX !!!!
# 4 datapoints are still not fixed, have not enough information to fix this yet. HHC7973, DEH6145, IFG4764, INM3250

  # fix project
  # STILL TO DO !!!
  # 32 with NA, figure out which project they belong to
  # plot_id might be missing for TSP leaves (16 leaves)
  mutate(project = case_when(project == "S" ~ "TS",
                             project == "P" ~ "TSP", # all P leaves are also S and T
                             project == "SP" ~ "TSP",
                             TRUE ~ project)) |>

  # fix plot_id
  # plot_id 6 is probably 0
  mutate(plot_id = if_else(plot_id == 6, 0, plot_id)) |>
  mutate(plot_id = case_when(id == "DXC4820" ~ 5,
                             id == "DXQ1948" ~ 5,
                             id == "DIX0335" ~ 1,
                             id == "IUT1727" ~ 1,
                             id == "HPJ5881" ~ 3,
                             id == "IKH3374" ~ 5,
                             id == "GFS6468" ~ 3,
                             id == "IVX4766" ~ 3,
                             TRUE ~ plot_id)) |>

  # fix plant_id (only TS, and not plot_id = 0)
  mutate(plant_id = case_when(id == "DQL4323" ~ 1,
                              id == "HPJ5881" ~ 2,
                              id == "DKQ6258" ~ 1,
                              id == "EDR9910" ~ 2,
                              id == "DGP4512" ~ 1,
                              id == "DGA2973" ~ 1,
                              id == "CYW3648" ~ 3,
                              id == "DKE3821" ~ 1,
                              id == "HSJ0266" ~ 1,
                              id == "DTV7294" ~ 3,
                              id == "DWE5688" ~ 1,
                              id == "DJO9541" ~ 2,
                              id == "DWJ2646" ~ 1,
                              id == "HTM1621" ~ 1,
                              id == "ISM5844" ~ 1,
                              id == "IVH0644" ~ 1,
                              id == "IOW4559" ~ 2,
                              id == "DCU2168" ~ 1,
                              id == "IFO3527" ~ 3,
                              id == "IYH4678" ~ 3,
                              id == "IVL2384" ~ 2,
                              id == "IPR0635" ~ 3,
                              id == "IMI6505" ~ 1,
                              id == "IOE1460" ~ 3,
                              id == "IRZ9337" ~ 1,
                              id == "DJK7780" ~ 1,
                              id == "HWH7146" ~ 1,
                              id == "ISO3429" ~ 3,
                              id == "IDU7735" ~ 1,
                              id == "IKD3755" ~ 1,
                              id == "HFI0860" ~ 1,
                              id == "CSE4132" ~ 1,
                              id == "DXL0983" ~ 3, # CHECK !!!
                              TRUE ~ plant_id)) |>
  # making commas to points
  mutate(veg_height_cm = as.numeric(str_replace(veg_height_cm, ",", ".")),
         rep_height_cm = as.numeric(str_replace(rep_height_cm, ",", ".")),
         leaf_thickness_1_mm = as.numeric(str_replace(leaf_thickness_1_mm, ",", ".")),
         leaf_thickness_2_mm = as.numeric(str_replace(leaf_thickness_2_mm, ",", ".")),
         leaf_thickness_3_mm = as.numeric(str_replace(leaf_thickness_3_mm, ",", ".")))





# to do

# still to fix plot_id
# dd |> filter(is.na(plot_id),
#              project != "TSP",
#              plot_id != 0) |>
#   as.data.frame()
# EGT2162 could be plot 2 or 3
# IBU7138 could be plot 2 or 4
# INM3250 probably east, but plot and plant id missing, check with veg
# IKB1474 only plant misspelled?
# IVF3113 could be plot 1 or 2, unclear check with veg data

dd |>
  count(plant_id)

# NAs in plant_id (4)
dd |> filter(is.na(plant_id),
             project != "TSP",
             plot_id != 0) |>
  View()

dd |>
  filter(site_id == 3,
         aspect == "east",
         plot_id == 1,
         species == "Helichrysum dachysephalum") |>
  arrange(aspect, plot_id, plant_id) |>
  as.data.frame()
