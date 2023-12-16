# clean trait data
#remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)
library(tidyverse)
library(tidylog)
library(readxl)
library(janitor)
library(PFTCFunctions)

get_file(node = "hk2cy",
         file = "PFTC7_SA_raw_traits_2023.xlsx",
         path = "raw_data",
         remote_path = "raw_data/raw_trait_data")


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

  # fix project
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
                              TRUE ~ plant_id))

# to do

# aspect
# IVX4766 east

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
