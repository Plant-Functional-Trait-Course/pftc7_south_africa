# TRAIT DATA CLEANING

# check IDs
# valid_codes <- PFTCFunctions::get_PFTC_envelope_codes(seed = 202312)
# dd |> anti_join(valid_codes, by = c("id" = "hashcode")) # zero

clean_traits_step1 <- function(raw_traits, name_trail, tochange, heli_naming_system, dry_mass, leaf_area){

  traits1 <- raw_traits |>
    clean_names() |>
    # remove dry mass columns, will come from different dataset
    select(-dry_mass_g, -dry_wet_mass_ratio, -remark_dry_mass) |>
    # remove empty rows and duplicates
    filter(!is.na(id)) |>
    distinct() |>

    # remove wrong duplicates
    filter(!c(id == "HCQ9074" & is.na(plot_id))) |>

    # fix id
    mutate(id = if_else(id == "ebh1122", "EBH1122", id)) |>

    # fixing site_id and elevation_m_asl so it matches.
    # looking at vegetation cover data and checking with which days we were collecting leaves from each site
    mutate(site_id = ifelse(id %in% c("DMG7207", "DNO6339"), 5, site_id)) %>%  # checked with vegetation cover
    mutate(elevation_m_asl = ifelse(id %in% c("DNV4909", "JDU3845"), 2200, elevation_m_asl)) |>
    mutate(elevation_m_asl = ifelse(id == "ITM0809", 2400, elevation_m_asl)) %>%  # checked with vegetation cover data
    mutate(elevation_m_asl = ifelse(id == "EEY3042", 2600, elevation_m_asl)) %>% # checked with vegetation cover data
    mutate(elevation_m_asl = ifelse(id == "HXU4345", 2600, elevation_m_asl)) %>% # checked with vegetation cover data
    mutate(elevation_m_asl = ifelse(id == "IHJ2692", 2600, elevation_m_asl)) %>% # checked with vegetation cover data
    mutate(elevation_m_asl = ifelse(id == "DNO6339", 2800, elevation_m_asl)) %>% # checked with vegetation cover data
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
    mutate(site_id = ifelse(id == "IFG4764", 4, site_id)) |>
    mutate(site_id = ifelse(id == "JDU3845", 2, site_id)) |>
    mutate(site_id = ifelse(id == "DMG7207", 5, site_id)) |>
    mutate(site_id = ifelse(id == "IZP8019", 3, site_id)) |>
    mutate(site_id = ifelse(id == "DEH6145", 5, site_id)) |>
    mutate(site_id = ifelse(id == "HHC7973", 4, site_id)) |>
    mutate(site_id = ifelse(id == "IYV6657", 2, site_id)) |> # most likely from site 2

    # species missing: ISE8213 -> It is a wiry grass. Not possible to identify the species. -> remove
    tidylog::filter(id != "ISE8213") |>
    # species unknown, no scan can be found -> remove
    tidylog::filter(id != "DZC6492") |>
    # unidentifiable species
    tidylog::filter(id != "DNQ6085") |>
    tidylog::filter(id != "DNL5251") |>
    # wrong species
    tidylog::filter(!id %in% c("DIG6453, IUM5537")) |>

    # fixing missing ASPECT
    mutate(aspect = case_when(id %in% c("EBC2849", "EMJ1959", "ETC7447", "EPX9304", "CYT0765", "GHC4651", "IRE6770", "ISD4620", "DVZ3020", "DVI0509", "IVX4766", "HTI1269", "DIH9486", "HXS1079", "HCL1010", "HZF7684", "EKB0320", "INM3250") ~ "east",
                              id %in% c("EAP2076", "DXL0983", "ILV9642", "DND2812", "DCV2133", "DCM1884", "HOL9126", "HYV0206", "DIM6158", "IGO9419", "HLX8263", "IEA0066", "DFL2326")  ~ "west",
                              TRUE ~ aspect)) |>  # Keep the original value for other IDs


    # fix project
    # 3 with NA, figure out which project they belong to
    # HKO1510, IYX6633 (hopeless)
    mutate(project = case_when(project == "S" ~ "TS",
                               project == "P" ~ "TSP", # all P leaves are also S and T
                               project == "SP" ~ "TSP",
                               is.na(project) & plot_id == 0 ~ "TS",
                               is.na(project) & plot_id %in% c(1, 2, 3, 4, 5) ~ "T",
                               id == "CWY3627" ~ "T",
                               id == "GLI4052" ~ "TS",
                               TRUE ~ project)) |>

    # fix plot_id
    # plot_id 6 is probably 0
    ## IF PROJECT == TPS NO PLOT IS FINE (n = 44),
    ## IF PROJECT == TS (n =), do we need plot_id??? DOES THIS NEED TO BE FIXED??? -> Nicole
    # missing plot_id: EGT2162 leaf missing
    # missing plot_id, checked on envelope: INM3250
    mutate(plot_id = if_else(plot_id == 6, 0, plot_id)) |>
    mutate(plot_id = case_when(id == "DXC4820" ~ 5,
                               id == "DXQ1948" ~ 5,
                               id == "DIX0335" ~ 1,
                               id == "IUT1727" ~ 1,
                               id == "HPJ5881" ~ 3,
                               id == "IKH3374" ~ 5,
                               id == "GFS6468" ~ 3,
                               id == "IVX4766" ~ 3,
                               id == "IBU7138" ~ 4,
                               id == "IVF3113" ~ 1,
                               id == "JDM1260" ~ 1,
                               id == "CWY3627" ~ 5, # this is an assumption
                               #project == "TS" ~ 0,
                               TRUE ~ plot_id)) |>


    # fix plant_id (only TS, and not plot_id = 0)
    # HKO1510 only leaves from S and P group ok to miss plot_id
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
                                id == "EGZ5730" ~ 1,
                                id == "CWY3627" ~ 2,
                                id == "DXL0983" ~ 3,
                                TRUE ~ plant_id)) |>


    # making commas to points
    mutate(veg_height_cm = as.numeric(str_replace(veg_height_cm, ",", ".")),
           rep_height_cm = as.numeric(str_replace(rep_height_cm, ",", ".")),
           wet_mass_g = as.numeric(str_replace(wet_mass_g, ",", ".")),
           leaf_thickness_1_mm = as.numeric(str_replace(leaf_thickness_1_mm, ",", ".")),
           leaf_thickness_2_mm = as.numeric(str_replace(leaf_thickness_2_mm, ",", ".")),
           leaf_thickness_3_mm = as.numeric(str_replace(leaf_thickness_3_mm, ",", ".")))


  # fix species names
  # general names
  sp_dicitonary <- name_trail |>
    pivot_longer(cols = c(synonym1, synonym2, synonym3), names_to = "synonym", values_to = "wrong_sp") |>
    filter(!is.na(wrong_sp)) |>
    mutate(wrong_sp = str_to_sentence(str_replace(wrong_sp, "_", " ")),
           correct_sp = str_to_sentence(str_replace(species, "_", " "))) |>
    select(-synonym, -species)

  traits2 <- traits1 |>
    left_join(sp_dicitonary, by = c("species" = "wrong_sp")) |>
    mutate(species = if_else(!is.na(correct_sp), correct_sp, species)) |>
    select(-correct_sp)


  ###Step 1 - changing sp names of incorrectly identified leaves in the trait data####
  #apply function to functional trait data
  traits3 <- correct_leaf_ID(data = traits2,
                  changes = tochange,
                  data_ID_column = "id",
                  data_species_column = "species")

  #### Step 2 - change the naming system for Helichrysum ####
  ##apply function to trait data and community data
  traits4 <- new_heli_naming_system(data = traits3,
                                    naming_system = heli_naming_system,
                                    data_species_column = "species")

  # other species name changes
  traits5 <- traits4 |>
    # remove duplicates
    filter(!c(id == "DBJ3944" & species == "Trifolium burchellianum")) |>
    filter(!c(id == "IEU4695" & species == "Senecio glaberrimus")) |>
    filter(!c(id == "EWH6385" & is.na(remark))) |>
    filter(!c(id == "JDM1260" & is.na(veg_height_cm))) |>
    mutate(species = if_else(id == "CYZ2241", "Helichrysum nudifolium", species)) |>
    # only Anthospermum hispidulum in comm data and matches plant_id
    mutate(species = if_else(id == "ENO0474", "Anthospermum hispidulum", species)) |>
    # was wrong species
    mutate(species = if_else(id %in% c("CUZ6089", "DBJ3944"), "Argyrolobium sp.", species)) |>
    # wrong species
    mutate(species = if_else(id %in% c("DGN1165", "DGR3781", "DVJ2610"), "Wahlenbergia krebsii", species)) |>
    # to get rid of duplicate DHD6397
    distinct()



  ### check area and dry mass issues
  # raw_rangex <- read_excel(path = "raw_data/PFTC7_SA_raw_traits_2023_versionJuly2024.xlsx",
  #                          sheet = "RangeX") |>
  #   clean_names()
  # roots <- read_excel(path = "raw_data/23-12-17_RootData - Copy.xlsx") |>
  #   clean_names() |>
  #   mutate(barcode = if_else(barcode == "FET9800", "FFT9800", barcode))

  # remove rangeX leaves (455 match with rangeX)
  # remove root leaves (94 match with root traits)
  # leaf_area1 <- leaf_area |>
  #   tidylog::anti_join(raw_rangex, by = "id") |>
  #   tidylog::anti_join(roots, by = c("id" = "barcode"))

  # 455 match with rangeX, none match with root traits
  # dry_mass1 <- dry_mass |>
  #   tidylog::anti_join(raw_rangex, by = "id") |>
  #   tidylog::anti_join(roots, by = c("id" = "barcode"))

  # create dd: traits6 |> plus first 4 commands.


  # in traits but have no mass, envelope not found. need to check if mass is on envelope: IYE4372, HUN5440

  # 2 in dry mass but not traits, one is rangeX, one has no data
  # dry_mass1 |>
  #   anti_join(dd, by = "id")
  # AFS2562 -> rangeX
  # IIR4269 -> lost leaf

  # 11 only in traits but no mass
  # dd |>
  #   anti_join(dry_mass1, by = "id") |> as.data.frame()
  # need comment
  # no dry mass, checked on envelope: EKN2983, EST1104, EXW4738, DLT2034, HFW2316, EER8225, GII3041, IKB1474
  # no dry mass, lost at thickness: ESF4997
  # did not find envelopes: IYE4372, HUN5440

  # 3 in area but no traits
  # AFS2562 -> rangeX
  # IIR4269 -> lost leaf
  # HNH0136 no traits, no dry mass no scan -> lost?
  # leaf_area1 |>
  #   anti_join(dd, by = "id")

  # 29 in traits but no area
  # dd |>
  #   anti_join(leaf_area1, by = "id") |> arrange(id) |> print(n = Inf)

  # cannot find scan: CYU7541, CZH2465, CZL2897, DFL2326, DFX2312, DNF7924, DSO1082, DTG6508, HHH1385, HJA5576, HSM2419, ILA4655, IPP6262, IVK1063, IYN5083 -> nobody can find them, no area
  # bad scan: EWD0224 bad scan, no area


  # merge dry mass and leaf area
  traits6 <- traits5 |>
    mutate(species = tolower(species)) |>
    # remove bulk nr because its wrong, has been fixed in dry mass
    select(-bulk_nr) |>

    # remove rejected samples and leaves that went missing
    tidylog::filter(! remark %in% c("missing leaf", "rejected sample", "rejected",
                                    "Not the leaf, corrupted sample", "No plant",
                                    "Leaf lost, empty envelope", "missing data and leaf",
                                    "leaf lost by weighing team")) |>
    # remove duplicate entry (entered both labels)
    tidylog::filter(! id %in% c("AAA4313")) |>

    ## joining dry mass
    #tidylog::anti_join(dry_mass, by = "id")
    # 11 only in traits, but no mass -> flag
    # 457 rangeX leaves
    # 4 in dry mass but no traits, could not find envelope.
    tidylog::left_join(dry_mass, by = "id") |>

    ## joining leaf area
    #tidylog::anti_join(leaf_area, by = "id") |>
    # 30 leaves only in traits but have no area (no scan, bad scan, are scanned but there is no area)
    # 455 match with rangeX, 94 match with root traits
    # 18 have area but no traits. 12 have no data and no scan, 6 not fixed
    tidylog::left_join(leaf_area, by = "id") |>

    # clean trait values
    mutate(leaf_thickness_1_mm = if_else(id == "CZY4701", NA_real_, leaf_thickness_1_mm),
           leaf_thickness_1_mm = if_else(id == "IYF7120", 0.168, leaf_thickness_1_mm),
           leaf_thickness_1_mm = if_else(id == "EER8225", 1.143, leaf_thickness_1_mm),
           leaf_thickness_1_mm = if_else(id == "IEA0066", 1.00, leaf_thickness_1_mm),
           leaf_thickness_1_mm = if_else(id == "HPG0608", 1.07, leaf_thickness_1_mm),
           leaf_thickness_2_mm = if_else(id == "ICM4654", 0.08, leaf_thickness_2_mm),
           leaf_thickness_2_mm = if_else(id == "HOI1639", 0.088, leaf_thickness_2_mm),
           leaf_thickness_3_mm = if_else(id == "HOI1639", 0.075, leaf_thickness_3_mm),
           leaf_thickness_3_mm = if_else(id == "EPN6225", 0.451, leaf_thickness_3_mm)) |>
    # date problems with height
    mutate(veg_height_cm = case_when(id == "HMF3325" ~ 20.4,
                                     id == "CZE1861" ~ 3.5,
                                     id == "DMT2867" ~ 6.6,
                                     id == "DWM8268" ~ 7.5,
                                     id == "CXN5400" ~ 4.5,
                                     id == "DEZ6179" ~ 1.5,
                                     id == "DJL6943" ~ 11.5,
                                     id == "EEI7122" ~ 1.7, # must be typo on envelope, 10x taller than other plants from same species
                                     id == "IRO4769" ~ 3.1, # must be typo on envelope, 10x taller than other plants from same species
                                     TRUE ~ veg_height_cm)) |>
    mutate(rep_height_cm = case_when(id == "EWZ1298" ~ 9.5,
                                     id == "EEV3986" ~ 9.5,
                                     TRUE ~ rep_height_cm)) |>
    # fixing wet mass
    # all eucomis sp have different wet to dry mass ratio
    # comments from Carmen
    # ETS4264- wet mass = 0.0624
    # DFL2326- wet mass = 0.3042
    # CWA1375- wet mass = 0.2547
    # GKD3027- wet mass = 1.0245
    # GHE4792- wet mass = 1.1990
    mutate(wet_mass_g = case_when(id == "HEW3373" ~ 0.2393,
                                  id == "DDO8298" ~ 1.2891,
                                  id == "IYP7607" ~ 0.8594,
                                  id == "IVS5393" ~ NA_real_,
                                  id == "DCX7119" ~ 0.4646, # was 36
                                  id == "GHI5958" ~ 9.0876,
                                  id == "ETS4264" ~ 0.0624,
                                  id == "DFL2326" ~ 0.3042,
                                  id == "CWA1375" ~ 0.2547,
                                  id == "GKD3027" ~ 1.0245,
                                  id == "GHE4792" ~ 1.1990,
                                  id == "EKT0311" ~ 0.0751,
                                  id == "ERV5631" ~ 0.0758, # must be typo on envelope
                                  id == "DEW6477" ~ 0.0724,
                                  id == "DXD4274" ~ 0.0522,
                                  id == "EJY1770" ~ 0.0463, # must be typo on envelope
                                  id == "IQG6978" ~ 0.0356,
                                  id == "HPM0334" ~ 0.0895,
                                  id == "CWU7548" ~ 0.0471,
                                  id == "HPB5029" ~ 0.0546, # assume typo on envelope
                                  id == "INH1905" ~ 0.0382, # assume typo on envelope
                                  id == "HVL1013" ~ 0.0327,
                                  id == "IBW3246" ~ 0.0493, # must be typo on envelope
                                  id == "IWX3665" ~ 0.0553, # assume typo on envelope
                                  id == "DKF6379" ~ 0.0379,
                                  TRUE ~ wet_mass_g)) %>%

    # remove area because leaves are folded
    mutate(leaf_area = if_else(id %in% c("IPF7335", "HXV6038", "HYX7739", "IAZ7307", "HYC9197"), NA_real_, leaf_area)) |>

      # fix bulk nr
      mutate(wet_bulk_nr = if_else(id == "DOI1985", NA_real_, wet_bulk_nr),
             remark = if_else(id == "DOI1985", "Wet bulk nr unknown, no wet mass", remark),
             wet_bulk_nr = if_else(id == "IDK9924", 1, wet_bulk_nr),
             dry_bulk_nr = if_else(id == "IDK9924", 1, dry_bulk_nr),
             wet_bulk_nr = if_else(id == "IOW4559", 3, wet_bulk_nr),
             dry_bulk_nr = if_else(id == "IOW4559", 3, dry_bulk_nr),
             wet_bulk_nr = if_else(id == "EAA3997", 10, wet_bulk_nr),
             dry_bulk_nr = if_else(id == "EAA3997", 10, dry_bulk_nr)) %>%
    # fix bulk nr for leaflets
    mutate(wet_bulk_nr = if_else(id %in% c("DWI9620", "IDO6772"), wet_bulk_nr/3, wet_bulk_nr),
           dry_bulk_nr = if_else(id %in% c("DWI9620", "IDO6772"), dry_bulk_nr/3, dry_bulk_nr),
           wet_bulk_nr = if_else(id == "DCO3139", 10/3, wet_bulk_nr),
           dry_bulk_nr = if_else(id == "DCO3139", 10/3, dry_bulk_nr)) %>%
      # DAM0612 and ERJ5650 dry bulk nr and nr pieces do not match. Checked area and mass ratio and they do not seem to be outliers, so I assume the bulk nr is correct.

    # average leaf thickness
    mutate(leaf_thickness_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")), na.rm = TRUE)) |>
    #select(-leaf_thickness_1_mm, -leaf_thickness_2_mm, -leaf_thickness_3_mm) |>

    # Fix leaf area columns
    rename(wet_mass_total_g = wet_mass_g,
           dry_mass_total_g = dry_mass_g,
           leaf_area_total_cm2 = leaf_area
           ) |>

    # Calculate values on the leaf level (mostly bulk samples)
    mutate(wet_mass_g = wet_mass_total_g / wet_bulk_nr,
           dry_mass_g = dry_mass_total_g / dry_bulk_nr,
           leaf_area_cm2 = leaf_area_total_cm2 / wet_bulk_nr
           ) |>

    # double area for microchloa caffra, rendlia altera
    # mutate(leaf_area_cm2 = if_else(str_detect(species, "microchloa|rendlia"), 2*leaf_area_cm2, leaf_area_cm2)) |>

      # Calculate SLA and LDMC (replace with wet mass for now)
    mutate(sla_cm2_g = leaf_area_cm2 / dry_mass_g,
           ldmc = dry_mass_g / wet_mass_g) |>

    # remove 13 leaves where dry mass > wet mass and cannot be fixed
    tidylog::filter((ldmc <= 1) %>% replace_na(TRUE)) |>

    # Make data pretty
    # 7 NAs for date, replace with 8, which is in the middle of the sampling campaign
    mutate(day_not_date = if_else(is.na(day_not_date), 8, day_not_date),
           date = ymd(paste0("2023-12-", day_not_date)))

  # flag data
  traits <- traits6 |>
    # remove bad data
    # lotononis lotononoides where leaves were wrongly sampled
    tidylog::filter(!id %in% c("DWI9620", "IDO6772", "DCI2655", "DCO3139")) |>

    # flagging missing meta and trait data
    mutate(missing_data_flag = case_when(is.na(site_id) ~ "missing metadata",
                                         is.na(aspect) ~ "missing metadata",
                                         id %in% c("INM3250") ~ "missing metadata",
                                         TRUE ~ NA_character_),
           missing_data_flag = case_when(is.na(wet_mass_g) ~"missing trait",
                                         is.na(dry_mass_g) ~"missing trait",
                                         is.na(leaf_area_cm2) ~"missing trait",
                                         is.na(leaf_thickness_mm) ~"missing trait",
                                         id %in% c("INM3250") ~"missing metadata and trait",
                                         TRUE ~ NA_character_)) |>
    # flag leaves that have damage (herbivory), missing petiole
    # affects mass and area, but ratios should be fine, plants do not look like outliers, are just a bit smaller than what they should be
    mutate(problem_flag = case_when(str_detect(remark, "damage|Damage|destroyed|eaten|herbivory|Herbivory|brown spots|leaf was very dry|Leaf tips brown|tip is dead") ~ "leaf damaged",
                                    str_detect(remarks_dm, "damage|Damage|eaten") ~ "leaf damaged",
                                    id %in% c("DDY0804", "CUQ2187") ~ "leaf damaged",
                                    id %in% c("DHQ3323", "DIM6158", "HGH2408", "DFE2170", "DSI3658",
                                              "EPI1411", "DTW0238", "IKV3000", "IDK9924",
                                              "IYN5083") ~ "missing petiole",
                                    id %in% c("IJQ3983", "DWI9620", "IDO6772", "DCI2655", "DCO3139") ~ "missing petiole and stipules",
                                    id %in% c("IIC0119", "IUX3982", "HZF7684", "CTN0733", "CTF6956",
                                              "DVU8856", "DVW3926", "DWI9620", "EFA6124", "EFH2853",
                                              "EGB4551", "HVR2599", "HYI2750", "HYM1283", "HZE2519",
                                              "HZI9086", "HZJ5499", "ICR9187", "HYX7739", "IAZ7307",
                                              "HYC9197", "EHK0364") ~ "missing stipules",
                                    id %in% c("HUW9936") ~ "missing leaflets",
                                    id %in% c("DBZ6019", "DRW9068", "IOF7531") ~ "leaflets overlapping",
                                    id %in% c("DMV5172") ~ "juvenile plant",
                                    TRUE ~ NA_character_),
           problem_flag = if_else(id == "EYL8181", "damage and missing petiole", problem_flag)) |>
    select(id, date, project, aspect, site_id, elevation_m_asl, plot_id, plant_id, species, veg_height_cm, rep_height_cm, wet_mass_g, dry_mass_g, leaf_area_cm2, leaf_thickness_mm, sla_cm2_g, ldmc, missing_data_flag, problem_flag) |>
    tidylog::pivot_longer(cols = c(veg_height_cm:ldmc), names_to = "traits", values_to = "value") |>
    filter(!is.na(value)) |>
    select(id:species, traits, value, problem_flag) |>
    # problems relevant for all traits except heights
    mutate(problem_flag = case_when(problem_flag %in% c("leaf damaged", "damage and missing petiole", "missing petiole", "missing stipules", "missing petiole and stipules", "missing leaflets") & traits %in% c("veg_height_cm", "rep_height_cm") ~ NA_character_,
           TRUE ~ problem_flag),
           # problem relevant for area and sla
           problem_flag = case_when(problem_flag == "leaflets overlapping" & !traits %in% c("leaf_area_cm2", "sla_cm2_g") ~ NA_character_,
                                    TRUE ~ problem_flag)) |>
    tidylog::distinct()

}


# While checking missing petioles and leaflets, a couple of issues with scans came up. Can the "leaf area group" please check if the following corrections have been done:
# Aster perfoliatus: DJK7780 leaf ripped where stem goes through leaf. May need colouring in -> has this been corrected?
# Senecio glaberrimus: Some stem included; must be painted out: CZI1643, DFS2483, DGH7570, DNT4004, ECI0999, GDB2914, GDQ5190, GDU1704, GEC5870, GEG4663, GEV6124, GFD2773, GFG4723, GFH4906, GJB2936, GKC1048, HHY6277, HOR3154, INZ5169. Has this been done?



# oxalis depressa: IOJ6405, HYQ0079, IPL2355 is area calculated correctly? Bad scan and seem to have too low area for their mass

# DRY AND WET MASS
# dry mass is larger than wet mass
# 13 leaves where dry mass is larger than wet mass that cannot be fixed
# traits |>
#   filter(dry_mass_g > wet_mass_g) |>
#   select(id, dry_mass_g, wet_mass_g, dry_bulk_nr, wet_bulk_nr) |>
#   arrange(id)

# traits |>
#   ggplot(aes(x = wet_mass_g)) + geom_histogram()

# traits |>
#   ggplot(aes(x = wet_mass_total_g, y = dry_mass_total_g,
# colour = dry_mass_total_g > wet_mass_total_g)) +
#   geom_point()

# ### AREA
# traits |>
#   ggplot(aes(x = wet_mass_g, y = dry_mass_g, colour = dry_mass_g > wet_mass_g)) +
#   ggplot(aes(x = leaf_area_cm2, y = dry_mass_g, colour = dry_mass_g > wet_mass_g)) +
#   geom_point()

# HEIGHT
# traits |>
#   ggplot(aes(x = rep_height_cm, y = veg_height_cm)) +
#   geom_point()

# THICKNESS
# ggplot(traits, aes(x = leaf_thickness_1_mm, y = leaf_thickness_2_mm)) +
#   geom_point()
# ggplot(traits, aes(x = leaf_thickness_1_mm, y = leaf_thickness_3_mm)) +
#   geom_point()
# ggplot(traits, aes(x = leaf_thickness_2_mm, y = leaf_thickness_3_mm)) +
#   geom_point()


# problems checked
# no site_id: DEC0977, cannot find envelope
# no site_id: IYX6633, too much info missing, cannot find envelop
# INM3250: site 3 elev 2000 do not match: is missing all meta data, needs to check all envelopes
# IKB1474 -> no plotID on evenlope

## flagging -> ok
# missing aspect: plot 0 leaves: GBH0541, GFH4906 - ask Nicole
# plot_id missing INM3250 (plot_id missing on envelope)
# plot_id missing IKB1474, EGT2162 -> has no plotID


# check some more outliers by species
# traits |> distinct(species) |> arrange(species) |> print(n = Inf)
# traits |>
#   group_by(species) |>
#   filter(n() > 5) |>
#   #filter(species == "eragrostis capensis") |>
#   ggplot(aes(x = leaf_thickness_1_mm, y = leaf_thickness_3_mm)) +
#   geom_point() +
#   facet_wrap(~ species, scales = "free")



### Check all remarks

# traits |>
#   anti_join(cut, by = "id") |>
#   anti_join(lost, by = "id") |>
#   anti_join(damage, by = "id") |>
#   anti_join(aspect, by = "id") |>
#   anti_join(missing_info, by = "id") |>
#   anti_join(petiol, by = "id") |>
#   anti_join(scan, by = "id") |>
#   anti_join(thick, by = "id") |>
#   distinct(remark) |>
#   #filter(str_detect(remark, "thick")) |>
#   print(n = Inf)

# remove -> have been removed from data
# lost_leaf <- traits |>
#   filter(str_detect(remark, "reject|corrupted sample|corrupted|missing leaf|missing data and leaf|Leaf lost|leaf lost") | id == "CZS2074") |>
#   as.data.frame()

# problems checked with leaf nr and pieces -> ok
# cut <- traits |>
#   filter(str_detect(remark, "cut|Cut|broke|Broke|crumb|Crumb|crush|pieces|split|Split|part|halved|torn|shattered|disassembled|ripped|tear")) |>
#   select(id, remark)

# ok -> need to flag leaves with missing data
# lost <- traits |>
#   filter(str_detect(remark, "lost|missing|Missing")) |>
#   select(id, remark)

# flagged
# damage <- traits |>
#   filter(str_detect(remark, "damage|Damage|destroyed|eaten|herbivory|Herbivory|brown spots|leaf was very dry|Leaf tips brown|tip is dead")) |>
#   select(id, remark) |> arrange(id) |> print(n = Inf)

# IDO6772 Leaves destroyed/disected -> problem with leaflets

# all scans have been checked for problematic species -> missing petiole and stipules, leaflets will be flagged
# petiol <- traits |>
#   filter(str_detect(remark, "stipul|petiol|Petiol|Stipul|leaflet|Leaflet")) |>
#   select(id, remark)

# missing data flagged
# scan <- traits |>
#   filter(str_detect(remark, "scan|Scan")) |>
#   select(id, remark)

# has been fixed if possible -> ok
# aspect <- traits |>
#   filter(str_detect(remark, "aspect|Aspect")) |>
#   select(id, remark)

# has been fixed if possible -> ok
# missing_info <- traits |>
#   filter(str_detect(remark, "PlantID|plantID|plant ID| Plant ID|project|Project|plotID|PlotID|plot ID|Plot ID|SiteID|leaf ID")) |>
#   select(id, remark)

# not much can be done -> ok
# thick <- traits |>
#   filter(str_detect(remark, "thickness|thiccness")) |>
#   select(id, remark)


# Species stuff
# DNQ6085 Species unknown -> remove
# DNL5251 Species not identified -> same as DNQ6085 -> removed
# EFC5349 Lesego has an idea about the sp name -> sp confirmed
# EOT7554 spelled as: Helichrysum orieolithum -> Name is Helichrysum aureonitens, checked on scan
# EOT7554 spelled as: Helichrysum orieolithum -> ok
# EON7315 Label says \"semitrifolus\ -> ok
# GEC5870 species name is glaberlium on sheet -> ok
# IHF5470 written as Morea saxicola -> ok
# JDN6683 Species Velaria capensis - typo? -> ok


# Other comments checked and fixed or that are ok
# EIW5495 reproductive height seems large compared to veg but recorded as written: ok is a graminoid
# HEK6391 veg height is 60 on card: ok was probably in mm
# EWX2199 exceptionally heavy for its size: seems ok
# ERJ5650 *tall variant: looks ok
# IVS5393 wet mass is 0.0000! -> check envelope if not then make NA
# IUW5407 very small: ok, similar to others
# IWT4742 seems awfully tall for it's weight: ok similar to others

