# TRAIT DATA CLEANING

# check IDs
# valid_codes <- PFTCFunctions::get_PFTC_envelope_codes(seed = 202312)
# dd |> anti_join(valid_codes, by = c("id" = "hashcode")) # zero

clean_traits_step1 <- function(raw_traits, tochange, heli_naming_system, dry_mass){

  traits1 <- raw_traits |>
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

    ### NEED CHECKING
    # check with Nicoles group
    #mutate(site_id = ifelse(id == "DEH6145", 5, site_id)) |>
    #mutate(site_id = ifelse(id == "HHC7973", 4, site_id))
    # IYV6657: site 4 elev 2200 do not match

    # no site_id: DEC0977, IYX6633
    # INM3250: site 3 elev 2000 do not match


    # missing aspect
    # plot 0 leaves: GBH0541, GFH4906 - > as Nicole

    # ask Imke et al.
    # CYZ2241 Helichrysum sp. maybe not useful sample, delete?
    # species missing: ISE8213 -> asked SA team

    # fixing missing ASPECT
    mutate(aspect = case_when(id %in% c("EBC2849", "EMJ1959", "ETC7447", "EPX9304", "CYT0765", "GHC4651", "IRE6770", "ISD4620", "DVZ3020", "DVI0509", "IVX4766", "HTI1269", "DIH9486", "HXS1079", "HCL1010", "HZF7684", "EKB0320", "INM3250") ~ "east",
                              id %in% c("EAP2076", "DXL0983", "ILV9642", "DND2812", "DCV2133", "DCM1884", "HOL9126", "HYV0206", "DIM6158", "IGO9419", "HLX8263", "IEA0066", "DFL2326")  ~ "west",
                              TRUE ~ aspect)) |>  # Keep the original value for other IDs


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
    ## IF PROJECT == TPS NO PLOT IS FINE (n = 44),
    ## IF PROJECT == TS, do we need plot_id??? DOES THIS NEED TO BE FIXED??? -> Nicole
    # missing plot_id: EGT2162 leaf missing
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
                               id == "CWY3627" ~ 5, # this is an assumption
                               TRUE ~ plot_id)) |>

    ### # IKB1474 only occurs in the east in comm data, is aspect also wrong? !!!! -> Imke et al.

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
  ###Step 1 - changing spnames of incorrectly identified leaves in the trait data####
  #apply function to functional trait data
  traits2 <- correct_leaf_ID(data = traits1,
                  changes = tochange,
                  data_ID_column = "id",
                  data_species_column = "species")

  #### Step 2 - change the naming system for Helichrysum ####
  ##apply function to trait data and community data
  traits3 <- new_heli_naming_system(data = traits2,
                                    naming_system = heli_naming_system,
                                    data_species_column = "species")


  ### !!! DUPLICATE IDS WHICH ARE NOT DUPLICATES !!!
  # traits3 |>
  #   group_by(id) |>
  #   mutate(n = n()) |>
  #   filter(n > 1) |>
  #   arrange(id) |> as.data.frame()

  #### > 400 leaves do not join
  # 442 rows match with RangeX traits, only 26 do not match
  not_matching_dm <- dry_mass |>
    anti_join(traits3)

  # !!! 26 leaves with dm but no traits.!!!
  not_matching_dm |>
    anti_join(rx, by = "id")
    # no match with root traits
    #tidylog::left_join(roots, by = "id")



  # merge dry mass and leaf area
  traits <- traits3 |>
    mutate(species = tolower(species)) |>
    # remove bulk nr because its wrong, has been fixed in dry mass
    select(-bulk_nr) |>
    ### NEEDS CHECKING!!!
    #tidylog::anti_join(dry_mass, by = "id") # 16 only in traits, 465 only in dry mass!!!
    ### !!! THERE ARE DUPLICATES IN DRY MASS, NEEDS FIXING BEFORE JOINING
    tidylog::left_join(dry_mass, by = "id") |>


  #anti_join(raw_area, by = "id") # 362 leaves are only in area
    #left_join(raw_area, by = "id") |>

    # clean trait values
    mutate(leaf_thickness_1_mm = if_else(id == "CZY4701", NA_real_, leaf_thickness_1_mm),
           leaf_thickness_3_mm = if_else(id == "EPN6225", 0.451, leaf_thickness_3_mm)) |>
    # date problems with height
    mutate(veg_height_cm = case_when(id == "HMF3325" ~ 20.4,
                                     id == "CZE1861" ~ 3.5,
                                     id == "DMT2867" ~ 6.6,
                                     id == "DWM8268" ~ 7.5,
                                     id == "CXN5400" ~ 4.5,
                                     id == "DEZ6179" ~ 1.5,
                                     id == "DJL6943" ~ 11.5,
                                     TRUE ~ veg_height_cm)) |>
    mutate(rep_height_cm = case_when(id == "EWZ1298" ~ 9.5,
                                     id == "EEV3986" ~ 9.5,
                                     TRUE ~ rep_height_cm)) |>
    mutate(wet_mass_g = case_when(id == "HEW3373" ~ 2.393,
                                  id == "DDO8298" ~ 1.2891,
                                  id == "IYP7607" ~ 0.8594,
                                  id == "IVS5393" ~ NA_real_,
                                  # !!!! NEEDS CHECKING ON EVENLOPE !!!!
                                  id == "DCX7119" ~ NA_real_, # is 36
                                  id == "GIM3565" ~ 1.6998, # was 12.6998
                                  id == "HZX1934" ~ 1.4706, # was 10.4706 should be 1. or 0.
                                  id == "GHV4624" ~ 1.2470, # was 10.2470 should be 1. or 0.
                                  id == "IWO1287" ~ 1.0396, # was 11.0396 should be 1. or 0.
                                  id == "IET3917" ~ 1.4043, # was 10.4043 should be 1. or 0.
                                  id == "ITP8823" ~ 1.0953, # was 12.0953 should be 1. or 0.
                                  id == "ITT4877" ~ 1.5649, # was 21,5649 should be 1. or 0.
                                  id == "ITK0324" ~ 1.6872, # was 11.6872 should be 1. or 0.
                                  id == "IVY3326" ~ 1.0625, # was 11.0625 should be 1. or 0.
                                  TRUE ~ wet_mass_g)) %>%

      # fix bulk nr
      mutate(wet_bulk_nr = if_else(id == "DOI1985", NA_real_, wet_bulk_nr),
             remark = if_else(id == "DOI1985", "Wet bulk nr unknown, no wet mass", remark)) |>
      ### NEED CHECKING: DAM0612 and ERJ5650 wet bulk nr is correct, but dry bulk nr and nr pieces does not match. Check with mass and area if can figure out how many leaves there ware with dry weight.
      #DAM0612: The original wet bulk number and number of pieces was 2 (correctly scanned), but dry mass was registered as 1. Dry bulk number was still registered as 2, but number of pieces as 1. I don't know if a leaf was lost and the dry bulk number is wrong, or if the number of pieces was wrongly entered. No remark was made in the document. ERJ5650:Exactly the same situation as DAM0612, but with 4 and 3.

    # average leaf thickness
    mutate(leaf_thickness_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")), na.rm = TRUE)) |>
    #select(-leaf_thickness_1_mm, -leaf_thickness_2_mm, -leaf_thickness_3_mm) |>

    # Fix leaf area columns
    rename(wet_mass_total_g = wet_mass_g,
           dry_mass_total_g = dry_mass_g#,
           #leaf_area_total_cm2 = leaf_area
           ) |>

    # Calculate values on the leaf level (mostly bulk samples)
    mutate(wet_mass_g = wet_mass_total_g / wet_bulk_nr,
           dry_mass_g = dry_mass_total_g / dry_bulk_nr#,
           #leaf_area_cm2 = leaf_area_total_cm2 / wet_bulk_nr
           ) |>

    # double area for microchloa caffra, rendlia altera
    # mutate(leaf_area_cm2 = if_else(str_detect(species, "microchloa|rendlia"), 2*leaf_area_cm2, leaf_area_cm2)) |>

      # Calculate SLA and LDMC (replace with wet mass for now)
    mutate(#sla_cm2_g = leaf_area_cm2 / dry_mass_g,
           ldmc = dry_mass_g / wet_mass_g) |>

      # order columns
      # select(id:rep_height_cm, wet_mass_g, dry_mass_g, leaf_area_cm2, leaf_thickness_mm, wet_sla_cm2_g, sla_cm2_g, ldmc, everything()) |>

    # Make data pretty
    # 7 NAs for date
    mutate(date = ymd(paste0("2023-12-", day_not_date)))
#   select(id, date, project, aspect, site_id, elevation_m_asl, plot_id, plant_id, species, veg_height_cm, rep_height_cm, wet_mass_g, dry_mass_g, leaf_thickness_mm)

}

# WHICH OF THESE NEED FIXING
# wet mass: HEW3373, DDO8298, IYP7607 wrong numbers, much larger than 100
# wet mass: DCX7119, GIM3565, wet vs dry mass ratio does not work, which one is wrong. check envelope.DCX could be 0.2393, GIM... could be 1.6998
# wet mass: ITT4877 seems to high, is probably 1.25649. check on envelope.
# what about values between 5-40 are these real or bad?
traits |>
  filter(wet_mass_g > 5) |> as.data.frame()
traits |> filter(wet_mass_g > 10, dry_mass_g < 0.3) |> as.data.frame()
traits |>
  filter(wet_mass_g < 100) |>
  ggplot(aes(x = wet_mass_g)) + geom_histogram()
# needs checking!!!
#traits |> filter(leaf_area > 50)
#mutate(leaf_area = if_else(leaf_area > 50, NA_real_, leaf_area))


# all wet mass > 5 look suspicious, all dry mass values in the upper left corner look strange need checking
traits |>
  filter(wet_mass_g < 20) |>
  filter(!c(wet_mass_g > 10 & dry_mass_g < 0.3)) |>
  ggplot(aes(x = wet_mass_g, y = dry_mass_g, colour = dry_mass_g > wet_mass_g)) +
  geom_point()

### NEED CHECKING: dry mass is larger than wet mass !!!!
traits |> filter(dry_mass_g > wet_mass_g) |>
  as.data.frame()

### HEIGHT: Needs checking: 10 leaves
traits |> filter(veg_height_cm > 100 | rep_height_cm > 100)
traits |>
  filter(veg_height_cm < 100) |>
  filter(rep_height_cm < 100) |>
  ggplot(aes(x = rep_height_cm, y = veg_height_cm)) +
  geom_point()

# Thickness
# !!!!! check these values:
# IYF7120: traits |> filter(leaf_thickness_1_mm > 1.5, leaf_thickness_2_mm < 0.25) |> as.data.frame()
# EER8225, ICM4654, HOI1639, IEA0066, HPG0608: traits |> filter(leaf_thickness_1_mm < 0.3, leaf_thickness_2_mm > 0.75) |> as.data.frame()
# HBB6136, IVY3326: traits |> filter(leaf_thickness_1_mm < 1.25, leaf_thickness_2_mm > 1.75) |> as.data.frame()
# DOA3758, IVY3326: traits |> filter(leaf_thickness_2_mm > 2) |> as.data.frame()
ggplot(traits, aes(x = leaf_thickness_1_mm, y = leaf_thickness_2_mm)) +
  geom_point()
ggplot(traits, aes(x = leaf_thickness_1_mm, y = leaf_thickness_3_mm)) +
  geom_point()
ggplot(traits, aes(x = leaf_thickness_2_mm, y = leaf_thickness_3_mm)) +
  geom_point()



### Check all remarks

traits |>
  anti_join(cut, by = "id") |>
  anti_join(lost, by = "id") |>
  anti_join(damage, by = "id") |>
  anti_join(aspect, by = "id") |>
  anti_join(missing_info, by = "id") |>
  anti_join(petiol, by = "id") |>
  anti_join(scan, by = "id") |>
  anti_join(thick, by = "id") |>
  distinct(remark) |>
  #filter(str_detect(remark, "thick")) |>
  print(n = Inf)

# remove -> need removing from data
lost_leaf <- traits |>
  filter(str_detect(remark, "reject|corrupted sample|corrupted|missing leaf|missing data and leaf|Leaf lost|leaf lost") | id == "CZS2074") |>
  as.data.frame()

# problems checked with leaf nr and pieces -> ok
# cut <- traits |>
#   filter(str_detect(remark, "cut|Cut|broke|Broke|crumb|Crumb|crush|pieces|split|Split|part|halved|torn|shattered|disassembled|ripped|tear")) |>
#   select(id, remark)

# ok
# lost <- traits |>
#   filter(str_detect(remark, "lost|missing|Missing")) |>
#   select(id, remark)

# needs checking if leaf is damaged and need flagging or if just cut!!!
damage <- traits |>
  filter(str_detect(remark, "damage|Damage|destroyed|eaten|herbivory|Herbivory|brown spots|leaf was very dry|Leaf tips brown|tip is dead")) |>
  select(id, remark)

# need to check scan and flag. Which species should be checked?
# petiol <- traits |>
#   filter(str_detect(remark, "stipul|petiol|Petiol|Stipul|leaflet|Leaflet")) |>
#   select(id, remark)

# not much to be done if scan is missing
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

# SPECIES STUFF -> ASK IMKE
# DYV1207 needs to be confirmed
# DZC6492 Species unknown. Enter as is. Species ID was incorrect and original material was discarded before finding mistake.
# DNQ6085 Species unknown
# EFC5349 Lesego has an idea about the sp name
# DNL5251 Species not identified
# ENO0474 herbaceum not in original community data.
# EOT7554 spelled as: Helichrysum orieolithum
# EON7315 Label says \"semitrifolus\
# GEC5870 species name is glaberlium on sheet
# IHF5470 written as Morea saxicola
# GEC5870 species name is glaberlium on sheet
# JDN6683 Species Velaria capensis - typo?



traits |>
  filter(id == "IWT4742") |> as.data.frame()
  #filter(str_detect(remark, "Species Velaria capensis - typo?")) |> as.data.frame()

traits |>
  filter(species == "themeda triandra") |>
  #filter(wet_mass_g < 100) |>
  #select(veg_height_cm, wet_mass_g, dry_mass_g, remark)
  ggplot(aes(x = wet_mass_g, y = veg_height_cm, colour = id == "IWT4742")) +
  geom_point()

# Other comments checked and fixed or that are ok
# EIW5495 reproductive height seems large compared to veg but recorded as written: ok is a graminoid
# HEK6391 veg height is 60 on card: ok was probably in mm
# EWX2199 exceptionally heavy for its size: seems ok
# ERJ5650 *tall variant: looks ok
# IVS5393 wet mass is 0.0000! -> check envelope if not then make NA
# IUW5407 very small: ok, similar to others
# IWT4742 seems awfully tall for it's weight: ok similar to others



# NEED CHECKING !!!
# EEI7122 tall variety: is larger than others. is this a typo, 1.7? -> Check envelope
# ERV5631 and EKT0311: wet mass seems too high compared to other ind of same species. -> check envelope

### need checking !!!!
# HTP5778 some leaflets *** from each leaf to make scanning possible -> check envelope and scan
# too dry for second and third measurement -> flagging?
# DDY0804 Some leaf missing -> flagging?

# info missing
# 7 days -> just fill in a date. does not matter really.
# 2 site_id

traits |>
  filter(is.na(elevation_m_asl)) |> as.data.frame()

traits |>
  filter(str_detect(species, "Ficinia cinnamomea")) |>
  arrange(site_id, aspect, plot_id, plant_id) |>
  select(site_id, aspect, plot_id, plant_id) |>
  print(n = Inf)



#figure out which scand belong to root group
roots <- read_excel("raw_data/23-12-17_RootData - Copy.xlsx") |>
  rename(id = Barcode)

roots |> anti_join(valid_codes, by = c("id" = "hashcode"))
roots |> anti_join(raw_area, by = "id")

area_no_data <- raw_area |>
  anti_join(raw_traits, by = "id") |>
  # remove root scans
  anti_join(roots, by = "id") |>
  arrange(id) |>
  print(n = Inf)





# # fix wet mass that is 10 times too large
# dd |>
#   mutate(ldmc = dry_mass_g/wet_mass_g)
#
# #wet_mass_g = if_else(ratio < 10, wet_mass_g/10, wet_mass_g))
# raw_traits |> filter(id == "ALW1014")
# dd |>
#   filter(leaf_thickness_1_mm < 0.2 & leaf_thickness_2_mm > 0.5) |> as.data.frame()
#
# dd |>
#   filter(sla_cm2_g < 5)
#   ggplot(aes(x = ldmc)) +
#   geom_histogram()
# dd |>
#   #filter(dry_mass_g < 0.4) |>
#   ggplot(aes(x = leaf_thickness_1_mm, y = leaf_thickness_2_mm)) +
#   geom_point()
#
#
# dd |>
#   ggplot(aes(x = leaf_area_cm2, y = dry_mass_g, colour = sla_cm2_g > 500)) +
#   geom_point()
#
# # to do
#
# # still to fix plot_id
# # dd |> filter(is.na(plot_id),
# #              project != "TSP",
# #              plot_id != 0) |>
# #   as.data.frame()
# # EGT2162 could be plot 2 or 3
# # IBU7138 could be plot 2 or 4
# # INM3250 probably east, but plot and plant id missing, check with veg
# # IKB1474 only plant misspelled?
# # IVF3113 could be plot 1 or 2, unclear check with veg data
#
# dd |>
#   count(plant_id)
#
# # NAs in plant_id (4)
# dd |> filter(is.na(plant_id),
#              project != "TSP",
#              plot_id != 0) |>
#   View()
#
# dd |>
#   filter(site_id == 3,
#          aspect == "east",
#          plot_id == 1,
#          species == "Helichrysum dachysephalum") |>
#   arrange(aspect, plot_id, plant_id) |>
#   as.data.frame()
#
