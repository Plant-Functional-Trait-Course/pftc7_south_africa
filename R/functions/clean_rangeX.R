### clean RangeX traits

# valid_codes <- PFTCFunctions::get_PFTC_envelope_codes(seed = 202312)
# dd |> anti_join(valid_codes, by = c("id" = "hashcode")) # zero

clean_rangex_traits <- function(raw_traitsX, dry_mass, leaf_area, rangeX_name){

  raw_traitsX |>
    clean_names() |>
    tidylog::filter(!vegetation %in% c("focal")) |>
    tidylog::filter(!is.na(id)) |>

    # fix id
    tidylog::mutate(id = case_when(id == "AFS25621" ~ "AFS2562",
                                   id == "ARU172" ~ "ARU9172",
                                   id == "F" ~ NA_character_,
                                   .default = id)) |>
    # add missing id
    mutate(id = if_else(species == "Lotononis laxa" & block_id == 1 & plot_id == 4, "ASF6749", id)) |>
    # remove duplicate row
    filter(!c(id == "ASF6749" & is.na(leaf_thickness_1_mm))) |>

    # fix site_id
    tidylog::mutate(site_id = if_else(is.na(site_id), "HS", site_id),
                    vegetation = if_else(is.na(vegetation), "native", vegetation),
                    treat_2 = if_else(is.na(treat_2), "vegetation", treat_2)) |>
         
       # fix treat_1
         mutate(treat_1 = if_else(id == "AOF3720", "ambient", treat_1)) |> 

    # add aspect
    mutate(aspect = "west") |>

    # fix project
    mutate(project = case_when(project == "S" ~ "TS",
                               is.na(project) ~ "T",
                               TRUE ~ project)) |>

    # making commas to points
    mutate(veg_height_cm = as.numeric(veg_height_cm),
           rep_height_cm = as.numeric(rep_height_cm),
           wet_mass_g = as.numeric(wet_mass_g),
           leaf_thickness_1_mm = as.numeric(leaf_thickness_1_mm),
           leaf_thickness_2_mm = as.numeric(leaf_thickness_2_mm),
           leaf_thickness_3_mm = as.numeric(leaf_thickness_3_mm)) |>

    # species to lower case
    mutate(species = tolower(species)) |>
    # join dry mass
    tidylog::left_join(dry_mass, by = "id") |>
    # joining leaf area
    tidylog::left_join(leaf_area, by = "id") |>

    # fix traits
    tidylog::mutate(dry_mass_g = if_else(id == "AFR7138", 0.0706, dry_mass_g),
                    # unclear what the problem is with these, large outliers
                    dry_mass_g = if_else(id %in% c("ACJ3379", "ASK3859", "ACA6981"), NA_real_, dry_mass_g),
                    wet_mass_g = if_else(id == "AGN6344", 0.3290, wet_mass_g),
                    wet_mass_g = if_else(id == "JAT6176", 0.9482, wet_mass_g),
                    wet_mass_g = if_else(id == "GJI5855", 0.2750, wet_mass_g),
                    wet_mass_g = if_else(id == "AHT8341", 0.0240, wet_mass_g)) |>

    # remove bulk nr because its wrong, has been fixed in dry mass
    select(-bulk_nr) %>%
    # average leaf thickness
    mutate(leaf_thickness_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")), na.rm = TRUE)) |>

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

    # double area for koeleria capensis
    mutate(leaf_area_cm2 = if_else(str_detect(species, "koeleria"), 2*leaf_area_cm2, leaf_area_cm2)) |>

    # double leaf thickness for koeleria capensis
    mutate(leaf_thickness_mm = if_else(str_detect(species, "koeleria"), 2*leaf_thickness_mm, leaf_thickness_mm)) |>

    # Calculate SLA and LDMC (replace with wet mass for now)
    mutate(sla_cm2_g = leaf_area_cm2 / dry_mass_g,
           ldmc = dry_mass_g / wet_mass_g) |>

    # Make data pretty
    # 7 NAs for date, replace with 8, which is in the middle of the sampling campaign
    mutate(day_not_date = if_else(is.na(day_not_date), 8, day_not_date),
           date = ymd(paste0("2023-12-", day_not_date))) |>

    # fix species names
    tidylog::left_join(rangeX_name |>
                         select(correct_name, synonym1), by = c("species" = "synonym1")) |>
    mutate(species = if_else(!is.na(correct_name), correct_name, species)) |>
    select(-correct_name) |>
    tidylog::left_join(rangeX_name |>
                         select(correct_name, synonym2), by = c("species" = "synonym2")) |>
    mutate(species = if_else(!is.na(correct_name), correct_name, species)) |>

    mutate(species = case_match(species,
                                "senecio sp. 1" ~ "senecio sp.",
                                "tenaxia wiry" ~ "tenaxia sp.",
                                "thesium sp 2" ~ "thesium sp.",
                                #"tenaxia distichia" ~ "tanaxia disticha",
                                .default = species),
          species = str_to_sentence(species)) |>

    mutate(flag = if_else(is.na(treat_1), "missing warming treatment, plot_id and block_id", NA_character_),
       flag = if_else(block_id == 4 & plot_id == 4, "unknown plot_id", flag),
       flag = if_else(block_id == 6 & plot_id == 3, "unknown plot_id", flag)) |>

    tidylog::select(id, date, project, aspect, site_id, elevation_m_asl, treatment_warming = treat_1, treatment_competition = treat_2, block_id, plot_id, species, veg_height_cm, rep_height_cm, wet_mass_g, dry_mass_g, leaf_area_cm2, leaf_thickness_mm, sla_cm2_g, ldmc, flag) |>
    tidylog::pivot_longer(cols = c(veg_height_cm:ldmc), names_to = "traits", values_to = "value") |>
    tidylog::filter(!is.na(value)) |> 
    # add column with units
         mutate(unit = case_when(
                str_detect(traits, "_cm2_g") ~ "cm2 g-1",
                str_detect(traits, "_cm2") ~ "cm2",
                str_detect(traits, "_cm") ~ "cm",
                str_detect(traits, "_mm") ~ "mm",
                str_detect(traits, "_g") ~ "g",
                TRUE ~ "g g-1")
               ) |> 
         mutate(
    traits = str_remove(
      traits,
      "_cm2_g$|_cm2$|_cm$|_mm$|_g$"  # matches any of the known suffixes at the end
    )
  ) |> 
         # merge block_id and plot_id 
         mutate(plot_id = paste(block_id, plot_id, sep = ".")) |> 
         select(-block_id)


}
