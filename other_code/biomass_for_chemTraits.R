### Checking biomass dry weight for chemical trait analysis

# Function to get the number of batches per plot and species
get_chem_trait_batch <- function(x, minDW, i = 1){
  if(length(x) == 0) return()
  # get cumsum of mass
  cx <- cumsum(x)
  # if not enough mass give NA
  if(max(cx) < minDW) return(rep(NA, times = length(x)))
  # put all the leaves in the same batch unitl minDW is reached
  batch <- which.max(cx > minDW)

  # recursive function (= calling own function again on the rest of the data)
  c(rep(i, times = batch), get_chem_trait_batch(x[(1:length(x)) > batch], minDW, i = i + 1))

}


# Minimum dry mass needed for CNP analysis in g
# 1.5 - 1.7 mg for CN + isotopes
# 3 sub-samples of 3.5 to 4 mg for phosphorus (12 mg)
# minDW_cn = 0.004

# Check for each value if enough material
#wide_traits <- traits %>%
  #select(id:species, traits, value, problem_flag) |>
  #pivot_wider(names_from = traits, values_from = value) |>
  # remove leaves where dry mass is missing
  #tidylog::filter(!is.na(dry_mass_total_g))

get_list_chem_traits <- function(leaf_traits_clean){

  wide_traits <- traits |>
    select(id, day_not_date, site_id, elevation_m_asl, aspect, species, project, plot_id, plant_id, dry_mass_total_g, problem_flag) |>
    tidylog::filter(!is.na(dry_mass_total_g)) |>
    tidylog::filter(plot_id != 0)

  # Minimum dry mass needed for CNP analysis in g
  # 1.5 - 1.7 mg for CN + isotopes
  # 3 sub-samples of 3.5 to 4 mg for phosphorus (12 mg)
  minDW_cnp = 0.014

  # cnp analysis
  cnp_traits <- wide_traits |>
    # group by plot and species and arrange by mass
    arrange(site_id, aspect, plot_id, plant_id, species, -dry_mass_total_g) |>
    group_by(site_id, aspect, plot_id, plant_id, species) |>
    # get cumsum and
    mutate(cum_mass = cumsum(dry_mass_total_g),
           batchNR = get_chem_trait_batch(x = dry_mass_total_g, minDW = minDW_cnp)) |>
    group_by(site_id, aspect, plot_id, plant_id, species, batchNR) |>
    mutate(chemID = if_else(!is.na(batchNR), str_c(id, collapse = "_"), NA_character_)) |>
    select(id:species, dry_mass_total_g, cum_mass, batchNR, chemID, everything())


}

cnp_traits |>
  ungroup() |>
  select(site_id, aspect, plot_id, plant_id, species, id, batchNR, chemID, dry_mass_total_g, cum_mass) |>
  tidylog::filter(!is.na(batchNR)) |>
  group_by(site_id, plot_id, aspect, species) |>
  # 3 individuals per site, species and treatment
  slice_sample(n = 1) |>
  ungroup() |>
  arrange(site_id, plot_id, aspect, species, batchNR) |>
  write_csv("PFTC7_cnp_traits_6-feb-2025.csv")


