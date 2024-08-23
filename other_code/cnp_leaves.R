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


#traits <- read_csv("clean_data/PFTC6_clean_leaf_traits_2022.csv")
# Check for each value if enough material
# wide_traits <- leaf_traits_clean %>%
#   select(ID:species, trait, value, comment, problem, flag) |>
#   pivot_wider(names_from = trait, values_from = value) |>
#   # remove leaves where dry mass is missing
#   filter(!is.na(dry_mass_total_g))

get_list_chem_traits <- function(traits){

  wide_traits <- traits |>
    filter(plot_id != 0) |>
    select(id, day_not_date, site_id, elevation_m_asl, aspect, plot_id, plant_id, species,
           dry_mass_total_g, remark, remarks_dm) |>
    tidylog::filter(!is.na(dry_mass_total_g)) |>
    tidylog::filter(!c(dry_mass_total_g == 0))

  # Minimum dry mass needed for CNP analysis in g
  # 1.5 - 1.7 mg for CN + isotopes
  # 3 sub-samples of 3.5 to 4 mg for phosphorus (12 mg)
  minDW_cnp = 0.014

  # cnp analysis
  cnp_traits <- wide_traits |>
    # group by plot and species and arrange by mass
    arrange(site_id, plot_id, aspect, species, -dry_mass_total_g) |>
    group_by(site_id, plot_id, aspect, species) |>
    # get cumsum and
    mutate(cum_mass = cumsum(dry_mass_total_g)) |>
    mutate(batch_nr = get_chem_trait_batch(x = dry_mass_total_g,
                                          minDW = minDW_cnp)) |>
    group_by(site_id, plot_id, aspect, species, batch_nr) |>
    mutate(chem_id = if_else(!is.na(batch_nr), str_c(id, collapse = "_"), NA_character_)) |>
    select(id:species, dry_mass_total_g, cum_mass, batch_nr, chem_id, everything())


}

# select 3 random samples per
cnp_traits |>
  ungroup() |>
  group_by(site_id, aspect, species) |>
  slice_sample(n = 3)



