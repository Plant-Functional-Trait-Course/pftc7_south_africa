# output plan

output_plan <- list(

  # export community data
  # gradient
  tar_target(
    name = community_gradient_out,
    command = save_csv(file = community_gradient,
                       nr = "i",
                       name = "elevationgradient_community_2023")
  ),

  # warming experiment
  tar_target(
    name = community_experiment_out,
    command = save_csv(file = community_experiment,
                       nr = "i",
                       name = "experiment_community_2023")
  ),

  # community structure
  # gradient
  tar_target(
    name = comm_structure_gradient_out,
    command = save_csv(file = comm_structure_gradient,
                       nr = "ii",
                       name = "elevationgradient_community_structure_2023")
  ),

  # experiment
  tar_target(
    name = comm_structure_experiment_out,
    command = save_csv(file = comm_structure_experiment,
                       nr = "ii",
                       name = "experiment_community_structure_2023")
  ),

  # traits
  # experiment
  tar_target(
    name = traits_out,
    command = save_csv(file = traits_clean,
                       nr = "iv",
                       name = "elevationgradient_traits_2023")
  ),

  # experiment
  tar_target(
    name = rangeX_traits_out,
    command = save_csv(file = rangeX_traits,
                       nr = "iv",
                       name = "experiment_traits_2023")
  ),

  # select cnp leaves and make table
  tar_target(
    name = cnp_full_list,
    command = get_list_chem_traits(traits_clean)
  ),

  # select 3 random samples per
  tar_target(
    name = cnp_traits,
    command = {
        set.seed(2024)
        cnp_full_list |>
          ungroup() |>
          group_by(site_id, aspect, species) |>
          slice_sample(n = 2)
      }
  ),

  # save file
  tar_target(
    name = cnp_trait_out,
    command = save_csv(file = cnp_traits,
                       nr = "iv",
                       name = "cnp_traits_2023")
  ),

  # save file
  tar_target(
    name = biomass_out,
    command = save_csv(file = biomass,
                       nr = "iii",
                       name = "biomass_2023")
  ),

  # species list
  tar_target(
    name = sp_out,
    command = make_sp_list(community_clean, traits_clean) %>%
      save_csv(file = .,
               nr = "i",
               name = "species_list_2023")
  ),

  # species list
  tar_target(
    name = geodiv_out,
    command = save_csv(file = geodiv,
                       nr = "xiii",
                       name = "geodiversity_microtopography_2023")
  )



)
