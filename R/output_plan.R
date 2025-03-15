# output plan

output_plan <- list(

  # export cover data
  tar_target(
    name = community_out,
    command = save_csv(file = community,
                       name = "community_2023")
  ),

  # community structure
  tar_target(
    name = comm_structure_out,
    command = save_csv(file = comm_structure,
                       name = "community_structure_2023")
  ),

  tar_target(
    name = traits_out,
    command = save_csv(file = traits,
                       name = "traits_2023")
  ),

  tar_target(
    name = rangeX_traits_out,
    command = save_csv(file = rangeX_traits,
                       name = "RangeX_traits_2023")
  ),

  # select cnp leaves and make table
  tar_target(
    name = cnp_full_list,
    command = get_list_chem_traits(traits)
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
                       name = "cnp_traits_2023")
  ),

  # save file
  tar_target(
    name = biomass_out,
    command = save_csv(file = biomass,
                       name = "biomass_2023")
  ),

  # species list
  tar_target(
    name = sp_out,
    command = make_sp_list(community, traits) %>%
      save_csv(file = .,
               name = "species_list_2023")
  ),

  # species list
  tar_target(
    name = geodiv_out,
    command = save_csv(file = geodiv,
                       name = "geodiversity_microtopography_2023")
  )



)


