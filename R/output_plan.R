# output plan

output_plan <- list(

  # export cover data
  tar_target(
    name = community_out,
    command = save_csv(file = community,
                       name = "community_2023")
  ),

  tar_target(
    name = traits_out,
    command = save_csv(file = traits,
                       name = "traits_2023")
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
          slice_sample(n = 3)
      }
  ),

  # save file
  tar_target(
    name = cnp_trait_out,
    command = save_csv(file = cnp_traits,
                       name = "cnp_traits_2023")
  )

)


