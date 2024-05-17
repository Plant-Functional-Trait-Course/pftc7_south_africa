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
  )

)



