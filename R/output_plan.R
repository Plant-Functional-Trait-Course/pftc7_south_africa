# output plan

output_plan <- list(

  # import cover data
  tar_target(
    name = community_out,
    command = save_csv(file = community,
                       name = "community_2023")
  )

)



