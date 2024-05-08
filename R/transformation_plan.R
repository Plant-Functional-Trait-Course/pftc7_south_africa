# transformation plan

tranformation_plan <- list(

  # clean dry mass
  tar_target(
    name = dry_mass,
    command = clean_dry_mass(raw_dry_mass)
  )

  # clean leaf area
  # tar_target(
  #   name = leaf_area,
  #   command = clean_leaf_area(raw_leaf_area)
  # ),

  # clean traits step 1
  # initial cleaning and fixing names
  # tar_target(
  #   name = traits1,
  #   command = clean_traits_step1(raw_traits)
  # ),
  #
  # # merge traits, mass and area
  # tar_target(
  #   name = traits,
  #   command = clean_traits_step2(traits1, dry_mass, leaf_area)
  # )


)
