# cleaning plan

cleaning_plan <- list(

  # clean dry mass
  tar_target(
    name = community,
    command = clean_community(raw_cover, raw_extra, raw_fertility, name_trail, heli_naming_system)
  ),

  # clean dry mass
  tar_target(
    name = dry_mass,
    command = clean_dry_mass(raw_dry_mass)
  ),

  # clean leaf area
  tar_target(
    name = leaf_area,
    command = clean_leaf_area(raw_leaf_area)
  ),

  # clean traits step 1: initial cleaning and fixing names
  tar_target(
    name = traits,
    command = clean_traits_step1(raw_traits, name_trail, tochange, heli_naming_system, dry_mass, leaf_area)
  )


)
