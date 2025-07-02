# cleaning plan

cleaning_plan <- list(

  # clean community
  tar_target(
    name = community,
    command = clean_community(raw_cover, raw_extra, raw_fertility, name_trail, heli_naming_system)
  ),

  # community gradient
  tar_target(
    name = community_gradient,
    command = community |> 
      filter(site_id != "6") |> 
      select(date, aspect, site_id, elevation_m_asl, species:fertility_all)
  ),

  # community warming experiment
  tar_target(
    name = community_experiment,
    command = community |> 
      filter(site_id == "6") |> 
      mutate(treatment_competition = "vegetation") |> 
      select(date, aspect, site_id, elevation_m_asl, treatment_warming = treatment_only_for_range_x, treatment_competition, plot_id, species:fertility_all)
  ),

  # clean comm structure
  # gradient
  tar_target(
    name = comm_structure_gradient,
    command = clean_comm_structure(raw_structure) |> 
      filter(site_id != "6") |> 
      select(-treatment_warming)
  ),

  # experiment
    tar_target(
    name = comm_structure_experiment,
    command = clean_comm_structure(raw_structure) |> 
      filter(site_id == "6") |> 
      mutate(treatment_competition = "vegetation") |> 
      select(date:elevation_m_asl, treatment_warming, treatment_competition, plot_id, variable, value, unit)
  ),

  # clean dry mass
  tar_target(
    name = dry_mass,
    command = clean_dry_mass(raw_dry_mass)
  ),

  # clean leaf area
  tar_target(
    name = leaf_area,
    command = clean_leaf_area(raw_leaf_area, raw_leaf_area2, raw_leaf_area3)
  ),

  # clean traits step 1: initial cleaning and fixing names
  tar_target(
    name = traits,
    command = clean_traits_step1(raw_traits, name_trail, tochange, heli_naming_system, dry_mass, leaf_area)
  ),

  # rangeX traits
  # clean traits step 1: initial cleaning and fixing names
  tar_target(
    name = rangeX_traits,
    command = clean_rangex_traits(raw_traitsX, dry_mass, leaf_area, rangeX_name)
  ),

  # clean biomass
  tar_target(
    name = biomass,
    command = clean_biomass(raw_biomass)
  ),

  # clean geodiversity
  tar_target(
    name = geodiv,
    command = clean_geodiv(raw_geodiv)
  )

)
