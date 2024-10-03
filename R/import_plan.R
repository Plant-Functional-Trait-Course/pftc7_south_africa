# download and import plan

import_plan <- list(

  # download raw community data
  tar_target(
    name = cover_download,
    command =  get_file(node = "hk2cy",
                        file = "Data entry 13Dec.xlsx",
                        path = "raw_data",
                        remote_path = "raw_data/raw_community_data"),
    format = "file"
  ),

  tar_target(
    name = extra_cover_download,
    command =  get_file(node = "hk2cy",
                        file = "Species_added_during_FT_sampling.xlsx",
                        path = "raw_data",
                        remote_path = "raw_data/raw_community_data"),
    format = "file"
  ),

  # name trail
  tar_target(
    name = name_trail_download,
    command =  get_file(node = "hk2cy",
                        file = "std_names_editing.csv",
                        path = "raw_data",
                        remote_path = "raw_data/raw_community_data"),
    format = "file"
  ),

  # download trait data from OSF
  tar_target(
    name = traits_download,
    command =  get_file(node = "hk2cy",
                        file = "PFTC7_SA_raw_traits_2023_versionAug2024.xlsx",
                        path = "raw_data",
                        remote_path = "raw_data/raw_trait_data"),
    format = "file"
  ),

  # dry mass
  tar_target(
    name = dry_mass_download,
    command =  get_file(node = "hk2cy",
                        file = "PFTC7_SA_raw_dry_mass_2023_versionJuly2024.csv",
                        path = "raw_data",
                        remote_path = "raw_data/raw_trait_data"),
    format = "file"
  ),

  # leaf area
  tar_target(
    name = leaf_area_download,
    command =  get_file(node = "hk2cy",
                        file = "leaf_area_scans_25Jun2024.csv",
                        path = "raw_data",
                        remote_path = "raw_data/raw_trait_data"),
    format = "file"
  ),

  tar_target(
    name = leaf_area2_download,
    command =  get_file(node = "hk2cy",
                        file = "leaf_area_scans_01Oct2024.csv",
                        path = "raw_data",
                        remote_path = "raw_data/raw_trait_data"),
    format = "file"
  ),

  # download table containing the leafID's of the records which need name changes
  tar_target(
    name = heli_name_changes_download,
    command =  get_file(node = "hk2cy",
                        file = "Heli_name_changes.xlsx",
                        path = "raw_data",
                        remote_path = "raw_data/raw_trait_data"),
    format = "file"
  ),

  # change the naming system for Helichrysum #
  tar_target(
    name = new_heli_naming_system_download,
    command =  get_file(node = "hk2cy",
                        file = "New Helichrysum naming system.xlsx",
                        path = "raw_data",
                        remote_path = "raw_data/raw_community_data"),
    format = "file"
  ),

  ### IMPORT DATA

  # import cover data
  tar_target(
    name = raw_cover,
    command = read_excel(path = cover_download,
                         sheet = "Sp cover")
  ),

  # import extra cover data
  tar_target(
    name = raw_extra,
    command = read_excel(path = extra_cover_download,
                         skip = 4)
  ),

  # fertility
  tar_target(
    name = raw_fertility,
    command = read_excel(path = cover_download,
                         sheet = "Sp fertility")
  ),

  # name trail
  tar_target(
    name = name_trail,
    command = read_csv(file = name_trail_download)
  ),

  # import raw traits
  tar_target(
    name = raw_traits,
    command = read_excel(path = traits_download,
                         sheet = "Gradient")
  ),

  tar_target(
    name = tochange,
    command = read_excel(path = heli_name_changes_download)
  ),

  #import new naming system
  tar_target(
    name = heli_naming_system,
    command = read_excel(path = new_heli_naming_system_download)
  ),

  # import raw dry mass
  tar_target(
    name = raw_dry_mass,
    command = read_csv(file = dry_mass_download)
  ),

  # import raw leaf area
  tar_target(
    name = raw_leaf_area,
    command = read_csv(file = leaf_area_download)
  ),

  tar_target(
    name = raw_leaf_area2,
    command = read_csv(file = leaf_area2_download)
  )

)
