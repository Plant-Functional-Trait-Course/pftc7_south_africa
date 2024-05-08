# download and import plan

import_plan <- list(

  # download trait data from OSF
  tar_target(
    name = traits_download,
    command =  get_file(node = "hk2cy",
               file = "PFTC7_SA_raw_traits_2023_version2024.xlsx",
               path = "raw_data",
               remote_path = "raw_data/raw_trait_data"),
    format = "file"
  ),

  # import raw traits
  tar_target(
    name = raw_traits,
    command = read_excel(path = traits_download,
                         sheet = "Gradient")
  ),

  # import raw dry mass
  tar_target(
    name = raw_dry_mass,
    command = read_excel(path = traits_download,
                         sheet = "DryMass")
  )


)
