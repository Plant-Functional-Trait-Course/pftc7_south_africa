# make species list

make_sp_list <- function(community, traits){

  bind_rows(
    community = community |>
      distinct(species),
    trait = traits |>
      distinct(species),
    .id = "type"
  ) |>
    mutate(presence = "x") |>
    pivot_wider(names_from = type, values_from = presence) |>
    arrange(species)

}
