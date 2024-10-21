# clean biomass data

clean_biomass <- function(raw_biomass){

  raw_biomass |>
    clean_names() |>
    mutate(aspect = tolower(aspect),
           biomass_g = bag_biomass_g - bag_weight_g,
           functional_group = if_else(woody_herbaceous_not_indicated == "Woody", "woody", "herbs")) |>
    # summarize biomass that was in one bag
    group_by(site_id, aspect, plot_id, functional_group) |>
    tidylog::summarise(biomass_g = mean(biomass_g)) |>
    ungroup()

}

