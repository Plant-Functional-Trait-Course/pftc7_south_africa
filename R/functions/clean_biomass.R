# clean biomass data

clean_biomass <- function(raw_biomass){

  raw_biomass |>
    clean_names() |>
    mutate(aspect = tolower(aspect),
           biomass_g = bag_biomass_g - bag_weight_g,
           functional_group = if_else(woody_herbaceous_not_indicated == "Woody", "woody", "herbs")) |>
    select(aspect, site_id, plot_id, biomass_g, functional_group)

}



