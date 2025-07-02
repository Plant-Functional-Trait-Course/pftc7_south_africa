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
    ungroup() |>
    # biomass is from an area of 0.12 m2 (3 x 20cm x 20cm)
    # needs transforming to plot size 1.44 m2 (1.2m x 1.2m)
    mutate(biomass_g = 1.44 * biomass_g / 0.12,
           date = ymd("2023-12-16"),
           elevation_m_asl = case_when(site_id == 1 ~ 2000,
                                       site_id == 2 ~ 2200,
                                       site_id == 3 ~ 2400,
                                       site_id == 4 ~ 2600,
                                       site_id == 5 ~ 2800)) |> 
    select(date, aspect, site_id, plot_id, functional_group, biomass = biomass_g)

}
