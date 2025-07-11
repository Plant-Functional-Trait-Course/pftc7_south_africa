# clean geodiv data

clean_geodiv <- function(raw_geodiv){

  raw_geodiv |> 
    clean_names() |>
    # add elevation
    mutate(elevation_m_asl = case_when(
      site_id == 1 ~ 2000,
      site_id == 2 ~ 2200,
      site_id == 3 ~ 2400,
      site_id == 4 ~ 2600,
      site_id == 5 ~ 2800)) |> 
    #mutate(across(c(organic_soil:mesotopography), as.numeric)) |>
    pivot_longer(cols = c(slope_degree:mesotopography), names_to = "variable", values_to = "value") |>
    tidylog::filter(!is.na(value)) |> 
    # add unit
    mutate(unit = case_when(
      str_detect(variable, "degree") ~ "degree",
      variable == "mesotopography" ~ "integer",
      variable %in% c("organic_soil", "silt", "sand", "stone", "boulder", "rock_outcrop") ~ "presence_absence",
      TRUE ~ "percentage"),
      # remove unit for some variables
      variable = str_replace(variable, "_degree", "")) |> 
    select(aspect, site_id, elevation_m_asl, plot_id, variable, value, unit)

}
