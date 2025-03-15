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
    mutate(across(c(organic_soil:mesotopography), as.numeric)) |>
    pivot_longer(cols = c(slope_degree:mesotopography), names_to = "variable", values_to = "value") |>
    filter(!is.na(value)) |>
    select(aspect, site_id, elevation_m_asl, plot_id, variable, value)

}
