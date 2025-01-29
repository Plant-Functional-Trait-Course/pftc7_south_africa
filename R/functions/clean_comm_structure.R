# clean community structure data

clean_comm_structure <- function(raw_structure){

  raw_structure |>
    t() |>
    row_to_names(row_number = 1) |>
    as_tibble() |>
    clean_names() %>%
    mutate(date = dmy(date),
           aspect = if_else(aspect == "W", "west", "east"),
           across(contains("height"), as.numeric),
           across(contains("cover"), as.numeric)) %>%
    mutate(vegetation_height = rowMeans(select(., contains("height")), na.rm = TRUE)) |>
    pivot_longer(cols = c(bare_soil_cover, rock_cover, lichen_cover, moss_cover, vegetation_height), names_to = "variable", values_to = "value") |>
    select(date, aspect, site_id, elevation_m_asl = elevation, plot_id, treatment = treatment_only_for_range_x, variable, value)

}



