#########################
#### Chemical Traits ####
#########################

clean_chem_traits <- function(raw_cn, leaf_traits) {

  valid_codes <- PFTCFunctions::get_PFTC_envelope_codes(seed = 202312)

  cn <- raw_cn |>
    clean_names() |>
    filter(!is.na(x1)) |>
    select(-any_of(paste0("x", 11:17))) |>
    rename(
      id = sample_id,
      site = x3,
      d15n = d15n_atm,
      d13c = d13c_vpdb
    ) |>
    select(id, site, c_percent, n_percent, c_n, d15n, d13c, filename) |>
    filter(!is.na(id)) |>
    mutate(id = case_when(
      id == "EKD5308" ~ "EKD5808",
      id == "EVA6254" ~ "EVA6264",
      id == "HZ08472" ~ "HZO8472",
      id == "CNT6130" ~ "CWT6130",
      id == "DNO06339" ~ "DNO6339",
      id == "FAD2120" ~ "FAD4120",
      id == "ECG9406" ~ "EGC9406",
      TRUE ~ id
    ))

  invalid_ids <- cn |>
    distinct(id) |>
    anti_join(valid_codes, by = c("id" = "hashcode"))

  if (nrow(invalid_ids) > 0) {
    stop(
      "Invalid envelope IDs in CN data: ",
      paste(invalid_ids$id, collapse = ", "),
      call. = FALSE
    )
  }

  cn <- cn |>
    arrange(id, filename) |>
    tidylog::distinct(id, site, c_percent, n_percent, c_n, d15n, d13c, .keep_all = TRUE)

  cn_isotopes <- cn |>
    pivot_longer(cols = c(c_percent:d13c), names_to = "traits", values_to = "value_raw") |>
    mutate(
      value = if_else(value_raw %in% c("REPEAT", "Empty Cell"), NA_character_, value_raw),
      value = as.numeric(value)
    ) |>
    tidylog::filter(!is.na(value)) |>
    group_by(id, site, traits) |>
    arrange(desc(filename), .by_group = TRUE) |>
    slice(1) |>
    ungroup() |>
    mutate(unit = case_when(
      traits %in% c("c_percent", "n_percent") ~ "%",
      traits == "c_n" ~ "ratio",
      traits %in% c("d15n", "d13c") ~ "permil",
      TRUE ~ NA_character_
    )) |>
    select(id, site, traits, value, unit, filename)

  gradient_meta <- leaf_traits |>
    distinct(id, date, aspect, site_id, elevation_m_asl, plot_id, plant_id, species) |>
    filter(!site_id %in% c("6", 6))

  cn_isotopes |>
    filter(id %in% gradient_meta$id) |>
    tidylog::left_join(gradient_meta, by = "id") |>
    select(
      id, date, aspect, site_id, elevation_m_asl, plot_id,
      plant_id, species, traits, value, unit, filename
    )
}
