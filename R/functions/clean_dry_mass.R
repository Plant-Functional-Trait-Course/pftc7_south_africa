### CLEAN DRY MASS

clean_dry_mass <- function(raw_dry_mass){

  dry_mass <- raw_dry_mass |>
    clean_names() |>
    filter(!is.na(id)) |>
    rename(dry_mass_g = dry_mass,
           wet_bulk_nr = bulk_nr,
           wet_nr_pieces = nr_pieces) |>
    mutate(id = case_when(id == "JBI8662" ~ "JBI8622",
                          id == "ARU172" ~ "ARU9172",
                          id == "AFS25621" ~ "AFS2562",
                          TRUE ~ id))

}



# check IDs all are ok :-)
#valid_codes <- PFTCFunctions::get_PFTC_envelope_codes(seed = 202312)
#dry_mass |> anti_join(valid_codes, by = c("id" = "hashcode")) # zero

# check distribution
# dry_mass |>
#   ggplot(aes(x = dry_mass_g)) +
#   geom_histogram()

