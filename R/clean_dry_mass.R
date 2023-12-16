### CLEAN DRY MASS

raw_dry_mass <- read_excel(path = "raw_data/PFTC7_SA_raw_traits_2023.xlsx",
           sheet = "DryMass")


dry_mass <- raw_dry_mass |>
  select(-...9) |>
  clean_names() |>
  filter(!is.na(id)) |>
  rename(wet_bulk_nr = bulk_nr, wet_nr_pieces = nr_pieces)

# all barcodes are fine :-)
# anti_join(valid_codes, by = c("id" = "hashcode"))


# check distribution
# dry_mass |>
#   ggplot(aes(x = dry_mass)) +
#   geom_histogram()
