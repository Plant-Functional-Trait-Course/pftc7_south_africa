# other functions

# save data as csv
save_csv <- function(file, nr, name) {

  filepath <- paste0("clean_data/", nr, "_", "PFTC7_clean_", name, ".csv")
  output <- write_csv(x = file, file = filepath)
  filepath
}
