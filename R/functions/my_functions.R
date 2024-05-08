# other functions

# save data as csv
save_csv <- function(file, name) {

  filepath <- paste0("clean_data/", "PFTC7_SA_clean_", name, ".csv")
  output <- write_csv(x = file, file = filepath)
  filepath
}
