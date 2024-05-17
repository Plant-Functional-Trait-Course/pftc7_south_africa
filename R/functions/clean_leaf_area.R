# CLEAN LEAF AREA

clean_leaf_area <- function(){

  # download trait data from OSF
  get_file(node = "hk2cy",
           file = "PFTC7_leaf_area_2023.csv",
           path = "raw_data",
           remote_path = "raw_data/raw_trait_data")

  raw_area <- read_csv("raw_data/PFTC7_leaf_area_2023.csv") |>
    clean_names() |>
    mutate(id = case_when(id == "mIWJ2357" ~ "IWJ2357",
                          id == "IMU9111I" ~ "IMU9111",
                          TRUE ~id)) |>
    select(-x1, -dir)

  # raw_area |>
  #   anti_join(valid_codes, by = c("id" = "hashcode"))

}



# raw_area |>
#   ggplot(aes(x = leaf_area)) +
#   geom_histogram()
