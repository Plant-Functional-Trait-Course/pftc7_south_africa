# CLEAN LEAF AREA

clean_leaf_area <- function(raw_leaf_area){

  # check IDs (only 2 wrong)
  # raw_leaf_area |>
  #   anti_join(valid_codes, by = c("ID" = "hashcode"))

  raw_area <- raw_leaf_area |>
    clean_names() |>
    mutate(id = case_when(id == "mIWJ2357" ~ "IWJ2357",
                          id == "IMU9111I" ~ "IMU9111",
                          TRUE ~id)) |>
    select(-x1, -dir) |>
    # remove 211 duplicates
    tidylog::distinct() |>
    # remove duplicates with different area (scaned twice)
    tidylog::filter(!c(id == "DHO7360" & leaf_area < 3)) |>
    tidylog::filter(!c(id == "EHS0662" & leaf_area < 0.6)) |>
    tidylog::filter(!c(id == "GHU6573" & leaf_area < 6.5)) |>
    tidylog::filter(!c(id == "HHU5593" & leaf_area < 11)) |>
    tidylog::filter(!c(id == "HVJ7082" & leaf_area < 18)) |>
    tidylog::filter(!c(id == "IMU9111" & leaf_area < 1.22)) |>
    tidylog::filter(!c(id == "IVJ5024" & leaf_area > 4.49))

}


# duplicates with same area (I assume can be removed)
# raw_area |>
#   group_by(id, n, leaf_area) |>
#   mutate(n = n()) |>
#   filter(n > 1) |>
#   arrange(id)

# duplicates with different area (need to find right one)
# raw_area |>
#   group_by(id) |>
#   mutate(n = n()) |>
#   filter(n > 1) |>
#   arrange(id)
#
# 1 DHO7360     2     2.90
# 2 DHO7360     2     6.34 -> probably correct
# 3 EHS0662     2     0.528
# 4 EHS0662     2     0.827 -> better, leaf more open
# 5 GHU6573     2     6.34
# 6 GHU6573     2     6.93 -> probably ok
# 7 HHU5593     2    10.6
# 8 HHU5593     2    22.9  -> probably ok
# 9 HVJ7082     2    17.8
# 10 HVJ7082     2    18.3  -> probably ok
# 11 IMU9111     2     1.21
# 12 IMU9111     2     1.25 -> probably ok
# 13 IVJ5024     2     4.50
# 14 IVJ5024     2     4.49 -> probably ok, no dust


# raw_area |>
#   ggplot(aes(x = leaf_area)) +
#   geom_histogram()
