# CLEAN LEAF AREA

clean_leaf_area <- function(raw_leaf_area, raw_leaf_area2, raw_leaf_area3){

  # check IDs (only 4 wrong)
  # valid_codes <- PFTCFunctions::get_PFTC_envelope_codes(seed = 202312)
  # raw_leaf_area |>
  #   anti_join(valid_codes, by = c("id" = "hashcode"))

  # raw_leaf_area3 |>
  #   anti_join(valid_codes, by = c("sample" = "hashcode"))

  raw_area <- raw_leaf_area |>
    clean_names() |>
    mutate(id = case_when(id == "mIWJ2357" ~ "IWJ2357",
                          id == "IMU9111I" ~ "IMU9111",
                          id == "FFT9801" ~ "FFT9800",
                          id == "Jan-96" ~ "JAN4796",
                          TRUE ~id)) |>
    select(-x1) |>
    # remove duplicates with different area (scaned twice)
    tidylog::filter(!c(id == "FFT9800" & leaf_area < 12.8)) |>
    tidylog::filter(!c(id == "IMU9111" & leaf_area < 1.22)) |>
    tidylog::filter(!c(id == "HIT7668" & leaf_area > 6)) |>
    # remove rejected samples
    tidylog::filter(!id %in% c("DNL5251", "DNQ6085")) |>
    # 12 no data and no scan: AAH1730, AAJ0773, AAK2991, AAL3746, AAM1419, AAN6487, AAO9227, AAP5468, AAR5505, AAT1531, ABR7593, IZZ6892
    filter(!id %in% c("AAH1730", "AAJ0773", "AAK2991", "AAL3746", "AAM1419", "AAN6487", "AAO9227", "AAP5468", "AAR5505", "AAT1531", "ABR7593", "IZZ6892", "ISE8213")) |>
    # fix wrong IDs from Sean
    tidylog::mutate(id = case_when(id == "AAA4313" ~ "IUA5525",
                          id == "AAB1562" ~ "ITS7233",
                          id == "AAC4898" ~ "IPV2198",
                          id == "AAE1525" ~ "JBF7217",
                          id == "AAG5074" ~ "GKO6816",
                          id == "HIE9439" ~ "HII4755",
                          id == "HIN5519" ~ "HIR2718",
                          id == "HIP0774" ~ "HIT7668",
                          id == "HIU1449" ~ "HIY2233",
                          id == "HIW7459" ~ "HJA5576",
                          id == "HIX6183" ~ "HJB1614",
                          id == "HJF7356" ~ "HJJ3155",
                          id == "HJK0237" ~ "HJO3994",
                          id == "HJN5785" ~ "HJR9445",
                          id == "HJS8517" ~ "HJW8540",
                          id == "HKA4733" ~ "HKE9266",
                          id == "HKB2288" ~ "HKF1799",
                          id == "HKI0144" ~ "HKM0259",
                          id == "HKJ2307" ~ "HKN0162",
                          id == "HKL2135" ~ "HKP6998",
                          id == "HKU4365" ~ "HKV1826",
                          id == "HKZ5408" ~ "HKY3789",
                          id == "HLB6513" ~ "HLF0616",
                          id == "HLL5321" ~ "HLH7627",
                          id == "HLO0768" ~ "HLS3698",
                          id == "HLP4217" ~ "HLT0616",
                          id == "HLR8378" ~ "HLV2827",
                          id == "HND8304" ~ "HNH0136",
                          id == "HNI7054" ~ "HNM6517",
                          id == "JAM7375" ~ "ITW4599",
                          TRUE ~ id)) |>
    # recoloured scans (see info below)
    # EWD0224 does not work for scanning, no area
    bind_rows(raw_leaf_area2 |>
                # filter only the once that have changed
                filter(id %in% c("ALG9934", "DRN5095", "EAA3997", "EHJ1141", "EIS1610", "END0045", "ENV6286", "EOT7554", "ETV8249", "EVV1051", "FAG5025", "HNZ8136", "HOY7210", "HRL1615", "HSI4571", "IWJ2357", "JAN4796")))

  raw_area_new <- raw_leaf_area3 |>
    rename(id = sample, leaf_area_new = total.leaf.area)

  raw_area <- raw_area |>
    tidylog::left_join(raw_area_new, by = "id") |>
    mutate(leaf_area = if_else(!is.na(leaf_area_new), leaf_area_new, leaf_area)) |>
    select(-leaf_area_new)



}

# ALL FIXED
# duplicates with different area (need to find right one)
# raw_area |>
#   group_by(id) |>
#   mutate(n = n()) |>
#   filter(n > 1) |>
#   arrange(id)

# id      leaf_area_nr leaf_area     n
# <chr>          <dbl>     <dbl> <int>
# 1 FFT9800            1     12.8      2
# 2 FFT9800            1     12.7      2
# 3 IMU9111            1      1.21     2
# 4 IMU9111            1      1.25     2

# very big leaves, is ok
# raw_area |> filter(leaf_area > 100)

# raw_area |>
#   ggplot(aes(x = leaf_area)) +
#   geom_histogram()


# recolouring of scans by Ludwig
# ALG9934 parts slightly too long for scan
# DRN5095 Contrast adjusted
# EAA3997 worked
# EHJ1141 Contrast adjusted
# EIS1610 Contrast adjusted
# END0045 Colored adjusted
# ENV6286 rotated, envelope removed
# EOT7554 colored
# ETV8249 removed envelope
# EVV1051 colored
# EWD0224 discarded, did not work
# FAG5025 worked
# HNZ8136 removed envelope
# HOY7210 removed envelope
# HRL1615 Contrast adjusted
# HSI4571 Contrast adjusted
# IWJ2357 worked
# JAN4796 worked
