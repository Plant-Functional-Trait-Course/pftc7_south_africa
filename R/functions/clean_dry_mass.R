### CLEAN DRY MASS

clean_dry_mass <- function(raw_dry_mass){

  dry_mass <- raw_dry_mass |>
    clean_names() |>
    filter(!is.na(id)) |>
    select(-c(orchids:x26)) |>
    rename(dry_mass_g = dry_mass,
           wet_bulk_nr = bulk_nr,
           wet_nr_pieces = nr_pieces) |>
    mutate(id = case_when(id == "JBI8662" ~ "JBI8622",
                          id == "ARU172" ~ "ARU9172",
                          id == "AFS25621" ~ "AFS2562",
                          TRUE ~ id)) |>
    mutate(dry_mass_g = as.numeric(str_replace(dry_mass_g, ",", ".")),
           dry_mass_g = if_else(id == "IVS5393", NA_real_, dry_mass_g)) |>
    select(-check_scan, -scan_checked) |>
    rename(remarks_dm = remarks) |>
    # remove duplicates
    tidylog::distinct()

}

# double dry mass!!!
# what is the problem with these???
dry_mass |>
  group_by(id) |>
  mutate(n = n()) |> filter(n > 1) |> arrange(id) |> as.data.frame()

# checked
dry_mass |>
  filter(check_scan == "yes") |>
  filter(!is.na(remarks)) |>
  distinct(id, remarks) |>
  print(n = Inf)

# comments about cutting and pieces, should have been checked
cut <- dry_mass |>
  filter(check_scan == "no") |>
  filter(str_detect(remarks, "cut|Cut|broke|Broke|crumb|Crumb|crush|pieces")) |>
  select(id, remarks, check_scan)

# lost missing leaves, should be NA
lost <- dry_mass |>
  filter(check_scan == "no") |>
  filter(str_detect(remarks, "lost|eaf missing|Leaves missing|Missing leaf"))
  select(id, remarks, check_scan)

  petiol <- dry_mass |>
    filter(check_scan == "no") |>
    filter(str_detect(remarks, "stipul|Stipul|petiol|Petiol|leaflet")) |>
    as.data.frame()
  select(id, remarks, check_scan)

# need flagging!
damage <- dry_mass |>
  filter(check_scan == "no") |>
  filter(str_detect(remarks, "damage|Damage|destroyed|eaten")) |>
  select(id, remarks, check_scan)

traits |> inner_join(lost, by = "id") |>
  select(id, remark, remarks_dm) |> print(n = Inf)

# problems
dm <- dry_mass |>
  filter(check_scan == "no") |>
  distinct(id, remarks, check_scan) |>
  filter(!is.na(remarks)) |>
  anti_join(cut, by = "id") |>
  anti_join(lost, by = "id") |>
  anti_join(petiol, by = "id") |>
  anti_join(damage, by = "id") |>
  print(n = Inf)

# relevant remarks from dm
# species ID -> Imke
# CYZ2241 "not able to ID species"
# DCI2655 "Check scan this is Lotononis Lotonoides whi.."
# JDQ3343 "Please check as this ID was not found on th.."
# DZC6492 Enter as is - species ID was incorrect and original material was
# DDF1749 =Pimpernella caffra

# error with leaf
# ISD4620 Stipules cut off; do include -> check scan
# EHK0364 Stipules missing
# DTW0238 petiole missing
# HZE2519 Missing stipules
# HYX7739 Missing stipules
# DCO3139 Incorrectly processed; no leaflets
# HUW9936 3 leaflets have fallen off
# HZE2519 Missing stipules

# mass
# CST7009 dry mass more than wet mass -> check envelope
# AOT1171 "Please check Envelope was marked as entered…" ok, is RangeX plant

# !!! check scans? - need flagging !!!!
# HFG6252 1 leaf does not look good
# HZJ5499 Immature leaves
# IEU4695 Only one sketchy leaf
# IKD3755 1 membrane seperated from its leaf
# AMM3671 Leaves dried onto envelope, may effect weight
# ARC2558 Some leaf matter may have dried to paper
# CZL2897 was not scanned- no leaf area

# don't understand these comments
# 3 DDN3372 KAG
# 4 DMS1965 W
# 5 IVJ5024 AG
# 6 DAE7178 KAG
# 7 CYD1816 W

# nr pieces higher than nr leaves - ok fixed
# AHS4588
# DOI1985
# DAM0612
# ERJ5650

# check IDs all are ok :-)
#valid_codes <- PFTCFunctions::get_PFTC_envelope_codes(seed = 202312)
#dry_mass |> anti_join(valid_codes, by = c("id" = "hashcode")) # zero

# check distribution
# dry_mass |>
#   ggplot(aes(x = dry_mass_g)) +
#   geom_histogram()

