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
    # remark damage
    mutate(remarks = if_else(id == "IEU4695", "damaged leaf", remarks)) |>
    select(-check_scan, -scan_checked) |>
    rename(remarks_dm = remarks) |>
    # remove duplicates
    tidylog::distinct()

}

# double dry mass!!!
# what is the problem with these??? -> check scans and envelopes, seems only to be one scan
# dry_mass |>
#   group_by(id) |>
#   mutate(n = n()) |> filter(n > 1) |> arrange(id) |> as.data.frame()

# checked
# dry_mass |>
#   filter(check_scan == "yes") |>
#   filter(!is.na(remarks)) |>
#   distinct(id, remarks) |>
#   print(n = Inf)

# comments about cutting and pieces, should have been checked
# ignore
# cut <- dry_mass |>
#   filter(check_scan == "no") |>
#   filter(str_detect(remarks, "cut|Cut|broke|Broke|crumb|Crumb|crush|pieces")) |>
#   select(id, remarks, check_scan)

# lost missing leaves, should be NA -> ok
# lost <- dry_mass |>
#   filter(check_scan == "no") |>
#   filter(str_detect(remarks, "lost|eaf missing|Leaves missing|Missing leaf")) |>
#   select(id, remarks, check_scan)

# missing petiole, leaflets
# needs checking!!!
# petiol <- dry_mass |>
#   filter(check_scan == "no") |>
#   filter(str_detect(remarks, "stipul|Stipul|petiol|Petiol|leaflet")) |>
#   as.data.frame() |>
#   select(id, remarks, check_scan)

# need flagging! -> do at the end in traits
# damage <- dry_mass |>
#   filter(check_scan == "no") |>
#   filter(str_detect(remarks, "damage|Damage|destroyed|eaten")) |>
#   select(id, remarks, check_scan)

# traits |> inner_join(lost, by = "id") |>
#   select(id, remark, remarks_dm) |> print(n = Inf)

# relevant problems, that need checking!!!
# dm <- dry_mass |>
#   filter(check_scan == "no") |>
#   distinct(id, remarks, check_scan) |>
#   filter(!is.na(remarks)) |>
#   anti_join(cut, by = "id") |>
#   anti_join(lost, by = "id") |>
#   anti_join(petiol, by = "id") |>
#   anti_join(damage, by = "id") |>
#   print(n = Inf)

# relevant remarks from dm
# species ID -> Imke
# CYZ2241 "not able to ID species"-> Name can be changed to Helichrysum pilosellum (old Helichrysum naming system), or Helichrysum nudifolium (New Helichrysum naming system). -> check!!!

# DCI2655 "Check scan this is Lotononis Lotonoides whi.." -> Species trifoliolate. Four leaflets were scanned, meaning that 1.33 leaves were scanned. The bulk number was changed accordingly. I think this is a bit dodgy so maybe this scan can be disregarded.
# JDQ3343 "Please check as this ID was not found on th.." !!!!

# DZC6492 Enter as is - species ID was incorrect and original material was -> Cannot find this scan on OSF. !!!

# DDF1749 =Pimpernella caffra -> The name is changed to Pimpinella caffra during the name cleaning. Comment can be ignored
!!!

# error with leaf -> check when checking stipules etc.
# ISD4620 Stipules cut off; do include -> check scan
# EHK0364 Stipules missing
# DTW0238 petiole missing
# HZE2519 Missing stipules
# HYX7739 Missing stipules
# DCO3139 Incorrectly processed; no leaflets
# HUW9936 3 leaflets have fallen off
# HZE2519 Missing stipules

# mass
# CST7009 dry mass more than wet mass -> envelope checked -> check visual !!!
# AOT1171 "Please check Envelope was marked as entered…" -> ok, is RangeX plant

# check scans - scans checked ok
# HFG6252 1 leaf does not look good -> looks ok
# HZJ5499 Immature leaves -> tiny but ok
# IKD3755 1 membrane seperated from its leaf -> ok
# AMM3671 Leaves dried onto envelope, may effect weight -> ok
# ARC2558 Some leaf matter may have dried to paper -> ok
# CZL2897 was not scanned- no leaf area
# IEU4695 Only one sketchy leaf -> needs remark damaged - done

# ignore, initials
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

