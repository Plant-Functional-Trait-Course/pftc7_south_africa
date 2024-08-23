# RangeX traits

# check if this leaf comes through:
# has area and mass AFS2562

# import data
raw_rangex <- read_excel(path = "raw_data/PFTC7_SA_raw_traits_2023_versionJuly2024.xlsx",
                         sheet = "RangeX") |>
  clean_names()

# 442 match
rx <- raw_rangex |>
  clean_names() |>
  tidylog::inner_join(not_matching_dm, by = "id")

meta <- read_csv("raw_data/rangeX_meta_treat.csv") |>
  filter(country == "south_africa") |>
  select(-country, -site, -block_plot) |>
  mutate(site_id = "HS",
         treat_2 = "vegetation") |>
  rename(block_id = block, plot_id = plot, vegetation = treat_veg, treat_1 = treat_temp) |>
  mutate(vegetation = if_else(vegetation == "focals", "focal", vegetation),
         treat_1 = if_else(vegetation == "warming", "warm", treat_1))

#dd |> anti_join(valid_codes, by = c("id" = "hashcode"))

trait <- raw_rangex |>
  clean_names() |>
  filter(!is.na(id)) |>
  mutate(id = case_when(id == "ARU172" ~ "ARU9172",
                        id == "AFS25621" ~ "AFS2562",
                        TRUE ~ id)) |>
  # fix date
  ### NEEDS CHECKING!!!
  mutate(day_not_date = if_else(day_not_date %in% c(13, 23), 12, day_not_date)) |>
  # fix site
  mutate(site_id = if_else(day_not_date == 12, "HS", "LS")) |>

  # fix treatment
  mutate(vegetation = if_else(id == "ADM8055", "native", vegetation)) |>
  mutate(treat_2 = if_else(is.na(treat_2), "vegetation", treat_2),
         treat_2 = if_else(id %in% c("JBE4019", "IFN4117", "JBU2017", "JBY0215"),
                           "bare", treat_2)) |>
  mutate(treat_1 = if_else(id == "ARV7048", "warm", treat_1),
         treat_1 = if_else(id %in% c("GJM6475", "AHY1498", "AMK3869"), "ambient", treat_1)) |>
  # THESE 2 NEED CHECKING!!!
  mutate(plot_id = if_else(id == "AOF3720", 3, plot_id),
         plot_id = if_else(id == "AOR0579", 4, plot_id)) |>

  # making commas to points
  mutate(veg_height_cm = as.numeric(str_replace(veg_height_cm, ",", ".")),
         rep_height_cm = as.numeric(str_replace(rep_height_cm, ",", ".")),
         wet_mass_g = as.numeric(str_replace(wet_mass_g, ",", ".")),
         leaf_thickness_1_mm = as.numeric(str_replace(leaf_thickness_1_mm, ",", ".")),
         leaf_thickness_2_mm = as.numeric(str_replace(leaf_thickness_2_mm, ",", ".")),
         leaf_thickness_3_mm = as.numeric(str_replace(leaf_thickness_3_mm, ",", "."))) |>

  mutate(bulk_nr = as.numeric(bulk_nr)) |>

  # quick and dirty fix for now!!!
  mutate(wet_mass_g = if_else(id == "GKH6324", NA_real_, wet_mass_g),
         leaf_thickness_3_mm = if_else(id == "GKH6324", 0.316, leaf_thickness_3_mm)) |>
  filter(!is.na(vegetation),
         !is.na(treat_1)) |>

  filter(vegetation == "native") |>

  left_join(raw_area, by = "id") |>
  left_join(dry_mass, by = "id") %>%

  # mean thickness
  mutate(leaf_thickness_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")), na.rm = TRUE)) |>

  # Fix leaf area columns
  rename(wet_mass_total_g = wet_mass_g,
         #dry_mass_total_g = dry_mass_g,
         leaf_area_total_cm2 = leaf_area) |>


  # Calculate values on the leaf level (mostly bulk samples)
  mutate(wet_mass_g = wet_mass_total_g / bulk_nr,
         #dry_mass_g = dry_mass_total_g / bulk_nr,
         leaf_area_cm2 = leaf_area_total_cm2 / bulk_nr)  |>
  # double area for Festuca, Avenella and Nardus
  #mutate(leaf_area_cm2 = if_else(grepl("Festuca|Avenella|Nardus", taxon), 2*leaf_area_cm2, leaf_area_cm2)) |>
  # Calculate SLA and LDMC (replace with wet mass for now)
  mutate(wet_sla_cm2_g = leaf_area_cm2 / wet_mass_g,
         #sla_cm2_g = leaf_area_cm2 / dry_mass_g,
         #ldmc = dry_mass_g / wet_mass_g
         ) |>

  select(id:rep_height_cm, wet_mass_g, leaf_area_cm2, leaf_thickness_mm, wet_sla_cm2_g, everything())

# bulk nr larger than nr of pieces
# AHS4588: The original bulk number registered was 5, but only 3 leaves were scanned. And then, for the dry weight it was noted that only 2 leaves remained. This one's kind of a mess.

comm <- community_join |>
  distinct() |>
  filter(!is.na(treatment_only_for_range_x)) |>
  select(-date, -fertility_all, -elevation) |>
  rename(treat_1 = treatment_only_for_range_x) |>
  mutate(aspect = if_else(aspect == "E", "east", "west"),
         treat_1 = if_else(treat_1 == "Control", "ambient", "warm")) |>
  mutate(treat_1 = as.factor(treat_1))


trait1 <- trait |>
  select(id:wet_sla_cm2_g) |>
  pivot_longer(cols = c(veg_height_cm:wet_sla_cm2_g), names_to = "trait", values_to = "value") |>
  mutate(plot_id = paste(block_id, plot_id, sep = ".")) |>
  filter(plot_id != "4.3") |>
  mutate(treat_1 = as.factor(treat_1)) |>
  mutate(species = tolower(species),
         species = str_replace(species, " ", "_"))

trait_filling <- trait_fill(
  comm = comm,
  traits = trait1,
  scale_hierarchy = c("plot_id"),
  taxon_col = "species",
  value_col = "value",
  trait_col = "trait",
  abundance_col = "cover",
  treatment_col = "treat_1",
  treatment_level = "plot_id"
)

bootstrap <- trait_np_bootstrap(trait_filling, nrep = 200)
trait_mean <- trait_summarise_boot_moments(bootstrap)

rangex_figure <- trait_mean |>
  mutate(trait = factor(trait, levels = c("veg_height_cm", "rep_height_cm", "wet_mass_g", "leaf_area_cm2", "leaf_thickness_mm", "wet_sla_cm2_g"))) |>
  rename(warming = treat_1_comm) |>
  ggplot(aes(x = warming, y = mean, fill = warming)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#999999", "#D55E00")) +
  labs(x = "", y = "Bootstrapped trait mean") +
  facet_wrap(~ trait, scales = "free_y") +
  theme_minimal() +
  theme(text = element_text(size = 20))

ggsave("RangeX_traits.jpeg", rangex_figure, width = 10, height = 8)


