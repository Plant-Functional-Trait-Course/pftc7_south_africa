comm <- community_join |>
  distinct() |>
  filter(is.na(treatment_only_for_range_x)) |>
  select(-date, -treatment_only_for_range_x, -fertility_all) |>
  distinct() |>
  #filter(site_id == 5, aspect == "E", plot_id == 1) |>
  #distinct(species)
  group_by(site_id, elevation, aspect, plot_id, species) |>
  mutate(n = 1:n()) |>
  filter(n < 2) |>
  mutate(aspect = if_else(aspect == "W", "west", "east"),
         site_id = as.character(site_id),
         plot_id = as.character(plot_id))
  # distinct(site_id, aspect, plot_id) |>
  # arrange(aspect, site_id, plot_id) |> print(n = Inf)
  #pivot_wider(names_from = species, values_from = cover, values_fill = 0)

get_file(node = "hk2cy",
         file = "PFTC_SA_clean_community_2023.csv
",
         path = "clean_data",
         remote_path = "community_data")


library(traitstrap)

comm |> filter(is.na(elevation)) |> View()
d1 |> ungroup() |> count(site_id, elevation_m_asl)

d1 <- dd |>
  filter(project != "TSP",
         !is.na(site_id),
         !is.na(site_id),
         !is.na(plot_id)) |>
  select(id, site_id, elevation_m_asl, aspect, plot_id, species, veg_height_cm, wet_mass_g, dry_mass_g, leaf_area_cm2, leaf_thickness_mm, sla_cm2_g, ldmc) |>
  mutate(veg_height_cm = if_else(veg_height_cm > 150, NA_real_, veg_height_cm),
         wet_mass_g = if_else(wet_mass_g > 50, NA_real_, wet_mass_g),
         leaf_thickness_mm = if_else(leaf_thickness_mm > 1, NA_real_, leaf_thickness_mm),
         ldmc = if_else(ldmc > 1, NA_real_, ldmc),
         sla_cm2_g = if_else(sla_cm2_g > 500, NA_real_, sla_cm2_g)) |>
  pivot_longer(cols = c(veg_height_cm:ldmc), names_to = "trait", values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(site_id = as.character(site_id),
         plot_id = as.character(plot_id)) |>
  #log transform size and area traits
  mutate(
    value_trans = if_else(
      trait %in% c(
        "veg_height_cm",
        "wet_mass_g",
        "dry_mass_g",
        "leaf_area_cm2",
        "leaf_thickness_mm"
      ),
      true = suppressWarnings(log(value)),# suppress warnings from log(-value) in isotopes (these are calculated but not kept)
      false = value
    ),
    trait_trans = recode(
      trait,
      "veg_height_cm" = "veg_height_cm_log",
      "wet_mass_g" = "wet_mass_g_log",
      "dry_mass_g" = "dry_mass_g_log",
      "leaf_area_cm2" = "leaf_area_cm2_log",
      "leaf_thickness_mm" = "thickness_mm_log"
    ))

trait_filling <- trait_fill(
  comm = comm,
  traits = d1,
  scale_hierarchy = c("site_id", "aspect", "plot_id"),
  taxon_col = "species",
  value_col = "value",
  trait_col = "trait_trans",
  abundance_col = "cover",
  other_col = "elevation"
)

bootstrap <- trait_np_bootstrap(trait_filling, nrep = 200)
trait_mean <- trait_summarise_boot_moments(bootstrap)

trait_mean |>
  mutate(trait_trans = factor(trait_trans, levels = c("veg_height_cm_log", "wet_mass_g_log", "dry_mass_g_log", "leaf_area_cm2_log", "thickness_mm_log", "ldmc", "sla_cm2_g"))) |>
  ggplot(aes(x = elevation, y = mean, colour = aspect)) +
  geom_point() +
  #geom_smooth() +
  facet_wrap(~ trait_trans, scales = "free")




library(vegan)
library(ggvegan)
library(glue)
library(patchwork)

## TRAITS (PCA)
make_trait_pca <- function(trait_mean){

  set.seed(32)

  # make wide trait table
  cwm_fat <- trait_mean %>% filter(is.na(elevation))
    ungroup() |>
    # remove nutrient ratio traits
    #filter(!trait_trans %in% c("CN_ratio", "NP_ratio")) |>
    select(site_id:mean, -n) %>%
    pivot_wider(names_from = "trait_trans", values_from = "mean")

  pca_output <- cwm_fat %>%
    select(-(site_id:elevation)) %>%
    rda(scale = TRUE, center = TRUE)

  pca_sites <- bind_cols(
    cwm_fat %>%
      select(site_id:elevation),
    fortify(pca_output, display = "sites")
  )

  # arrows
  pca_traits <- fortify(pca_output, display = "species") %>%
    mutate(trait_trans = label)
    #fancy_trait_name_dictionary()

  # permutation test
  # traits
  raw <- cwm_fat %>% select(-(Gradient:SoilTemperature))
  # meta data
  meta <- cwm_fat %>% select(Gradient:SoilTemperature) %>%
    mutate(Site = factor(Site))

  # adonis test
  if(meta %>% distinct(Gradient) %>% count() == 2){
    adonis_result <- adonis2(raw ~ Gradient*Mean_elevation , data = meta, permutations = 999, method = "euclidean")
  } else {
    adonis_result <- adonis2(raw ~ Mean_elevation, data = meta, permutations = 999, method = "euclidean")
  }

  outputList <- list(pca_sites, pca_traits, pca_output, adonis_result)

  return(outputList)
}



make_pca_plot <- function(trait_pca){

  # adding pics
  # bird <- grid::rasterGrob(png::readPNG("bird.png"), interpolate = TRUE)
  # ref <- grid::rasterGrob(png::readPNG("ref.png"), interpolate = TRUE)
  # add this to figure
  # annotation_custom(bird, xmin = -2.5, xmax = -3.8, ymin = 1.3, ymax = 2.6)

  # both gradients
  #e_B <- eigenvals(trait_pca[[3]])/sum(eigenvals(trait_pca[[3]]))
  e_B <- eigenvals(pca_output)/sum(eigenvals(pca_output))

  #pcaC <- trait_pca[[1]] %>%
  pca <- pca_sites %>%
    #filter(Gradient == "C") |>
    mutate(SA = paste0(site_id, "_", aspect)) |>
    ggplot(aes(x = PC1, y = PC2, colour = site_id, shape = aspect)) +
    geom_point(size = 2) +
    coord_equal() +
    #stat_ellipse() +
    scale_colour_viridis_d(end = 0.8, option = "inferno", direction = -1, name = "Elevation m a.s.l.") +
    labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)")) +
    facet_wrap(~ aspect) +
    theme_bw()

  arrows <- pca_sites %>%
    ggplot(aes(x = PC1, y = PC2)) +
    geom_segment(data = pca_traits,
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm")),
                 inherit.aes = FALSE) +
    geom_text(data = pca_traits,
                # mutate(PC1 = case_when(Label == "Thickness_mm_log" ~ 1.23,
                #                        Label == "LDMC" ~ -1,
                #                        Label == "Temperature" ~ -0.4,
                #                        Label == "Moisture" ~ -1,
                #                        TRUE ~ PC1),
                #        PC2 = case_when(Label == "dC13_permil" ~ -1.25,
                #                        Label == "C_percent" ~ -0.3,
                #                        Label == "dN15_permil" ~ -0.3,
                #                        Label == "Temperature" ~ 0.4,
                #                        Label == "Moisture" ~ 0.06,
                #                        TRUE ~ PC2)),
              aes(x = PC1+0.3, y = PC2, label = trait_trans),
              size = 2.5,
              inherit.aes = FALSE,
              show.legend = FALSE, parse = TRUE) +
    coord_equal() +
    labs(x = "PCA1", y = "PCA2") +
    scale_x_continuous(expand = c(.2, 0)) +
    scale_linetype_manual(name = "", values = c("solid", "dashed", "solid", "solid")) +
    #scale_colour_manual(name = "", values = c("black", "grey40", "grey70", "cornflowerblue")) +
    theme_bw()

  pca_all <- (pca / arrows) &
    theme(legend.box = "horizontal")


  return(trait_ordination_plot)

}


ggsave(pca_all)
