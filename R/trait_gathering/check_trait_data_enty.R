# check trait data
library(tidylog)

d5 <- read_excel("raw_data/Kopie von PFTC7_SA_raw_traits_2023 – 5. Dezember, 18_00.xlsx") |> filter(!is.na(ID))
d6 <- read_excel("raw_data/Kopie von PFTC7_SA_raw_traits_2023 – 6. Dezember, 19_12.xlsx") |> filter(!is.na(ID))
d7 <- read_excel("raw_data/Kopie von PFTC7_SA_raw_traits_2023 – 7. Dezember, 18_10.xlsx") |> filter(!is.na(ID))
d8 <- read_excel("raw_data/Kopie von PFTC7_SA_raw_traits_2023 – 8. Dezember, 17_55.xlsx") |> filter(!is.na(ID))
d9 <- read_excel("raw_data/Kopie von PFTC7_SA_raw_traits_2023 – 9. Dezember, 18_06.xlsx") |> filter(!is.na(ID))
d10 <- read_excel("raw_data/Kopie von PFTC7_SA_raw_traits_2023 – 10. Dezember, 18_03.xlsx") |> filter(!is.na(ID))


# leaves that did not make it to the next dataset
only5 <- d5 |>
  anti_join(d6)
# nothing in 6 that is not in 7
# nothing in 7 that is not in 8
# nothing in 8 that is not in 9
only9 <- d9 |>
  anti_join(d10) |> View()
d10 |>
  left_join(d8)

d10 |> filter(ID == "GCQ0157")



raw_traits <- read_excel("raw_data/PFTC7_SA_raw_traits_2023.xlsx") |>
  clean_names()
raw_traits |>
  # remove empty rows
  filter(!is.na(id)) |>
  mutate(veg_height_cm = as.numeric(str_replace(veg_height_cm, ",", ".")),
         rep_height_cm = as.numeric(str_replace(rep_height_cm, ",", ".")),
         leaf_thickness_1_mm = as.numeric(str_replace(leaf_thickness_1_mm, ",", ".")),
         leaf_thickness_2_mm = as.numeric(str_replace(leaf_thickness_2_mm, ",", ".")),
         leaf_thickness_3_mm = as.numeric(str_replace(leaf_thickness_3_mm, ",", ".")))

dd |>
  filter(rep_height_cm > 10) |>
  ggplot(aes(x = leaf_thickness_3_mm)) +
  geom_histogram()


  select(veg_height_cm, veg_height_cm2) |>
  filter(veg_height_cm != veg_height_cm2) |> print(n = Inf)

  #filter(is.na(elevation_m_asl))
  count(plot_id)
  arrange(site_id)
  #count(species) |> arrange(species) |> print(n = Inf)

raw_traits |>
  ggplot(aes(x = leaf_thickness_1_mm, y = leaf_thickness_3_mm, colour = as.factor(site_id))) +
  geom_point()


plot <- raw_traits |>
  ggplot(aes(x = site_id, y = elevation_m_asl)) +
  geom_point() +
  #geom_jitter() +
  labs(x = "Site ID", y = "Elevation in m a.s.l.") +
  annotate("text", label = "R2 = 0.9938",
           x = 5, y = 2000, size = 4) +
  theme_bw()
ggsave("first_plot.png", plot)
fit <- lm(elevation_m_asl ~ site_id, raw_traits)
summary(fit)


raw_traits |> count(day_not_date, site_id) |>
  arrange(site_id) |> print(n = Inf)



read_excel("raw_data/PFTC7_SA_raw_traits_2023.xlsx", sheet = "RangeX") |>
  clean_names() |>
  filter(!is.na(id)) |>
  mutate(veg_height_cm2 = str_replace(veg_height_cm, ",", "."),
         veg_height_cm3 = as.numeric(veg_height_cm2)) |>
  select(veg_height_cm, veg_height_cm2, veg_height_cm3) |> View()
