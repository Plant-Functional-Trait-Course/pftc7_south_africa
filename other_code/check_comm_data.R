library(tidyverse)
library(readxl)
library(janitor)
library(vegan)
library(ggvegan)
library(glue)

raw_community <- read_excel("raw_data/Data entry 9Dec.xlsx") |>
  t() |>
  row_to_names(row_number = 1) |>
  clean_names()

date <- rownames(raw_community) |>
  as_tibble() |>
  rename(date = value) |>
  mutate(date = str_remove(date, "x"),
         date = gsub("_[^_]*$", "", date),
         date = dmy(date))

community <- bind_cols(date, raw_community) |>
  pivot_longer(cols = -c(date:plot_id), names_to = "species", values_to = "cover") |>
  filter(!is.na(cover)) |>
  mutate(cover = as.numeric(cover),
         elevation = as.numeric(elevation))

community |>
  group_by(site_id, elevation, aspect, plot_id) |>
  summarise(n = n()) |>
  ggplot(aes(x = as.character(elevation), y = n,
             fill = aspect, shape = aspect)) +
  geom_boxplot() +
  scale_fill_viridis_d() +
  theme_bw()


comm_wide <- community |>
  pivot_wider(names_from = species, values_from = cover, values_fill = 0)

comm_sp <- comm_wide %>%
  select(-c(date:plot_id))

# meta data
comm_info <- comm_wide %>%
  select(date:plot_id)

# make pca
res <- rda(comm_sp)

out <- bind_cols(comm_info, fortify(res) |>
                   filter(score == "sites"))

sp <- fortify(res) |>
  filter(score == "species")

e_B <- eigenvals(res)/sum(eigenvals(res))

# make main figure
# reference
out |>
  ggplot(aes(x = PC1, y = PC2, colour = site_id, shape = aspect)) +
  geom_point(size = 2) +
  coord_equal() +
  labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
       y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)")) +
  stat_ellipse(aes(colour = site_id)) +
  scale_colour_viridis_d(end = 0.9, option = "inferno", direction = -1, name = "Elevation m a.s.l.") +
  scale_shape_manual(values = c(16, 1)) +
  theme_bw()



