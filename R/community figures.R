###Descriptive statistics and figures of the community data####
library(tidyverse)
library(tidylog)
library(ggplot2)
library(vegan)

#import community data
comm <- read.csv("clean_data/PFTC7_SA_clean_community_19Apr2024.csv", row.names = 1)
comm$site_id <- as.factor(comm$site_id)
comm$aspect <- as.factor(comm$aspect)
comm$plot_id <- as.factor(as.numeric(comm$plot_id))
comm$cover <- as.numeric(comm$cover)

#transform to wide so that we can use it in nmds
comm_wide <- comm |>
  filter(is.na(treatment_only_for_range_x)) |> #only work with site 1-5, not rangex site
  mutate(plotref = paste(site_id, aspect, plot_id, sep = "_")) |>
  select(plotref, species, cover) |>
  pivot_wider(names_from = species, values_from = cover) |>
  column_to_rownames(var = "plotref")
comm_wide <- as.data.frame(comm_wide)


comm |>
  filter(is.na(treatment_only_for_range_x)) |> #only work with site 1-5, not rangex site
  mutate(plotref = paste(site_id, aspect, plot_id, sep = "_")) |>
  select(plotref, species, cover) |>
  group_by(plotref, species) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1L)

comm |>
  mutate(plotref = paste(site_id, aspect, plot_id, sep = "_")) |>
  filter(plotref == "2_E_4") |>
  select(species) |>
  arrange(species)

#there must be multiple entries of the same species in a plot causing the list

#replace NULL values with 0
for(r in 1:nrow(comm_wide)) {
  for(c in 1:ncol(comm_wide)) {
    if(comm_wide[r,c])
  }
}

