###Descriptive statistics and figures of the community data####
library(tidyverse)
library(tidylog)
library(ggplot2)
library(RColorBrewer)
library(vegan)

#import community data
comm <- read.csv("clean_data/PFTC7_SA_clean_community_19Apr2024.csv", row.names = 1)
comm$site_id <- as.factor(comm$site_id)
comm$aspect <- as.factor(comm$aspect)
comm$plot_id <- as.factor(as.numeric(comm$plot_id))
comm$cover <- as.numeric(comm$cover)

#transform to wide so that we can use it in nmds
comm_wide <- comm |>
  #filter(is.na(treatment_only_for_range_x)) |> #only work with site 1-5, not rangex site
  mutate(plotref = paste(site_id, aspect, plot_id, sep = "_")) |>
  select(plotref, species, cover) |>
  pivot_wider(names_from = species, values_from = cover) |>
  column_to_rownames(var = "plotref")
comm_wide <- as.data.frame(comm_wide)

#replace NA values with 0
for(r in 1:nrow(comm_wide)) {
  for(c in 1:ncol(comm_wide)) {
    if(is.na(comm_wide[r,c])) {
      comm_wide[r,c] <- 0
    }
  }
}


##Now we can make the nmds
set.seed(10)
ord <-metaMDS(comm_wide, distance = "bray", k = 3)

#get the site scores
site_scores <- as.data.frame(scores(ord, display = "sites"))
#add the elevation and aspect to the sites_scores dataframe
siteinfo <- comm |>
  #filter(is.na(treatment_only_for_range_x)) |> #remove rangex records
  distinct(site_id, elevation, aspect, treatment_only_for_range_x, plot_id) |>
  mutate(plotref = paste(site_id, aspect, plot_id, sep = "_"))

site_scores_join <- site_scores |>
  rownames_to_column(var = "plotref") |>
  left_join(siteinfo, by = "plotref") |>
  #create a "treatment"column with either the aspects or th erangex treatments
  mutate(trmt = case_when(is.na(treatment_only_for_range_x) ~ as.character(aspect),
                          .default = as.character(treatment_only_for_range_x)))
site_scores_join$elevation <- as.factor(site_scores_join$elevation)

pal <- brewer.pal(7, "Set1")[c(1,2,3,4,5,7)]

##now create the ordination plot
nmds_sites <- ggplot(site_scores_join, aes(x = NMDS1, y = NMDS2, color = elevation,
                                           shape = factor(trmt, levels = c("E", "W", "Control", "OTC")))) +
                geom_point() +
                theme_classic() +
                scale_color_manual(values = c(pal)) +
                scale_shape_manual(labels = c("East", "West", "Control", "Warming"), values = c(1, 16, 12,15)) +
                labs(color = "Elevation (m.a.s.l.)", shape = "Treatment") +
                theme(legend.position = "right")

ggsave("nmds_sites.png", nmds_sites, path = "Figures", height = 1200, width = 1500, units = "px")




