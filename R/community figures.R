###Descriptive statistics and figures of the community data####
library(tidyverse)
library(tidylog)
library(ggplot2)
library(RColorBrewer)
library(vegan)

#import community data
comm <- read.csv("clean_data/PFTC7_SA_clean_community_19Apr2024.csv", row.names = 1) |>
  mutate(plotref = paste(site_id, aspect, plot_id, sep = "_"))
comm$plotref <- as.factor(comm$plotref)
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


####Descriptive stats####
#dominant species
sp_totalcover <- comm |> #sort species by highest total cover
  group_by(species) |>
  summarise(totalcover = sum(cover)) |>
  arrange(desc(totalcover))


#number of species
nsp <- ncol(comm_wide) #173
#number of plots
nplots <- nrow(comm_wide) #60

#species richness per site
sitelevel_stats <- comm |>
  group_by(site_id) |>
  distinct(species) |>
  summarise(nsp = n())

#species richness per plot
plotlevel_stats <- comm |>
                    group_by(plotref) |>
                    summarise (sprichness = n()) |>
  left_join(siteinfo, by = "plotref")

#mean overall species richness
mean_sprichness <- mean(plotlevel_stats$sprichness)
se_mean_sprichness <- sd(plotlevel_stats$sprichness)/sqrt(nplots)


##Species richness on different aspects and along the elevation gradient##
aspect_level_stats <- comm |>
  #filter(is.na(treatment_only_for_range_x)) |>
  group_by(plotref) |>
  mutate(nsp_plot = n()) |>
  ungroup() |>
  group_by(site_id, aspect) |>
  mutate(mean_sprichness = mean(nsp_plot),
         se_sprichness = sd(nsp_plot)/sqrt(5)) |>
  distinct(site_id, elevation, aspect,
           mean_sprichness, se_sprichness)
aspect_level_stats$elevation <- as.factor(aspect_level_stats$elevation)

aspect_barplot <- ggplot(aspect_level_stats, aes(x = elevation, y = mean_sprichness, fill = aspect)) +
  geom_bar(position = "dodge", width = 0.7, stat = "identity", alpha = 0.7, colour = "black") +
  scale_fill_manual(values = c("white", "black"), labels = c("East", "West")) +
  geom_errorbar(aes(x = elevation, ymin = mean_sprichness - se_sprichness, ymax = mean_sprichness + se_sprichness),
                width = 0.4, position = position_dodge(width = 0.7)) +
  labs(x = 'Elevation (m.a.s.l)', y = "Mean species richness", fill = 'Aspect') +
  theme_classic()

ggsave("aspect_barplot.png", aspect_barplot, path = "Figures", height = 1000, width = 1400, units = "px")


###Fertility###
#what percentage of records were fertile?
fertile <- comm |>
  filter(fertility_all == "y") |>
  summarise(n_fertile_records = n())

infertile <- comm |>
  filter(fertility_all == "n") |>
  summarise(n_infertile_records = n())

percent_fertile_records <- fertile$n_fertile_records/(fertile$n_fertile_records + infertile$n_infertile_records)

##proportion of fertile sp along elevation##
prop <- comm |>
  filter(!is.na(fertility_all)) |>
  group_by(elevation, fertility_all) |>
  summarize(count = n()) |>
  ungroup() |>
  group_by(elevation) |>
  mutate(proportion = count/sum(count))

palette <- brewer.pal(8, "Dark2")

fertplot <- ggplot(prop, aes(x = as.factor(elevation), y = proportion)) +
  geom_bar(aes(fill = fertility_all), position = position_stack(), stat = "identity", alpha = 0.6) +
  scale_fill_manual(values = c(palette[7], palette[4]) ,
                    labels = c("Not fertile", "Fertile")) +
  theme_classic() +
  xlab("Elevation (m.a.s.l.)") +
  ylab("Proportion") +
  theme(legend.title = element_blank())

ggsave("fertility_barplot.png",fertplot,path = "Figures")
