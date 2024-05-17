### Trait density plots ###

library(tidyverse)
library(ggpubr)
library(ggridges)


#the most recent version of clean traits
tdr<- read.csv("raw_data/PFTC7_SA_clean_traits_19Apr2024.csv")




#excluding wrong outliers for plots
trait_data <- tdr %>%
  mutate(veg_height_cm = ifelse(veg_height_cm>200, NA, veg_height_cm), #is not yet excluding the senecio glamrimous above 1 m
         wet_mass_g = ifelse(wet_mass_g > 20, NA, wet_mass_g),
         #dry_mass_g = ifelse(dry_mass_g > 1, NA, dry_mass_g),
         leaf_area_cm2 = ifelse(leaf_area_cm2 > 50, NA, leaf_area_cm2),
         leaf_thickness_mm = ifelse(leaf_thickness_mm > 100, NA, leaf_thickness_mm),
         sla_cm2_g = ifelse(sla_cm2_g>600, NA, sla_cm2_g),
         ldmc = ifelse(ldmc > 1, NA, ldmc),
         rep_height_cm = ifelse(rep_height_cm>100, NA, rep_height_cm),
         elevation_m_asl = as.factor(elevation_m_asl))%>%
  filter(!is.na(aspect),!is.na(elevation_m_asl))



### Version 1: traits by aspect


#setting some ploting values to change easily in all figures at the same time if needed
siz = 11
col.axis = "grey30"
alp= 0.3
cols = RColorBrewer::brewer.pal(3, "Set2")


vg<-trait_data %>% ggplot(aes(x= log(veg_height_cm), fill = aspect, color = aspect))+
  #xlim(0,100)+
  geom_density(alpha = alp)+
  scale_color_manual(values = cols, labels = c("East", "West"))+
  scale_fill_manual(values = cols, labels = c("East", "West")) +
  labs( x = "Plant_Height_cm", color = "Slope aspect", fill ="Slope aspect")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.line = element_line(color = col.axis))

wm<-trait_data %>% ggplot(aes(x= log(wet_mass_g), fill = aspect, color = aspect))+
  geom_density(alpha = alp)+
  scale_color_manual(values = cols, labels = c("East", "West"))+
  scale_fill_manual(values = cols, labels = c("East", "West")) +
  labs( x = "Wet_Mass_g", color = "Slope aspect", fill ="Slope aspect")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.line = element_line(color = col.axis))

dm<-trait_data %>% ggplot(aes(x= log(dry_mass_g), fill = aspect, color = aspect))+
  geom_density(alpha = alp)+
  scale_color_manual(values = cols, labels = c("East", "West"))+
  scale_fill_manual(values = cols, labels = c("East", "West")) +
  labs( x = "Dry_Mass_g", color = "Slope aspect", fill ="Slope aspect")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.line = element_line(color = col.axis))

la<-trait_data %>% ggplot(aes(x= log(leaf_area_cm2), fill = aspect, color = aspect))+
  geom_density(alpha = alp)+
  scale_color_manual(values = cols, labels = c("East", "West"))+
  scale_fill_manual(values = cols, labels = c("East", "West")) +
  labs( x = "Leaf_Area_cm2", color = "Slope aspect", fill ="Slope aspect")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.line = element_line(color = col.axis))

lt<-trait_data %>% ggplot(aes(x= log(leaf_thickness_mm), fill = aspect, color = aspect))+
  geom_density(alpha = alp)+
  scale_color_manual(values = cols, labels = c("East", "West"))+
  scale_fill_manual(values = cols, labels = c("East", "West")) +
  labs( x = "Leaf_Thickness_mm", color = "Slope aspect", fill ="Slope aspect")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.line = element_line(color = col.axis))

sl<-trait_data %>% ggplot(aes(x= log(sla_cm2_g), fill = aspect, color = aspect))+
  geom_density(alpha = alp)+
  scale_color_manual(values = cols, labels = c("East", "West"))+
  scale_fill_manual(values = cols, labels = c("East", "West")) +
  labs( x = "SLA_cm2_g", color = "Slope aspect", fill ="Slope aspect")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.line = element_line(color = col.axis))

ld<-trait_data %>% ggplot(aes(x= ldmc, fill = aspect, color = aspect))+
  geom_density(alpha = alp)+
  scale_color_manual(values = cols, labels = c("East", "West"))+
  scale_fill_manual(values = cols, labels = c("East", "West")) +
  labs( x = "LDMC", color = "Slope aspect", fill ="Slope aspect")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.line = element_line(color = col.axis))


trait_aspect<-ggarrange(vg, wm, dm, la, lt, sl, ld, common.legend = TRUE, legend= "right", align = "v")%>%
  annotate_figure(left = text_grob("Density",rot = 90))
trait_aspect

ggsave("trait_aspect.png", trait_aspect, path= "Figures", height = 5, width = 7 , bg= "white", units= "in")







### Version 2: Traits by elevation (1)

cols2<-rev(RColorBrewer::brewer.pal(5, "YlGnBu"))

alp2 = 0.2

vg2<-trait_data %>% ggplot(aes(x= log(veg_height_cm), fill = factor(elevation_m_asl, levels = c(2800,  2600, 2400,2200, 2000)), color = factor(elevation_m_asl, levels = c(2800,  2600, 2400,2200, 2000))))+
  geom_density(alpha = alp2)+
  scale_fill_manual(values = cols2)+
  scale_color_manual(values = cols2)+
  labs( x = "Plant_Height_cm", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())


wm2<-trait_data %>% ggplot(aes(x= log(wet_mass_g), fill = factor(elevation_m_asl, levels = c(2800,  2600, 2400,2200, 2000)), color = factor(elevation_m_asl, levels = c(2800,  2600, 2400,2200, 2000))))+
  geom_density(alpha = alp2)+
  scale_fill_manual(values = cols2)+
  scale_color_manual(values = cols2)+
  labs( x = "Wet_Mass_g", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

dm2<-trait_data %>% ggplot(aes(x= log(dry_mass_g), fill = factor(elevation_m_asl, levels = c(2800,  2600, 2400,2200, 2000)), color = factor(elevation_m_asl, levels = c(2800,  2600, 2400,2200, 2000))))+
  geom_density(alpha = alp2)+
  scale_fill_manual(values = cols2)+
  scale_color_manual(values = cols2)+
  labs( x = "Dry_Mass_g", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

la2<-trait_data %>% ggplot(aes(x= log(leaf_area_cm2), fill = factor(elevation_m_asl, levels = c(2800,  2600, 2400,2200, 2000)), color = factor(elevation_m_asl, levels = c(2800,  2600, 2400,2200, 2000))))+
  geom_density(alpha = alp2)+
  scale_fill_manual(values = cols2)+
  scale_color_manual(values = cols2)+
  labs( x = "Leaf_Area_cm2", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

lt2<-trait_data %>% ggplot(aes(x= log(leaf_thickness_mm), fill = factor(elevation_m_asl, levels = c(2800,  2600, 2400,2200, 2000)), color = factor(elevation_m_asl, levels = c(2800,  2600, 2400,2200, 2000))))+
  geom_density(alpha = alp2)+
  scale_fill_manual(values = cols2)+
  scale_color_manual(values = cols2)+
  labs( x = "Leaf_Thickness_mm", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

sl2<-trait_data %>% ggplot(aes(x= log(sla_cm2_g), fill = factor(elevation_m_asl, levels = c(2800,  2600, 2400,2200, 2000)), color = factor(elevation_m_asl, levels = c(2800,  2600, 2400,2200, 2000))))+
  geom_density(alpha = alp2)+
  scale_fill_manual(values = cols2)+
  scale_color_manual(values = cols2)+
  labs( x = "SLA_cm2_g", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

ld2<-trait_data %>% ggplot(aes(x= ldmc, fill = factor(elevation_m_asl, levels = c(2800,  2600, 2400,2200, 2000)), color = factor(elevation_m_asl, levels = c(2800,  2600, 2400,2200, 2000))))+
  geom_density(alpha = alp2)+
  scale_fill_manual(values = cols2)+
  scale_color_manual(values = cols2)+
  labs( x = "LDMC", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())


trait_elev1<-ggarrange( vg2, wm2, dm2, la2, lt2, sl2, ld2, common.legend = TRUE,legend = "right", align = "v") %>%
  annotate_figure(left = text_grob("Density",rot = 90))

ggsave("trait_elev1.png", trait_elev1, path= "Figures", height = 5, width = 7 , bg= "white", units= "in")







### Version 3: Traits by elevation (2)


scl= 2
alp3 = 0.4


vg3<-trait_data %>% ggplot(aes(x= log(veg_height_cm), y = elevation_m_asl, fill= elevation_m_asl))+
  geom_density_ridges(alpha= alp3, scale = scl)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  labs( x = "Plant_Height_m", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())


wm3<-trait_data %>% filter(!wet_mass_g == 0) %>% ggplot(aes(x= log(wet_mass_g), y = elevation_m_asl, fill= elevation_m_asl))+#geom_density_ridges() apparently can't deal with log(0) results, and throws an error, so one value = 0 must be manually excluded
  geom_density_ridges(alpha= alp3, scale = scl)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  labs( x = "Wet_Mass_g", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())



dm3<-trait_data %>% filter(!dry_mass_g == 0) %>% ggplot(aes(x= log(dry_mass_g), y = elevation_m_asl, fill= elevation_m_asl))+#geom_density_ridges() apparently can't deal with log(0) results, and throws an error, so one value = 0 must be manually excluded
  geom_density_ridges(alpha= alp3, scale = scl)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  labs( x = "Dry_Mass_g", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

la3<-trait_data %>% ggplot(aes(x= log(leaf_area_cm2), y = elevation_m_asl, fill= elevation_m_asl))+
  geom_density_ridges(alpha= alp3, scale = scl)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  labs( x = "Leaf_Area_cm2", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())


lt3<-trait_data %>% ggplot(aes(x= log(leaf_thickness_mm), y = elevation_m_asl, fill= elevation_m_asl))+
  geom_density_ridges(alpha= alp3, scale = scl)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  labs( x = "Leaf_Thickness_mm", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

sl3<-trait_data %>% ggplot(aes(x= log(sla_cm2_g), y = elevation_m_asl, fill= elevation_m_asl))+
  geom_density_ridges(alpha= alp3, scale = scl)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  labs( x = "SLA_cm2_g", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

ld3<-trait_data %>% ggplot(aes(x= ldmc, y = elevation_m_asl, fill= elevation_m_asl))+
  geom_density_ridges(alpha= alp3, scale = scl)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  labs( x = "LDMC", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())



trait_elev2<-ggarrange( vg3, wm3, dm3, la3, lt3, sl3, ld3, common.legend = TRUE,legend = "right", align = "v") %>%
  annotate_figure(left = text_grob("Elevation_m",rot = 90))
trait_elev2

ggsave("trait_elev2.png", trait_elev2, path= "Figures", height = 5, width = 7 , bg= "white", units= "in")







