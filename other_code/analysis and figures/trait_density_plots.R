### Trait density plots ###

library(tidyverse)
library(ggpubr)
library(ggridges)
library(RColorBrewer)


#the most recent version of clean traits
trait_data<- read.csv("raw_data/PFTC7_SA_clean_traits_2023.csv") %>%
  pivot_wider(names_from = traits, values_from = value) %>%
  mutate(elevation_m_asl = as.factor(elevation_m_asl))%>%
  filter(!is.na(aspect),!is.na(elevation_m_asl))


### Traits by elevation

scl= 10
alp3 = 0.6
cols2 <-plasma(5)

lwd = 0.5


#Figures by trait
vg3<-trait_data %>% ggplot(aes(x= log(veg_height_cm), y = elevation_m_asl, fill= elevation_m_asl))+
  geom_density_ridges(alpha= alp3, scale = scl, linewidth=lwd)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  #scale_color_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 1.85)))+
  labs( x = "Plant_Height (cm)", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

rh3<-trait_data %>% ggplot(aes(x= log(rep_height_cm), y = elevation_m_asl, fill= elevation_m_asl))+
  geom_density_ridges(alpha= alp3, scale = scl, linewidth=lwd)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  #scale_color_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 2.6)))+
  labs( x = "Reproductive_Height (cm)", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())


wm3<-trait_data %>%  ggplot(aes(x= log(wet_mass_g), y = elevation_m_asl, fill= elevation_m_asl))+
  geom_density_ridges(alpha= alp3, scale = scl)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  #scale_color_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 2.15)))+
  labs( x = "Wet_Mass (g)", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())



dm3<-trait_data %>% filter(!dry_mass_g == 0) %>% ggplot(aes(x= log(dry_mass_g), y = elevation_m_asl, fill= elevation_m_asl))+#geom_density_ridges() apparently can't deal with log(0) results, and throws an error, so one value = 0 must be manually excluded
  geom_density_ridges(alpha= alp3, scale = scl)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 2.3)))+
  labs( x = "Dry_Mass (g)", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

la3<-trait_data %>% ggplot(aes(x= log(leaf_area_cm2), y = elevation_m_asl, fill= elevation_m_asl))+
  geom_density_ridges(alpha= alp3, scale = scl)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 2.5)))+
  labs(x = expression(paste("Leaf_Area (",cm^2,")")), color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())


lt3<-trait_data %>% ggplot(aes(x= log(leaf_thickness_mm), y = elevation_m_asl, fill= elevation_m_asl))+
  geom_density_ridges(alpha= alp3, scale = scl)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 2.5)))+
  labs( x = "Leaf_Thickness (mm)", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

sl3<-trait_data %>% ggplot(aes(x= log(sla_cm2_g), y = elevation_m_asl, fill= elevation_m_asl))+
  geom_density_ridges(alpha= alp3, scale = scl)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 2.4)))+
  labs( x = expression(paste("SLA (",cm^2/g,")")), color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

ld3<-trait_data %>% ggplot(aes(x= ldmc, y = elevation_m_asl, fill= elevation_m_asl))+
  geom_density_ridges(alpha= alp3, scale = scl)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400,2200, 2000))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 2.6)))+
  labs( x = "LDMC", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())


#Creating single figure
trait_elev2<-ggarrange(vg3 + rremove("y.text"), rh3+ rremove("y.text"), wm3+ rremove("y.text"), dm3+ rremove("y.text"), la3+ rremove("y.text"), lt3+ rremove("y.text"), sl3+ rremove("y.text"), ld3+ rremove("y.text"), common.legend = TRUE,legend = "right", align = "v", label.y = 1) %>%
  annotate_figure(left = text_grob("Density",rot = 90))
trait_elev2

#Saving figure
#ggsave("trait_elev2.png", trait_elev2, path= "Figures", height = 5, width = 7 , bg= "white", units= "in")



### Traits by aspect


siz = 11
col.axis = "grey30"
alp= 0.3
cols = c("#f9ca00",  "#0000f8")

#Figures by trait

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

rh<-trait_data %>% ggplot(aes(x= log(rep_height_cm), fill = aspect, color = aspect))+
  #xlim(0,100)+
  geom_density(alpha = alp)+
  scale_color_manual(values = cols, labels = c("East", "West"))+
  scale_fill_manual(values = cols, labels = c("East", "West")) +
  labs( x = "Reproductive_Height_cm", color = "Slope aspect", fill ="Slope aspect")+
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

#Creating single figure
trait_aspect<-ggarrange(vg, rh, wm, dm, la, lt, sl, ld, common.legend = TRUE, legend= "right", align = "v")%>%
  annotate_figure(left = text_grob("Density",rot = 90))
trait_aspect

#ggsave("trait_aspect.png", trait_aspect, path= "Figures", height = 5, width = 7 , bg= "white", units= "in")




















