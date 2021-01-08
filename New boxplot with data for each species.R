##########################################################
#
# New boxplot with data for each species
#
##########################################################


# Calculating new density and biomass data --------------------------------

#new density data
Den2 <- Density_sp %>% 
  group_by(Site) %>% 
  #Sum all N for each site
  mutate(N_all = sum(N_site_sp)) %>% 
  ungroup()

#New boxplot with data for each species
Bio2 <- Biomass_sp %>% 
  group_by(Site) %>% 
  #Sum all N for each site
  mutate(Kg_all = sum(Kg_site_sp)) %>% 
  ungroup()


# Plotting new boxplots for density and biomass ---------------------------

#Plot density with significance
Den2_boxplot <- ggplot(Den2, aes(x = Fishing, y = N_all, fill = Method)) +
  geom_boxplot(fatten = 3) +
  ggtitle("Mean density per 500"~m^2) +
  geom_signif(annotations = paste0("p = 0.013"), 
              y_position = max(Den2$N_all)*1.1, 
              xmin = "Closed", xmax = "Open", textsize = 5, 
              vjust = -0.2) +
  scale_fill_manual(name = "Method", labels = c("Stereo-DOVs", "UVC"), 
                    values = grey.colors(2, start = 0.1, end = 0.5)) +
  scale_y_continuous(name = "Number of individuals/500"~m^2) +
  scale_x_discrete(labels = c("No-take zone", "Fishing zone")) +
  theme_classic() +
  theme(plot.title = element_text(color="black", face="bold", hjust = 0.5),
        legend.title = element_text(color = "black"), 
        legend.text = element_text(color = "black"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 15), 
        axis.text.y = element_text(color = "black", size = 15), 
        title = element_text(color = "black", size = 16)) +
  coord_cartesian(clip = "off")

Den2_boxplot
#The outlier is Genovesa norte DOVS, this is due to a lot of Lutjanus argentiventris
#There are also L. argentiventris in the UVC data, but not nearly as many. 

#plot biomass in kg per 500m2
Bio2_boxplot <- ggplot(Bio2, aes(x = Fishing, y = Kg_all, fill = Method)) +
  geom_boxplot(fatten = 3) + 
  ggtitle("Mean biomass per 500"~m^2) +
  geom_signif(annotations = paste0("p = 0.001"), 
              y_position = max(Bio2$Kg_all)*1.1, 
              xmin = "Closed", xmax = "Open", textsize = 5, 
              vjust = -0.2) +
  scale_fill_manual(name = "Method", labels = c("Stereo-DOVs", "UVC"), 
                    values = grey.colors(2, start = 0.1, end = 0.5)) +
  scale_x_discrete(labels = c("No-take zone", "Fishing zone")) +
  scale_y_continuous(name = "Kg/500"~m^2) +
  theme_classic() +
  theme(plot.title = element_text(color="black", face="bold", hjust = 0.5),
        legend.title = element_text(color = "black"), 
        legend.text = element_text(color = "black"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 15), 
        axis.text.y = element_text(color = "black", size = 15), 
        title = element_text(color = "black", size = 16))+
  coord_cartesian(clip = "off")

Bio2_boxplot


#Bio plot with log(x+1) axis
#plot biomass in kg per 500m2
Bio2_boxplot2 <- ggplot(Bio2, aes(x = Fishing, y = log10(Kg_all+1), fill = Method)) +
  geom_boxplot(fatten = 3) + 
  ggtitle("Mean biomass per 500"~m^2) +
  geom_signif(annotations = paste0("p = 0.001"), 
              y_position = max(log10(Bio2$Kg_all+1))*1.1, 
              xmin = "Closed", xmax = "Open", textsize = 5, 
              vjust = -0.2) +
  scale_fill_manual(name = "Method", labels = c("Stereo-DOVs", "UVC"), 
                    values = grey.colors(2, start = 0.1, end = 0.5)) +
  scale_x_discrete(labels = c("No-take zone", "Fishing zone")) +
  scale_y_continuous(name = bquote(Log[10](1+Kg/500~m^2))) +
  theme_classic() +
  theme(plot.title = element_text(color="black", face="bold", hjust = 0.5),
        legend.title = element_text(color = "black"), 
        legend.text = element_text(color = "black"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 15), 
        axis.text.y = element_text(color = "black", size = 15), 
        title = element_text(color = "black", size = 16))+
  coord_cartesian(clip = "off")

Bio2_boxplot2


# Combining plots ---------------------------------------------------------

#Combining plots
new_boxplot <- ggarrange(nrow = 1, ncol = 3, Ric_boxplot, Den2_boxplot, Bio2_boxplot,
                         align = "v", common.legend = TRUE, legend = "right")
new_boxplot

#Saving composite image with different ratios - DFA
ggsave("Figures/New_boxplot.tiff", new_boxplot, device = "tiff", dpi = 300, width = 18, height = 6.5)


#Combining plots - log(x+1) axis for biomass plot
new_boxplot2 <- ggarrange(nrow = 1, ncol = 3, Ric_boxplot, Den2_boxplot, Bio2_boxplot2,
                          align = "v", common.legend = TRUE, legend = "right")
new_boxplot2

#Saving composite image with different ratios - DFA
ggsave("Figures/New_boxplot2.tiff", new_boxplot2, device = "tiff", dpi = 300, width = 18, height = 6.5)


rm(Den2_boxplot, Bio2_boxplot, Bio2_boxplot2, new_boxplot, new_boxplot2)


# Boxplot for bioregion ---------------------------------------------------
#Making a dataframe for the boxplot
Bioreg <- Richness %>% 
  #Adding density column
  left_join(Den2 %>% select(Site, Method, N_all), by = c("Site", "Method")) %>% 
  #Adding biomass column
  left_join(Bio2 %>% select(Site, Method, Kg_all), by = c("Site", "Method")) %>% 
  #Adding bioregion data from SiteInfo
  #left_join(SiteInfo %>% select(Site, Bioregion) %>% unique(), by = "Site") %>% 
  #reordering columns
  unique() %>% 
  select(Site, Site_sp_500m2, N_all, Kg_all, Bioregion, everything()) %>% 
  rename(Zone = Fishing) %>% 
  #renaming bioregions
  mutate(Bioregion = recode(Bioregion, 
                            "Lejano Norte" = "Far Northern", 
                            "Norte" = "Northern", 
                            "Centro Sur" = "Central South-eastern", 
                            "Oeste Fria" = "Western")) %>% 
  #column with combined bioregion and zone
  mutate(Bioreg_zone = paste(Bioregion, Zone))

#UNIVARIATE PERMANOVA for species richness
perm_ric <- adonis(Site_sp_500m2 ~ Zone*Bioregion, data = Bioreg, 
                   permutations = 9999, method = "euclidean")
perm_ric

#No take pairwise adonis
No_take_bioreg <- Bioreg %>% filter(Zone == "Closed")
pair_adonis1 <- pairwise.adonis(No_take_bioreg[,"Site_sp_500m2"], No_take_bioreg$Bioreg_zone, 
                                sim.method = "euclidean", perm = 9999)
pair_adonis1 <- as.data.frame(pair_adonis1)

#No take pairwise adonis
Fishing_bioreg <- Bioreg %>% filter(Zone == "Open")
pair_adonis2 <- pairwise.adonis(Fishing_bioreg[,"Site_sp_500m2"], Fishing_bioreg$Bioreg_zone, 
                                sim.method = "euclidean", perm = 9999)
pair_adonis2 <- as.data.frame(pair_adonis2)


#Plot species richness with significance
Bioreg_ric <- ggplot(Bioreg, aes(x = Zone, y = Site_sp_500m2, fill = Bioregion)) +
  geom_boxplot(fatten = 3) +
  ggtitle("Mean species richness per 500"~m^2) +
  #geom_signif(annotations = paste0("p = ", perm_ric$aov.tab$`Pr(>F)`[1]), 
  #           y_position = max(Richness$Site_sp_500m2)*1.1, 
  #          xmin = "Closed", xmax = "Open", textsize = 5, 
  #         vjust = -0.2) +
  #scale_fill_manual(values = grey.colors(4, start = 0.1, end = 0.6)) +
  scale_y_continuous(name = "Number of species/500"~m^2) +
  scale_x_discrete(labels = c("No-take zone", "Fishing zone")) +
  theme_classic() +
  theme(plot.title = element_text(color="black", face="bold", hjust = 0.5),
        legend.title = element_text(color = "black", size = 14), 
        legend.text = element_text(color = "black", size = 13),
        legend.key.size = unit(1,"cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 15), 
        axis.text.y = element_text(color = "black", size = 15), 
        title = element_text(color = "black", size = 16)) +
  guides(fill = guide_legend(title.position = "top", ncol = 1)) +
  coord_cartesian(clip = "off")

Bioreg_ric



# Note checked yet - maybe use pairwise.adonis based on PCO dist --------

#UNIVARIATE PERMANOVA for density 
perm_den <- adonis(N_site_500m2^0.5 ~ Zone*Bioregion, data = Bioreg, 
                   permutations = 9999, method = "euclidean")
perm_den

#No take pairwise adonis -- # should I also do the transformation in the pairwise adonis??
pair_adonis3 <- pairwise.adonis(No_take_bioreg[,"N_site_500m2"]^0.5, No_take_bioreg$Bioreg_zone, 
                                sim.method = "euclidean", perm = 9999)
pair_adonis3 <- as.data.frame(pair_adonis3)

#No take pairwise adonis
pair_adonis4 <- pairwise.adonis(Fishing_bioreg[,"N_site_500m2"]^0.5, Fishing_bioreg$Bioreg_zone, 
                                sim.method = "euclidean", perm = 9999)
pair_adonis4 <- as.data.frame(pair_adonis4)

#Plot density with significance
Bioreg_den <- ggplot(Bioreg, aes(x = Zone, y = N_site_500m2, fill = Bioregion)) +
  geom_boxplot(fatten = 3) +
  ggtitle("Mean density per 500"~m^2) +
  #geom_signif(annotations = paste0("p = ", perm_den$aov.tab$`Pr(>F)`[1]), 
  #           y_position = max(Density$N_site_500m2)*1.1, 
  #          xmin = "Closed", xmax = "Open", textsize = 5, 
  #         vjust = -0.2) +
  #scale_fill_manual(values = grey.colors(4, start = 0.1, end = 0.6)) +
  scale_y_continuous(name = "Number of individuals/500"~m^2) +
  scale_x_discrete(labels = c("No-take zone", "Fishing zone")) +
  theme_classic() +
  theme(plot.title = element_text(color="black", face="bold", hjust = 0.5),
        legend.title = element_text(color = "black"), 
        legend.text = element_text(color = "black"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 15), 
        axis.text.y = element_text(color = "black", size = 15), 
        title = element_text(color = "black", size = 16)) +
  coord_cartesian(clip = "off")

Bioreg_den


#UNIVARIATE PERMANOVA for biomass
perm_bio <- adonis(Kg_500m2_site^0.25 ~ Zone*Bioregion, data = Bioreg, 
                   permutations = 9999, method = "euclidean")
perm_bio

#No take pairwise adonis
pair_adonis5 <- pairwise.adonis(No_take_bioreg[,"Kg_500m2_site"], No_take_bioreg$Bioreg_zone, 
                                sim.method = "euclidean", perm = 9999)
pair_adonis5 <- as.data.frame(pair_adonis5)

#No take pairwise adonis
pair_adonis6 <- pairwise.adonis(Fishing_bioreg[,"Kg_500m2_site"], Fishing_bioreg$Bioreg_zone, 
                                sim.method = "euclidean", perm = 9999)
pair_adonis6 <- as.data.frame(pair_adonis6)


#plot biomass in kg per 500m2
Bioreg_bio <- ggplot(Bioreg, aes(x = Zone, y = Kg_500m2_site, fill = Bioregion)) +
  geom_boxplot(fatten = 3) + 
  ggtitle("Mean biomass per 500"~m^2) +
  #geom_signif(annotations = paste0("p = ", perm_bio$aov.tab$`Pr(>F)`[1]), 
  #           y_position = max(Biomass$Kg_500m2_site)*1.1, 
  #          xmin = "Closed", xmax = "Open", textsize = 5, 
  #         vjust = -0.2) +
  #scale_fill_manual(values = grey.colors(4, start = 0.1, end = 0.6)) +
  scale_x_discrete(labels = c("No-take zone", "Fishing zone")) +
  scale_y_continuous(name = "Kg/500"~m^2) +
  theme_classic() +
  theme(plot.title = element_text(color="black", face="bold", hjust = 0.5),
        legend.title = element_text(color = "black"), 
        legend.text = element_text(color = "black"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 15), 
        axis.text.y = element_text(color = "black", size = 15), 
        title = element_text(color = "black", size = 16))+
  coord_cartesian(clip = "off")

Bioreg_bio


#plot biomass in kg per 500m2
Bioreg_bio2 <- ggplot(Bioreg, aes(x = Zone, y = log10(Kg_500m2_site+1), fill = Bioregion)) +
  geom_boxplot(fatten = 3) + 
  ggtitle("Mean biomass per 500"~m^2) +
  #geom_signif(annotations = paste0("p = ", perm_bio$aov.tab$`Pr(>F)`[1]), 
  #           y_position = max(Biomass$Kg_500m2_site)*1.1, 
  #          xmin = "Closed", xmax = "Open", textsize = 5, 
  #         vjust = -0.2) +
  #scale_fill_manual(values = grey.colors(4, start = 0.1, end = 0.6)) +
  scale_x_discrete(labels = c("No-take zone", "Fishing zone")) +
  scale_y_continuous(name = bquote(Log[10](1+Kg/500~m^2))) +
  theme_classic() +
  theme(plot.title = element_text(color="black", face="bold", hjust = 0.5),
        legend.title = element_text(color = "black"), 
        legend.text = element_text(color = "black"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 15), 
        axis.text.y = element_text(color = "black", size = 15), 
        title = element_text(color = "black", size = 16))+
  coord_cartesian(clip = "off")

Bioreg_bio2


#Combining boxplot for richness, density and biomass
boxplot_bioreg <- ggarrange(nrow = 1, ncol = 3, Bioreg_ric, Bioreg_den, Bioreg_bio,
                            align = "v", common.legend = TRUE, legend = "right")
boxplot_bioreg

#Saving composite image with different ratios - DFA
#ggsave("Figures/boxplot_bioreg.tiff", boxplot_bioreg, device = "tiff", dpi = 300, width = 18, height = 6.5)


#Combining boxplot for richness, density and biomass
boxplot_bioreg2 <- ggarrange(nrow = 1, ncol = 3, Bioreg_ric, Bioreg_den, Bioreg_bio2,
                             align = "v", common.legend = TRUE, legend = "right")
boxplot_bioreg2

#Saving composite image with different ratios - DFA
#ggsave("Figures/boxplot_bioreg2.tiff", boxplot_bioreg2, device = "tiff", dpi = 300, width = 18, height = 6.5)


#Delete variables used for boxplots
rm(Bioreg, boxplot_bioreg, boxplot_bioreg2, Bioreg_ric, Bioreg_den, Bioreg_bio, Bioreg_bio2)


#Deleting permanova and pairwise.adonis variables
rm(perm_ric, perm_den, perm_bio, pair_adonis1, pair_adonis2, pair_adonis3, 
   pair_adonis4, pair_adonis5, pair_adonis6, No_take_bioreg, Fishing_bioreg)


