##########################################################
#
# New boxplot with data for each species
#
##########################################################

#new density data
Den2 <- Density_sp %>% 
  group_by(Site) %>% 
  #Sum all N for each site
  mutate(N_all = sum(N_site_sp)) %>% 
  ungroup()

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


#New boxplot with data for each species
Bio2 <- Biomass_sp %>% 
  group_by(Site) %>% 
  #Sum all N for each site
  mutate(Kg_all = sum(Kg_site_sp)) %>% 
  ungroup()

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

