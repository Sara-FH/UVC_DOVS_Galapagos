##########################################################
#
# Figure biomass and density for key species
#
##########################################################

# Loading libraries -------------------------------------------------------

library(ggpubr) #ggarrange
library(ggsignif) #Shows level of significance in ggplots

# Figure species of interest biomass and density --------------------------

#Option 1 to select species of interest
# spInt <- c("Sphyrna lewini", "Lutjanus argentiventris", "Mycteroperca olfax")
# Biomass_sp %>% filter(grepl(paste(spInt, collapse = "|"), ValidName))

#Option 2 to select species of interest
#Creating data frame with species of interest
spInt <- data.frame(
  ValidName = c("Sphyrna lewini", "Lutjanus argentiventris", "Mycteroperca olfax", 
                "Triaenodon obesus", "Paralabrax albomaculatus", "Hypanus dipterurus",
                "Carcharhinus galapagensis", "Carcharhinus limbatus", "Caranx melampygus", 
                "Aetobatus laticeps", "Lutjanus novemfasciatus", "Hoplopagrus guentherii", 
                "Seriola rivoliana", "Caranx lugubris", "Dermatolepis dermatolepis"))

plot1 <- Biomass_sp %>% 
  #Complete sites for all species and methods
  complete(Site, nesting(ValidName, Method)) %>% 
  #Selecting species of interest using spInt
  right_join(spInt, by = "ValidName") %>% 
  #Replacing NAs in biomass with zeroes
  mutate(Biomass_site_sp = replace_na(Biomass_site_sp, 0)) %>%
  #Removing fishing status
  select(-Fishing) %>%
  #Joining fishing status to get correct info
  left_join(SiteInfo %>% select(Site, Fishing) %>% unique(), by = "Site")

#Density 
plot2 <- Density_sp %>% 
  #Complete sites for all species and methods
  complete(Site, nesting(ValidName, Method)) %>% 
  #Selecting species of interest using spInt
  right_join(spInt, by = "ValidName") %>% 
  #Replacing NAs in biomass with zeroes
  mutate(N_site_sp = replace_na(N_site_sp, 0)) %>%
  #Removing fishing status
  select(-Fishing) %>%
  #Joining fishing status to get correct info
  left_join(SiteInfo %>% select(Site, Fishing) %>% unique(), by = "Site")

#Creating empty lists to save figures
fig_list <- list()
#Creating combined plots for density and biomass per species
for(i in seq_along(spInt$ValidName)){
  bio <- plot1 %>% filter(ValidName == spInt$ValidName[i]) 
  Pbio <- adonis(Biomass_site_sp ~ Method*Fishing, data = bio, permutations = 9999, method = "euclidean")
  f1 <- bio %>%
    #Create a plot
    #log10+1 applied to biomass so it can be seen better
    ggplot(aes(x = Fishing, y = log10(Biomass_site_sp+1), fill = Method))+
    geom_boxplot(width = 0.8) + #outlier.size changes the size of the dots representing outliers
    #To access p value for PERMANOVA comparison for fishing status use: Pbio$aov.tab$`Pr(>F)`[2]
    geom_signif(annotations = Pbio$aov.tab$`Pr(>F)`[2], 
                y_position = max(log10(bio$Biomass_site_sp+1))*1.1, 
                xmin = "Closed", xmax = "Open", textsize = 4, 
                vjust = -0.2) +
    scale_x_discrete(expand = c(0.5, 0)) +
    #Change the y axis label
    labs(y = "Biomass grams/500"~m^2, title = spInt$ValidName[i]) +
    theme_classic() +
    theme(axis.text.x = element_text(color = "black", size = 11),
          axis.text.y = element_text(color = "black", size = 11),
          axis.title.x = element_blank(),
          legend.position = "hide",
          plot.title = element_text(color = "black", face = "italic", size = 14, vjust = 2)) +
    coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
    scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot
  
  
  #Density
  den <- plot2 %>% filter(ValidName == spInt$ValidName[i])
  Pden <- adonis(N_site_sp ~ Method*Fishing, data = den, permutations = 9999, method = "euclidean")
  f2 <- den %>%
    #Create a plot
    ggplot(aes(x = Fishing, y = N_site_sp, fill = Method))+
    geom_boxplot(width = 0.8)+ #outlier.size changes the size of the dots representing outliers
    #To access p value for PERMANOVA comparison for fishing status use: Pbio$aov.tab$`Pr(>F)`[2]
    geom_signif(annotations = Pden$aov.tab$`Pr(>F)`[2], 
                y_position = max(den$N_site_sp)*1.1, 
                xmin = "Closed", xmax = "Open", textsize = 4, 
                vjust = -0.2) +
    scale_x_discrete(expand = c(0.5, 0))+
    #Change the y axis label
    labs(y = "Density individuals/500"~m^2)+
    theme_classic() +
    theme(axis.text.x = element_text(color = "black", size = 11),
          axis.text.y = element_text(color = "black", size = 11),
          axis.title.x = element_blank(),
          legend.position = "hide") +
    coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
    scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot
  
  #Combining both plots (biomass and density)
  fcomb <- ggarrange(f1, f2, nrow = 2, align = "hv")+
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"),
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank())
  
  #Saving combined plot in empty list created at the top
  fig_list[[i]] <- fcomb
}

#Creating composite image using the list of plots saved in the loop above
compfig <- ggarrange(plotlist = fig_list, ncol = 5, nrow = 3, 
                     labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O"), 
                     common.legend = T, hjust = -1,  vjust = 1.8)

compfig

#Saving composite image as tiff file and changing dimensions
ggsave("Figures/compfig2.tiff", compfig, device = "tiff", dpi = 300, width = 25, height = 25)


# Splitting the figure - part 1 --------------------------------------------

#Creating data frame with species of interest
spInt <- data.frame(
  ValidName = c("Sphyrna lewini", "Lutjanus argentiventris", "Mycteroperca olfax", 
                "Triaenodon obesus", "Paralabrax albomaculatus", "Hypanus dipterurus",
                "Carcharhinus galapagensis", "Carcharhinus limbatus"))

plot1 <- Biomass_sp %>% 
  #Complete sites for all species and methods
  complete(Site, nesting(ValidName, Method)) %>% 
  #Selecting species of interest using spInt
  right_join(spInt, by = "ValidName") %>% 
  #Replacing NAs in biomass with zeroes
  mutate(Biomass_site_sp = replace_na(Biomass_site_sp, 0)) %>%
  #Removing fishing status
  select(-Fishing) %>%
  #Joining fishing status to get correct info
  left_join(SiteInfo %>% select(Site, Fishing) %>% unique(), by = "Site")

#Density 
plot2 <- Density_sp %>% 
  #Complete sites for all species and methods
  complete(Site, nesting(ValidName, Method)) %>% 
  #Selecting species of interest using spInt
  right_join(spInt, by = "ValidName") %>% 
  #Replacing NAs in biomass with zeroes
  mutate(N_site_sp = replace_na(N_site_sp, 0)) %>%
  #Removing fishing status
  select(-Fishing) %>%
  #Joining fishing status to get correct info
  left_join(SiteInfo %>% select(Site, Fishing) %>% unique(), by = "Site")

#Creating empty lists to save figures
fig_list <- list()

#Creating empty lists to save PERMANOVAS
perm_bio_list <- list()
perm_den_list <- list()

#Creating combined plots for density and biomass per species
for(i in seq_along(spInt$ValidName)){
  bio <- plot1 %>% filter(ValidName == spInt$ValidName[i]) 
  Pbio <- adonis(Biomass_site_sp^0.25 ~ Method*Fishing, data = bio, permutations = 9999, method = "euclidean")
  #To access p value for PERMANOVA comparison for fishing status use: Pbio$aov.tab$`Pr(>F)`[2]
  f1 <- bio %>%
    #Create a plot
    #log10+1 applied to biomass so it can be seen better
    ggplot(aes(x = Fishing, y = log10(Biomass_site_sp+1), fill = Method))+
    geom_boxplot(width = 0.8) + #outlier.size changes the size of the dots representing outliers
    geom_signif(annotations = paste0("p = ", Pbio$aov.tab$`Pr(>F)`[2]), 
                y_position = max(log10(bio$Biomass_site_sp+1))*1.1, 
                xmin = "Closed", xmax = "Open", textsize = 4, 
                vjust = -0.2) +
    scale_x_discrete(expand = c(0.5, 0), 
                     labels = c("No-take zone", "Fishing zone")) +
    #Change the y axis label
    labs(y = "Biomass grams/500"~m^2, title = spInt$ValidName[i]) +
    theme_classic() +
    theme(axis.text.x = element_text(color = "black", size = 11),
          axis.text.y = element_text(color = "black", size = 11),
          axis.title.x = element_blank(),
          legend.position = "hide",
          plot.title = element_text(color = "black", face = "italic", size = 14, vjust = 1.5)) +
    coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
    scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot
  
  
  #Density
  den <- plot2 %>% filter(ValidName == spInt$ValidName[i])
  Pden <- adonis(N_site_sp ~ Method*Fishing, data = den, permutations = 9999, method = "euclidean")
  f2 <- den %>%
    #Create a plot
    ggplot(aes(x = Fishing, y = N_site_sp, fill = Method))+
    geom_boxplot(width = 0.8) + #outlier.size changes the size of the dots representing outliers
    geom_signif(annotations = paste0("p = ", Pden$aov.tab$`Pr(>F)`[2]), 
                y_position = max(den$N_site_sp)*1.1, 
                xmin = "Closed", xmax = "Open", textsize = 4, 
                vjust = -0.2) +
    scale_x_discrete(expand = c(0.5, 0), 
                     labels = c("No-take zone", "Fishing zone")) +
    #Change the y axis label
    labs(y = "Density individuals/500"~m^2)+
    theme_classic() +
    theme(axis.text.x = element_text(color = "black", size = 11),
          axis.text.y = element_text(color = "black", size = 11),
          axis.title.x = element_blank(),
          legend.position = "hide") +
    coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
    scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot
  
  #Combining both plots (biomass and density)
  fcomb <- ggarrange(f1, f2, nrow = 2, align = "hv")+
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"),
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank())
  
  #Saving combined plot in empty list created at the top
  fig_list[[i]] <- fcomb
  
  #Saving biomass PERMANOVAS
  perm_bio_list[[i]] <- Pbio$aov.tab
  #Saving density PERMANOVAS
  perm_den_list[[i]] <- Pden$aov.tab
}

#Creating composite image using the list of plots saved in the loop above
compfig <- ggarrange(plotlist = fig_list, ncol = 4, nrow = 2, 
                     labels = c("A)", "B)", "C)", "D)", "E)", "F)", "G)", "H)"),
                     font.label = list(size = 14),
                     common.legend = T, hjust = -1.2,  vjust = 1.8)

compfig

#Saving composite image as tiff file and changing dimensions
ggsave("Figures/compfig_part1.tiff", compfig, device = "tiff", dpi = 300, width = 15, height = 10)


# PERMANOVAS for figure part 1 --------------------------------------------

#Making results from PERMANOVA biomass part 1 ready for excel
results <- perm_bio_list
#Names of the species in the PERMANOVA
perm_bio_cols <- c("Sphyrna_lewini", "Lutjanus_argentiventris", "Mycteroperca_olfax", 
                   "Triaenodon_obesus", "Paralabrax_albomaculatus", "Hypanus_dipterurus",
                   "Carcharhinus_galapagensis", "Carcharhinus_limbatus")
#Adding names from species to the PERMANOVA results
names(results) <- perm_bio_cols
#Format ready for excel
results <- do.call(rbind, results)
#writing 
write.xlsx(results, "Figures/PERMANOVA_bio_1.xlsx")


#Making results from PERMANOVA biomass part 1 ready for excel
results <- perm_den_list
#Names of the species in the PERMANOVA
perm_den_cols <- c("Sphyrna_lewini", "Lutjanus_argentiventris", "Mycteroperca_olfax", 
                   "Triaenodon_obesus", "Paralabrax_albomaculatus", "Hypanus_dipterurus",
                   "Carcharhinus_galapagensis", "Carcharhinus_limbatus")
#Adding names from species to the PERMANOVA results
names(results) <- perm_den_cols
#Format ready for excel
results <- do.call(rbind, results)
#writing 
write.xlsx(results, "Figures/PERMANOVA_den_1.xlsx")


# Splitting the figure part 2 ----------------------------------------------

#Creating data frame with species of interest
spInt <- data.frame(
  ValidName = c("Caranx melampygus", "Aetobatus laticeps", "Lutjanus novemfasciatus", 
                "Hoplopagrus guentherii", "Seriola rivoliana", "Caranx lugubris", 
                "Dermatolepis dermatolepis"))

plot1 <- Biomass_sp %>% 
  #Complete sites for all species and methods
  complete(Site, nesting(ValidName, Method)) %>% 
  #Selecting species of interest using spInt
  right_join(spInt, by = "ValidName") %>% 
  #Replacing NAs in biomass with zeroes
  mutate(Biomass_site_sp = replace_na(Biomass_site_sp, 0)) %>%
  #Removing fishing status
  select(-Fishing) %>%
  #Joining fishing status to get correct info
  left_join(SiteInfo %>% select(Site, Fishing) %>% unique(), by = "Site")

#Density 
plot2 <- Density_sp %>% 
  #Complete sites for all species and methods
  complete(Site, nesting(ValidName, Method)) %>% 
  #Selecting species of interest using spInt
  right_join(spInt, by = "ValidName") %>% 
  #Replacing NAs in biomass with zeroes
  mutate(N_site_sp = replace_na(N_site_sp, 0)) %>%
  #Removing fishing status
  select(-Fishing) %>%
  #Joining fishing status to get correct info
  left_join(SiteInfo %>% select(Site, Fishing) %>% unique(), by = "Site")

#Creating empty lists to save figures
fig_list <- list()

#Creating empty lists to save PERMANOVAS
perm_bio_list <- list()
perm_den_list <- list()

#Creating combined plots for density and biomass per species
for(i in seq_along(spInt$ValidName)){
  bio <- plot1 %>% filter(ValidName == spInt$ValidName[i]) 
  Pbio <- adonis(Biomass_site_sp^0.25 ~ Method*Fishing, data = bio, permutations = 9999, method = "euclidean")
  #To access p value for PERMANOVA comparison for fishing status use: Pbio$aov.tab$`Pr(>F)`[2]
  f1 <- bio %>%
    #Create a plot
    #log10+1 applied to biomass so it can be seen better
    ggplot(aes(x = Fishing, y = log10(Biomass_site_sp+1), fill = Method))+
    geom_boxplot(width = 0.8) + #outlier.size changes the size of the dots representing outliers
    geom_signif(annotations = Pbio$aov.tab$`Pr(>F)`[2], 
                y_position = max(log10(bio$Biomass_site_sp+1))*1.1, 
                xmin = "Closed", xmax = "Open", textsize = 4, 
                vjust = -0.2) +
    scale_x_discrete(expand = c(0.5, 0)) +
    #Change the y axis label
    labs(y = "Biomass grams/500"~m^2, title = spInt$ValidName[i]) +
    theme_classic() +
    theme(axis.text.x = element_text(color = "black", size = 11),
          axis.text.y = element_text(color = "black", size = 11),
          axis.title.x = element_blank(),
          legend.position = "hide",
          plot.title = element_text(color = "black", face = "italic", size = 14, vjust = 1.5)) +
    coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
    scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot
  
  
  #Density
  den <- plot2 %>% filter(ValidName == spInt$ValidName[i])
  Pden <- adonis(N_site_sp ~ Method*Fishing, data = den, permutations = 9999, method = "euclidean")
  f2 <- den %>%
    #Create a plot
    ggplot(aes(x = Fishing, y = N_site_sp, fill = Method))+
    geom_boxplot(width = 0.8)+ #outlier.size changes the size of the dots representing outliers
    geom_signif(annotations = Pden$aov.tab$`Pr(>F)`[2], 
                y_position = max(den$N_site_sp)*1.1, 
                xmin = "Closed", xmax = "Open", textsize = 4, 
                vjust = -0.2) +
    scale_x_discrete(expand = c(0.5, 0))+
    #Change the y axis label
    labs(y = "Density individuals/500"~m^2)+
    theme_classic() +
    theme(axis.text.x = element_text(color = "black", size = 11),
          axis.text.y = element_text(color = "black", size = 11),
          axis.title.x = element_blank(),
          legend.position = "hide") +
    coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
    scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot
  
  #Combining both plots (biomass and density)
  fcomb <- ggarrange(f1, f2, nrow = 2, align = "hv")+
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"),
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank())
  
  #Saving combined plot in empty list created at the top
  fig_list[[i]] <- fcomb
  
  #Saving biomass PERMANOVAS
  perm_bio_list[[i]] <- Pbio$aov.tab
  #Saving density PERMANOVAS
  perm_den_list[[i]] <- Pden$aov.tab
}

#Creating composite image using the list of plots saved in the loop above
compfig <- ggarrange(plotlist = fig_list, ncol = 4, nrow = 2, 
                     labels = c(" I)", "J)", "K)", "L)", "M)", "N)", "O)"),
                     font.label = list(size = 14),
                     common.legend = T, hjust = -1.2,  vjust = 1.8)

compfig

#Saving composite image as tiff file and changing dimensions
ggsave("Figures/compfig_part2.tiff", compfig, device = "tiff", dpi = 300, width = 15, height = 10)


# PERMANOVAS for figure part 2 --------------------------------------------

#Making results from PERMANOVA biomass part 2 ready for excel
results <- perm_bio_list
#Names of the species in the PERMANOVA
perm_bio_cols <- c("Caranx melampygus", "Aetobatus laticeps", "Lutjanus novemfasciatus", 
                   "Hoplopagrus guentherii", "Seriola rivoliana", "Caranx lugubris", 
                   "Dermatolepis dermatolepis")
#Adding names from species to the PERMANOVA results
names(results) <- perm_bio_cols
#Format ready for excel
results <- do.call(rbind, results)
#writing 
write.xlsx(results, "Figures/PERMANOVA_bio_2.xlsx")


#Making results from PERMANOVA biomass part 2 ready for excel
results <- perm_den_list
#Names of the species in the PERMANOVA
perm_den_cols <- c("Caranx melampygus", "Aetobatus laticeps", "Lutjanus novemfasciatus", 
                   "Hoplopagrus guentherii", "Seriola rivoliana", "Caranx lugubris", 
                   "Dermatolepis dermatolepis")
#Adding names from species to the PERMANOVA results
names(results) <- perm_den_cols
#Format ready for excel
results <- do.call(rbind, results)
#writing 
write.xlsx(results, "Figures/PERMANOVA_den_2.xlsx")


#Remove variables from Figures
rm(spInt, plot1, plot2, fig_list, perm_bio_list, perm_den_list, 
   i, bio, Pbio, f1, den, Pden, f2, fcomb, compfig)
#Remove variables from PERMANOVAS - from figures
rm(results, perm_bio_cols, perm_den_cols)

