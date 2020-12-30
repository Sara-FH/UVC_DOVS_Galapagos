##########################################################
#
# Figure biomass and density for key species
#
#########################################################

#Question for Denisse: 
#I have pretty much just copied the code for every species and then changed the name of each plot. 
#However, is there a better way to do it? e.g. make a function that runs the code and then all I need
#to do is add the name?


# Loading libraries -------------------------------------------------------

library(ggpubr) #ggarrange

# Sphyrna lewini calculations ----------------------------------------------------------
#Making variable for Sphyrna lewini biomass
Sphyrna_lewini <- Biomass_sp %>% 
  filter(ValidName == "Sphyrna lewini")

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sphyrna_lewini %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for S. lewini
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = "Sphyrna lewini") #Adding column with species name

#Binding empty sites to biomass data
Sphyrna_lewini <- Sphyrna_lewini %>% 
  rbind(Empty)

#Making temporary variable for Sphyrna lewini density
temp <- Density_sp %>% 
  filter(ValidName == "Sphyrna lewini")

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the sphyrna lewini density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for S. lewini
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = "Sphyrna lewini") #Adding column with species name

#Combine density data for Sphyrna lewini
temp <- temp %>% 
  rbind(Empty)

#Binding to empty sites to biomass data
Sphyrna_lewini <- Sphyrna_lewini %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

# Sphyrna lewini (a) ------------------------------------------------------

#plot biomass Sphyrna lewini
a1 <- ggplot(Sphyrna_lewini, aes(x = Fishing, y = Biomass_site_sp, fill = Method, size = 3)) +
  geom_boxplot(width = 0.8) +  
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 4) +
  scale_x_discrete(expand = c(0.5, 0)) +
  #scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "A) Sphyrna lewini") +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        #legend.key.size = unit(0.2, "cm"),
        #legend.title = element_text(size = 8),
        #legend.text = element_text(size = 6),
        #axis.title.y = element_text(size = 8),
        #axis.title.x = element_blank(),
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

a1


#plot density Sphyrna lewini
a2 <- ggplot(Sphyrna_lewini, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 4) +
  scale_x_discrete(expand = c(0.5, 0)) +
  #scale_y_continuous(name = expression(atop("Density", "Individuals/500 "*m^2))) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        #legend.key.size = unit(0.2, "cm"),
        #legend.title = element_text(size = 8),
        #legend.text = element_text(size = 6),
        #axis.title.y = element_text(size = 8),
        #axis.title.x = element_blank()
        axis.title = element_blank(),
        legend.position = "hide") +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

a2

#With ggarrange
a <- ggarrange(a1, a2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

a

#Removing unnecessary variables
#rm(a1, a2, temp, Empty)


# Pairwise adonis ---------------------------------------------------------

Bio_compare <- as.data.frame(Bio_mat) %>% 
  rownames_to_column(., var = "Site") %>% 
  mutate(Site = str_remove_all(Site, " DOVS| UVC")) %>% 
  left_join(Sphyrna_lewini %>% select(Site, Comparison, Method, Fishing), by = "Site")

##start copy here for function pairwise.adonis()
pairwise.adonis <- function(x,factors, sim.function = 'vegdist', sim.method = 'bray', p.adjust.m ='bonferroni')
{
  library(vegan)
  
  co = combn(unique(as.character(factors)),2)
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  
  
  for(elem in 1:ncol(co)){
    if(sim.function == 'daisy'){
      library(cluster); x1 = daisy(x[factors %in% c(co[1,elem],co[2,elem]),],metric=sim.method)
    } else{x1 = vegdist(x[factors %in% c(co[1,elem],co[2,elem]),],method=sim.method)}
    
    ad = adonis(x1 ~ factors[factors %in% c(co[1,elem],co[2,elem])] );
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  sig = c(rep('',length(p.adjusted)))
  sig[p.adjusted <= 0.05] <-'.'
  sig[p.adjusted <= 0.01] <-'*'
  sig[p.adjusted <= 0.001] <-'**'
  sig[p.adjusted <= 0.0001] <-'***'
  
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted,sig)
  print("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
  return(pairw.res)
  
} 

## end copy here


#Here I am trying to do the comparison as a permanova instead of a wilcoxon (which is currently in plots)
pairwise.adonis(Bio_compare[,2:20], Bio_compare$Comparison)
#It is not working. I think it is because of the many zeroes. 

#Remove pairwise adonis variables
rm(Bio_compare)


# Lutjanus argentiventris calculations ------------------------------------
#Variable for species name
Sp_name <- "Lutjanus argentiventris"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

# Lutjanus argentiventris (b) ------------------------------------------------------

#plot biomass Lutjanus argentiventris
b1 <- ggplot(Sp_dataframe, aes(x = Fishing, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  labs(title = "B) Lutjanus argentiventris") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

b1


#plot density 
b2 <- ggplot(Sp_dataframe, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(), 
        legend.position = "hide") +
  coord_cartesian(clip = "off") +
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

b2

#With ggarrange
b <- ggarrange(b1, b2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

b


#Removing unnecessary variables
#rm(b1, b2, temp, Empty)


# Mycteroperca olfax calculations -----------------------------------------
#Variable for species name
Sp_name <- "Mycteroperca olfax"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods


# Mycteroperca olfax (c) ------------------------------------------------------

#plot biomass Mycteroperca olfax
c1 <- ggplot(Sp_dataframe, aes(x = Fishing, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  labs(title = "C) Mycteroperca olfax") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

c1


#plot density 
c2 <- ggplot(Sp_dataframe, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide") +
  coord_cartesian(clip = "off") +
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

c2

#With ggarrange
c <- ggarrange(c1, c2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

c


#Removing unnecessary variables
#rm(c1, c2, temp, Empty)


# Triaenodon obesus calculations -------------------------------------------------------
#Variable for species name
Sp_name <- "Triaenodon obesus"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

# Triaenodon obesus (d) ------------------------------------------------------

#plot biomass Triaenodon obesus
d1 <- ggplot(Sp_dataframe, aes(x = Fishing, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  labs(title = "D) Triaenodon obesus") +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

d1


#plot density 
d2 <- ggplot(Sp_dataframe, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide") +
  coord_cartesian(clip = "off") +
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

d2


#With ggarrange
d <- ggarrange(d1, d2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

d

#Removing unnecessary variables
#rm(d1, d2, temp, Empty)


# Paralabrax albomaculatus calculations -------------------------------------------------------
#Variable for species name
Sp_name <- "Paralabrax albomaculatus"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

# Paralabrax albomaculatus (e) ------------------------------------------------------

#plot biomass Paralabrax albomaculatus
e1 <- ggplot(Sp_dataframe, aes(x = Fishing, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  labs(title = "E) Paralabrax albomaculatus") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

e1


#plot density 
e2 <- ggplot(Sp_dataframe, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide") +
  coord_cartesian(clip = "off") +
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

e2


#With ggarrange
e <- ggarrange(e1, e2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

e


#Removing unnecessary variables
#rm(e1, e2, temp, Empty)


# Hypanus dipterurus calculations -------------------------------------------------------
#Variable for species name
Sp_name <- "Hypanus dipterurus"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

# Hypanus dipterurus (f) ------------------------------------------------------

#plot biomass Hypanus dipterurus
f1 <- ggplot(Sp_dataframe, aes(x = Fishing, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  labs(title = "F) Hypanus dipterurus") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

f1


#plot density 
f2 <- ggplot(Sp_dataframe, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide") +
  coord_cartesian(clip = "off") +
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

f2


#With ggarrange
f <- ggarrange(f1, f2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

f

#Removing unnecessary variables
#rm(f1, f2, temp, Empty)


# Carcharhinus galapagensis calculations -------------------------------------------------------
#Variable for species name
Sp_name <- "Carcharhinus galapagensis"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

# Carcharhinus galapagensis (g) ------------------------------------------------------

#plot biomass Carcharhinus galapagensis
g1 <- ggplot(Sp_dataframe, aes(x = Fishing, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 4) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  labs(title = "G) Carcharhinus galapagensis") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

g1


#plot density 
g2 <- ggplot(Sp_dataframe, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 4) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide") +
  coord_cartesian(clip = "off") +
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

g2


#With ggarrange
g <- ggarrange(g1, g2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

g

#Removing unnecessary variables
#rm(g1, g2, temp, Empty)


# Carcharhinus limbatus calculations -------------------------------------------------------
#Variable for species name
Sp_name <- "Carcharhinus limbatus"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

# Carcharhinus limbatus (h) ------------------------------------------------------

#plot biomass Carcharhinus limbatus
h1 <- ggplot(Sp_dataframe, aes(x = Fishing, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 4) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  labs(title = "H) Carcharhinus limbatus") +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

h1


#plot density 
h2 <- ggplot(Sp_dataframe, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 4) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide") +
  coord_cartesian(clip = "off") +
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

h2


#With ggarrange
h <- ggarrange(h1, h2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

h

#Removing unnecessary variables
#rm(h1, h2, temp, Empty)


# Caranx melampygus calculations -------------------------------------------------------
#Variable for species name
Sp_name <- "Caranx melampygus"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

# Caranx melampygus (i) ------------------------------------------------------

#plot biomass Caranx melampygus
i1 <- ggplot(Sp_dataframe, aes(x = Fishing, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 4) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  labs(title = "I) Caranx melampygus") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

i1


#plot density 
i2 <- ggplot(Sp_dataframe, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 4) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide") +
  coord_cartesian(clip = "off") +
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

i2


#With ggarrange
i <- ggarrange(i1, i2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

i

#Removing unnecessary variables
#rm(i1, i2, temp, Empty)


# Aetobatus laticeps calculations -------------------------------------------------------
#Variable for species name
Sp_name <- "Aetobatus laticeps"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

# Aetobatus laticeps (j) ------------------------------------------------------

#plot biomass Aetobatus laticeps
j1 <- ggplot(Sp_dataframe, aes(x = Fishing, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  labs(title = "J) Aetobatus laticeps") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

j1


#plot density 
j2 <- ggplot(Sp_dataframe, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide") +
  coord_cartesian(clip = "off") +
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

j2


#With ggarrange
j <- ggarrange(j1, j2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

j

#Removing unnecessary variables
#rm(j1, j2, temp, Empty)


# Lutjanus novemfasciatus calculations -------------------------------------------------------
#Variable for species name
Sp_name <- "Lutjanus novemfasciatus"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

# Lutjanus novemfasciatus (k) ------------------------------------------------------

#plot biomass Lutjanus novemfasciatus
k1 <- ggplot(Sp_dataframe, aes(x = Fishing, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  labs(title = "K) Lutjanus novemfasciatus") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

k1


#plot density 
k2 <- ggplot(Sp_dataframe, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide") +
  coord_cartesian(clip = "off") +
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

k2


#With ggarrange
k <- ggarrange(k1, k2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

k

#Removing unnecessary variables
#rm(k1, k2, temp, Empty)


# Hoplopagrus guentherii calculations -------------------------------------------------------
#Variable for species name
Sp_name <- "Hoplopagrus guentherii"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

# Hoplopagrus guentherii (l) ------------------------------------------------------

#plot biomass Hoplopagrus guentherii
l1 <- ggplot(Sp_dataframe, aes(x = Fishing, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  labs(title = "L) Hoplopagrus guentherii") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

l1


#plot density 
l2 <- ggplot(Sp_dataframe, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(), 
        legend.position = "hide") +
  coord_cartesian(clip = "off") +
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

l2


#With ggarrange
l <- ggarrange(l1, l2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

l

#Removing unnecessary variables
#rm(l1, l2, temp, Empty)


# Seriola rivoliana calculations -------------------------------------------------------
#Variable for species name
Sp_name <- "Seriola rivoliana"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

# Seriola rivoliana (m) ------------------------------------------------------

#plot biomass Seriola rivoliana
m1 <- ggplot(Sp_dataframe, aes(x = Fishing, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  labs(title = "M) Seriola rivoliana") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

m1


#plot density 
m2 <- ggplot(Sp_dataframe, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide") +
  coord_cartesian(clip = "off") +
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

m2


#With ggarrange
m <- ggarrange(m1, m2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

m

#Removing unnecessary variables
#rm(m1, m2, temp, Empty)


# Caranx lugubris calculations -------------------------------------------------------
#Variable for species name
Sp_name <- "Caranx lugubris"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

# Caranx lugubris (n) ------------------------------------------------------

#plot biomass Caranx lugubris
n1 <- ggplot(Sp_dataframe, aes(x = Fishing, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 4) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  labs(title = "N) Caranx lugubris") +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

n1


#plot density 
n2 <- ggplot(Sp_dataframe, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 4) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide") +
  coord_cartesian(clip = "off") +
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

n2

#With ggarrange
n <- ggarrange(n1, n2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

n

#Removing unnecessary variables
#rm(n1, n2, temp, Empty)


# Dermatolepis dermatolepis calculations -------------------------------------------------------
#Variable for species name
Sp_name <- "Dermatolepis dermatolepis"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

# Dermatolepis dermatolepis (o) ------------------------------------------------------

#plot biomass Lutjanus argentiventris
o1 <- ggplot(Sp_dataframe, aes(x = Fishing, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  labs(title = "O) Dermatolepis dermatolepis") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide",
        plot.title = element_text(color = "black", face = "bold", size = 9)) +
  coord_cartesian(clip = "off") + #Keeps the top of plot with significance level
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

o1


#plot density 
o2 <- ggplot(Sp_dataframe, aes(x = Fishing, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE), 
              textsize = 2.5) +
  scale_x_discrete(expand = c(0.4, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "hide") +
  coord_cartesian(clip = "off") +
  scale_fill_grey(start = 0.1, end = 0.7) #color of boxplot

o2


#With ggarrange
o <- ggarrange(o1, o2, ncol = 1, nrow = 2, align = "v") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, linetype = "solid", color = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

o

#Removing unnecessary variables
#rm(o1, o2, temp, Empty)


# Simple combined plot -------------------------------------------------------------

#Combining all plots using ggarrange
combined <- ggarrange(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
                       ncol = 3, nrow = 5)

combined

#Save figure as pdf
ggsave(file = "combined.pdf", combined,
       width = 210, height = 297, units = "mm", dpi = 300,
       scale = 1, limitsize = TRUE)

#Remove unnecessary variables
rm(a1, a2, b1, b2, c1, c2, d1, d2, e1, e2, f1, f2, g1, g2, h1, h2, i1, i2, j1, j2, k1, k2,
   l1, l2, m1, m2, n1, n2, o1, o2, temp, Empty, Sp_dataframe, Sp_name)
#Remove variables from figures
rm(combined, a, b, c, d, f, e, g, h, i, j, k, l, m, n, o)

# -------------------------------------- Notes -------------------------------------------------------
# Combination using ggarrange and annotate_figure -------------------------
#This combination does nok look good when I combine all the plots in the end
#They look good, when I look at one row e.g. abc and with the y-axes in abc2
#This was made before the single plots got species names, but since I could not make it work
#I ended up trying something else. 

library(patchwork) #lets me combine plots made with different packages

#Defining the two y-axis titles for plots
y <- expression(atop(bold("Biomass"), "g/500 "*m^2))
y2 <- expression(atop(bold("Density"), "Individuals/500 "*m^2))   

#Combining plots for 3 species at a time (making one row)
#I realize this is not an ideal way to make a graph, it was just a try. 
abc <- ggarrange(a, b, c, 
                 ncol = 3, nrow = 1, heights = c(4, 4), 
                 labels = c("A", "B", "C"), 
                 hjust = -1)
abc2 <- annotate_figure(abc, left = text_grob(y, rot = 90, hjust = -2.45),  #left top is biomass
                        #left bottom is density
                        right = text_grob(y2, rot = 90, hjust = 1.97, vjust = -25.8)) 
abc2

def <- ggarrange(d, e, f, 
                 ncol = 3, nrow = 1, heights = c(4, 4), 
                 labels = c("D", "E", "F"), 
                 hjust = -1)
def2 <- annotate_figure(def, left = text_grob(y, rot = 90, hjust = -2.45), 
                        right = text_grob(y2, rot = 90, hjust = 1.97, vjust = -25.8))

ghi <- ggarrange(g, h, i, 
                 ncol = 3, nrow = 1, heights = c(4, 4), 
                 labels = c("G", "H", "I"), 
                 hjust = -1)
ghi2 <- annotate_figure(ghi, left = text_grob(y, rot = 90, hjust = -2.45), 
                        right = text_grob(y2, rot = 90, hjust = 1.97, vjust = -25.8))

jkl <- ggarrange(j, k, l, 
                 ncol = 3, nrow = 1, heights = c(4, 4), 
                 labels = c("J", "K", "L"), 
                 hjust = -1)
jkl2 <- annotate_figure(jkl, left = text_grob(y, rot = 90, hjust = -2.45), 
                        right = text_grob(y2, rot = 90, hjust = 1.97, vjust = -25.8))

mno <- ggarrange(m, n, o, 
                 ncol = 3, nrow = 1, heights = c(4, 4), 
                 labels = c("M", "N", "O"), 
                 hjust = -1)
mno2 <- annotate_figure(mno, left = text_grob(y, rot = 90, hjust = -2.45), 
                        right = text_grob(y2, rot = 90, hjust = 1.97, vjust = -25.8))

#combining all the rows into one plot
#Here the labels for the y-axes are all over the plot
combined <- ggarrange(abc2, def2, ghi2, jkl2, mno2, 
                      ncol = 1, nrow = 5)

combined


#ggsave(file = "combined.pdf", combined,
#       width = 210, height = 297, units = "mm", dpi = 300,
#       scale = 1, limitsize = TRUE)

#Remove unnecessary variables
rm(y, y2, abc, abc2, def, def2, ghi, ghi2, jkl, jkl2, mno, mno2, combined)

# -------------------------- Checking significance between methods -----------------------------------
# Sphyrna lewini calculations and methods ---------------------------------
#Making variable for Sphyrna lewini biomass
Sphyrna_lewini <- Biomass_sp %>% 
  filter(ValidName == "Sphyrna lewini")

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sphyrna_lewini %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for S. lewini
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = "Sphyrna lewini") #Adding column with species name

#Binding empty sites to biomass data
Sphyrna_lewini <- Sphyrna_lewini %>% 
  rbind(Empty)

#Making temporary variable for Sphyrna lewini density
temp <- Density_sp %>% 
  filter(ValidName == "Sphyrna lewini")

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the sphyrna lewini density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for S. lewini
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = "Sphyrna lewini") #Adding column with species name

#Combine density data for Sphyrna lewini
temp <- temp %>% 
  rbind(Empty)

#Binding to empty sites to biomass data
Sphyrna_lewini <- Sphyrna_lewini %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods


#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sphyrna_lewini, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "A) Sphyrna lewini") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sphyrna_lewini, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "A) Sphyrna lewini") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

# *** Lutjanus argentiventris calculations and methods ------------------------
#Variable for species name
Sp_name <- "Lutjanus argentiventris"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "B) Lutjanus argentiventris") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Significant between closed areas for UVC and DOVS.

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "B) Lutjanus argentiventris") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

# Mycteroperca olfax calculations and methods------------------------------
#Variable for species name
Sp_name <- "Mycteroperca olfax"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "C) Mycteroperca olfax") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "C) Mycteroperca olfax") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

# Triaenodon obesus calculations and methods ------------------------------

#Variable for species name
Sp_name <- "Triaenodon obesus"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "D) Triaenodon obesus") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "D) Triaenodon obesus") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

# *** Paralabrax albomaculatus calculations and methods -----------------------
#Variable for species name
Sp_name <- "Paralabrax albomaculatus"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "E) Paralabrax albomaculatus") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Significant difference between Open areas for DOVS and UVC

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "E) Paralabrax albomaculatus") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Significant difference between Open areas for DOVS and UVC

# Hypanus dipterurus calculations and methods -----------------------------
#Variable for species name
Sp_name <- "Hypanus dipterurus"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "F) Hypanus dipteruru") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "F) Hypanus dipteruru") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

# Carcharhinus galapagensis calculations and methods ----------------------
#Variable for species name
Sp_name <- "Carcharhinus galapagensis"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "G) Carcharhinus galapagensis") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "G) Carcharhinus galapagensis") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

# Carcharhinus limbatus calculations and methods --------------------------
#Variable for species name
Sp_name <- "Carcharhinus limbatus"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "H) Carcharhinus limbatus") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "H) Carcharhinus limbatus") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

# Caranx melampygus calculations and methods ------------------------------
#Variable for species name
Sp_name <- "Caranx melampygus"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "I) Caranx melampygus") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "I) Caranx melampygus") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

# Aetobatus laticeps calculations and methods -----------------------------
#Variable for species name
Sp_name <- "Aetobatus laticeps"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "J) Aetobatus laticeps") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "J) Aetobatus laticeps") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

# Lutjanus novemfasciatus calculations and methods ------------------------
#Variable for species name
Sp_name <- "Lutjanus novemfasciatus"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "K) Lutjanus novemfasciatus") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "K) Lutjanus novemfasciatus") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

# Hoplopagrus guentherii calculations and methods -------------------------
#Variable for species name
Sp_name <- "Hoplopagrus guentherii"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "L) Hoplopagrus guentherii") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "L) Hoplopagrus guentherii") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

# Seriola rivoliana calculations and methods --------------------------
#Variable for species name
Sp_name <- "Seriola rivoliana"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "M) Seriola rivoliana") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "M) Seriola rivoliana") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

# Caranx lugubris calculations and methods --------------------------------
#Variable for species name
Sp_name <- "Caranx lugubris"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "N) Caranx lugubris") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "N) Caranx lugubris") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

# Dermatolepis dermatolepis calculations and methods ----------------------
#Variable for species name
Sp_name <- "Dermatolepis dermatolepis"

#Making variable for species biomass
Sp_dataframe <- Biomass_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
Empty <- Biomass_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(Sp_dataframe %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for the species
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(Biomass_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Binding empty sites to biomass data
Sp_dataframe <- Sp_dataframe %>% 
  rbind(Empty)

#Making temporary variable for density
temp <- Density_sp %>% 
  filter(ValidName == Sp_name)

#Making data frame for empty periods (no species = no biomass) in DOVS data
#Remember that temp currently is the density
Empty <- Density_sp %>% 
  select(Site, Method, Fishing, SiteCode) %>% 
  mutate(All_site_method = paste(Site, Method, sep = " ")) %>% 
  distinct() %>% 
  left_join(temp %>% 
              select(Site, Method) %>% 
              mutate(Site_method = paste(Site, Method, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Method")) %>% 
  #Filter the sites with methods that are empty for Lutjanus argentiventris
  filter(!All_site_method %in% Site_method) %>% 
  select(-c(All_site_method, Site_method)) %>% #Removing the two columns that are no longer needed
  mutate(N_site_sp = 0) %>% #Adding empty biomass column
  mutate(ValidName = Sp_name) #Adding column with species name

#Combine density data and empty sites
temp <- temp %>% 
  rbind(Empty)

#Binding density data to biomass data in species dataframe
Sp_dataframe <- Sp_dataframe %>% 
  left_join(temp %>% select(Site, Method, N_site_sp), by = c("Site", "Method")) %>% 
  mutate(Comparison = paste(Fishing, Method)) #Adding column for statistical comparison between methods

#Biomass - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = Biomass_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Biomass", "g/500 "*m^2))) +
  theme_classic() +
  labs(title = "O) Dermatolepis dermatolepis") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant

#Density - Checking significance between methods DOVS and UVC (for closed and open)
ggplot(Sp_dataframe, aes(x = Comparison, y = N_site_sp, fill = Method)) +
  geom_boxplot() + 
  geom_signif(test = "wilcox.test", 
              comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)) +
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = expression(atop("Density", "Individuals/500  "*m^2))) +
  theme_classic() +
  labs(title = "O) Dermatolepis dermatolepis") +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"))
#Not significant
