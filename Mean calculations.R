##########################################################
#
# Mean calculations
#
##########################################################


# General counts ----------------------------------------------------------
#Sites that are open and closed to fishing
SiteInfo %>% select(Site, Fishing) %>%  unique() %>% filter(Fishing == "Closed")
SiteInfo %>% select(Site, Fishing) %>%  unique() %>% filter(Fishing == "Open")

SiteInfo %>% select(Site, Year, Month) %>% unique()
#November 2013 to April 2015 
#but in the paper by Salinas de Léon about largest shark biomass says: November 2013 and August 2014

#Total abundance
DOVS %>% ungroup() %>% summarise(sum(N))
UVC %>% ungroup() %>% summarise(sum(N))

#Total species richness
DOVS %>% ungroup() %>%  select(ValidName, Method) %>% unique() %>% nrow()
UVC %>% ungroup() %>%  select(ValidName, Method) %>% unique() %>% nrow()

#Different species in UVC
UVC %>% ungroup() %>%  select(ValidName, Method) %>% unique() %>% arrange() %>% 
  left_join(DOVS %>% ungroup() %>%  select(ValidName, Method) %>% unique() %>% arrange(), by = "ValidName") %>% 
  mutate(Method.y = replace_na(Method.y, 0)) %>% 
  filter(!Method.y == "DOVS")

#Total number of families
DOVS %>% ungroup() %>% select(Family) %>%  unique() %>% arrange() %>% nrow()
#Dasyatidae is spelled wrong in DOVS
#Mobulidae should be Myliobatidae.
#Therefore DOVS should be 10
UVC %>% ungroup() %>% select(Family) %>%  unique() %>% arrange() %>% nrow()

UVC %>% ungroup() %>% select(Family, Method) %>% unique() %>% arrange() %>% 
  left_join(DOVS %>% ungroup() %>%  select(Family, Method) %>% unique() %>% arrange(), by = "Family") %>% 
  mutate(Method.y = replace_na(Method.y, 0))


# Mean biomass, richness and density for fishing --------------------------

#Mean biomass - fishing
mean_biomass <- Biomass %>% 
  group_by(Fishing) %>% 
  summarise(Mean_bio = mean(Kg_500m2_site), Fishing, Kg_500m2_site) %>% 
  #Standard error of the mean
  mutate(SE = sd(Kg_500m2_site)/sqrt(length(Kg_500m2_site)))


#mean species richness - fishing
mean_richness <- Richness %>% 
  group_by(Fishing) %>% 
  summarise(Mean_rich = mean(Site_sp_500m2), Fishing, Site_sp_500m2) %>% 
  #standard error of the mean
  mutate(SE = sd(Site_sp_500m2)/sqrt(length(Site_sp_500m2)))


#mean density - fishing
mean_density <- Density %>% 
  group_by(Fishing) %>% 
  summarise(Mean_den = mean(N_site_500m2), Fishing, N_site_500m2) %>% 
  #SE
  mutate(SE = sd(N_site_500m2)/sqrt(length(N_site_500m2)))


rm(mean_biomass, mean_richness, mean_density)

# Mean biomass, richness and density for bioregion and interaction --------

#mean species richness
mean_richness1 <- Richness %>% 
  group_by(Bioregion) %>% 
  summarise(Mean_rich = mean(Site_sp_500m2), Bioregion, Site_sp_500m2) %>% 
  #standard error of the mean
  mutate(SE = sd(Site_sp_500m2)/sqrt(length(Site_sp_500m2))) %>% 
  select(-Site_sp_500m2) %>% 
  unique()

#mean density - bioregion
mean_density1 <- Density %>% 
  left_join(SiteInfo %>% select(Site, Bioregion) %>% unique(), by = "Site") %>% 
  group_by(Bioregion) %>% 
  summarise(Mean_den = mean(N_site_500m2), Bioregion, N_site_500m2) %>% 
  #SE
  mutate(SE = sd(N_site_500m2)/sqrt(length(N_site_500m2))) %>% 
  select(-N_site_500m2) %>% 
  unique()

#Mean biomass - bioregion - interaction with fishing
mean_biomass1 <- Biomass %>% 
  left_join(SiteInfo %>% select(Site, Bioregion) %>% unique(), by = "Site") %>% 
  group_by(Bioregion) %>% 
  summarise(Mean_bio = mean(Kg_500m2_site), Bioregion, Kg_500m2_site) %>% 
  #Standard error of the mean
  mutate(SE = sd(Kg_500m2_site)/sqrt(length(Kg_500m2_site))) %>% 
  select(-Kg_500m2_site) %>% 
  unique()


### Interaction

#mean species richness - interaction with fishing
mean_richness2 <- Richness %>% 
  group_by(Bioregion, Fishing) %>% 
  summarise(Mean_rich = mean(Site_sp_500m2), Bioregion, Fishing, Site_sp_500m2) %>% 
  #standard error of the mean
  mutate(SE = sd(Site_sp_500m2)/sqrt(length(Site_sp_500m2))) %>% 
  select(-Site_sp_500m2) %>% 
  unique()

#mean density - bioregion - interaction with fishing
mean_density2 <- Density %>% 
  left_join(SiteInfo %>% select(Site, Bioregion) %>% unique(), by = "Site") %>% 
  group_by(Bioregion, Fishing) %>% 
  summarise(Mean_den = mean(N_site_500m2), Bioregion, Fishing, N_site_500m2) %>% 
  #SE
  mutate(SE = sd(N_site_500m2)/sqrt(length(N_site_500m2))) %>% 
  select(-N_site_500m2) %>% 
  unique()

#Mean biomass - bioregion - interaction with fishing
Mean_biomass2 <- Biomass %>% 
  left_join(SiteInfo %>% select(Site, Bioregion) %>% unique(), by = "Site") %>% 
  group_by(Bioregion, Fishing) %>% 
  summarise(Mean_bio = mean(Kg_500m2_site), Bioregion, Fishing, Kg_500m2_site) %>% 
  #Standard error of the mean
  mutate(SE = sd(Kg_500m2_site)/sqrt(length(Kg_500m2_site))) %>% 
  select(-Kg_500m2_site) %>% 
  unique()

rm(mean_richness1, mean_richness2, mean_density1, mean_density2, 
   mean_biomass1, Mean_biomass2)


# Species on arrows in PCO plots ------------------------------------------

#PCO for density, mean density for species on arrows
sp_den_PCO <- Density_sp %>% 
  filter(ValidName == "Lutjanus argentiventris" | 
         ValidName == "Triaenodon obesus" |
         ValidName == "Mycteroperca olfax" |
         ValidName == "Hypanus dipterurus" |
         ValidName == "Carcharhinus galapagensis" |
         ValidName == "Sphyrna lewini") %>% 
  group_by(ValidName) %>% 
  summarise(Mean_den = mean(N_site_sp), ValidName, N_site_sp) %>% 
  #SE
  mutate(SE = sd(N_site_sp)/sqrt(length(N_site_sp))) %>% 
  select(-N_site_sp) %>% 
  unique()



#PCO for biomass, mean density for species on arrows
sp_bio_PCO <- Biomass_sp %>% 
  filter(ValidName == "Lutjanus argentiventris" | 
           ValidName == "Triaenodon obesus" |
           ValidName == "Mycteroperca olfax" |
           ValidName == "Carcharhinus limbatus" |
           ValidName == "Caranx melampygus" |
           ValidName == "Carcharhinus galapagensis" |
           ValidName == "Sphyrna lewini" |
           ValidName == "Lutjanus novemfasciatus" ) %>% 
  group_by(ValidName) %>% 
  summarise(Mean_den = mean(Kg_site_sp), ValidName, Kg_site_sp) %>% 
  #SE
  mutate(SE = sd(Kg_site_sp)/sqrt(length(Kg_site_sp))) %>% 
  select(-Kg_site_sp) %>% 
  unique()


rm(sp_den_PCO, sp_bio_PCO)
