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
#but in the paper by Salinas de L�on about largest shark biomass says: November 2013 and August 2014

#Total abundance
DOVS %>% ungroup() %>% summarise(sum(N))
UVC %>% ungroup() %>% summarise(sum(N))

#Total species richness
DOVS %>% ungroup() %>%  select(ValidName, Method) %>% unique() %>% nrow()
UVC %>% ungroup() %>%  select(ValidName, Method) %>% unique() %>% nrow()

#Species across all sites
Sp <- UVC %>% 
  ungroup() %>%  
  select(Family, ValidName, Method) %>% 
  unique() %>% 
  left_join(DOVS %>% ungroup() %>% select(ValidName, Method) %>% unique(), by = "ValidName")
rm(Sp)

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


# Species detected at sites -----------------------------------------------

#How many sites is each species detected at
Sp_sites <- UVC %>% 
  select(Site, ValidName, Method) %>% 
  #join DOVS data
  rbind(DOVS %>% select(Site, ValidName, Method)) %>% 
  unique() %>% 
  group_by(ValidName) %>% 
  #column with count of site for each species
  mutate(sp_sites = length(unique(Site))) %>% 
  ungroup()

#Removing variable
rm(Sp_sites)

# Mean biomass, richness and density for fishing --------------------------

#Mean biomass - fishing
mean_biomass <- Biomass %>% 
  group_by(Fishing) %>% 
  #Question DFA - is this the correct way to calculate the mean, since sites with zero individuals of 
  #a species is not counted
  summarise(Mean_bio = mean(Kg_500m2_site), Fishing, Kg_500m2_site) %>% 
  #Standard error of the mean
  mutate(SE = sd(Kg_500m2_site)/sqrt(length(Kg_500m2_site)))


#mean species richness - fishing
mean_richness <- Richness %>% 
  group_by(Fishing) %>% 
  #Question - check mean calculation
  summarise(Mean_rich = mean(Site_sp_500m2), Fishing, Site_sp_500m2) %>% 
  #standard error of the mean
  mutate(SE = sd(Site_sp_500m2)/sqrt(length(Site_sp_500m2)))


#mean density - fishing
mean_density <- Density %>% 
  group_by(Fishing) %>% 
  #Question - check mean calculation
  summarise(Mean_den = mean(N_site_500m2), Fishing, N_site_500m2) %>% 
  #SE
  mutate(SE = sd(N_site_500m2)/sqrt(length(N_site_500m2)))


rm(mean_biomass, mean_richness, mean_density)

# Mean biomass, richness and density for bioregion and interaction --------

#mean species richness
mean_richness1 <- Richness %>% 
  group_by(Bioregion) %>% 
  #Question - check mean calculation
  summarise(Mean_rich = mean(Site_sp_500m2), Bioregion, Site_sp_500m2) %>% 
  #standard error of the mean
  mutate(SE_R = sd(Site_sp_500m2)/sqrt(length(Site_sp_500m2))) %>% 
  select(-Site_sp_500m2) %>% 
  unique()

#mean density - bioregion
mean_density1 <- Density %>% 
  left_join(SiteInfo %>% select(Site, Bioregion) %>% unique(), by = "Site") %>% 
  group_by(Bioregion) %>% 
  #Question - check mean calculation
  summarise(Mean_den = mean(N_site_500m2), Bioregion, N_site_500m2) %>% 
  #SE
  mutate(SE_D = sd(N_site_500m2)/sqrt(length(N_site_500m2))) %>% 
  select(-N_site_500m2) %>% 
  unique()

#Mean biomass - bioregion - interaction with fishing
mean_biomass1 <- Biomass %>% 
  left_join(SiteInfo %>% select(Site, Bioregion) %>% unique(), by = "Site") %>% 
  group_by(Bioregion) %>% 
  #Question - check mean calculation
  summarise(Mean_bio = mean(Kg_500m2_site), Bioregion, Kg_500m2_site) %>% 
  #Standard error of the mean
  mutate(SE_B = sd(Kg_500m2_site)/sqrt(length(Kg_500m2_site))) %>% 
  select(-Kg_500m2_site) %>% 
  unique()

#Combine means for bioregions
means <- mean_richness1 %>% 
  left_join(mean_density1, by = "Bioregion") %>% 
  left_join(mean_biomass1, by = "Bioregion")


rm(mean_richness1, mean_density1, mean_biomass1)

### Interaction

#mean species richness - interaction with fishing
mean_richness2 <- Richness %>% 
  group_by(Bioregion, Fishing) %>% 
  #Question - check mean calculation
  summarise(Mean_rich = mean(Site_sp_500m2), Bioregion, Fishing, Site_sp_500m2) %>% 
  #standard error of the mean
  mutate(SE_R = sd(Site_sp_500m2)/sqrt(length(Site_sp_500m2))) %>% 
  select(-Site_sp_500m2) %>% 
  unique()

#mean density - bioregion - interaction with fishing
mean_density2 <- Density %>% 
  left_join(SiteInfo %>% select(Site, Bioregion) %>% unique(), by = "Site") %>% 
  group_by(Bioregion, Fishing) %>%
  #Question - check mean calculation
  summarise(Mean_den = mean(N_site_500m2), Bioregion, Fishing, N_site_500m2) %>% 
  #SE
  mutate(SE_D = sd(N_site_500m2)/sqrt(length(N_site_500m2))) %>% 
  select(-N_site_500m2) %>% 
  unique()

#Mean biomass - bioregion - interaction with fishing
mean_biomass2 <- Biomass %>% 
  left_join(SiteInfo %>% select(Site, Bioregion) %>% unique(), by = "Site") %>% 
  group_by(Bioregion, Fishing) %>% 
  #Question - check mean calculation
  summarise(Mean_bio = mean(Kg_500m2_site), Bioregion, Fishing, Kg_500m2_site) %>% 
  #Standard error of the mean
  mutate(SE_B = sd(Kg_500m2_site)/sqrt(length(Kg_500m2_site))) %>% 
  select(-Kg_500m2_site) %>% 
  unique()

#Combine means for bioregions
means_bioreg <- mean_richness2 %>% 
  left_join(mean_density2, by = c("Bioregion", "Fishing")) %>% 
  left_join(mean_biomass2, by = c("Bioregion", "Fishing"))


rm(mean_richness2, mean_density2, mean_biomass2)

rm(means, means_bioreg)


# Species on arrows in PCO plots ------------------------------------------

#Question - check new names after Pearson correlation corrections
#PCO for density, mean density for species on arrows
sp_den_PCO <- Density_sp %>% 
  filter(ValidName == "Lutjanus argentiventris" | 
         ValidName == "Triaenodon obesus" |
         ValidName == "Mycteroperca olfax" |
         ValidName == "Hypanus dipterurus" |
         ValidName == "Carcharhinus galapagensis" |
         ValidName == "Sphyrna lewini") %>% 
  group_by(ValidName) %>% 
  #Question - check mean calculation
  summarise(Mean_den = mean(N_site_sp), ValidName, N_site_sp) %>% 
  #SE
  mutate(SE = sd(N_site_sp)/sqrt(length(N_site_sp))) %>% 
  select(-N_site_sp) %>% 
  unique()


#Question - check new names after Pearson correlation corrections
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
  #Question - check mean calculation
  summarise(Mean_den = mean(Kg_site_sp), ValidName, Kg_site_sp) %>% 
  #SE
  mutate(SE = sd(Kg_site_sp)/sqrt(length(Kg_site_sp))) %>% 
  select(-Kg_site_sp) %>% 
  unique()


rm(sp_den_PCO, sp_bio_PCO)


# Mean for species biomass and density Figure B and D ---------------------

#Calculating biomass for DOVS
Bio_sp_D <- DOVS %>%
  mutate(LenLenRatio = as.numeric(LenLenRatio)) %>% 
  #The fish biomass equation is W = a*L^b, therefore first transform the length, 
  #then apply the exponent b and finally multiply by a.
  #The equation is therefore: a*((LenLenRatio*Length_cm)^b)
  mutate(Biomass = a*((LenLenRatio*Length_cm)^b)) %>% #Biomass for 1 individual of each species
  mutate(Biomass_N = Biomass*N) %>% #Biomass for all individuals of each species
  group_by(Site, Period, ValidName) %>% 
  #Summing biomass for each species in each period, so I have total biomass per species per period
  summarise(Biomass_sp_period = sum(Biomass_N), 
            Transect_length_m, Method, Fishing, SiteCode, Period) %>% 
  unique() %>% 
  mutate(Transect_area = Transect_length_m*5) %>% #Calculating transect area using transect width 5m
  group_by(Site, ValidName) %>% 
  #Calculating gram per 500m2
  #Question - change they way to calculate per 500m2 if we decide to do it for all calculations
  summarise(Gram_500m2 = (Biomass_sp_period/Transect_area)*500, 
            Method, Fishing, SiteCode, Biomass_sp_period, Transect_area, Period) %>% 
  #Calculating the biomass of each species per site in g per 500m2 (as average of periods)
  #Question - check mean calculations, is it correct not to use species with zero counts??
  summarise(Gram_site_sp = mean(Gram_500m2), 
            ValidName, Method, Fishing, SiteCode, Biomass_sp_period, Transect_area, Period) %>%
  #Going from g to kg for the biomass
  mutate(Kg_site_sp = Gram_site_sp/1000) %>% #Biomass in kg
  select(-Gram_site_sp) %>% #Removing the Gram_site_sp column
  #Making Biomass_sp_period into kg from g
  mutate(Biomass_sp_period = Biomass_sp_period/1000) %>% 
  #Calculating biomass per period per 500m2
  mutate(Kg_period_500m2 = (Biomass_sp_period/Transect_area)*500) %>% 
  group_by(Site, ValidName) %>% 
  #Calculating standard error
  mutate(SE_B = sd(Kg_period_500m2)/sqrt(length(Kg_period_500m2))) %>% 
  ungroup()

#Calculating biomass UVC
Bio_sp_U <- UVC %>% 
  #The fish biomass equation is W = a*L^b, therefore first transform the length, 
  #then apply the exponent b and finally multiply by a.
  #The equation is therefore: a*((LenLenRatio*Length_cm)^b)
  mutate(Biomass = a*((TLRatio*Length_cm)^b)) %>% #Biomass for 1 individual of each species
  mutate(Biomass_N = Biomass*N) %>% #Biomass for all individuals of each species
  group_by(Site, Period, ValidName) %>% 
  #Summing biomass for each species in each period, so I have total biomass per species per site
  summarise(Biomass_sp_period = sum(Biomass_N), 
            Transect_length_m, Method, Fishing, SiteCode, Period) %>% 
  unique() %>% 
  mutate(Transect_area = Transect_length_m*5) %>% #Calculating transect area using transect width 5m
  group_by(Site, ValidName) %>% 
  #Calculating gram per 500m2
  #Question - change they way to calculate per 500m2 if we decide to do it for all calculations
  summarise(Gram_500m2 = (Biomass_sp_period/Transect_area)*500, 
            Method, Fishing, SiteCode, Biomass_sp_period, Transect_area, Period) %>% 
  #Calculating the biomass of each species per site in g per 500m2 (as average of periods)
  #Question - check mean calculation
  summarise(Gram_site_sp = mean(Gram_500m2), 
            ValidName, Method, Fishing, SiteCode, Biomass_sp_period, Transect_area, Period) %>%
  #Going from g to kg for the biomass
  mutate(Kg_site_sp = Gram_site_sp/1000) %>% #Biomass in kg
  select(-Gram_site_sp) %>% #Removing the Gram_site_sp column
  #Making Biomass_sp_period into kg from g
  mutate(Biomass_sp_period = Biomass_sp_period/1000) %>% 
  #Calculating biomass per period per 500m2
  mutate(Kg_period_500m2 = (Biomass_sp_period/Transect_area)*500) %>% 
  group_by(Site, ValidName) %>% 
  #Calculating standard error
  mutate(SE_B = sd(Kg_period_500m2)/sqrt(length(Kg_period_500m2))) %>% 
  ungroup()

#Combine biomass per species data frames
Bio_sp <- rbind(Bio_sp_D, Bio_sp_U) %>% 
  #removing columns used to calculate SE
  select(-c(Biomass_sp_period, Transect_area, Period, Kg_period_500m2)) %>% 
  unique()

#Removing unnecessary variables
rm(Bio_sp_D, Bio_sp_U)


#Calculating average density of each species per site for DOVS
Den_sp_D <- DOVS %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  mutate(Transect_area = Transect_length_m*5) %>%  #calculating transect area by multiplying with width 5m
  group_by(Site, Period, ValidName) %>% 
  summarise(N_period = sum(N), #Sum of abundance of species within each period
            Method, Fishing, SiteCode, Transect_area, Period) %>% 
  unique() %>%
  #Question - change they way to calculate per 500m2 if we decide to do it for all calculations
  mutate(N_500m2 = (N_period/Transect_area)*500) %>% #Number/500m2
  group_by(Site, ValidName) %>% 
  #Question - check mean calculation
  summarise(N_site_sp = mean(N_500m2), #Calculating average abundance of each species per site
            Method, Fishing, SiteCode, ValidName, N_500m2, Transect_area, Period) %>% 
  group_by(Site, ValidName) %>% 
  #Calculating standard error
  mutate(SE_D = sd(N_500m2)/sqrt(length(N_500m2))) %>% 
  ungroup()


#Calculating average density of each species per site for UVC
Den_sp_U <- UVC %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  mutate(Transect_area = Transect_length_m*5) %>%  #calculating transect area by multiplying with width 5m
  group_by(Site, Period, ValidName) %>% 
  summarise(N_period = sum(N), #Sum of abundance of species within each period
            Method, Fishing, SiteCode, Transect_area, Period) %>% 
  unique() %>%
  #Question - change they way to calculate per 500m2 if we decide to do it for all calculations
  mutate(N_500m2 = (N_period/Transect_area)*500) %>% #Number/500m2
  group_by(Site, ValidName) %>% 
  #Question - check mean calculation
  summarise(N_site_sp = mean(N_500m2), #Calculating average abundance of each species per site
            Method, Fishing, SiteCode, ValidName, N_500m2, Transect_area, Period) %>% 
  group_by(Site, ValidName) %>% 
  #Calculating standard error
  mutate(SE_D = sd(N_500m2)/sqrt(length(N_500m2))) %>% 
  ungroup()

#Combining average density for DOVS and UVC and making a matrix
Den_sp <- rbind(Den_sp_D, Den_sp_U) %>% 
  select(-c(N_500m2, Transect_area, Period))

rm(Den_sp_D, Den_sp_U)


# Mean biomass and density for PCO's --------------------------------------

### no-take and fishing zones ###

#mean biomass for PCO - zones
mean_zones1 <- Biomass_sp %>% 
  group_by(ValidName, Fishing) %>% 
  #making column for mean biomass per species per zone
  #Question - check mean calculation
  mutate(Kg_500m2_zone = mean(Kg_site_sp)) %>% 
  #SE
  mutate(SE_kg = sd(Kg_site_sp)/sqrt(length(Kg_site_sp))) %>% 
  ungroup() %>% 
  select(-c(Kg_site_sp, Site, SiteCode, Method)) %>% 
  unique()

#mean density for PCO zones
mean_zones2 <- Density_sp %>% 
  group_by(ValidName, Fishing) %>% 
  #making column for mean biomass per species per zone
  #Question - check mean calculation
  mutate(N_500m2_zone = mean(N_site_sp)) %>% 
  #SE
  mutate(SE_kg = sd(N_site_sp)/sqrt(length(N_site_sp))) %>% 
  ungroup() %>% 
  select(-c(N_site_sp, Site, SiteCode, Method)) %>% 
  unique()

#mean biomass for PCO - bioregion
mean_bioreg1 <- Biomass_sp %>% 
  #adding bioregion info
  left_join(SiteInfo %>% select(Site, Bioregion) %>% unique(), by = "Site") %>% 
  group_by(ValidName, Bioregion) %>% 
  #making column for mean biomass per species per bioregion
  #Question - check mean calculation
  mutate(Kg_500m2_bioregion = mean(Kg_site_sp)) %>% 
  #SE
  mutate(SE = sd(Kg_site_sp)/sqrt(length(Kg_site_sp))) %>% 
  ungroup() %>% 
  #remove unnecessary columns
  select(-c(Kg_site_sp, Site, SiteCode, Method, Fishing)) %>% 
  unique()

#mean density for PCO - bioregion
mean_bioreg2 <- Density_sp %>% 
  #adding bioregion info
  left_join(SiteInfo %>% select(Site, Bioregion) %>% unique(), by = "Site") %>% 
  group_by(ValidName, Bioregion) %>% 
  #making column for mean biomass per species per bioregion
  #Question - check mean calculation
  mutate(N_500m2_bioregion = mean(N_site_sp)) %>% 
  #SE
  mutate(SE = sd(N_site_sp)/sqrt(length(N_site_sp))) %>% 
  ungroup() %>% 
  #remove unnecessary columns
  select(-c(N_site_sp, Site, SiteCode, Method, Fishing)) %>% 
  unique()
  
