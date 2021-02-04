###############################################################################################################
# Title: Calculating mean species richness, mean density and mean biomass
# Author: Sara Færch Hansen
# Assisting: Denisse Fierro Arcos
# Version: 1
# Date last updated: 2021-01-28
###############################################################################################################

# Species richness boxplot ------------------------------------------------

#Making data frame for empty periods (no species) in DOVS data
EmptyPeriods_DOVS <- SiteInfo %>% 
  select(Site, Period, SiteCode, Transect_length_m, Fishing) %>% 
  mutate(SiteInfo_period = paste(Site, Period, sep = " ")) %>% 
  distinct() %>% 
  left_join(DOVS %>% #combine with periods from DOVS - so we can find out which have no species in them
              select(Site, Period) %>% 
              mutate(DOVS_period = paste(Site, Period, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Period")) %>% 
  filter(!DOVS_period %in% SiteInfo_period) %>% #Filter the periods that are not in DOVS, but are in SiteInfo
  select(-c(SiteInfo_period, DOVS_period)) %>% #Remove columns used for comparison
  mutate(N = 0) %>% #Adding empty abundance column
  mutate(Method = paste("DOVS")) %>% #Adding method column
  mutate(ValidName = NA) #Adding empty species column

#Calculating species richness in DOVS
DOVS_richness <- DOVS %>% 
  #Selecting columns to be used
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  rbind(EmptyPeriods_DOVS) %>% #Binding rows with no fish in them
  group_by(Site, Period) %>% 
  mutate(Richness_period = length(unique(na.omit(ValidName)))) %>% #richness per period/transect
  mutate(Transect_area = Transect_length_m*5) %>% #Area of each period/transect in m2
  #Question DFA - We discussed this as a bad way to calculate richness per 500m2, so I suggest
  #Question DFA - We discussed this as a bad way to calculate richness per 500m2, so I suggest
  #Number for transect area to be multiplied with to get 500m2
  mutate(Transect_to500 = 500/Transect_area) %>% 
  mutate(Sp_500m2 = Richness_period*Transect_to500) %>% #Species_period/500m2
  #However it give the same result for species richness as the former calculation (below)
  #mutate(Sp_500m2 = (Richness_period/Transect_area)*500) %>% #Species_period/500m2
  #Removing abundance and species names, as they are not used for species richness calculations
  select(-c(N, ValidName)) %>% 
  unique() %>% 
  group_by(Site) %>% 
  #Question DFA - is this the correct way to calculate the mean? This is including zeroes for some periods.
  summarise(Site_sp_500m2 = mean(Sp_500m2), #calculating mean species richness per 500m2 for each site
            Method, Fishing, SiteCode) %>% 
  ungroup() %>% 
  unique()

#Making data frame for empty periods (no species) in UVC data
EmptyPeriods_UVC <- SiteInfo %>% 
  select(Site, Period, SiteCode, Transect_length_m, Fishing) %>% 
  mutate(SiteInfo_period = paste(Site, Period, sep = " ")) %>% 
  distinct() %>% 
  left_join(UVC %>% 
              select(Site, Period) %>% 
              mutate(UVC_period = paste(Site, Period, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Period")) %>% 
  filter(!UVC_period %in% SiteInfo_period) %>% #Filter the periods that are not in 
  select(-c(SiteInfo_period, UVC_period)) %>% 
  mutate(N = 0) %>% #Adding empty abundance column
  mutate(Method = paste("UVC")) %>% #Adding method column
  mutate(ValidName = NA) #Adding empty species column

#Calculating species richness in UVC
UVC_richness <- UVC %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  rbind(EmptyPeriods_UVC) %>% #Binding rows with no fish in them
  group_by(Site, Period) %>% 
  mutate(Richness_period = length(unique(na.omit(ValidName)))) %>% #richness per period/transect
  mutate(Transect_area = Transect_length_m*5) %>% #Area of each period/transect
  #Question - change they way to calculate per 500m2 if we decide to do it for all calculations
  mutate(Sp_500m2 = (Richness_period/Transect_area)*500) %>% #Species/500m2
  #Removing abundance and species names, as they are not used for species richness calculations
  select(-c(N, ValidName)) %>% 
  unique() %>% 
  group_by(Site) %>% 
  #Question - check mean calculation
  summarise(Site_sp_500m2 = mean(Sp_500m2), #calculating mean species per 500m2 for each site
            Method, Fishing, SiteCode) %>% 
  ungroup() %>% 
  unique()

#Combining richness for DOVS and UVC
Richness <- rbind(DOVS_richness, UVC_richness) %>% 
  left_join(SiteInfo %>% select(Site, Bioregion) %>% unique(), by = "Site")

#Deleting variables that are no longer needed
rm(DOVS_richness, UVC_richness)


# Density boxplot ---------------------------------------------------------

#Calculating density in DOVS for each site per 500m2
DOVS_density <- DOVS %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  rbind(EmptyPeriods_DOVS) %>% #Binding rows with no fish in them
  mutate(Transect_area = Transect_length_m*5) %>%  #calculating transect area by multiplying with width 5m
  group_by(Site, Period) %>% 
  summarise(N_period = sum(N), #Sum of abundance of species within each period
            Method, Fishing, SiteCode, Transect_area) %>% 
  unique() %>% 
  #Question - change they way to calculate per 500m2 if we decide to do it for all calculations
  mutate(N_500m2 = (N_period/Transect_area)*500) %>% #Number/500m2
  group_by(Site) %>% 
  #Question - check mean calculation
  summarise(N_site_500m2 = mean(N_500m2), 
            Method, Fishing, SiteCode) %>% 
  unique() %>% 
  ungroup()

#Calculating density in UVC for each site per 500m2
UVC_density <- UVC %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  rbind(EmptyPeriods_UVC) %>% #Binding rows with no fish in them
  mutate(Transect_area = Transect_length_m*5) %>%  #calculating transect area by multiplying with width 5m
  group_by(Site, Period) %>% 
  summarise(N_period = sum(N), #Sum of abundance of species within each period
            Method = Method, Fishing = Fishing, SiteCode = SiteCode, Transect_area) %>% 
  unique() %>% 
  #Question - change they way to calculate per 500m2 if we decide to do it for all calculations
  mutate(N_500m2 = (N_period/Transect_area)*500) %>% #Number/500m2
  group_by(Site) %>% 
  #Question - check mean calculation
  summarise(N_site_500m2 = mean(N_500m2), 
            Method, Fishing, SiteCode) %>% 
  unique() %>% 
  ungroup()

#Combining density for DOVS and UVC
Density <- rbind(DOVS_density, UVC_density)

#Deleting variables that are no longer needed
rm(DOVS_density, UVC_density)


# Biomass calculations DOVS ----------------------------------------------------
#Quality control
#1. RMS <= 60
#2. Precision <= 10% estimated Length
#In the following no data is removed, as we check that RMS is below 60 and precision is no more 
#than 10% of length
DOVS <- DOVS %>% filter(RMS_mm <= 60) %>% 
  #Because of blurry video RMS is higher than 20, no RMS was higher than 59
  #We change the units of the lengths from mm to cm prior to biomass calculation
  mutate(Length_mm = Length_mm/10) %>% 
  #Now we rename the column to avoid confusion
  rename("Length_cm"="Length_mm") %>% 
  #We will drop columns we do not need
  select(-c(Precision_mm, RMS_mm, Range_mm))

#Preparing the EmptyPeriods data frames for DOVS
EmptyPeriods_DOVS <- EmptyPeriods_DOVS %>% 
  rename(Biomass_N = N) #Making biomass column with 0 biomass, as there are no biomass in empty periods

#Calculating biomass DOVS
Biomass_DOVS <- DOVS %>%
  mutate(LenLenRatio = as.numeric(LenLenRatio)) %>% 
  #The fish biomass equation is W = a*L^b, therefore first transform the length to Fork Length 
  #then apply the exponent b and finally multiply by a.
  #The equation is therefore: a*((LenLenRatio*Length_cm)^b)
  mutate(Biomass = a*((LenLenRatio*Length_cm)^b)) %>% #Biomass for 1 individual
  mutate(Biomass_N = Biomass*N) %>% #Biomass for all individual species of each length
  select(Site, Period, Method, ValidName, Fishing, SiteCode, Transect_length_m, Biomass_N) %>% 
  rbind(EmptyPeriods_DOVS) %>% #Binding the periods with no fish in them
  mutate(Transect_area = Transect_length_m*5) %>% #Calculating transect area using transect width 5m
  group_by(Site, Period) %>% 
  #Biomass of all species per period per site AND Calculating it in grams per 500m2
  #Question - change they way to calculate per 500m2 if we decide to do it for all calculations
  summarise(Gram_500m2_period = (sum(Biomass_N)/Transect_area)*500,
            Method, Fishing, SiteCode, Transect_area) %>% #In g/500m2
  unique() %>% 
  group_by(Site) %>% 
  #Question - check mean calculation
  summarise(Gram_500m2_site = mean(Gram_500m2_period), #Calculating average biomass of periods as site biomass
            Method, Fishing, SiteCode) %>% 
  mutate(Kg_500m2_site = Gram_500m2_site/1000) %>% #Calculating kg per 500m2 at each site
  unique()

# Biomass calculations UVC ---------------------------------------------
#Loading total length ratio for FishDB data
TLRatio <- openxlsx::read.xlsx("../Data/TLRatio.xlsx", sheet = 1) %>% 
  mutate(TLRatio = as.numeric(TLRatio))
str(TLRatio) #TLRation is numeric

#Adding TLRatio to UVC data
UVC <- UVC %>% 
  left_join(TLRatio %>% select(ValidName, TLRatio), by = c("ValidName")) %>% 
  select(-LenLenRatio)
#Removing TLRation
rm(TLRatio)

#Preparing the EmptyPeriods data frames for UVC
EmptyPeriods_UVC <- EmptyPeriods_UVC %>% 
  rename(Biomass_N = N) #Making biomass column with 0 biomass, as there are no biomass in empty periods

#Calculating biomass UVC
Biomass_UVC <- UVC %>%
  #The fish biomass equation is W = a*L^b, therefore first transform the length to Total length
  #then apply the exponent b and finally multiply by a.
  #The equation is therefore: a*((LenLenRatio*Length_cm)^b)
  mutate(Biomass = a*((TLRatio*Length_cm)^b)) %>% #Biomass for 1 individual
  mutate(Biomass_N = Biomass*N) %>% #Biomass for all individual species of each length
  select(Site, Period, Method, ValidName, Fishing, SiteCode, Transect_length_m, Biomass_N) %>% 
  rbind(EmptyPeriods_UVC) %>% #Binding the periods with no fish in them
  mutate(Transect_area = Transect_length_m*5) %>% #Calculating transect area using transect width 5m
  group_by(Site, Period) %>% 
  #Biomass of all species per period per site AND Calculating g per 500m2
  #Question - change they way to calculate per 500m2 if we decide to do it for all calculations
  summarise(Gram_500m2_period = (sum(Biomass_N)/Transect_area)*500,
            Method, Fishing, SiteCode, Transect_area) %>% #In g per 500m2
  unique() %>% 
  group_by(Site) %>% 
  #Question - check mean calculation
  summarise(Gram_500m2_site = mean(Gram_500m2_period), #Calculating average biomass of periods as site biomass
            Method, Fishing, SiteCode) %>% 
  mutate(Kg_500m2_site = Gram_500m2_site/1000) %>% #Calculating kg per 500m2 at each site
  unique()

#Combining biomass data for DOVS and UVC
Biomass <- rbind(Biomass_DOVS, Biomass_UVC)

#Deleting variables that are no longer needed
rm(EmptyPeriods_DOVS, EmptyPeriods_UVC, Biomass_DOVS, Biomass_UVC)

