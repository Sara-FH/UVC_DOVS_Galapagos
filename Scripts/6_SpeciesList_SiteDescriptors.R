###############################################################################################################
# Title: Species list and study site descriptors
# Author: Sara Færch Hansen
# Assisting: Denisse Fierro Arcos
# Version: 1
# Date last updated: 2021-01-28
###############################################################################################################


# Species list ------------------------------------------------------------
#Species list from both UVC and DOVS
splist <- UVC %>% 
  select(Family, ValidName) %>% 
  unique() %>% 
  rbind(DOVS %>% select(Family, ValidName) %>% unique()) %>% 
  mutate(Family = recode(Family, 
                         "Dasyatididae" = "Dasyatidae",
                         "Myliobatidae" = "Mobulidae")) %>% 
  unique() %>% 
  arrange(Family, ValidName)

#write to excel table
write.xlsx(splist, "../Tables/Specieslist.xlsx")

#Remove variables after use
rm(splist)


# Study site descriptors --------------------------------------------------
#Descriptors to add - Site, status, N transects (replicates), Bioregion, Island
sites <- SiteInfo %>% 
  group_by(Site) %>% 
  #Counting the number of transects at each site and summarising
  summarise(Transects = sum(length(Period)), 
            Site, Island, Bioregion, Lat, Long, Type, Fishing, Zoning) %>% 
  unique() %>% 
  ungroup() %>% 
  #Renaming columns
  rename(Latitude = Lat, Longitude = Long, Area_description = Type, Zone = Fishing) %>% 
  #Recoding area description, zone and bioregion
  mutate(Area_description = recode(Area_description,
                                   "Conservation" = "Conservation (2.1)",
                                   "Tourism" = "Tourism (2.2)", 
                                   "Fishing" = "Fishing (2.3)", 
                                   "Special Use" = "Special use (2.4)")) %>% 
  mutate(Zone = recode(Zone, 
                       "Closed" = "No-take", 
                       "Open" = "Fishing")) %>% 
  mutate(Bioregion = recode(Bioregion, 
                            "Lejano Norte" = "Far Northern", 
                            "Norte" = "Northern", 
                            "Centro Sur" = "Central South-eastern", 
                            "Oeste Fria" = "Western")) %>% 
  #Changing the order of the columns and dropping Zoning
  select(Site, Zone, Area_description, Transects, Bioregion, Island, Latitude, Longitude)

#write to excel table
write.xlsx(sites, "../Tables/Site_descriptors.xlsx")

#Remove variables after use
rm(sites)