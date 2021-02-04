###############################################################################################################
# Title: Data cleaning UVC and DOVS Data Galapagos
# Author: Sara Færch Hansen
# Assisting: Denisse Fierro Arcos
# Version: 1
# Date last updated: 2021-01-28
###############################################################################################################

# Uploading libraries -----------------------------------------------------
{library(tidyverse)
  library(chron)
  library(openxlsx)}

# Uploading supporting datasets -------------------------------------------
#We upload the supporting info first to make our life easier when cleaning the data
#Loading excel file containing site keys for all common sites across sampling methods
SiteKeys <- openxlsx::read.xlsx("../Data/SiteKeys.xlsx")  %>%
  select(-Island) %>% 
  #Remove white space from site names
  mutate(Site = str_trim(Site, "both")) %>% 
  #Removing accents (e.g., Bahía to Bahia) from all columns
  mutate_all(~stringi::stri_trans_general(., "Latin-ASCII")) %>% 
  #Changing to title case (i.e., first letter of each word is capitalised) between UVC and Site columns
  mutate_at(vars(-SiteCode), ~str_to_title(.))

#Vector containing names of non fish species
NonFish <- c("Eretmochelys imbricata", "Chelonia mydas", "Zalophus wollebaeki", 
             "Phalacrocorax harrisi", "Cardisoma crassum", "Panulirus gracilis",
             "Scyllarides astori", "Spheniscus mendiculus", "Arctocephalus galapagoensis",
             "Lepidochelys olivacea")

#Access to the Fish data set with correct names and a/b variables to calculate biomass
FishDB <- read_csv("https://raw.githubusercontent.com/lidefi87/MangroveProject_CDF/master/Data/FishDB.csv")
#Database updated in January 2021


# Loading UVC and DOVS datasets -------------------------------------------
#Loading and cleaning UVC data
UVC <- read.csv(file = "../Data/UVC.csv", header = TRUE,
                stringsAsFactors = FALSE) %>% select(-X) %>% 
  #Remove white space from site names
  mutate(Site = str_trim(Site, "both")) %>% 
  #Removing accents (e.g., Bahía to Bahia) from site names
  mutate(Site = stringi::stri_trans_general(Site, "Latin-ASCII")) %>% 
  #Changing site names to title case (i.e., first letter of each word is capitalised)
  mutate(Site = str_to_title(Site)) %>% 
  #Correcting site names using Site Keys - Sites not in the list will be dropped
  left_join(SiteKeys %>% select(UVC, Site, SiteCode), by = c("Site" = "UVC")) %>% 
  #Remove original site column
  select(-Site) %>% 
  #Rename new site column as Site
  rename("Site" = "Site.y") %>% 
  #Remove any sites with no corrected name
  drop_na(Site)


#Loading and cleaning DOVS data
DOVS_FullDB <- read.csv(file = "../Data/DOVS.csv", header = TRUE, 
                        stringsAsFactors = FALSE) %>% select(-X) %>% 
  #Remove white space from site names
  mutate(Site = str_trim(Site, "both")) %>% 
  #Removing accents (e.g., Bahía to Bahia) from site names
  mutate(Site = stringi::stri_trans_general(Site, "Latin-ASCII")) %>% 
  #Changing site names to title case (i.e., first letter of each word is capitalised)
  mutate(Site = str_to_title(Site)) %>% 
  #Correcting site names using Site Keys - Sites not in the list will be dropped
  left_join(SiteKeys %>% select(DOVS, Site, SiteCode), by = c("Site" = "DOVS")) %>% 
  #Remove original site column
  select(-Site) %>% 
  #Rename new site column as Site
  rename("Site" = "Site.y") %>% 
  #Remove any sites with no corrected name
  drop_na(Site)


#Loading data with coordinates and open/closed to fishing status
SiteInfo <- read.csv("../Data/GPScoords_BacalaoMMT_Corrected.csv") %>% 
  #Changing first letter of column names to uppercase
  janitor::clean_names(case = "title") %>%
  #We basically apply the same methods as before to ensure consistency
  #Remove white space from site names
  mutate(Site = str_trim(Site, "both")) %>% 
  #Removing accents (e.g., Bahía to Bahia) from site names, islands and zoning
  mutate_at(vars(c(Site, Island, Zoning)), ~stringi::stri_trans_general(., "Latin-ASCII")) %>% 
  #Changing site names and bioregions to title case (i.e., first letter of each word is capitalised)
  mutate_at(vars(c(Site, Bioregion)), str_to_title) %>% 
  #Correcting site names using Site Keys - Sites not in the list will be dropped
  left_join(SiteKeys %>% select(SiteName, Site, SiteCode), by = c("Site" = "SiteName")) %>% 
  #Remove dashes from bioregion names
  mutate(Bioregion = str_replace(Bioregion, "-", " ")) %>% 
  #Remove original site column
  select(-Site) %>% 
  #Rename new site column as Site
  rename("Site" = "Site.y") %>% 
  #Remove any sites with no corrected name
  drop_na(Site)


#Loading data for site length
SiteLength <- openxlsx::read.xlsx("../Data/Periods & Transect Lengths_Bacalao Magic mystery tour_2014.xlsx",
                                  detectDates = T) %>% 
  mutate(Time = chron::times(as.numeric(Time))) %>% #date was written in two different formats and it
  #was causing problems when reading these values, time was changed using the chron library
  select(-c(Diver, Dive.duration, Island, Bioregion, Date)) %>% 
  #Changing first letter of column names to uppercase and an underscore between words
  janitor::clean_names(case = "parsed") %>%
  #Rename date corrected to date
  rename("Date" = "Date_Corrected") %>%
  mutate(Transect_code = paste0("T", Transect_code)) %>%  #Adding T, so that Transect_code 
  #and period from DOVS_FullDB can be matched when joining data frames
  #Remove white space from site names
  mutate(Site = str_trim(Site, "both")) %>% 
  #Removing accents (e.g., Bahía to Bahia) from site names
  mutate(Site = stringi::stri_trans_general(Site, "Latin-ASCII")) %>% 
  #Changing site names to title case (i.e., first letter of each word is capitalised)
  mutate(Site = str_to_title(Site)) %>% 
  #Correcting site names using Site Keys - Sites not in the list will be dropped
  left_join(SiteKeys %>% select(SiteName, Site, SiteCode), by = c("Site" = "SiteName")) %>% 
  #Remove original site column
  select(-Site) %>% 
  #Rename new site column as Site
  rename("Site" = "Site.y") %>% 
  #Remove any sites with no corrected name
  drop_na(Site)


#Joining the data from SiteLength and SiteInfo in one dataframe
SiteInfo <- SiteInfo %>% left_join(SiteLength, by = c("Site", "SiteCode")) %>% 
  rename(Period = Transect_code)

#Removing excess variables
rm(SiteKeys, SiteLength)


# Tidying up UVC data -----------------------------------------------------
#Tidying up UVC data
UVC <- UVC %>%
  #Dropping columns we do not need
  select(-c(Time, Diver, Current, Temperature_unit, Thermocline_depth, Species, Dive_duration, 
            Census_duration, Distance_unit, Sex, Depth, Transect_length)) %>%
  #Joining to keep correct (valid) names of species in UVC data from FishDB
  left_join(FishDB %>% select(ScientificName, ValidName, Family, Genus, a, b, 
                              LengthType, LenLenRatio),
            by = c("SpeciesName" = "ScientificName")) %>% 
  #Remove any non-fish species
  filter(!ValidName %in% NonFish) %>% 
  select(-c(Island)) %>% #Island will be added when SiteInfo is joined
  #Add T to Transect_code and renaming to period, so it is similar to the column in SiteInfo
  mutate(Transect_code = paste0("T", Transect_code)) %>% 
  rename(Period = Transect_code) %>% 
  #Adding SiteInfo to the sites in the UVC data
  left_join(SiteInfo %>% select(-c(Date, Year, Month, Bioregion)), 
            by = c("Site", "Period", "SiteCode")) %>% 
  #Going from sizeclass to length of fish in cm
  #renaming the sizeclass that is smaller than 30 cm, and longer than 300 cm.
  #Question for DFA - How would you put this in the paper and is it the right thing to do?
  mutate(SizeClass = gsub("S", "", SizeClass)) %>% 
  rename(Length_cm = SizeClass) %>% 
  #Making length a numeric value
  mutate(Length_cm = as.numeric(Length_cm)) %>% 
  filter(!N == 0) #Removing UVC rows with no abundance (N = 0)

#Extracting rows with NA values under ValidName, but keeping all columns
#to identify if there are any species not included in our FishDB
UVC %>% filter(is.na(ValidName), .preserve = T) %>% 
  #Now let's extract the unique values of SpeciesNames for which we do not have a valid name
  distinct(SpeciesName) #FishDB was updated with Decapterus sanctahelenae (now knoen as D. punctatus)
#This species is not recorded in the Pacific and is a plantivore, so of no interest to the study.
#Deleting the one occurence of Decapterus sanctahelenae
UVC <- UVC %>% filter(!SpeciesName == "Decapterus sanctaehelenae") %>% 
  select(-SpeciesName) #SpeciesName column is no longer needed, as D. sanctahelenae has been removed.

#Finding Periods that are in the UVC and not in the SiteIfo, meaning they have no site length
UVC %>% 
  select(Site, Period) %>% 
  mutate(UVC_period = paste(Site, Period, sep = " ")) %>% 
  distinct() %>% 
  left_join(SiteInfo %>% 
              select(Site, Period) %>% 
              mutate(SiteInfo_period = paste(Site, Period, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Period")) %>% 
  filter(!SiteInfo_period %in% UVC_period) %>% 
  subset(!Period == "") #Subsetting rows where there is a period, but no data in SiteInfo_period

#Deleting Periods with no site length
UVC <- UVC %>% filter(!(Site == "Isabela Este 5" & Period == "T3"))

#Making list of unique species in UVC data, to filter species in DOVS data
SpeciesUVC <- UVC$ValidName %>% unique()

#Uncounting UVC data, to keep data format similar in DOVS and UVC
UVC <- UVC %>% 
  uncount(., N) %>% #uncounting N to have one individual per row
  mutate(N = 1) #Adding column for N, as it is removed by uncount


# Tidying up DOVS data -----------------------------------------------------
#Tidying up DOVS data - Saving the clean data set as a different variable
DOVS <- DOVS_FullDB %>% 
  filter(Family != "" & Period != "") %>% #Removing rows of un-identified species and empty periods
  select(-c(Comment, Stage, Depth)) %>% #Removing columns that are not needed for analysis
  #Removing spaces in Genus column
  mutate(Genus = str_trim(Genus, side = "both"),
         #I also want the empty Species rows to now include sp
         Species = case_when(Species == "" ~ "sp",
                             #This means that everything else remains the same
                             TRUE ~ Species)) %>% 
  #Now we create a new column for the combined species name
  #If Genus is not empty, our new column will join Genus and Species
  mutate(SpeciesName = case_when(Genus != "" ~ paste(Genus, Species, sep = " "),
                                 #If Genus is empty, then join Species and Family
                                 Genus == "" ~ paste(Family, Species, sep = " "))) %>%
  #Since there is only one species of Mycteroperca, Mycteroperca sp is changes to Mycteroperca olfax
  mutate(SpeciesName = case_when(SpeciesName == "Mycteroperca sp" ~ "Mycteroperca olfax",
                                 #Correcting species that are identified to genus level, 
                                 #but only have 1 species in their genus in the GMR
                                 SpeciesName == "Zanclus sp" ~ "Zanclus cornutus",
                                 SpeciesName == "Aulostomidae sp" ~ "Aulostomus chinensis",
                                 SpeciesName == "Aulostomus sp" ~ "Aulostomus chinensis",
                                 SpeciesName == "Holacanthus sp" ~ "Holacanthus passer",
                                 SpeciesName == "Sufflamen sp" ~ "Sufflamen verres",
                                 SpeciesName == "Uraspis sp" ~ "Uraspis helvola",
                                 SpeciesName == "Aetobatus sp" ~ "Aetobatus narinari",
                                 TRUE ~ SpeciesName)) %>% 
  #Using join to keep correct names of species in DOVS data
  left_join(FishDB %>% select(ScientificName, ValidName, a, b, LengthType, LenLenRatio),
            by = c("SpeciesName" = "ScientificName")) %>% 
  #Now we remove non fish species
  filter(!ValidName %in% NonFish) %>% 
  select(-c(SpeciesName, Island)) %>% 
  #Left joining info about sites
  left_join(SiteInfo %>% select(-c(Depth)), by = c("Site", "SiteCode", "Period"))

#Remove variables that are no longer needed
rm(DOVS_FullDB)

#Finding Periods that are in the DOVS data and not in the SiteInfo, meaning they have no site length
DOVS %>% 
  select(Site, Period) %>% 
  mutate(DOVS_period = paste(Site, Period, sep = " ")) %>% 
  distinct() %>% 
  left_join(SiteInfo %>% 
              select(Site, Period) %>% 
              mutate(SiteInfo_period = paste(Site, Period, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Period")) %>% 
  filter(!SiteInfo_period %in% DOVS_period) %>% 
  subset(!Period == "")  #Subsetting rows where there is a period, but no data in SiteInfo_period

#Deleting Periods with no site length
DOVS <- DOVS %>% filter(!(Site == "Punta Espinoza" & Period == "T14"), 
                        !(Site == "Genovesa Norte" & Period == "T10"), 
                        !(Site == "Isabela Este 5" & Period == "T3"), 
                        !(Site == "Banana" & Period == "T9"))

#These are the species that are not in the UVC data which are in the DOVS data
unique(DOVS$ValidName[!(DOVS$ValidName %in% UVC$ValidName)])

#Keeping only species in DOVS that are in the UVC data
DOVS <- DOVS %>% filter(ValidName %in% SpeciesUVC)
