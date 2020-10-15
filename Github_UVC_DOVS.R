###################################################################################################################
# Title: Github data cleaning and analysis UVC and DOVS Galapagos
# Author: Sara Færch Hansen
# Assisting: Denisse Fierro Arcos
# Version: 1
# Date last updated: 2020-10-08
# Aim: Compare UVC and DOVS data in the Galapagos related to the Master of Science of Sara Færch Hansen
###################################################################################################################

# Uploading libraries -----------------------------------------------------
library(tidyverse)
library(data.table)
library(vegan)
library(chron)
library(xlsx)
library(ggplot2)
library(ape) #For principal coordinates analysis

#DFA comments
#Ideally you want just one dataset that includes all info you need to do the analysis
#We have too many datasets and lots of information which is irrelevant to this project
#Be organised from the beginning or you will have too many variables and it makes the
#code harder to follow, while the info that you do not use is occupying space in memory
#and slowing down computer performance for no reason. Merge files when possible and delete
#anything that you do not need

#I have done this by collecting all the info about the sites, e.g. coordinates and lengths in SiteInfo

# Uploading data ----------------------------------------------------------
#Loading UVC and DOVS data
UVC <- read.csv(file = "Data/UVC.csv", header = TRUE,
                stringsAsFactors = FALSE) %>% select(-X)

DOVS_FullDB <- read.csv(file = "Data/DOVS.csv", header = TRUE, 
                        stringsAsFactors = FALSE) %>% select(-X)
DOVS_FullDB %>% select(Site) %>% subset(endsWith(Site, " ")) %>% unique()
#there are two sites that ends with a space
#correcting the names of Shark Bay and Isabela Isla Tortuga, by trimming the Site column
DOVS_FullDB <- DOVS_FullDB %>% mutate(Site = str_trim(Site, side = "both"))

#Checking if the two methods have the same number of sites
unique(DOVS_FullDB$Site)
unique(UVC$Site) 
#They do not have the same number of sites - also they have different names.

#Deleting Sites only in UVC data
UVC <- UVC %>% filter(!Site == "Punta Calle", 
                      !Site == "Santiago noroeste", 
                      !Site == "Punta Vicente Roca") %>% 
  #Renaming Sites that are named differently in UVC than in DOVS and SiteInfo
  mutate(Site = 
           recode(Site, 
                  "El Arco" = "Al Arco (Cleaning Station)",
                  "Bahía Gardner norte" = "Bahía Gardner Norte", 
                  "Bartholome" = "Bartolome",
                  "Botella" = "La Botella", 
                  "Botellita" = "La Botellita", 
                  "4 Hermanos" = "Cuatro Hermanos", 
                  "Daphne menor" = "Daphne Menor", 
                  "El Finado" = "Marchena El Finado", 
                  "Radar Isabela" = "El Radar", 
                  "Isla Enderby" = "Enderby", 
                  "Punta Espinoza" = "Fernandina Punta Espinoza", 
                  "Punta Manngle" = "Fernandina Punta Mangle", 
                  "Este 7" = "Isabela Este 7", 
                  "Este 8" = "Isabela Este 8", 
                  "Isla Tortuga" = "Isabela Isla Tortuga", 
                  "Isabela Sur 1" = "Isabela Sur", 
                  "Luz de día" = "Luz de Día", 
                  "Marchena oeste" = "Marchena Oeste",
                  "Pinta Oueste" = "Pinta Oeste", 
                  "Pinzon" = "Pinzon Norte", 
                  "Punta Albermarle" = "Punta Albemarle", 
                  "Marchena Este (Pta Espejo)" = "Punta Espejo", 
                  "Punta Núñez" = "Punta Nuñez", 
                  "Rocha sin nombre" = "Roca Sin Nombre", 
                  "Santiago noreste" = "Santiago Noreste", 
                  "Poza Azules" = "Santiago Poza Azules", 
                  "Santiago sureste" = "Santiago Sureste", 
                  "Seymour norte" = "Seymour Norte"))

#Deleting Sites from DOVS that are not in UVC data
DOVS_FullDB <- DOVS_FullDB %>% filter(!Site == "Punta Calle 1", 
                                      !Site == "Punta Calle 2",
                                      !Site == "Santiago Suroeste", 
                                      !Site == "Isabela Alcedo")
#comparing sites again
unique(DOVS_FullDB$Site[!(DOVS_FullDB$Site %in% UVC$Site)])
#the Sites "Isabela Isla tortuga", "Islote Gardner", "Pared Norte", 
#do not have any species present in UVC data and are therefore absent from the site column. 

#Vector containing names of non fish species
NonFish <- c("Eretmochelys imbricata", "Chelonia mydas", "Zalophus wollebaeki", 
             "Phalacrocorax harrisi", "Cardisoma crassum", "Panulirus gracilis",
             "Scyllarides astori", "Spheniscus mendiculus", "Arctocephalus galapagoensis",
             "Lepidochelys olivacea")

#Access to the Fish data set with correct names and a/b variables to calculate biomass
FishDB <- read_csv("https://raw.githubusercontent.com/lidefi87/MangroveProject_CDF/master/Data/FishDB.csv")

#Loading site keys - Spreadsheet matching site names in both methods - DFA
#I have updated this excel and moved the old one to archives, I have also changed SiteNames to 
#have spaces in it, so I can use that column to merge SiteKeys and Status - SFH
SiteKeys <- openxlsx::read.xlsx("Data/SiteKeys.xlsx")  %>% #Loading from excel
  select(-Island)

#Loading data with coordinates and open/closed to fishing status
Status <- read.csv("Data/GPScoords_BacalaoMMT_Corrected.csv") 
colnames(Status) <- str_to_title(colnames(Status)) #Changing first letter of column to uppercase
#Checking if Status and SiteKeys has the same site names
Status$Site[!(Status$Site %in% SiteKeys$SiteName)]
#Renaming sites, so they are similar in Status and SiteKeys
Status <- Status %>% mutate(Site = 
                              recode(Site, 
                                     "Bahía Gardner norte" =  "Bahía Gardner Norte", 
                                     "Isabela Sur " = "Isabela Sur", 
                                     "Daphne menor" = "Daphne Menor", 
                                     "Luz de día" = "Luz de Día",
                                     "Punta Vicente Roca (No DOVS)" = "Punta Vicente Roca"))
#Checking that the sites are the same
Status$Site[!(Status$Site %in% SiteKeys$SiteName)]
#Sites that are not the same, but will be deleted at the end:
#"Isabela Alcedo", "Punta Calle 1", "Punta Calle 2", "Punta Vicente Roca", "Santiago Suroeste"

#Merging Status and Sitekeys in SiteInfo, all information in one place
SiteInfo <- Status %>% left_join(SiteKeys, by = c("Site"="SiteName"))

#Loading data for site length
SiteLength <- openxlsx::read.xlsx("Data/Periods & Transect Lengths_Bacalao Magic mystery tour_2014.xlsx",
                                  detectDates = T) %>% 
  mutate(Time = chron::times(as.numeric(Time))) %>% #date was written in two different formats and it
  #was causing problems when reading these values, time was changed using the chron library
  select(-c(Diver, Dive.duration, Island, Bioregion, Date)) %>% 
  rename("Transect_code" = "Transect.code", 
         "Date" = "DateCorrected",
         "Transect_length_m" = "Transect.length.(m)") %>% 
  mutate(Transect_code = as.character(paste("T", Transect_code, sep = ""))) #Adding T, so that Transect_code 
#and period from DOVS_FullDB can be macthed when joining data frames

#Checking if SiteInfo and SiteLength has the same site names
unique(SiteLength$Site[!(SiteLength$Site %in% SiteInfo$Site)])
#Changing site names so they match in SiteLength and SiteInfo
SiteLength <- SiteLength %>% mutate(Site = 
                                      recode(Site, 
                                             "Bahía Gardner norte" =  "Bahía Gardner Norte", 
                                             "Daphne menor" = "Daphne Menor",
                                             "Isabela Sur " = "Isabela Sur",
                                             "Luz de día" = "Luz de Día"))
#Checking that the sites are the same
unique(SiteLength$Site[!(SiteLength$Site %in% SiteInfo$Site)]) #Sites are the same

#Joining the data from SiteLength and SiteInfo in one dataframe
SiteInfo <- SiteInfo %>% left_join(SiteLength, by = "Site") %>% 
  rename(Period = Transect_code) %>%  #Now Period is the same in SiteInfo and DOVS_FullDB
  mutate(Site = stringi::stri_trans_general(Site, "Latin-ASCII"),
         Island = stringi::stri_trans_general(Island, "Latin-ASCII")) %>% 
  #Removing sites from SiteInfo, that are missing either from UVC or DOVS
  filter(!Site == "Isabela Alcedo", 
         !Site == "Punta Calle 1", 
         !Site == "Punta Calle 2", 
         !Site == "Punta Vicente Roca", 
         !Site == "Santiago Suroeste")

#Removing excess variables
rm(SiteKeys, Status, SiteLength)

# Tidying up UVC data -----------------------------------------------------
#Checking species names in UVC data
UVC %>% distinct(Species) %>% arrange(Species)
#Tidying up UVC data
UVC <- UVC %>% mutate(SpeciesName =
                        recode(Species, "bonito" = "Sarda orientalis", #The bonito in Ecuador is Sarda orientalis
                               #Correcting misspelled names in UVC data
                               "Paralabrax albomaclatus" = "Paralabrax albomaculatus",
                               "yellow tail snapper" = "Lutjanus argentiventris",
                               "Zalophus wolebacki" = "Zalophus wollebaeki",
                               "Hoplopagrus guenteri" = "Hoplopagrus guentherii",
                               "Zalophus wollebackii" = "Zalophus wollebaeki",
                               "Zalophus wollebacki" = "Zalophus wollebaeki",
                               "Heterodonthus quoyi" = "Heterodontus quoyi",
                               "Myvteroperca olfax" = "Mycteroperca olfax", 
                               "Dermatolepsis dermatolepsis" = "Dermatolepis dermatolepis")) %>%
  #Dropping columns we do not need
  select(-c(Time, Diver, Current, Temperature_unit, Thermocline_depth, Species, Dive_duration, 
            Census_duration, Distance_unit, Sex, Depth, Transect_length)) %>%
  #Joining to keep correct (valid) names of species in UVC data from FishDB
  left_join(FishDB %>% select(ScientificName, ValidName, Family, Genus, a, b, 
                              LengthType, LenLenRatio),
            by = c("SpeciesName" = "ScientificName")) %>% 
  #Decide what to do for Decapterus sanctahelenae, e.g. should the name be changed?
  #Now that corrections for D. sanctahelenae have been made
  #Remove any non-fish species
  filter(!ValidName %in% NonFish) %>% 
  #Remove SpeciesName column when D. santaehelenae is sorted out
  #select(-SpeciesName) %>% 
  select(-c(Island)) %>% 
  #Add T to Transect_code and renaming to period, so it is similar to the column in SiteInfo
  mutate(Transect_code = as.character(paste("T", Transect_code, sep = ""))) %>% 
  rename(Period = Transect_code) %>% 
  #Remove any accents in the names of sites, before joining with SiteInfo
  mutate(Site = stringi::stri_trans_general(Site, "Latin-ASCII")) %>% 
  #Adding SiteInfo to the sites in the UVC data
  left_join(SiteInfo %>% select(-c(DOVS, UVC, Date, Year, Month, Bioregion, )), 
            by = c("Site" = "Site", "Period" = "Period")) %>% 
  #Transect_length_m.x is from the UVC data and Transect_length_m.y is from SiteInfo, 
  #I would guess that these are supposed to be the same? As the data was at the same time
  #then the length of the UVC sites would be the same as the lengths of the DOVS sites right?
  #Going from sizeclass to length of fish in cm (renaming those that has formely been named wrongly)
  mutate(SizeClass = gsub("S", "", SizeClass)) %>% 
  mutate(SizeClass = as.numeric(recode(SizeClass, 
                                       "25" = "30", 
                                       "301" = "300"))) %>% 
  rename(Length_cm = SizeClass) %>% 
  filter(!N == 0) #Removing UVC rows with no abundance (N = 0)

#Extracting rows with NA values under ValidName, but keeping all columns
#to identify if there are any species not included in our FishDB
UVC %>% filter(is.na(ValidName), .preserve = T) %>% 
  #Now let's extract the unique values of SpeciesNames for which we do not have a valid name
  distinct(SpeciesName) #FishDB was updated with Decapterus sanctahelenae (now knoen as D. punctatus)
#This species is not recorded in the Pacific and is a plantivores, so of no interest to the study.
#Deleting the one occurence of Decapterus sanctahelenae
UVC <- UVC %>% filter(!SpeciesName == "Decapterus sanctaehelenae")

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
  subset(!Period == "")  #Subsetting rows where there is a period, but no data in SiteInfo_period

#Deleting Periods with no site length
UVC <- UVC %>% filter(!(Site == "Isabela Este 5" & Period == "T3"))

#Making list of unique species in UVC data, to filter species in DOVS data
SpeciesUVC <- UVC$ValidName %>% unique()


# Cleaning multiple individuals with one measurement for raw DOVS data ---------
#Cleaning multiple individuals with one measurement in DOVS_FullDB
MultiLength <- DOVS_FullDB %>% 
  #Removing columns that are not needed for this analysis - DFA
  select(-c(Comment, Stage, Depth)) %>% 
  #Removing empty rows in Family and period
  filter(Family != "" & Period != "") %>% 
  #Keep rows with more than one individual
  filter(N > 1) %>% 
  #Remove any instances where the precision is > 10% of length
  filter(Precision_mm < Length_mm*0.1)

#We can check the total number of actual measurements against N - DFA
x <- MultiLength %>% 
  group_by(Site, Period, Genus, Species, N, Length_mm) %>% 
  summarise(counts = n()) %>% 
  group_by(Site, Period, Genus, Species, N) %>% 
  summarise(counts = sum(counts)) %>% 
  ungroup()
#Here we can see that there are some cases were there are more measurements than individuals
#reported in N. Maybe this means that in these cases, they underreported individuals, but
#it could also be that they took multiple measurements for the same individual. Check with
#Pelayo what he wants to do, but I will keep the number of measurements identified as N
x %>% 
  filter(counts > N)

#For the rows were there is only one measurement, we will use the same measurement for 
#all individuals in N
#First we will extract these rows from the MultiLength set
temp <- MultiLength %>% 
  right_join(x %>% filter(counts == 1)) %>% 
  #Repeat each row as many times as stated by N
  uncount(N) %>% 
  rename("N" = "counts") %>% 
  relocate(N, .before = Method)

#Now we remove rows from MultiLength
MultiLength <- MultiLength %>% 
  anti_join(x %>% filter(counts == 1)) %>% 
  #And add the corrected information (temp) to the MultiLength variable
  bind_rows(temp)
#Remove temp because we do not need it
rm(temp)

#Remove the rows we have dealt with
x <- x %>% 
  filter(counts > 1)

#We will check instances when N is the same as the amount of measurements
temp <- MultiLength %>% 
  right_join(x %>% filter(counts == N)) %>% 
  #Change N to 1
  mutate(N = 1) %>% 
  select(-counts)

#Remove the rows we have dealt with
x <- x %>% 
  filter(counts != N)

#We will now extract the rows that match the items left to correct
temp <- MultiLength %>% 
  right_join(x %>% filter(counts < N)) %>% 
  #Now we need an average length
  group_by(Site, Period, Genus, Species, N) %>% 
  summarise(mLength = mean(Length_mm)) %>% 
  #Join with problem rows to calculate how many times we need to apply mean length
  left_join(x) %>% 
  #Calculate new N
  mutate(NewCount = N-counts) %>% 
  select(-counts) %>% 
  ungroup()

MLcorr <- bind_rows(MultiLength %>% 
                      right_join(temp %>% select(-c(mLength, NewCount))) %>% 
                      mutate(N = 1),
                    MultiLength %>% 
                      right_join(temp) %>% 
                      mutate(N = NewCount,
                             Length_mm = mLength) %>% 
                      distinct(Site, Period, Genus, Species, N, .keep_all = T) %>% 
                      select(-c(mLength, NewCount)) %>% 
                      uncount(N),
                    MultiLength %>% 
                      anti_join(temp %>% select(-c(mLength, NewCount)))) %>% 
  mutate(N = case_when(is.na(N) ~ 1,
                       TRUE ~ N))
#Remove temp because we do not need it
rm(temp)

#Remove the rows we have dealt with
x <- x %>% 
  filter(counts > N)
#We only have left the rows for which we have more measurements than individuals reported
#under N. Remember to check with Pelayo about what to do with these ones.

#Once they are all corrected, we could apply corrections to the data set
#I will show you below how to do this with the raw data because we do not have many examples
#in the clean DOVS dataset
corrDOVS <- DOVS_FullDB %>% 
  #Removing columns that are not needed for this analysis - DFA
  select(-c(Comment, Stage, Depth)) %>% 
  #Removing empty rows in Family and period
  filter(Family != "" & Period != "") %>% 
  #Keep rows with more than one individual
  filter(N == 1) %>% 
  #Remove any instances where the precision is > 10% of length
  filter(Precision_mm < Length_mm*0.1) %>% 
  bind_rows(MLcorr)

#Taking the species that did not need correction (N=1) from DOVS_FullDB and adding to corrDOVS
N1_DOVS <- DOVS_FullDB %>% 
  #Removing columns that are not needed for this analysis - DFA
  select(-c(Comment, Stage, Depth)) %>% 
  #Removing empty rows in Family and period
  filter(Family != "" & Period != "") %>% 
  #Keep rows with more than one individual
  filter(N == 1) %>% 
  #Remove any instances where the precision is > 10% of length
  filter(Precision_mm < Length_mm*0.1)

#Binding N=1 data to corrected DOVS data
corrDOVS <- corrDOVS %>% 
  rbind(N1_DOVS)

#Removing variables we no longer need
rm(MLcorr, x, N1_DOVS)


# Tidying up DOVS data -----------------------------------------------------
#Tidying up DOVS data - Saving the clean data set as a different variable - DFA
DOVS <- corrDOVS %>% 
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
  #Remove any accents in the names of sites, before joining with SiteInfo
  mutate(Site = stringi::stri_trans_general(Site, "Latin-ASCII")) %>% 
  #Using inner join so only site names that appear in both dataframes are kept - DFA
  #inner_join(SiteInfo %>% select(-UVC), by = c("Site" = "Site")) %>% 
  #add siteinfo - is innerjoin better than left_join??
  left_join(SiteInfo %>% select(-c(UVC, DOVS, Depth)), by = c("Site" = "Site", "Period" = "Period")) 
#I have decided to keep the site column, so I can remember what the sitecode stands for

#Remove variables that are no longer needed
rm(corrDOVS, MultiLength, DOVS_FullDB)

#Finding Periods that are in the DOVS_FullDB and not in the SiteIfo, meaning they have no site length
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
DOVS <- DOVS %>% filter(!(Site == "Fernandina Punta Espinoza" & Period == "T14"), 
                        !(Site == "Genovesa Norte" & Period == "T10"), 
                        !(Site == "Isabela Este 5" & Period == "T3"), 
                        !(Site == "Banana" & Period == "T9"))

#These are the species that are not in the UVC data which are in the DOVS data
unique(DOVS$ValidName[!(DOVS$ValidName %in% UVC$ValidName)])
#Checking Families that are in DOVS, but not in UVC data
unique(DOVS$Family[!(DOVS$Family %in% UVC$Family)]) #can be deleted after decision on what species to keep

#Keeping only species in DOVS that are in the UVC data
DOVS <- DOVS %>% filter(ValidName %in% SpeciesUVC)


# Species richness boxplot ------------------------------------------------

#Sites in SiteInfo, that do not have clean data in DOVS and UVC
unique(SiteInfo$Site[!SiteInfo$Site %in% DOVS$Site]) #No fish in DOVS
unique(SiteInfo$Site[!SiteInfo$Site %in% UVC$Site]) #No fish in UVC
#Different sites have dissapeared from the DOVS and UVC data after the cleaning process - removing non-predators. 
#These sites are kept as empty sites.

#Making data frame for empty periods (no species) in DOVS data
EmptyPeriods_DOVS <- SiteInfo %>% 
  select(Site, Period, SiteCode, Transect_length_m, Fishing) %>% 
  mutate(SiteInfo_period = paste(Site, Period, sep = " ")) %>% 
  distinct() %>% 
  left_join(DOVS %>% 
              select(Site, Period) %>% 
              mutate(DOVS_period = paste(Site, Period, sep = " ")) %>% 
              distinct(), 
            by = c("Site", "Period")) %>% 
  filter(!DOVS_period %in% SiteInfo_period) %>% #Filter the periods that are not in 
  select(-c(SiteInfo_period, DOVS_period)) %>% 
  mutate(N = 0) %>% #Adding empty abundance column
  mutate(Method = paste("DOVS")) %>% #Adding method column
  mutate(ValidName = NA) #Adding empty species column

#Calculating species richness in DOVS
DOVS_richness <- DOVS %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  rbind(EmptyPeriods_DOVS) %>% #Binding rows with no fish in them
  group_by(Site, Period) %>% 
  mutate(Richness_period = length(unique(na.omit(ValidName)))) %>% #richness per period/transect
  mutate(Transect_area = Transect_length_m*5) %>% #Area of each period/transect
  mutate(Sp_hectare = Richness_period/(Transect_area/(10^4))) %>% #Species/hectare
  group_by(Site, Period, ValidName) %>% 
  summarise(N = sum(N), #Adding up the abundances to get better overview of data frame
            Method, Fishing, SiteCode, Transect_length_m, 
            Richness_period, Transect_area, Sp_hectare) %>% 
  unique() %>% 
  group_by(Site) %>% 
  summarise(Site_sp_hectare = mean(Sp_hectare), #calculating mean species per hectare for each site
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
  mutate(Sp_hectare = Richness_period/(Transect_area/(10^4))) %>% #Species/hectare
  group_by(Site, Period, ValidName) %>% 
  summarise(N = sum(N), #Adding up the abundances to get better overview of data frame
            Method, Fishing, SiteCode, Transect_length_m, 
            Richness_period, Transect_area, Sp_hectare) %>% 
  unique() %>% 
  group_by(Site) %>% 
  summarise(Site_sp_hectare = mean(Sp_hectare), #calculating mean species per hectare for each site
            Method, Fishing, SiteCode) %>% 
  ungroup() %>% 
  unique()

#Combining richness for DOVS and UVC
Richness <- rbind(DOVS_richness, UVC_richness)

#plot species richness
ggplot(Richness, aes(x = Fishing, y = Site_sp_hectare, fill = Method)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = "Number of species"~h^-1) +
  theme_classic()

#Deleting variables that are no longer needed
rm(DOVS_richness, UVC_richness)


# Density boxplot ---------------------------------------------------------

#Calculating density in DOVS for each site per hectare
DOVS_density <- DOVS %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  rbind(EmptyPeriods_DOVS) %>% #Binding rows with no fish in them
  mutate(Transect_area = Transect_length_m*5) %>%  #calculating transect area by multiplying with width 5m
  group_by(Site, Period) %>% 
  summarise(N_period = sum(N), #Sum of abundance of species within each period
            Method, Fishing, SiteCode, Transect_area) %>% 
  unique() %>% 
  mutate(N_hectare = N_period/(Transect_area/(10^4))) %>% #Number/hectare
  group_by(Site) %>% 
  summarise(N_site_hectare = mean(N_hectare), 
            Method, Fishing, SiteCode) %>% 
  unique() %>% 
  ungroup()

#Calculating density in UVC for each site per hectare
UVC_density <- UVC %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  rbind(EmptyPeriods_UVC) %>% #Binding rows with no fish in them
  mutate(Transect_area = Transect_length_m*5) %>%  #calculating transect area by multiplying with width 5m
  group_by(Site, Period) %>% 
  summarise(N_period = sum(N), #Sum of abundance of species within each period
            Method = Method, Fishing = Fishing, SiteCode = SiteCode, Transect_area) %>% 
  unique() %>% 
  mutate(N_hectare = N_period/(Transect_area/(10^4))) %>% #Number/hectare
  group_by(Site) %>% 
  summarise(N_site_hectare = mean(N_hectare), 
            Method, Fishing, SiteCode) %>% 
  unique() %>% 
  ungroup()

#Combining density for DOVS and UVC
Density <- rbind(DOVS_density, UVC_density)

#plot species density
ggplot(Density, aes(x = Fishing, y = N_site_hectare, fill = Method)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = "Number of individuals"~h^-1) +
  theme_classic()

#plotting species density without outlier
test <- Density %>% 
  filter(!(N_site_hectare > 400))

#plot
ggplot(test, aes(x = Fishing, y = N_site_hectare, fill = Method)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = "Number of individuals"~h^-1) +
  theme_classic()
rm(test)

#Deleting variables that are no longer needed
rm(DOVS_density, UVC_density)


# Biomass calculations DOVS ----------------------------------------------------
#Quality Control
#Prior to calculating biomass we need our DOVS measurements to meet two requirements
#1. RMS <= 20
#2. Precision <= 10% estimated Length 
#point 2 has already been done in Cleaning multiple individuals with one measurement
DOVS <- DOVS %>% filter(RMS_mm <= 50) %>% #Because of blurry video RMS is higher than 20, no RMS was higher than 50
  #We also need to change the units of the lengths from mm to cm prior to biomass calculation
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
  #The fish biomass equation is W = a*L^b, therefore first transform the length, 
  #then apply the exponent b and finally multiply by a.
  #The equation is therefore: a*((LenLenRatio*Length_cm)^b)
  mutate(Biomass_N = a*((LenLenRatio*Length_cm)^b)) %>% #Biomass for the number of individuals of each species
  select(Site, Period, Method, ValidName, Fishing, SiteCode, Transect_length_m, Biomass_N) %>% 
  rbind(EmptyPeriods_DOVS) %>% #Binding the periods with no fish in them
  mutate(Transect_area = Transect_length_m*5) %>% #Calculating transect area using transect width 5m
  group_by(Site, Period) %>% 
  #Biomass of all species per period per site AND Calculating tons per hectare as gram/m2 divided by 100
  summarise(Tons_hectare_period = (sum(Biomass_N)/Transect_area)/100,
            Method, Fishing, SiteCode, Transect_area) %>% #In tons per hectare
  unique() %>% 
  group_by(Site) %>% 
  summarise(Tons_hectare_site = mean(Tons_hectare_period), #Calculating average biomass of periods as site biomass
            Method, Fishing, SiteCode) %>% 
  unique()


# Biomass calculations UVC ---------------------------------------------
#Loading total length ratio for FishDB data
TLRatio <- read.xlsx2("Data/TLRatio.xlsx",
                      sheetName = 1, header = TRUE, stringsAsFactors = FALSE) %>% 
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
  #The fish biomass equation is W = a*L^b, therefore first transform the length, 
  #then apply the exponent b and finally multiply by a.
  #The equation is therefore: a*((LenLenRatio*Length_cm)^b)
  mutate(Biomass_N = a*((TLRatio*Length_cm)^b)) %>% #Biomass for the number of individuals of each species
  select(Site, Period, Method, ValidName, Fishing, SiteCode, Transect_length_m, Biomass_N) %>% 
  rbind(EmptyPeriods_UVC) %>% #Binding the periods with no fish in them
  mutate(Transect_area = Transect_length_m*5) %>% #Calculating transect area using transect width 5m
  group_by(Site, Period) %>% 
  #Biomass of all species per period per site AND Calculating tons per hectare as gram/m2 divided by 100
  summarise(Tons_hectare_period = (sum(Biomass_N)/Transect_area)/100,
            Method, Fishing, SiteCode, Transect_area) %>% #In tons per hectare
  unique() %>% 
  group_by(Site) %>% 
  summarise(Tons_hectare_site = mean(Tons_hectare_period), #Calculating average biomass of periods as site biomass
            Method, Fishing, SiteCode) %>% 
  unique()


# Biomass boxplot ---------------------------------------------------------

#Removing EmptyPeriods data frames, as they are no longer needed
rm(EmptyPeriods_DOVS, EmptyPeriods_UVC)

#Combining biomass data for DOVS and UVC
Biomass <- rbind(Biomass_DOVS, Biomass_UVC)


#plot biomass in tons per hectare
ggplot(Biomass, aes(x = Fishing, y = Tons_hectare_site, fill = Method)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = "Tons"~hectare^-1) +
  theme_classic()

#plotting biomass without outlier
test <- Biomass %>% 
  filter(!(Tons_hectare_site > 10))

#plot biomass in tons per hectare
ggplot(test, aes(x = Fishing, y = Tons_hectare_site, fill = Method)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = "Tons"~hectare^-1) +
  theme_classic()
rm(test)

#Deleting variables that are no longer needed
rm(Biomass_DOVS, Biomass_UVC)


# Biomass calculations for PCO ---------------------------------------------

#For these calculations there is no need to make use of the empty periods, as averages are calculated 
#per species, so empty periods are not affecting the average biomass per sp per site
#These calculations will be used for the matrix to do the biomass PCO

#Calculating biomass for DOVS
Biomass_sp_DOVS <- DOVS %>%
  mutate(LenLenRatio = as.numeric(LenLenRatio)) %>% 
  #The fish biomass equation is W = a*L^b, therefore first transform the length, 
  #then apply the exponent b and finally multiply by a.
  #The equation is therefore: a*((LenLenRatio*Length_cm)^b)
  mutate(Biomass_N = a*((LenLenRatio*Length_cm)^b)) %>% #Biomass for the number of individuals of each species
  group_by(Site, Period, ValidName) %>% 
  #Summing biomass for each species in each period, so I have total biomass per species per period
  summarise(Biomass_sp_period = sum(Biomass_N), 
            Transect_length_m, Method, Fishing, SiteCode) %>% 
  unique() %>% 
  mutate(Transect_area = Transect_length_m*5) %>% #Calculating transect area using transect width 5m
  #Calculating tons per hectare as gram/m2 divided by 100
  summarise(Tons_hectare = (Biomass_sp_period/Transect_area)/100, 
            Method, Fishing, SiteCode) %>% 
  group_by(Site, ValidName) %>% 
  #Calculating the biomass of each species per site in tons per hectare (as average of periods)
  summarise(Biomass_site_sp = mean(Tons_hectare), 
            ValidName, Method, Fishing, SiteCode) %>% 
  unique() %>% 
  ungroup()

#Calculating biomass UVC
Biomass_sp_UVC <- UVC %>% 
  #The fish biomass equation is W = a*L^b, therefore first transform the length, 
  #then apply the exponent b and finally multiply by a.
  #The equation is therefore: a*((LenLenRatio*Length_cm)^b)
  mutate(Biomass_N = a*((TLRatio*Length_cm)^b)) %>% 
  group_by(Site, Period, ValidName) %>% 
  #Summing biomass for each species in each period, so I have total biomass per species per site
  summarise(Biomass_sp_period = sum(Biomass_N), 
            Transect_length_m, Method, Fishing, SiteCode) %>% 
  unique() %>% 
  mutate(Transect_area = Transect_length_m*5) %>% #Calculating transect area using transect width 5m
  #Calculating tons per hectare as gram/m2 divided by 100
  summarise(Tons_hectare = (Biomass_sp_period/Transect_area)/100, 
            Method, Fishing, SiteCode) %>% 
  group_by(Site, ValidName) %>% 
  #Calculating the biomass of each species per site in tons per hectare (as average of periods)
  summarise(Biomass_site_sp = mean(Tons_hectare), 
            ValidName, Method, Fishing, SiteCode) %>% 
  unique() %>% 
  ungroup()

#Combine biomass per species data frames
Biomass_sp <- rbind(Biomass_sp_DOVS, Biomass_sp_UVC)


# PCO plot for biomass --------------------------------------

#Making matrix for dissimilarity calculation DOVS and UVC
Bio_mat <- Biomass_sp %>% 
  select(-c(Fishing, SiteCode)) %>% 
  unite(SiteMet, Site, Method, sep = " ") %>% 
  pivot_wider(names_from = "ValidName", values_from = "Biomass_site_sp") %>% #Making the format right for the matrix
  column_to_rownames("SiteMet") %>% #Making a column into row names for the matrix
  as.matrix() %>% 
  replace_na(0)#Putting 0 instead of NA, when the species was not observed at the site.

#Checking QQplot for biomass of both methods
qqnorm(sqrt(sqrt(Biomass_sp$Biomass_site_sp)))
qqline(sqrt(sqrt(Biomass_sp$Biomass_site_sp)), col = "red")

#Applying a 4th root transformation to matrix
Bio_mat <- sqrt(sqrt(Bio_mat))
#Calculating dissimilarity distance using vegan package, the default is Bray Curtis
Bio_mat_dist <- vegdist(Bio_mat, method = "bray")
#Create a PCoA (Principal Co-ordinates Analysis) plot
Bio_mat_pco <- wcmdscale(Bio_mat_dist, eig = TRUE) #returns matrix of scores scaled by eigenvalues
#Show plot
plot(Bio_mat_pco, type = "points") #Add type points to remove labels

# Principal coordinate analysis and simple ordination plot
Bio_mat_pcoa <- pcoa(Bio_mat_dist)
#Biplot with arrows
biplot(Bio_mat_pcoa, Bio_mat)
biplot(Bio_mat_pcoa, Bio_mat, expand = 2)

#binding PCO coordinates to dataframe
PCO_biomass <- as.data.frame(Bio_mat_pco$points[,1:2])
PCO_biomass <- setDT(PCO_biomass, keep.rownames = TRUE)[]
PCO_biomass <- PCO_biomass %>% 
  rename(Site = rn, PC1 = Dim1, PC2 = Dim2) %>% 
  mutate(Method = case_when(endsWith(Site, "DOVS") ~ "DOVS",
                            endsWith(Site, "UVC") ~ "UVC")) %>% #Getting methods from site name
  mutate(Site = str_remove_all(Site, " DOVS| UVC")) %>% #Removing DOVS and UVC from site names
  left_join(Biomass %>% select(Site, Method, Fishing) %>% unique(), 
            by = c("Site", "Method"))

#plotting PCO with methods based on dissimilarity from biomass
ggplot(PCO_biomass, aes(PC1, PC2, col = Method, fill = Method)) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, col = "black") +
  theme_bw() + 
  theme(panel.grid = element_blank())

#plotting PCO with status based on dissimilarity from biomass
ggplot(PCO_biomass, aes(PC1, PC2, col = Fishing, fill = Fishing)) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, col = "black") +
  theme_bw() + 
  theme(panel.grid = element_blank())

#plot with color indicating zonation (open or closed to fishing), shape = method
ggplot(PCO_biomass) + 
  geom_point(aes(PC1, PC2, color = Fishing, shape = Method), size = 2) +
  scale_color_grey() +
  theme_classic()

#Remove unnecessary variables
rm(PCO_biomass, Bio_mat, Bio_mat_pco, Bio_mat_pcoa)


# PERMANOVA biomass ---------------------------------------------------

#Distance matrix from biomass data
Dist_mat <- as.matrix(Bio_mat_dist)

#Sites in SiteInfo, that do not have any fish
unique(SiteInfo$Site[!SiteInfo$Site %in% DOVS$Site]) #No fish in DOVS
unique(SiteInfo$Site[!SiteInfo$Site %in% UVC$Site]) #No fish in UVC

#Data frame for permanova Factors to be tested
Factors <- Biomass %>% 
  right_join(Richness %>% select(Site, Method, Site_sp_hectare), 
             by = c("Site", "Method")) %>% 
  left_join(Density %>% select(Site, Method, N_site_hectare), 
            by = c("Site", "Method")) %>% 
  #Removing empty sites, as they have no dissimilarity values
  filter(!(Tons_hectare_site == 0 & Site_sp_hectare == 0 & N_site_hectare == 0))

#One permanova to test biomass between methods, sites and status (Fishing, open or closed)
adonis(Dist_mat ~ Method/Fishing + Method/Tons_hectare_site + Tons_hectare_site/Fishing,
       data = Factors, permutations = 999)
#I have done it this way because Method is nested within Fishing status (open or closed)
#Method is also nested within each site, with the site data being of biomass
#Lastly, the site is nested within the Fishing status

#Individual Permanovas, I think the right way to do it is to combine them as above
#PERMANOVA fishing
adonis(Dist_mat ~ Method/Fishing, data = Factors, permutations = 9999)
#PERMANOVA biomass
adonis(Dist_mat ~ Method/Biomass_site, data = Factors, permutations = 9999)
#PERMANOVA fishing and biomass
adonis(Dist_mat ~ Biomass_site/Fishing, data = Factors, permutations = 9999)


#Checking for dispersion of groups - Method
dispersion <- betadisper(Bio_mat_dist, group = Factors$Method)
permutest(dispersion)
#Plotting dispersion
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse

#Checking for dispersion of groups - Fishing
dispersion <- betadisper(Bio_mat_dist, group = Factors$Fishing)
permutest(dispersion)
#Plotting dispersion
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse

rm(dispersion, Dist_mat, Bio_mat, Bio_mat_dist)


# PCO plot for density ---------------------------------------------------------

#Calculating average density of each species per site for DOVS
Den_sp_DOVS <- DOVS %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  mutate(Transect_area = Transect_length_m*5) %>%  #calculating transect area by multiplying with width 5m
  group_by(Site, Period, ValidName) %>% 
  summarise(N_period = sum(N), #Sum of abundance of species within each period
            Method, Fishing, SiteCode, Transect_area) %>% 
  unique() %>%
  mutate(N_hectare = N_period/(Transect_area/(10^4))) %>% #Number/hectare
  group_by(Site, ValidName) %>% 
  summarise(N_site_sp = mean(N_hectare), #Calculating average abundance of each species per site
            Method, Fishing, SiteCode, ValidName, N_site_sp) %>% 
  unique()

#Calculating average density of each species per site for UVC
Den_sp_UVC <- UVC %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  mutate(Transect_area = Transect_length_m*5) %>%  #calculating transect area by multiplying with width 5m
  group_by(Site, Period, ValidName) %>% 
  summarise(N_period = sum(N), #Sum of abundance of species within each period
            Method, Fishing, SiteCode, Transect_area) %>% 
  unique() %>%
  mutate(N_hectare = N_period/(Transect_area/(10^4))) %>% #Number/hectare
  group_by(Site, ValidName) %>% 
  summarise(N_site_sp = mean(N_hectare), #Calculating average abundance of each species per site
            Method, Fishing, SiteCode, ValidName, N_site_sp) %>% 
  unique()

#Combining average density for DOVS and UVC and making a matrix
Den_sp <- rbind(Den_sp_DOVS, Den_sp_UVC)
Den_mat <- Den_sp %>% 
  select(-c(Fishing, SiteCode)) %>% 
  unite(SiteMet, Site, Method, sep = " ") %>% 
  pivot_wider(names_from = "ValidName", values_from = "N_site_sp") %>% #Making the format right for the matrix
  column_to_rownames("SiteMet") %>% #Making a column into row names for the matrix
  as.matrix() %>% 
  replace_na(0)#Putting 0 instead of NA, when the species was not observed at the site.

#Removing unnecessary variables
rm(Den_sp_DOVS, Den_sp_UVC)

#Checking QQplot for biomass of both methods
qqnorm(sqrt(sqrt(Den_sp$N_site_sp)))
qqline(sqrt(sqrt(Den_sp$N_site_sp)), col = "red")

#Applying a 4th root transformation to matrix
Den_mat <- sqrt(sqrt(Den_mat))
#Calculating dissimilarity distance using vegan package, the default is Bray Curtis
Den_mat_dist <- vegdist(Den_mat, method = "bray")
#Create a PCoA (Principal Co-ordinates Analysis) plot
Den_mat_pco <- wcmdscale(Den_mat_dist, eig = TRUE) #returns matrix of scores scaled by eigenvalues
#Show plot
plot(Den_mat_pco, type = "points") #Add type points to remove labels

# Principal coordinate analysis and simple ordination plot
Den_mat_pcoa <- pcoa(Den_mat_dist)
#Biplot with arrows
biplot(Den_mat_pcoa, Den_mat)

#binding PCO coordinates to dataframe
PCO_density <- as.data.frame(Den_mat_pco$points[,1:2])
PCO_density <- setDT(PCO_density, keep.rownames = TRUE)[]
PCO_density <- PCO_density %>% 
  rename(Site = rn, PC1 = Dim1, PC2 = Dim2) %>% 
  mutate(Method = case_when(endsWith(Site, "DOVS") ~ "DOVS",
                            endsWith(Site, "UVC") ~ "UVC")) %>% #Getting methods from site name
  mutate(Site = str_remove_all(Site, " DOVS| UVC")) %>% #Removing DOVS and UVC from site names
  left_join(Density %>% select(Site, Method, Fishing) %>% unique(), 
            by = c("Site", "Method"))

#plotting PCO with methods based on dissimilarity from density
ggplot(PCO_density, aes(PC1, PC2, col = Method, fill = Method)) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, col = "black") +
  theme_bw() + 
  theme(panel.grid = element_blank())

#plotting PCO with status based on dissimilarity from density
ggplot(PCO_density, aes(PC1, PC2, col = Fishing, fill = Fishing)) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, col = "black") +
  theme_bw() + 
  theme(panel.grid = element_blank())

#plot with color indicating zonation (open or closed to fishing), shape = method
ggplot(PCO_density) + 
  geom_point(aes(PC1, PC2, color = Fishing, shape = Method), size = 2) +
  scale_color_grey() +
  theme_classic()

#Remove unnecessary variables
rm(PCO_density, Den_sp, Den_mat, Den_mat_pco, Den_mat_pcoa)


# PERMANOVA density and species richness -------------------------------------------------------

#Distance matrix from biomass data
Dist_mat <- as.matrix(Den_mat_dist)

#One permanova to test biomass between methods, sites and status (Fishing, open or closed)
adonis(Dist_mat ~ Method/Fishing + Method/N_site_hectare + N_site_hectare/Fishing,
       data = Factors, permutations = 999)
#I have done it this way because Method is nested within Fishing status (open or closed)
#Method is also nested within each site, with the site data being of biomass
#Lastly, the site is nested within the Fishing status

#Checking for dispersion of groups - Method
dispersion <- betadisper(Den_mat_dist, group = Factors$Method)
permutest(dispersion)
#Plotting dispersion
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse

#Checking for dispersion of groups - Fishing
dispersion <- betadisper(Den_mat_dist, group = Factors$Fishing)
permutest(dispersion)
#Plotting dispersion
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse


### Species Richness ###
#For species richness PERMANOVA the same distance matrix is used as for abundance

#One permanova to test biomass between methods, sites and status (Fishing, open or closed)
adonis(Dist_mat ~ Method/Fishing + Method/Site_sp_hectare + Site_sp_hectare/Fishing,
       data = Factors, permutations = 999)
#I have done it this way because Method is nested within Fishing status (open or closed)
#Method is also nested within each site, with the site data being of biomass
#Lastly, the site is nested within the Fishing status

#The dispersion for the PERMANOVA for species richness will be the same as for density, 
#because the same distance matrix is used. 


#Removing unnecessary variables
rm(dispersion, Dist_mat, Den_mat_dist, Factors)


# PCO plot for biomass with numbers ---------------------------------------

#Adding number instead of site
Number <- Biomass_sp %>% 
  select(Site) %>% 
  unique() %>% 
  mutate(SiteNumber = replace(Site, Site == Site, 1:73))
Biomass_sp <- Biomass_sp %>% 
  left_join(Number, by = "Site") %>% 
  mutate(Method = recode(Method, 
                         "DOVS" = "D", 
                         "UVC" = "U"))
rm(Number)

#Making matrix for dissimilarity calculation DOVS and UVC
Bio_mat <- Biomass_sp %>% 
  select(-c(Fishing, SiteCode, Site)) %>%
  unite(SiteMet, SiteNumber, Method, sep = " ") %>% 
  pivot_wider(names_from = "ValidName", values_from = "Biomass_site_sp") %>% #Making the format right for the matrix
  column_to_rownames("SiteMet") %>% #Making a column into row names for the matrix
  as.matrix() %>% 
  replace_na(0)#Putting 0 instead of NA, when the species was not observed at the site.

#Applying a 4th root transformation to matrix
Bio_mat <- sqrt(sqrt(Bio_mat))
#Calculating dissimilarity distance using vegan package, the default is Bray Curtis
Bio_mat_dist <- vegdist(Bio_mat, method = "bray")
#Create a PCoA (Principal Co-ordinates Analysis) plot
Bio_mat_pco <- wcmdscale(Bio_mat_dist, eig = TRUE) #returns matrix of scores scaled by eigenvalues
#Show plot
plot(Bio_mat_pco, type = "points") #Add type points to remove labels

# Principal coordinate analysis and simple ordination plot
Bio_mat_pcoa <- pcoa(Bio_mat_dist)
#Biplot with arrows
biplot(Bio_mat_pcoa, Bio_mat)
biplot(Bio_mat_pcoa, Bio_mat, expand = 30)

#Remove unnecessary variables
rm(PCO_biomass, Bio_mat, Bio_mat_pco, Bio_mat_pcoa, Bio_mat_dist)

# Notes --------------------------------------------

#Species richness can be calculated with vegan::specnumber() on the density matrix
#While vegan::diversity() will allow you to calculate diversity indices



