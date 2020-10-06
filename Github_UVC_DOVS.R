###################################################################################################################
# Title: Github data cleaning and analysis UVC and DOVS Galapagos
# Author: Sara Færch Hansen
# Assisting: Denisse Fierro Arcos
# Version: 1
# Date last updated: 2020-09-24
# Aim: Compare UVC and DOVS data in the Galapagos related to the Master thesis of Sara Færch Hansen
###################################################################################################################

# Uploading libraries -----------------------------------------------------
library(tidyverse) 
library(data.table)
library(vegan)
library(chron)
library(ggplot2)

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
                                             "Luz de día" = "Luz de Día",))
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
#Removing variables we no longer need
rm(MLcorr, x)


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


# Richness & Density calculations ----------------------------------------------------

library(ggplot2)

#Calculating species richness in DOVS
DOVS_richness <- DOVS %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  mutate(Site_area = Transect_length_m*5) %>% #Each site is 5m wide, calculating area in square meters
  group_by(Site, Period) %>% 
  mutate(Richness = length(unique(ValidName))) %>% #Richness of species per site, per period (replicate)
  mutate(Richness_area = Richness/(Site_area/100)) #Richness per 100 square meters

#Calculating species richness in UVC
UVC_richness <- UVC %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>%
  mutate(Site_area = Transect_length_m*5) %>% #Each site is 5m wide, calculating area in square meters
  group_by(Site, Period) %>% 
  mutate(Richness = length(unique(ValidName))) %>% #Richness of species per site, per period (replicate)
  mutate(Richness_area = Richness/(Site_area/100)) #Richness per 100 square meters

Richness <- rbind(DOVS_richness, UVC_richness)

#plot species richness
ggplot(Richness, aes(x = Fishing, y = Richness_area, fill = Method)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = "Number of species/100"~m^2) +
  theme_classic()

#Deleting variables that are no longer needed
rm(DOVS_richness, UVC_richness)


#Calculating density in DOVS
DOVS_density <- DOVS %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  mutate(Site_area = Transect_length_m*5) %>% #Each site is 5m wide, calculating area in square meters
  group_by(Site, Period) %>% 
  mutate(Abundance = sum(N)) %>% #Abundance of all species per site, per period (replicate)
  mutate(Abundance_area = Abundance/(Site_area/100)) #Richness per 100 square meters

UVC_density <- UVC %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  mutate(Site_area = Transect_length_m*5) %>% #Each site is 5m wide, calculating area in square meters
  group_by(Site, Period) %>% 
  mutate(Abundance = sum(N)) %>% #Abundance of all species per site, per period (replicate)
  mutate(Abundance_area = Abundance/(Site_area/100)) #Richness per 100 square meters

Density <- rbind(DOVS_density, UVC_density)

#plot species abundance
ggplot(Density, aes(x = Fishing, y = Abundance_area, fill = Method)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = "Number of individuals/100"~m^2) +
  theme_classic()

#Deleting variables that are no longer needed
rm(DOVS_density, UVC_density)


# Biomass calculations DOVS ----------------------------------------------------
#Quality Control
#Prior to calculating biomass we need our DOVS measurements to meet two requirements
#1. RMS <= 20
#2. Precision <= 10% estimated Length
DOVS <- DOVS %>% filter(RMS_mm <= 20) %>% 
  filter(Precision_mm <= Length_mm*.1) %>% 
  #We also need to change the units of the lengths from mm to cm prior to biomass calculation
  mutate(Length_mm = Length_mm/10) %>% 
  #Now we rename the column to avoid confusion
  rename("Length_cm"="Length_mm") %>% 
  #We will drop columns we do not need
  select(-c(Precision_mm, RMS_mm, Range_mm))

#DFA comments - I do not understand what you are trying to achieve in this section. You
#are making calculations and deleting them? What is the purpose of that? Variables are
#not even saved.

#Calculate the biomass for DOVS using FishDB a and b values for FL
str(DOVS) #LenLenRatio is character and needs to be numeric
Biomass_DOVS <- DOVS %>% 
  mutate(LenLenRatio = as.numeric(LenLenRatio)) %>% 
  #The fish biomass equation is W = a*L^b, therefore first transform the length, 
  #then apply the exponent b and finally multiply by a.
  #The equation is therefore: a*((LenLenRatio*Length_cm)^b)
  mutate(Biomass_N = a*((LenLenRatio*Length_cm)^b))

#Calculating mean biomass for each family at each site
mean_bio_family <- Biomass_DOVS %>% 
  filter(!endsWith(ValidName, " sp")) %>% #removing rows only identified to genus or family level
  group_by(SiteName, Family) %>% 
  #calculating mean biomass for each family at each site
  summarise(mean_bio_family = mean(Biomass_N))
#REMEMBER!!!!!!! Do NOT use mutate to perform calculations based on groups, such as this one
#Instead use summarise(). Mutate is only good if you do a line by line calculation.
#Fix the code below based on example above.

#Finding the mean biomass for the family balistidae at site Arrecife Antiguo
#as this is where we have the two individuals identified as "Balistidae sp"
mean_bio_family[17,] # 1138.24431
#DFA responds: I do not understand what you mean

#Calculating mean biomass for each genus at each site
mean_bio_genus <- Biomass_DOVS %>% 
  filter(!endsWith(ValidName, " sp")) %>% #removing rows only identified to genus or family level
  group_by(Family, Genus) %>% 
  mutate(mean_all_sites = mean(Biomass_N)) %>% #3 sites do not have any scarus except "Scarus sp", 
  #therefore I calculate a value for mean biomass of each genus based on all sites
  group_by(Site, Family, Genus) %>% 
  mutate(mean_bio_genus = mean(Biomass_N)) %>% #calculating mean biomass for each genus at each site
  select(-c(Length_cm, N, Species, ValidName, a, b, LengthType, LenLenRatio)) %>% 
  distinct() %>% 
  ungroup(Site, Family, Genus)
mean_bio_genus[71,] # 4732.50971, Lutjanus sp in Cabo Marshall
mean_bio_genus[152,] # 313.61883, Lutjanus sp in Isabella Esta 8
mean_bio_genus %>% filter(Genus == "Scarus")


#What is this for? It really does not make sense to me. Why would you change biomass values 
#manually? - DFA
Biomass_DOVS <- Biomass_DOVS %>% 
  mutate(Biomass_N = case_when(ValidName == "Balistidae sp" & Site == "Arrecife Antiguo" ~ 1138.24431,
                               ValidName == "Lutjanus sp" & Site == "Cabo Marshall" ~ 4732.50971,
                               ValidName == "Lutjanus sp" & Site == "Isabela Este 8" ~ 313.61883, 
                               ValidName == "Scarus sp" & Site == "Caleta Iguana" ~ 852.0596,
                               ValidName == "Scarus sp" & Site == "Isabela Alcedo" ~ 1126.2327,
                               ValidName == "Scarus sp" & Site == "Isabela Este 6" ~ 1064.1793,
                               ValidName == "Scarus sp" & Site == "Isla Lobos" ~ 2223.2636,
                               ValidName == "Scarus sp" & Site == "León Dormido" ~ 2488.1143,
                               ValidName == "Scarus sp" & Site == "Marchena Norte" ~ 968.8443,
                               ValidName == "Scarus sp" & Site == "Punta Albemarle" ~ 3432.8070,
                               ValidName == "Scarus sp" & Site == "Punta Espejo" ~ 1955.1796, 
                               ValidName == "Scarus sp" & Site == "Punta Pitt" ~ 1820.0815,
                               ValidName == "Scarus sp" & Site == "Roca Unión" ~ 1315.8535,
                               ValidName == "Scarus sp" & Site == "Seymour Norte" ~ 863.7705,
                               ValidName == "Scarus sp" & Site == "Al Arco (Cleaning Station)" ~ 1593.466,
                               ValidName == "Scarus sp" & Site == "Isabela Sur" ~ 1593.466,
                               ValidName == "Scarus sp" & Site == "Tagus Norte" ~ 1593.466,
                               TRUE ~ Biomass_N)) %>% 
  filter(!str_detect(ValidName, "Gerreidae sp"))

#Remove mean_bio_family and mean_bio_genus, as they are no longer needed
rm(mean_bio_family)
rm(mean_bio_genus)

#summing biomass for each species in Biomass_sp
Biomass_DOVS <- Biomass_DOVS %>% 
  group_by(Site, Method, ValidName) %>% 
  summarise(Biomass_sp = sum(Biomass_N)) %>%
  ungroup(Site, ValidName)

#Making matrix for dissimilarity calculation
Biomass_DOVS_mat <- Biomass_DOVS %>% 
  unite(SiteMet, Site, Method, sep = " ")

any(is.na(Biomass_DOVS_mat)) #Now there is no NA's because the individuals with sp now have biomass value

#Is the above the correct way to get mean values for biomass for species identified to Family and Genus level?
#I got mean for the sites where the "sp" ID's were from, and when there were no other species from the Family
#or Genus at the site, I used the mean for that Genus (scarus) across all sites.

#Making matrix
DOVS_mat <- DOVS_mat %>% 
  pivot_wider(names_from = "ValidName", values_from = "Biomass_sp") %>% 
  column_to_rownames("SiteMet") %>%
  as.matrix() %>% 
  replace_na(0) #Putting 0 instead of NA, when the species was not observed at the site.

any(is.na(Biomass_DOVS_mat)) #no NA's


# Biomass calculations UVC ---------------------------------------------


#Calculating biomass for UVC
Biomass_UVC <- UVC %>% select(Site, Length_cm, N, Method, ValidName, a, b, 
                              LengthType, TLRatio) %>% 
  #The fish biomass equation is W = a*L^b, therefore first transform the length, 
  #then apply the exponent b and finally multiply by a.
  #The equation is therefore: a*((TLRatio*Length_cm)^b)
  mutate(Biomass_N = a*((TLRatio*Length_cm)^b))

#summing biomass for each species in the UVC data in Biomass_sp
Biomass_UVC <- Biomass_UVC %>% distinct() %>% 
  group_by(Site, Method, ValidName) %>% 
  summarise(Biomass_sp = sum(Biomass_N)) %>% 
  ungroup(Site, Method, ValidName)

#Making matrix for UVC biomass data
Biomass_UVC_mat <- Biomass_UVC %>% 
  unite(SiteMet, Site, Method, sep = " ") %>% 
  pivot_wider(names_from = "ValidName", values_from = "Biomass_sp") %>% 
  column_to_rownames("SiteMet") %>% 
  as.matrix() %>% 
  replace_na(0)

#Biomass both methods
biomass <- rbind(Biomass_DOVS, Biomass_UVC) %>% 
  unite(SiteMet, Site, Method, sep = " ")

x <- biomass %>% 
  pivot_wider(names_from = "ValidName", values_from = "Biomass_sp") %>% 
  column_to_rownames("SiteMet") %>% 
  as.matrix() %>% 
  replace_na(0)

#Producing qq-plot for biomass of DOVS, to see effect of transformation
qqnorm(Biomass_DOVS$Biomass_sp^(1/4))
qqline(Biomass_DOVS$Biomass_sp^(1/4), col = "red")
qqnorm(Biomass_UVC$Biomass_sp^(1/4))
qqline(Biomass_UVC$Biomass_sp^(1/4), col = "red")

#Applying a 4th root transformation to matrix
x <- x^(1/4)
#Calculating dissimilarity distance using vegan package, the default is Bray Curtis
y <- vegdist(x, method = "bray")
#Create a PCoA (Principal Co-ordinates Analysis) plot
z <- wcmdscale(y, eig = T)
#Show plot
plot(z, type = "points") #Add type points to remove labels

#binding PCO coordinates to dataframe
pco1 <- as.data.frame(z$points[,1:2])
pco1 <- setDT(pco1, keep.rownames = TRUE)[]
pco1 <- pco1 %>% rename(Sites = rn, PC1 = Dim1, PC2 = Dim2)
pco2 <- pco1 %>% mutate(Method = case_when(endsWith(Sites, "DOVS") ~ "DOVS",
                                           endsWith(Sites, "UVC") ~ "UVC"))
#plotting PCO with methods
ggplot(pco2, aes(PC1, PC2, col = Method, fill = Method)) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, col = "black") +
  theme_bw() + 
  theme(panel.grid = element_blank())

#DFA comments: Before we are able to make any comparisons in biomass results, we need to
#ensure biomass was measured within a given area. If we do not consider the area, and we
#happen to sample a longer transect in UVCs than in DOVS, we will get a difference simply
#because our data was obtained for a different total area, and not necessarily because
#there is an actual difference between methods.

#In regards to your code, there is no need to create so many intermediate variables. This
#is when the tidyverse becomes handy. You can do whatever calculations or manipulation you
#need and just save the end result you are after. There is no point in making temporary
#variables if you will delete them later. It is often confusing to have intermediate 
#variables and also use up your memory. Avoid whenever possible.

#Name your variables something useful. Having variables named x, y, z is not advisable
#unless they are temporary only.

#Make sure you comment your code. This is really handy not only for you when you come back
#to check it after months of not working on this project, but also for other people who 
#may want to use it. Plus, it is always better to have nicely commented code on your 
#portfolio.

#Finally, check your output. I cannot stress this enough. You need to know if what you are
#trying to do with your code is what you actually get. The biomass calculation for example,
#results were less than 1 gram for sharks of over 2 m. If results do not make sense, then 
#you know a mistake has been made and a correction is needed.

######################### Dummy data, until I have UVC data #############################
#Dummy_data to plot two different data sets
Dummy_data <- Biomass_DOVS_mat
Dummy_data$SiteMet <- Dummy_data$SiteMet %>% str_replace_all("DOVS", "Dummy")
#Dropping rows with NA values and deleting duplicates
Dummy_data <- Dummy_data %>% drop_na() %>% 
  filter(!duplicated(.))
Dummy_data$Biomass_sp <- sample(0:1000, nrow(Dummy_data))

#Making matrix dummy_data
Dummy_mat <- Dummy_data %>% 
  pivot_wider(names_from = "ValidName", values_from = "Biomass_sp") %>% 
  column_to_rownames("SiteMet") %>%
  as.matrix() %>% 
  replace_na(0)

#Combining DOVS and dummy_data
DOVS_dummy <- rbind(DOVS_mat, Dummy_mat)

#Producing qq-plot for biomass of DOVS, to see effect of transformation
qqnorm(Biomass_DOVS$Biomass_N^(1/4))
qqline(Biomass_DOVS$Biomass_N^(1/4), col = "red")
qqnorm(Biomass_DOVS$Biomass_sp^(1/4))
qqline(Biomass_DOVS$Biomass_sp^(1/4), col = "red")

#Applying a 4th root transformation to matrix
x <- DOVS_dummy^(1/4)
#Calculating dissimilarity distance using vegan package, the default is Bray Curtis
y <- vegdist(x, method = "bray")
#Create a PCoA (Principal Co-ordinates Analysis) plot
z <- wcmdscale(y, eig = T)
#Show plot
plot(z, type = "points") #Add type points to remove labels

#binding PCO coordinates to dataframe
pco1 <- as.data.frame(z$points[,1:2])
pco1 <- setDT(pco1, keep.rownames = TRUE)[]
pco1 <- pco1 %>% rename(Sites = rn, PC1 = Dim1, PC2 = Dim2)
pco2 <- pco1 %>% mutate(Method = case_when(endsWith(Sites, "DOVS") ~ "DOVS",
                                           endsWith(Sites, "Dummy") ~ "Dummy"))
#plotting PCO with methods
ggplot(pco2, aes(PC1, PC2, col = Method, fill = Method)) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, col = "black") +
  theme_bw() + 
  theme(panel.grid = element_blank())

################################################################################################

#Now I only need to calculate the biomass for the UVC data, so that I can add these to the plot


# Species Richness calculation --------------------------------------------

#Species richness can be calculated with vegan::specnumber() on the density matrix
#While vegan::diversity() will allow you to calculate diversity indices

#Species richness for DOVS sites
DOVS_richness <- DOVS %>% 
  select(Site, Method, ValidName) %>% 
  distinct() %>% 
  group_by(Site, Method) %>% 
  summarise(richness = n()) %>% 
  ungroup(Site, Method)

#Species richness for UVC sites
UVC_richness <- UVC_clean %>% 
  select(Site, Method, ValidName) %>% 
  distinct() %>% 
  group_by(Site, Method) %>% 
  summarise(richness = n()) %>% 
  ungroup(Site, Method)

#old species richness calculations
#Making abundance matrix
Abun_DOVS <- DOVS %>%
  group_by(Site, ValidName, Method) %>%
  summarise(N_sum = sum(N)) %>% 
  ungroup(Site, ValidName, Method) %>% 
  unite(SiteMet, Site, Method, sep = " ") %>%
  distinct()



Abun_DOVS_mat <- Abun_DOVS %>%
  pivot_wider(names_from = "ValidName", values_from = "N_sum") %>% 
  column_to_rownames("SiteMet") %>%
  as.matrix() %>% 
  replace_na(0)

#Species richness for sites
DOVS_richness <- specnumber(Abun_DOVS_mat)

#species accumulation curve
specaccum(Abun_DOVS_mat)
plot(spa_DOVS)

#Dissimilarity matrix for Abundance DOVS
Abun_DOVS_mat2 <- vegdist(Abun_DOVS_mat, method = "bray")
PCO_abun <- wcmdscale(Abun_DOVS_mat2, eig = T)
plot(PCO_abun)

a <- as.data.frame(PCO_abun$points[,1:2]) 
a <- setDT(a, keep.rownames = TRUE)[]
a <- a %>% rename(Sites = rn, PC1 = Dim1, PC2 = Dim2) %>% 
  mutate(Method = case_when(endsWith(Sites, "DOVS") ~ "DOVS"))

ggplot(a, aes(PC1, PC2, col = Method, fill = Method)) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, col = "black") +
  theme_bw() + 
  theme(panel.grid = element_blank())

# Notes --------------------------------------------

#Species richness can be calculated with vegan::specnumber() on the density matrix
#While vegan::diversity() will allow you to calculate diversity indices

#Species richness for DOVS sites
DOVS_richness <- DOVS %>% 
  select(Site, Method, ValidName) %>% 
  distinct() %>% 
  group_by(Site, Method) %>% 
  summarise(richness = n()) %>% 
  ungroup(Site, Method)

#Species richness for UVC sites
UVC_richness <- UVC_clean %>% 
  select(Site, Method, ValidName) %>% 
  distinct() %>% 
  group_by(Site, Method) %>% 
  summarise(richness = n()) %>% 
  ungroup(Site, Method)

# OLD PCO and dissimilarity calculations

#Applying a 4th root transformation to matrix
x <- DOVS_mat^(1/4)
#Calculating dissimilarity distance using vegan package, the default is Bray Curtis
y <- vegdist(x, method = "bray")
#Create a PCoA (Principal Co-ordinates Analysis) plot
z <- wcmdscale(y, eig = T)
#Show plot
plot(z)


# NMDS plot for sites and species ------------------------------------------

##########Things to consider
#Remember that we need to compare densities, so the abundance values will need to be
#divided by the area under study

#Assigning columns with species names and abundance from DOVS
Abun_DOVS <- subset(DOVS, select = c("Site", "SpeciesName", "N", "Method"))
#Combine site and methods column
Abun_DOVS$SiteMet <- paste(Abun_DOVS$Site, Abun_DOVS$Method)
Abun_DOVS <- Abun_DOVS %>% select(-c(Site, Method))
#Summing abundance of each species per site
Abun_DOVS <- Abun_DOVS %>%
  group_by(SiteMet, SpeciesName) %>%
  summarise(Abundance = sum(N, na.rm = T)) %>% 
  ungroup()

#Assigning columns with species names and abundance from UVC
Abun_UVC <- subset (UVC, select = c("Site", "SpeciesName", "N", "Method"))
#Combine site and methods column
Abun_UVC$SiteMet <- paste(Abun_UVC$Site, Abun_UVC$Method)
Abun_UVC <- Abun_UVC %>% select(-c(Site, Method))
#Summing abundance of each species per site
Abun_UVC <- Abun_UVC %>%
  group_by(SiteMet, SpeciesName) %>%
  summarise(Abundance = sum(N, na.rm = T)) %>% 
  ungroup()

#Combining DOVS and UVC data
Abun <- rbind(Abun_DOVS, Abun_UVC)

#Making matrix to enable NMDS plot
Abun_mat <- Abun %>%
  pivot_wider(names_from = "SpeciesName", values_from = "Abundance") %>%
  #Use the Site column as row names
  column_to_rownames("SiteMet") %>%
  #Turn this tibble into a matrix for further processing
  as.matrix() %>% 
  #replacing NA values with zero
  replace_na(0)

# Multivariate analysis ---------------------------------------------------

#Applying a 4th root transformation to matrix
x <- sqrt(sqrt(Abun_mat))
#Calculating dissimilarity distance using vegan package, the default is Bray Curtis
y <- vegdist(x, method="bray")
#Create a PCoA (Principal Co-ordinates Analysis) plot
z <- wcmdscale(y, eig = T)
#Show plot
plot(z)

#NMDS plot on transformed abundance data
NMDS <- metaMDS(x)
stressplot(NMDS)
treat = c(rep("DOVS", 78), rep("UVC",68))
ordiplot(NMDS, type = "n")
ordihull(NMDS, groups = treat, draw = "polygon", col = c("green", "blue"), label = F)
orditorp(NMDS, display = "sites", col = c(rep("green",78), rep("blue",68)), 
         air = 0.01, cex = 1.25)
#Maybe this can be done in a nicer way? 
#No matter what it seems like the two treatments DOVS and UVC are overlapping in their abundances.

#DFA answers: If they overlap, it simply means that there is no difference in what you are
#plotting. If you want a statistical test, check PERMANOVA and PERMDISP.

#Looking at what species I need to make a TLRatio for to calculate biomass for the UVC data
sp_UVC <- UVC_clean %>% select(SpeciesName, ValidName, a, b, LengthType, LenLenRatio) %>% 
  distinct()

write_excel_csv2(sp_UVC, "sp_UVC.csv")



