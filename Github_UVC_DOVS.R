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
library(ggsignif) #Shows level of significance in ggplots
library(pairwiseAdonis) #pairwise adonis
library(ggpubr) #ggarrange, for arranging graphs


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
#Correcting two Max length typos in the database
FishDB <- FishDB %>% 
  mutate(MaxLgth_m = ifelse(ValidName == "Mobula birostris" & MaxLgth_m == 0.91, 9.1, MaxLgth_m)) %>% 
  mutate(MaxLgth_m = ifelse(ValidName == "Taeniurops meyeni" & MaxLgth_m == 0.33, 3.3, MaxLgth_m))

#Loading site keys excel file
SiteKeys <- openxlsx::read.xlsx("Data/SiteKeys.xlsx")  %>%
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
         !Site == "Santiago Suroeste") %>% 
  mutate(Bioregion = recode(Bioregion, 
                            "Centro-sur" = "Centro Sur", 
                            "Centro sur" = "Centro Sur", 
                            "Oeste fria" = "Oeste Fria"))

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
  #Remove any non-fish species
  filter(!ValidName %in% NonFish) %>% 
  select(-c(Island)) %>% #Island will be added when SiteInfo is joined
  #Add T to Transect_code and renaming to period, so it is similar to the column in SiteInfo
  mutate(Transect_code = as.character(paste("T", Transect_code, sep = ""))) %>% 
  rename(Period = Transect_code) %>% 
  #Remove any accents in the names of sites, before joining with SiteInfo
  mutate(Site = stringi::stri_trans_general(Site, "Latin-ASCII")) %>% 
  #Adding SiteInfo to the sites in the UVC data
  left_join(SiteInfo %>% select(-c(DOVS, UVC, Date, Year, Month, Bioregion, )), 
            by = c("Site" = "Site", "Period" = "Period")) %>% 
  #Going from sizeclass to length of fish in cm
  #renaming the sizeclass that is smaller than 30 cm, and longer than 300 cm.
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


# Tidying up DOVS data -----------------------------------------------------
#Tidying up DOVS data - Saving the clean data set as a different variable - DFA
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
  #Remove any accents in the names of sites, before joining with SiteInfo
  mutate(Site = stringi::stri_trans_general(Site, "Latin-ASCII")) %>% 
  #Using inner join so only site names that appear in both dataframes are kept - DFA
  #inner_join(SiteInfo %>% select(-UVC), by = c("Site" = "Site")) %>% 
  #add siteinfo - is innerjoin better than left_join??
  left_join(SiteInfo %>% select(-c(UVC, DOVS, Depth)), by = c("Site" = "Site", "Period" = "Period")) 
  #I have decided to keep the site column, so I can remember what the sitecode stands for

#Remove variables that are no longer needed
rm(DOVS_FullDB)

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
#I suggest that we add the following carnivores: 
#Lutjanus jordani, Caranx sexfasciatus and Epinephelus labriformis



# Length check ------------------------------------------------------------

#Identifying individuals that are too large in UVC data
TooLarge_UVC <- UVC %>% #Using data from UVC
  left_join(FishDB %>% select(ValidName, MaxLgth_m) %>% unique(), #Joining max length and Trophic category
            by = "ValidName") %>% 
  mutate(MaxLgth_m = MaxLgth_m*100) %>% #Max length to cm
  rename(MaxLgth_cm = MaxLgth_m) %>% #renaming column to cm
  filter(Length_cm > MaxLgth_cm) %>% 
  select(Period, Site, ValidName, Length_cm, N, MaxLgth_cm)

#Checking species against lengths in UVC data and removing individuals that are too large
UVC <- UVC %>% #Using data from UVC
  left_join(FishDB %>% select(ValidName, MaxLgth_m) %>% unique(), #Joining max length and Trophic category 
            by = "ValidName") %>% 
  mutate(MaxLgth_m = MaxLgth_m*100) %>% #Max length to cm
  rename(MaxLgth_cm = MaxLgth_m) %>% #renaming column to cm
  filter(!Length_cm > MaxLgth_cm) %>% 
  select(-MaxLgth_cm)


#Identifying individuals that are too large in DOVS data
TooLarge_DOVS <- DOVS %>% #Using data from DOVS
  left_join(FishDB %>% select(ValidName, MaxLgth_m) %>% unique(), #Joining max length
            by = "ValidName") %>% 
  mutate(MaxLgth_m = MaxLgth_m*1000) %>% #Max length to mm
  rename(MaxLgth_mm = MaxLgth_m) %>% #renaming column to mm
  filter(Length_mm > MaxLgth_mm) %>% 
  select(Period, Site, ValidName, Length_mm, N, MaxLgth_mm, Precision_mm, RMS_mm)

#Checking species against lengths in DOVS data and removing individuals that are too large
DOVS <- DOVS %>% #Using data from DOVS
  left_join(FishDB %>% select(ValidName, MaxLgth_m) %>% unique(), #Joining max length
            by = "ValidName") %>% 
  mutate(MaxLgth_m = MaxLgth_m*1000) %>% #Max length to mm
  rename(MaxLgth_mm = MaxLgth_m) %>% #renaming column to mm
  filter(is.na(Length_mm) | !Length_mm > MaxLgth_mm) %>% 
  select(-MaxLgth_mm)

#Remove unnecessary variables
rm(TooLarge_DOVS, TooLarge_UVC)


# Species richness boxplot ------------------------------------------------

#Sites in SiteInfo, that do not have clean data in DOVS and UVC
unique(SiteInfo$Site[!SiteInfo$Site %in% DOVS$Site]) #No fish in DOVS
unique(SiteInfo$Site[!SiteInfo$Site %in% UVC$Site]) #No fish in UVC
#Different sites have disappeared from the DOVS and UVC data after the cleaning process - removing non-predators. 
#These sites are kept as empty sites.

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
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  rbind(EmptyPeriods_DOVS) %>% #Binding rows with no fish in them
  group_by(Site, Period) %>% 
  mutate(Richness_period = length(unique(na.omit(ValidName)))) %>% #richness per period/transect
  mutate(Transect_area = Transect_length_m*5) %>% #Area of each period/transect in m2
  mutate(Sp_500m2 = (Richness_period/Transect_area)*500) %>% #Species/500m2
  #Removing abundance and species names, as they are not used for species richness calculations
  select(-c(N, ValidName)) %>% 
  unique() %>% 
  group_by(Site) %>% 
  summarise(Site_sp_500m2 = mean(Sp_500m2), #calculating mean species per 500m2 for each site
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
  mutate(Sp_500m2 = (Richness_period/Transect_area)*500) %>% #Species/500m2
  #Removing abundance and species names, as they are not used for species richness calculations
  select(-c(N, ValidName)) %>% 
  unique() %>% 
  group_by(Site) %>% 
  summarise(Site_sp_500m2 = mean(Sp_500m2), #calculating mean species per 500m2 for each site
            Method, Fishing, SiteCode) %>% 
  ungroup() %>% 
  unique()

#Combining richness for DOVS and UVC
Richness <- rbind(DOVS_richness, UVC_richness)

#Making column for significance comparison
TempRichness <- Richness %>% 
  mutate(Comparison = paste(Fishing, Method))

#Plot species richness with significance
#Univariate permanova - DFA
adonis(Site_sp_500m2 ~ Fishing*Method, data = Richness, permutations = 9999, method = "euclidean")

Ric_boxplot  <- ggplot(Richness, aes(x = Fishing, y = Site_sp_500m2, fill = Method)) +
  geom_boxplot(fatten = 3) +
  ggtitle("Total richness per 500"~m^2) +
  geom_signif(comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test = "wilcox.test", 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE)) +
  scale_fill_manual(name = "Method", labels = c("Stereo-DOVs", "UVC"), 
                    values = grey.colors(2, start = 0.1, end = 0.5)) +
  scale_y_continuous(name = "Number of species/500"~m^2) +
  scale_x_discrete(labels = c("No-take zone", "Fishing zone")) +
  theme_classic() +
  theme(#aspect.ratio = 1.5, 
        plot.title = element_text(color="black", face="bold", hjust = 0.5),
        legend.title = element_text(color = "black"), 
        legend.text = element_text(color = "black"), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black")) +
  coord_cartesian(clip = "off")

Ric_boxplot

#plot for significance between methods
ggplot(TempRichness, aes(x = Comparison, y = Site_sp_500m2, fill = Comparison)) +
  geom_boxplot(fatten = 3) +
  geom_signif(comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test = "wilcox.test", 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE)) +
  theme_classic()

#Deleting variables that are no longer needed
rm(DOVS_richness, UVC_richness, TempRichness)


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
  mutate(N_500m2 = (N_period/Transect_area)*500) %>% #Number/500m2
  group_by(Site) %>% 
  summarise(N_site_500m2 = mean(N_500m2), 
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
  mutate(N_500m2 = (N_period/Transect_area)*500) %>% #Number/500m2
  group_by(Site) %>% 
  summarise(N_site_500m2 = mean(N_500m2), 
            Method, Fishing, SiteCode) %>% 
  unique() %>% 
  ungroup()

#Combining density for DOVS and UVC
Density <- rbind(DOVS_density, UVC_density)

#Making column for significance comparison
TempDensity <- Density %>% 
  mutate(Comparison = paste(Fishing, Method))

#Plot density with significance
Den_boxplot <- ggplot(Density, aes(x = Fishing, y = N_site_500m2, fill = Method)) +
  geom_boxplot(fatten = 3) +
  ggtitle("Total density per 500"~m^2) +
  geom_signif(comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test = "wilcox.test",
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE)) +
  scale_fill_manual(name = "Method", labels = c("Stereo-DOVs", "UVC"), 
                    values = grey.colors(2, start = 0.1, end = 0.5)) +
  scale_y_continuous(name = "Number of individuals/500"~m^2) +
  scale_x_discrete(labels = c("No-take zone", "Fishing zone")) +
  theme_classic() +
  theme(#aspect.ratio = 1.5, 
        plot.title = element_text(color="black", face="bold", hjust = 0.5),
        legend.title = element_text(color = "black"), 
        legend.text = element_text(color = "black"), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black")) +
  coord_cartesian(clip = "off")

Den_boxplot
#The outlier is Genovesa norte DOVS, this is due to a lot of Lutjanus argentiventris
#There are also L. argentiventris in the UVC data, but not nearly as many. 

#plot for significance between methods
ggplot(TempDensity, aes(x = Comparison, y = N_site_500m2, fill = Comparison)) +
  geom_boxplot(fatten = 3) +
  geom_signif(comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test = "wilcox.test",
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE)) +
  theme_classic()

#Is it possible to make a plot, where we cut the y-axis so it is easier to read the boxplots?

#Deleting variables that are no longer needed
rm(DOVS_density, UVC_density, TempDensity)


# Biomass calculations DOVS ----------------------------------------------------
#Quality Control
#Prior to calculating biomass we need our DOVS measurements to meet two requirements
#1. RMS <= 50 (we have raised it from 20, as some of the videos are very blurry and this affects RMS)
#2. Precision <= 10% estimated Length

#To live up to the precision requirements, I have in the following lines of code removed length measurements
#that do not live up to the length requirements. Afterwards I have replaced them with average lengths for
#each species, based on the length measurements with good precision
GoodPrecision <- DOVS %>% 
  #When precision is worse than 10% of length, I remove the length so an average can be calculated
  mutate(Length_mm = ifelse(!Precision_mm < Length_mm*0.1, NA, Length_mm)) %>% 
  #When precision is worse than 10% of length, the length is removed
  #and when there are no lengths, I remove the precision as well
  mutate(Precision_mm = ifelse(is.na(Length_mm), 0, Precision_mm)) %>% 
  #I also remove RMS, when the length is removec
  mutate(RMS_mm = ifelse(is.na(Length_mm), 0, RMS_mm)) %>%
  #ifelse(Length_mm == NA, 0, Precision_mm)) %>% 
  group_by(Site, Period, ValidName) %>% 
  #Calculating average length per species within each period
  mutate(AvgLength = mean(na.omit(Length_mm))) %>% 
  #na.omit returns the object (e.g. dataframe) with NA values removed
  group_by(Site, ValidName) %>% 
  #Calculating average length per species within a site (for species that do not have average per period)
  mutate(AvgLengthSite = mean(na.omit(Length_mm))) %>% 
  group_by(ValidName) %>% 
  #Calculating average length per species across all sites (for those that do not have a length otherwise)
  mutate(AvgLengthAll = mean(na.omit(Length_mm)))

#NA's in length GoodPrecision
sum(is.na(GoodPrecision$Length_mm)) #229
#NA's after average length within site and period
sum(is.na(GoodPrecision$AvgLength)) #45
#NA's after average length within site
sum(is.na(GoodPrecision$AvgLengthSite)) #8
#NA's after average length of species across all sites
sum(is.na(GoodPrecision$AvgLengthAll)) #0
#All NA values for length replaced with averages for each species

#Replacing NA values with averages based on measurements with good precision
GoodPrecision <- GoodPrecision %>% 
  #NA values are replaced with values in AvgLength
  mutate(Length_mm = coalesce(Length_mm, AvgLength)) %>%
  #NAN values are replaced with values in AvgLengthSite
  mutate(Length_mm = coalesce(Length_mm, AvgLengthSite)) %>% 
  #NAN values are replaced with values in AvgLengthAll
  mutate(Length_mm = coalesce(Length_mm, AvgLengthAll))

#Quality control
#1. RMS <= 50
#2. Precision <= 10% estimated Length
#In the following no data is removed, as we check that RMS is below 60 and precision is no more than 10% of length
DOVS <- GoodPrecision %>% filter(RMS_mm <= 60) %>% 
  #Because of blurry video RMS is higher than 20, no RMS was higher than 59
  #We change the units of the lengths from mm to cm prior to biomass calculation
  mutate(Length_mm = Length_mm/10) %>% 
  #Now we rename the column to avoid confusion
  rename("Length_cm"="Length_mm") %>% 
  #We will drop columns we do not need
  select(-c(Precision_mm, RMS_mm, Range_mm, AvgLength, AvgLengthSite, AvgLengthAll))

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
  summarise(Gram_500m2_period = (sum(Biomass_N)/Transect_area)*500,
            Method, Fishing, SiteCode, Transect_area) %>% #In g/500m2
  unique() %>% 
  group_by(Site) %>% 
  summarise(Gram_500m2_site = mean(Gram_500m2_period), #Calculating average biomass of periods as site biomass
            Method, Fishing, SiteCode) %>% 
  mutate(KG_500m2_site = Gram_500m2_site/1000) %>% #Calculating kg per 500m2 at each site
  unique()

#Removing variables that are no longer necessary
rm(GoodPrecision)

# Biomass calculations UVC ---------------------------------------------
#Loading total length ratio for FishDB data
TLRatio <- xlsx::read.xlsx2("Data/TLRatio.xlsx",
                      sheetIndex = 1, header = TRUE, stringsAsFactors = FALSE) %>% 
  mutate(TLRatio = as.numeric(TLRatio))
#In case it does not work, try the line below
TLRatio <- readxl::read_excel("Data/TLRatio.xlsx", sheet = 1) %>%
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
  summarise(Gram_500m2_period = (sum(Biomass_N)/Transect_area)*500,
            Method, Fishing, SiteCode, Transect_area) %>% #In g per 500m2
  unique() %>% 
  group_by(Site) %>% 
  summarise(Gram_500m2_site = mean(Gram_500m2_period), #Calculating average biomass of periods as site biomass
            Method, Fishing, SiteCode) %>% 
  mutate(KG_500m2_site = Gram_500m2_site/1000) %>% #Calculating kg per 500m2 at each site
  unique()

#Removing EmptyPeriods data frames, as they are no longer needed
rm(EmptyPeriods_DOVS, EmptyPeriods_UVC)


# Biomass boxplot ---------------------------------------------------------
#Combining biomass data for DOVS and UVC
Biomass <- rbind(Biomass_DOVS, Biomass_UVC)

#Making column for significance comparison
TempBiomass <- Biomass %>% 
  mutate(Comparison = paste(Fishing, Method))

#plot biomass in g per 500m2
Bio_boxplot <- ggplot(Biomass, aes(x = Fishing, y = Gram_500m2_site/1000, fill = Method)) +
  geom_boxplot(fatten = 3) + 
  ggtitle("Total biomass per 500"~m^2) +
  geom_signif(comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test = "wilcox.test",
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE)) +
  scale_fill_manual(name = "Method", labels = c("Stereo-DOVs", "UVC"), 
                    values = grey.colors(2, start = 0.1, end = 0.5)) +
  scale_x_discrete(labels = c("No-take zone", "Fishing zone")) +
  scale_y_continuous(name = "Kg/500"~m^2) +
  theme_classic() +
  theme(#aspect.ratio = 1.5, 
        plot.title = element_text(color="black", face="bold", hjust = 0.5),
        legend.title = element_text(color = "black"), 
        legend.text = element_text(color = "black"), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        legend.position = "bottom")+
  #guides(fill = guide_legend(title.position = "top", ncol = 1))+ - DFA
  coord_cartesian(clip = "off")

Bio_boxplot
#Significant p < 0.05 for *
#The three outliers in Closed DOVS are: Al Arco (Darwin), Arrecife Antiguo (Darwin), Shark Bay (Wolf)
#The three top in UVC are: Al Arco (Darwin), Shark Bay (Wolf), Derrumbe (Wolf)

#plot for significance between methods
ggplot(TempBiomass, aes(x = Comparison, y = Gram_500m2_site, fill = Comparison)) +
  geom_boxplot(fatten = 3) +
  geom_signif(comparisons = list(c("Closed DOVS", "Closed UVC"), c("Open DOVS", "Open UVC")), 
              map_signif_level = TRUE, 
              test = "wilcox.test",
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE)) +
  theme_classic()

#plot biomass in kg per 500m2
Bio_boxplot2 <- ggplot(Biomass, aes(x = Fishing, y = KG_500m2_site, fill = Method)) +
  geom_boxplot() +
  ggtitle("Total biomass per 500"~m^2) +
  geom_signif(comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test = "wilcox.test",
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE)) +
  scale_fill_manual(name = "Method", labels = c("Stereo-DOVs", "UVC"), 
                    values = grey.colors(2, start = 0.1, end = 0.5)) +
  scale_y_continuous(name = "kg/500"~m^2) +
  scale_x_discrete(labels = c("No-take zone", "Fishing zone")) +
  theme_classic() +
  theme(aspect.ratio = 1.5, 
        plot.title = element_text(color="black", face="bold", hjust = 0.5),
        legend.title = element_text(color = "black"), 
        legend.text = element_text(color = "black"), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black")) +
  coord_cartesian(clip = "off")

Bio_boxplot2
#Significant p < 0.05 for *


#Deleting variables that are no longer needed
rm(Biomass_DOVS, Biomass_UVC, TempBiomass)


# Combining boxplots into figure ------------------------------------------
boxplot_all <- ggarrange(nrow = 1, ncol = 3, Ric_boxplot, Den_boxplot, Bio_boxplot,
          align = "v", common.legend = TRUE, legend = "bottom")
boxplot_all

#Saving composite image with different ratios - DFA
ggsave("Figures/test.tiff", boxplot_all, device = "tiff", dpi = 300, width = 20, height = 6.5)
#When I use align v, they are the same size, but the distance between y-axis labels to the y-axis is different
#when I use align h, the plots are not the same size, but the distance to the y-axis is the same
#I don't understand...

#How do I save the plot in a good size?
#ggsave("boxplot_all.pdf", width = 3, height = 4)
#ggsave("boxplot_all.png", width = 3, height = 4, dpi=100)

ggarrange(nrow = 1, ncol = 3, Ric_boxplot, Den_boxplot, Bio_boxplot2,
          align = "v", common.legend = TRUE, legend = "bottom")


#Delete variables used for boxplots
rm(boxplot_all, Ric_boxplot, Den_boxplot, Bio_boxplot)


# Univariate PERMANOVA for boxplot ----------------------------------------




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
  mutate(Biomass = a*((LenLenRatio*Length_cm)^b)) %>% #Biomass for 1 individual of each species
  mutate(Biomass_N = Biomass*N) %>% #Biomass for all individuals of each species
  group_by(Site, Period, ValidName) %>% 
  #Summing biomass for each species in each period, so I have total biomass per species per period
  summarise(Biomass_sp_period = sum(Biomass_N), 
            Transect_length_m, Method, Fishing, SiteCode) %>% 
  unique() %>% 
  mutate(Transect_area = Transect_length_m*5) %>% #Calculating transect area using transect width 5m
  group_by(Site, ValidName) %>% 
  #Calculating gram per 500m2
  summarise(Gram_500m2 = (Biomass_sp_period/Transect_area)*500, 
            Method, Fishing, SiteCode) %>% 
  #Calculating the biomass of each species per site in g per 500m2 (as average of periods)
  summarise(Gram_site_sp = mean(Gram_500m2), 
            ValidName, Method, Fishing, SiteCode) %>%
  #Going from g to kg for the biomass
  mutate(Biomass_site_sp = Gram_site_sp/1000) %>% #Biomass in kg
  select(-Gram_site_sp) %>% #Removing the Gram_site_sp column
  unique() %>% 
  ungroup()

#Calculating biomass UVC
Biomass_sp_UVC <- UVC %>% 
  #The fish biomass equation is W = a*L^b, therefore first transform the length, 
  #then apply the exponent b and finally multiply by a.
  #The equation is therefore: a*((LenLenRatio*Length_cm)^b)
  mutate(Biomass = a*((TLRatio*Length_cm)^b)) %>% #Biomass for 1 individual of each species
  mutate(Biomass_N = Biomass*N) %>% #Biomass for all individuals of each species
  group_by(Site, Period, ValidName) %>% 
  #Summing biomass for each species in each period, so I have total biomass per species per site
  summarise(Biomass_sp_period = sum(Biomass_N), 
            Transect_length_m, Method, Fishing, SiteCode) %>% 
  unique() %>% 
  mutate(Transect_area = Transect_length_m*5) %>% #Calculating transect area using transect width 5m
  group_by(Site, ValidName) %>% 
  #Calculating gram per 500m2
  summarise(Gram_500m2 = (Biomass_sp_period/Transect_area)*500, 
            Method, Fishing, SiteCode) %>% 
  #Calculating the biomass of each species per site in g per 500m2 (as average of periods)
  summarise(Gram_site_sp = mean(Gram_500m2), 
            ValidName, Method, Fishing, SiteCode) %>%
  #Going from g to kg for the biomass
  mutate(Biomass_site_sp = Gram_site_sp/1000) %>% #Biomass in kg
  select(-Gram_site_sp) %>% #Removing the Gram_site_sp column
  unique() %>% 
  ungroup()

#Combine biomass per species data frames
Biomass_sp <- rbind(Biomass_sp_DOVS, Biomass_sp_UVC)

#Sites in SiteInfo, that do not have any fish
unique(SiteInfo$Site[!SiteInfo$Site %in% DOVS$Site]) #No fish in DOVS
unique(SiteInfo$Site[!SiteInfo$Site %in% UVC$Site]) #No fish in UVC
#Making data frame with empty sites
EmptySites <- SiteInfo %>% 
  select(Site, SiteCode, Fishing) %>% 
  unique() %>% 
  mutate(Method = "DOVS") %>% 
  filter(!(Site %in% DOVS$Site)) %>% 
  rbind(SiteInfo %>% 
          select(Site, SiteCode, Fishing) %>% 
          unique() %>% 
          mutate(Method = "UVC") %>% 
          filter(!(Site %in% UVC$Site))) %>% 
  mutate(ValidName = "Dummy") %>% 
  mutate(Biomass_site_sp = 0.001)
#Adding the sites that have no fish to Biomass_sp
Biomass_sp <- Biomass_sp %>% 
  rbind(EmptySites)

#Remove unnecessary variables
rm(Biomass_sp_DOVS, Biomass_sp_UVC, EmptySites)


# PCO plot for biomass --------------------------------------

#Making matrix for dissimilarity calculation DOVS and UVC
Bio_mat <- Biomass_sp %>% #Based on biomass calculations for individual species at each site
  select(-c(Fishing, SiteCode)) %>% 
  #Adding the Method to the site name (I split them later for the plot)
  unite(SiteMet, Site, Method, sep = " ") %>% 
  #Making the format right for the matrix
  pivot_wider(names_from = "ValidName", values_from = "Biomass_site_sp") %>% 
  mutate(Dummy = 0.001) %>% #Adding dummy species to all sites, to enable dissimilarity calculations later
  #As I have some empty sites, where it is important to see how the methods UVC and DOVS differ
  arrange(SiteMet) %>% #Arranging site names
  column_to_rownames("SiteMet") %>% #Making a column into row names for the matrix
  as.matrix() %>% 
  replace_na(0) #Putting 0 instead of NA, when the species was not observed at a site

#Checking ranges for transformations and no transformation
range(Bio_mat)
range(Bio_mat^0.5)
range(Bio_mat^0.25)
#I have read online that I should get in the 0-10 range
#To achieve this I am doing a fourth root transformation

#Checking QQplot for biomass of both methods
qqnorm(Biomass_sp$Biomass_site_sp^0.25)
qqline(Biomass_sp$Biomass_site_sp^0.25, col = "red")
#The data is not normally distributed, but transforming the data makes it a little closer to normal distribution
#I can do the PERMANOVA based on the dissimilarity despite not having normally distributed data

#Applying a 4th root transformation to biomass data - and making a matrix
Bio_mat <- Bio_mat^0.25
#Calculating dissimilarity distance using vegan package, method bray curtis
Bio_mat_dist <- vegdist(Bio_mat, method = "bray")
#Create a weighted Principal Coordinates Analysis plot
Bio_mat_pco <- wcmdscale(Bio_mat_dist, eig = TRUE) #returns matrix of scores scaled by eigenvalues
#Show plot
plot(Bio_mat_pco, type = "points") #Add type points to remove labels

# Principal coordinate analysis and simple ordination plot
Bio_mat_pcoa <- pcoa(Bio_mat_dist)
# barplot of eigenvalues 1-10
barplot(Bio_mat_pcoa$values$Relative_eig[1:10])
#Biplot with arrows
biplot(Bio_mat_pcoa, Bio_mat)
#The principal coordinates analysis from pcoa function from the Ape package is similar to the 
#weighted principal coordinates analysis plot from wcmdscale 

#binding PCO coordinates to dataframe
PCO_biomass <- as.data.frame(Bio_mat_pco$points[,1:2])
PCO_biomass <- setDT(PCO_biomass, keep.rownames = TRUE)[]
PCO_biomass <- PCO_biomass %>% 
  rename(Site = rn, PC1 = Dim1, PC2 = Dim2) %>% 
  mutate(Method = case_when(endsWith(Site, "DOVS") ~ "DOVS",
                            endsWith(Site, "UVC") ~ "UVC")) %>% #Getting methods from site name
  mutate(Site = str_remove_all(Site, " DOVS| UVC")) %>% #Removing DOVS and UVC from site names
  left_join(SiteInfo %>% select(Site, Fishing, Bioregion, Island) %>% unique(), 
            by = c("Site")) #Adding site info on fishing (closed or open to fishing), bioregion and island

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

#plotting PCO with bioregion based on dissimilarity from biomass
ggplot(PCO_biomass, aes(PC1, PC2, col = Bioregion, fill = Bioregion)) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, col = "black") +
  theme_bw() + 
  theme(panel.grid = element_blank())

#plotting PCO with bioregion based on dissimilarity from biomass
ggplot(PCO_biomass, aes(PC1, PC2, col = Island, fill = Island)) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, col = "black") +
  theme_bw() + 
  theme(panel.grid = element_blank())

#plot with color indicating zonation (open or closed to fishing), shape = method
ggplot(PCO_biomass) + 
  geom_point(aes(PC1, PC2, color = Fishing, shape = Method), size = 2) +
  scale_color_grey() +
  theme_classic()

#plot with color indicating bioregion and shape = method
ggplot(PCO_biomass) + 
  geom_point(aes(PC1, PC2, color = Bioregion, shape = Method), size = 2) +
  theme_classic()

#plot with color indicating island and shape = method
ggplot(PCO_biomass) + 
  geom_point(aes(PC1, PC2, color = Island, shape = Method), size = 2) +
  theme_classic()


# Names on arrows in PCO plot --------------------------------------------------
#Funcktion that computes arrows from a pcoa and a species matrix
compute_arrows <-  function(Bio_mat_pcoa, Bio_mat) {
  
  # Keeping the species that has the largest arrows (from former PCO plot)
  Bio_mat = Bio_mat[ ,c("Lutjanus argentiventris", "Triaenodon obesus", "Mycteroperca olfax", 
                        "Carcharhinus limbatus", "Caranx melampygus", "Carcharhinus galapagensis", 
                        "Sphyrna lewini")]
  
  n <- nrow(Bio_mat)
  points.stand <- scale(Bio_mat_pcoa$vectors)
  
  # Compute covariance of variables with all axes
  S <- cov(Bio_mat, points.stand)
  
  # Select only positive eigenvalues
  pos_eigen = Bio_mat_pcoa$values$Eigenvalues[seq(ncol(S))]
  
  # Standardize value of covariance (see Legendre & Legendre 1998)
  U <- S %*% diag((pos_eigen/(n - 1))^(-0.5))
  colnames(U) <- colnames(Bio_mat_pcoa$vectors)
  
  # Add values of covariances inside object
  Bio_mat_pcoa$U <- U
  
  return(Bio_mat_pcoa)
}

#computing arrows for species using the function compute_arrows
species_pcoa_arrows <- compute_arrows(Bio_mat_pcoa, Bio_mat)
#chaning vectors to data.frame before putting it in ggplot2
species_pcoa_arrows$vectors <- as.data.frame(species_pcoa_arrows$vectors)

#making arrows smaller, so they fit better in the PCO
arrows_df <- as.data.frame(species_pcoa_arrows$U/15)
arrows_df$variable <- rownames(arrows_df)

#Making an anchor for the arrows
Anchor <- c(0.45,-0.4) #upper right corner

#Constant adjusting the size of vectors
K <- 1 #not actually necessary, as it is currentlt 1, but good for playing around with the code

#define other coordinates for arrows
X2 <- (arrows_df$Axis.1 + Anchor[1])*K
Y2 <- (arrows_df$Axis.2 + Anchor[2])*K

#plotting biomass, method, fishing and arrows for species with largest biomasses
ggplot(PCO_biomass) + 
  #Adding color for fishing and shapes for method
  geom_point(aes(PC1, PC2, color = Fishing, shape = Method), size = 2.5) + 
  geom_segment(data = arrows_df, #Adding arrows
               x = Anchor[1], y = Anchor[2],
               mapping = aes(xend = X2, yend = Y2),
               arrow = arrow(length = unit(2, "mm")), #Adding arrow head
               size = 0.8) +
  #Adding arrow labels for L. argenticentris and T. obsus
  geom_text(data = arrows_df, aes(label = arrows_df$variable),
            size = 5, fontface = "italic",
            lineheight = 0.6, 
            x = X2, y = Y2) +
  xlim(-0.6, 0.6) +
  scale_color_grey() +
  theme_classic() +
  #moving legend in plot and making box around it
  theme(legend.position = c(0.9, 0.87), 
        legend.box.background = element_rect(size = 0.7, linetype = "solid", colour ="black"), 
        legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), 
        axis.text.x = element_text(color = "black", size = 14), 
        axis.text.y = element_text(color = "black", size = 14), 
        axis.title.x = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14))

# PCO Figure for biomass ----------------------------------------------------

#Renaming methods and fishing status in PCO_biomass for better names on graph
PCO_biomass <- PCO_biomass %>% 
  mutate(Method = recode(Method, "DOVS" = "Stereo-DOVs")) %>% 
  mutate(Fishing = recode(Fishing, 
                          "Closed" = "No-take", 
                          "Open" = "Fishing")) %>% 
  rename(Zone = Fishing) %>% 
  mutate(Bioregion = recode(Bioregion, 
                            "Lejano Norte" = "Far Northern", 
                            "Norte" = "Northern", 
                            "Centro Sur" = "Central South-eastern", 
                            "Oeste Fria" = "Western"))

#plotting biomass, method, fishing and arrows for species with largest biomasses
ggplot(PCO_biomass) + 
  #Adding color for fishing and shapes for method
  geom_point(aes(PC1, PC2, color = Zone, shape = Method), size = 4) + 
  geom_segment(data = arrows_df, #Adding arrows
               x = Anchor[1], y = Anchor[2],
               mapping = aes(xend = X2, yend = Y2),
               arrow = arrow(length = unit(2, "mm")), #Adding arrow head
               size = 0.8) +
  #Adding arrow labels for C. galapagensis and S. lewini
  geom_text(data = arrows_df[c(6:7),], aes(label = arrows_df$variable[c(6:7)]),
            size = 5, fontface = "italic",
            lineheight = 0.6, 
            x = X2[c(6:7)], y = Y2[c(6:7)], 
            hjust = 0.1, vjust = -0.5) +
  #Adding arrow labels for C. melampygus, so it can be read
  geom_text(data = arrows_df[5,], aes(label = arrows_df$variable[5]),
            size = 5, fontface = "italic",
            lineheight = 0.6, 
            x = X2[5], y = Y2[5], 
            hjust = 1.15, vjust = -1.7) +
  #Adding arrow labels for C. limbatus
  geom_text(data = arrows_df[c(4),], aes(label = arrows_df$variable[c(4)]),
            size = 5, fontface = "italic",
            lineheight = 0.6,  
            x = X2[c(4)], y = Y2[c(4)], 
            hjust = 1.12, vjust = -0.8) +
  #Adding arrow labels for M. olfax.
  geom_text(data = arrows_df[3,], aes(label = arrows_df$variable[3]),
            size = 5, fontface = "italic",
            lineheight = 0.6, 
            x = X2[3], y = Y2[3], 
            hjust = 0.45, vjust = 1) +
  #Adding arrow labels for L. argenticentris and T. obsus
  geom_text(data = arrows_df[1:2,], aes(label = arrows_df$variable[1:2]),
            size = 5, fontface = "italic",
            lineheight = 0.6, 
            x = X2[1:2], y = Y2[1:2], 
            hjust = 0.05, vjust = 1.2) +
  xlim(-0.6, 0.6) +
  scale_color_grey(start = 0.1, end = 0.5) +
  theme_classic() +
  xlab("PCO1") +
  ylab("PCO2") +
  #moving legend in plot and making box around it
  theme(legend.position = c(0.9, 0.85), 
        legend.box.background = element_rect(size = 0.7, linetype = "solid", colour ="black"), 
        legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.title = element_text(color = "black", size = 17),
        legend.text = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 14), 
        axis.text.y = element_text(color = "black", size = 14), 
        axis.title.x = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14), 
        axis.line.x = element_line(color = "black", size = 0.5), 
        axis.line.y = element_line(color = "black", size = 0.5))


#plotting biomass, Bioregion and fishing, along with arrows for species with largest biomasses
ggplot(PCO_biomass) + 
  #Adding ellipse around fishing status
  #stat_ellipse(geom = "polygon", col = "black", alpha = 0.3, 
  #             aes(PC1, PC2, col = Bioregion, fill = Bioregion)) +
  #Adding color for fishing and shapes for method
  geom_point(aes(PC1, PC2, color = Bioregion, shape = Fishing), size = 2.5) + 
  geom_segment(data = arrows_df, #Adding arrows
               x = Anchor[1], y = Anchor[2],
               mapping = aes(xend = X2, yend = Y2),
               arrow = arrow(length = unit(2, "mm")), #Adding arrow head
               size = 0.8) +
  labs(title = "Principal Coordinate Ordination Biomass") + 
  #Adding arrow labels for species with arrows upwards in plot
  geom_text(data = arrows_df[1:3,], aes(label = arrows_df$variable[1:3]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[1:3], y = Y2[1:3], 
            vjust = -0.8) +
  #Adding arrow labels for species with arrows downwards in plot
  geom_text(data = arrows_df[c(4, 6:7),], aes(label = arrows_df$variable[c(4, 6:7)]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[c(4, 6:7)], y = Y2[c(4, 6:7)], 
            vjust = 1) +
  #Adding arrow labels for C. melampygus, so it can be read
  geom_text(data = arrows_df[5,], aes(label = arrows_df$variable[5]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[5], y = Y2[5], 
            vjust = 1.8) +
  xlim(-0.86, 0.58) +
  #scale_color_grey() +
  theme_classic() +
  #moving legend in plot and making box around it
  theme(legend.position = c(0.9, 0.87), 
        legend.box.background = element_rect(size = 0.7, linetype = "solid", colour ="black"), 
        legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))

#remove unnecessary variables
rm(compute_arrows, species_pcoa_arrows, arrows_df, Anchor, K, X2, Y2)
rm(PCO_biomass, Bio_mat_pco, Bio_mat_pcoa)


# PERMANOVA biomass ---------------------------------------------------

#Data frame for PERMANOVA Factors to be tested
Factors <- Biomass %>% 
  left_join(Richness %>% select(Site, Method, Site_sp_500m2), #joining richness data
             by = c("Site", "Method")) %>% 
  left_join(Density %>% select(Site, Method, N_site_500m2), #joining density data
            by = c("Site", "Method")) %>% 
  left_join(SiteInfo %>% select(Site, Bioregion, Island) %>% unique(), by = "Site") %>% 
  ungroup() %>% #Ungrouping, as site is grouped to something.
  mutate_if(is.character, as.factor) %>% 
  arrange(Site)
#Checking type of data for all columns in Factors
str(Factors)

#PERMANOVA Main effects and interactions
adonis(Bio_mat ~ Method*Fishing*Bioregion,
       data = Factors, method = "bray", permutations = 9999)

#PERMANOVA - method main effect
adonis(Bio_mat ~ Method, data = Factors, method = "bray", permutations = 9999)
#Non-significant. Therefore I can exclude it from the final calculations.
#Justification - there is no real difference between methods. 

#PERMANOVA without method
adonis(Bio_mat ~ Fishing*Bioregion,
       data = Factors, method = "bray", permutations = 9999)


#Betadispersion Fishing status
dispersion <- betadisper(Bio_mat_dist, group = Factors$Fishing)
permutest(dispersion, permutations = 9999)
#Non-significant
#Plotting dispersion
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse

#Betadispersion Bioregion
dispersion <- betadisper(Bio_mat_dist, group = Factors$Bioregion)
permutest(dispersion, permutations = 9999)
#Significant
#Plotting dispersion
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse


#Pairwise PERMANOVA to test which of the bioregions differ significantly from each other
pairwise.adonis(Bio_mat_dist, Factors$Bioregion, perm = 9999)

#Removing unnecessary variables
rm(dispersion, Dist_mat, Bio_mat_dist)


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
  unique() %>% 
  ungroup()

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
  unique() %>% 
  ungroup()

#Combining average density for DOVS and UVC and making a matrix
Density_sp <- rbind(Den_sp_DOVS, Den_sp_UVC)

#Sites in SiteInfo, that do not have any fish
unique(SiteInfo$Site[!SiteInfo$Site %in% DOVS$Site]) #No fish in DOVS
unique(SiteInfo$Site[!SiteInfo$Site %in% UVC$Site]) #No fish in UVC
#Making data frame with empty sites
EmptySites <- SiteInfo %>% 
  select(Site, SiteCode, Fishing) %>% 
  unique() %>% 
  mutate(Method = "DOVS") %>% 
  filter(!(Site %in% DOVS$Site)) %>% 
  rbind(SiteInfo %>% 
          select(Site, SiteCode, Fishing) %>% 
          unique() %>% 
          mutate(Method = "UVC") %>% 
          filter(!(Site %in% UVC$Site))) %>% 
  mutate(ValidName = "Dummy") %>% 
  mutate(N_site_sp = 1)
#Adding the sites that have no fish to Density_sp
Density_sp <- Density_sp %>% 
  rbind(EmptySites)
#Remove empty sites
rm(EmptySites)

#Making Matrix for density of species
Den_mat <- Density_sp %>% 
  select(-c(Fishing, SiteCode)) %>% 
  unite(SiteMet, Site, Method, sep = " ") %>% 
  pivot_wider(names_from = "ValidName", values_from = "N_site_sp") %>% #Making the format right for the matrix
  mutate(Dummy = 1) %>% #Adding dummy species to all sites, to enable dissimilarity calculations later
  column_to_rownames("SiteMet") %>% #Making a column into row names for the matrix
  as.matrix() %>% 
  replace_na(0)#Putting 0 instead of NA, when the species was not observed at the site.

#Removing unnecessary variables
rm(Den_sp_DOVS, Den_sp_UVC)

#Checking ranges for transformations and no transformation
range(Den_mat)
range(Den_mat^0.5)
range(Den_mat^0.25)
#I have read online that I should get in the 0-10 range
#To achieve this I am doing a fourth root transformation

#Checking QQplot for density of both methods
qqnorm(Density_sp$N_site_sp^0.25)
qqline(Density_sp$N_site_sp^0.25, col = "red")

#Applying a 4th root transformation to matrix
Den_mat <- Den_mat^0.25
#Calculating dissimilarity distance using vegan package, the default is Bray Curtis
Den_mat_dist <- vegdist(Den_mat, method = "bray")
#Create a PCoA (Principal Co-ordinates Analysis) plot
Den_mat_pco <- wcmdscale(Den_mat_dist, eig = TRUE) #returns matrix of scores scaled by eigenvalues
#Show plot
plot(Den_mat_pco, type = "points") #Add type points to remove labels

#Principal coordinate analysis and simple ordination plot
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
  left_join(SiteInfo %>% select(Site, Fishing, Bioregion, Island) %>% unique(), 
            by = c("Site")) #Adding site info on fishing (closed or open to fishing), bioregion and island

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

#plotting PCO with bioregion based on dissimilarity from density
ggplot(PCO_density, aes(PC1, PC2, col = Bioregion, fill = Bioregion)) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, col = "black") +
  theme_bw() + 
  theme(panel.grid = element_blank())

#plotting PCO with bioregion based on dissimilarity from density
ggplot(PCO_density, aes(PC1, PC2, col = Island, fill = Island)) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, col = "black") +
  theme_bw() + 
  theme(panel.grid = element_blank())

#plot with color indicating zonation (open or closed to fishing), shape = method
ggplot(PCO_density) + 
  geom_point(aes(PC1, PC2, color = Fishing, shape = Method), size = 2) +
  scale_color_grey() +
  theme_classic()

#plot with color indicating bioregion and shape = method
ggplot(PCO_density) + 
  geom_point(aes(PC1, PC2, color = Bioregion, shape = Method), size = 2) +
  theme_classic()

#plot with color indicating island and shape = method
ggplot(PCO_density) + 
  geom_point(aes(PC1, PC2, color = Island, shape = Method), size = 2) +
  theme_classic()



# PCO density with arrows -------------------------------------------------
#Funcktion that computes arrows from a pcoa and a species matrix
compute_arrows <-  function(Den_mat_pcoa, Den_mat) {
  
  # Keeping the species that has the largest arrows (from former PCO plot)
  Den_mat = Den_mat[ ,c("Lutjanus argentiventris", "Triaenodon obesus", "Mycteroperca olfax", 
                        "Paralabrax albomaculatus", "Hypanus dipterurus", "Carcharhinus galapagensis", 
                        "Sphyrna lewini")]
  
  n <- nrow(Den_mat)
  points.stand <- scale(Den_mat_pcoa$vectors)
  
  # Compute covariance of variables with all axes
  S <- cov(Den_mat, points.stand)
  
  # Select only positive eigenvalues
  pos_eigen = Den_mat_pcoa$values$Eigenvalues[seq(ncol(S))]
  
  # Standardize value of covariance (see Legendre & Legendre 1998)
  U <- S %*% diag((pos_eigen/(n - 1))^(-0.5))
  colnames(U) <- colnames(Den_mat_pcoa$vectors)
  
  # Add values of covariances inside object
  Den_mat_pcoa$U <- U
  
  return(Den_mat_pcoa)
}

#computing arrows for species using the function compute_arrows
species_pcoa_arrows <- compute_arrows(Den_mat_pcoa, Den_mat)
#chaning vectors to data.frame before putting it in ggplot2
species_pcoa_arrows$vectors <- as.data.frame(species_pcoa_arrows$vectors)

#making arrows smaller, so they fit better in the PCO
arrows_df <- as.data.frame(species_pcoa_arrows$U/15)
arrows_df$variable <- rownames(arrows_df)

#Making an anchor for the arrows
Anchor <- c(0.53, 0.15) 

#Constant adjusting the size of vectors
K <- 1 #not actually necessary, as it is currentlt 1, but good for playing around with the code

#define other coordinates for arrows
X2 <- (arrows_df$Axis.1 + Anchor[1])*K
Y2 <- (arrows_df$Axis.2 + Anchor[2])*K

#plotting biomass, method, fishing and arrows for species with largest biomasses
ggplot(PCO_density) + 
  #Adding ellipse around fishing status
  #stat_ellipse(geom = "polygon", col = "black", alpha = 0.3, 
  #             aes(PC1, PC2, col = Fishing, fill = Fishing)) +
  #Adding color for fishing and shapes for method
  geom_point(aes(PC1, PC2, color = Fishing, shape = Method), size = 2.5) + 
  geom_segment(data = arrows_df, #Adding arrows
               x = Anchor[1], y = Anchor[2],
               mapping = aes(xend = X2, yend = Y2),
               arrow = arrow(length = unit(2, "mm")), #Adding arrow head
               size = 0.8) +
  labs(title = "Principal Coordinate Ordination Density") + 
  #Adding arrow labels for species with arrows upwards in plot
  geom_text(data = arrows_df[c(1:3,5),], aes(label = arrows_df$variable[c(1:3,5)]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[c(1:3,5)], y = Y2[c(1:3,5)], 
            vjust = -0.8) +
  #Adding arrow labels for species with arrows upwards in plot
  geom_text(data = arrows_df[4,], aes(label = arrows_df$variable[4]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[4], y = Y2[4], 
            vjust = -0.4) +
  #Adding arrow labels for species with arrows downwards in plot
  geom_text(data = arrows_df[6,], aes(label = arrows_df$variable[6]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[6], y = Y2[6], 
            vjust = 1) +
  #Adding arrow labels for C. melampygus, so it can be read
  geom_text(data = arrows_df[7,], aes(label = arrows_df$variable[7]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[7], y = Y2[7], 
            vjust = 1.8) +
  xlim(-0.55, 1.1) +
  scale_color_grey() +
  theme_classic() +
  #moving legend in plot and making box around it
  theme(legend.position = c(0.9, 0.87), 
        legend.box.background = element_rect(size = 0.7, linetype = "solid", colour ="black"), 
        legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))


#adjusting anchor for the following plot
Anchor <- c(0.55, 0.07) 

#Constant adjusting the size of vectors
K <- 1 #not actually necessary, as it is currently 1, but good for playing around with the code

#define other coordinates for arrows
X2 <- (arrows_df$Axis.1 + Anchor[1])*K
Y2 <- (arrows_df$Axis.2 + Anchor[2])*K

#plotting biomass, Bioregion and fishing, along with arrows for species with largest biomasses
ggplot(PCO_density) + 
  #Adding ellipse around fishing status
  #stat_ellipse(geom = "polygon", col = "black", alpha = 0.3, 
  #             aes(PC1, PC2, col = Bioregion, fill = Bioregion)) +
  #Adding color for fishing and shapes for method
  geom_point(aes(PC1, PC2, color = Bioregion, shape = Fishing), size = 2.5) + 
  geom_segment(data = arrows_df, #Adding arrows
               x = Anchor[1], y = Anchor[2],
               mapping = aes(xend = X2, yend = Y2),
               arrow = arrow(length = unit(2, "mm")), #Adding arrow head
               size = 0.8) +
  labs(title = "Principal Coordinate Ordination Density") + 
  #Adding arrow labels for species with arrows upwards in plot
  geom_text(data = arrows_df[c(1:3,5),], aes(label = arrows_df$variable[c(1:3,5)]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[c(1:3,5)], y = Y2[c(1:3,5)], 
            vjust = -0.8) +
  #Adding arrow labels for species with arrows upwards in plot
  geom_text(data = arrows_df[4,], aes(label = arrows_df$variable[4]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[4], y = Y2[4], 
            vjust = -0.4) +
  #Adding arrow labels for species with arrows downwards in plot
  geom_text(data = arrows_df[6,], aes(label = arrows_df$variable[6]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[6], y = Y2[6], 
            vjust = 1) +
  #Adding arrow labels for C. melampygus, so it can be read
  geom_text(data = arrows_df[7,], aes(label = arrows_df$variable[7]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[7], y = Y2[7], 
            vjust = 1.8) +
  xlim(-0.55, 1.12) +
  #scale_color_grey() +
  theme_classic() +
  #moving legend in plot and making box around it
  theme(legend.position = c(0.9, 0.87), 
        legend.box.background = element_rect(size = 0.7, linetype = "solid", colour ="black"), 
        legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))


#adjusting anchor for the following plot
Anchor <- c(0.55, 0.07) 

#Constant adjusting the size of vectors
K <- 1 #not actually necessary, as it is currently 1, but good for playing around with the code

#define other coordinates for arrows
X2 <- (arrows_df$Axis.1 + Anchor[1])*K
Y2 <- (arrows_df$Axis.2 + Anchor[2])*K

#plotting biomass, Bioregion and fishing, along with arrows for species with largest biomasses
ggplot(PCO_density) + 
  #Adding ellipse around fishing status
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.3, 
               aes(PC1, PC2, col = Bioregion, fill = Bioregion)) +
  #Adding color for fishing and shapes for method
  geom_point(aes(PC1, PC2, color = Bioregion, shape = Fishing), size = 2.5) + 
  geom_segment(data = arrows_df, #Adding arrows
               x = Anchor[1], y = Anchor[2],
               mapping = aes(xend = X2, yend = Y2),
               arrow = arrow(length = unit(2, "mm")), #Adding arrow head
               size = 0.8) +
  labs(title = "Principal Coordinate Ordination Density") + 
  #Adding arrow labels for species with arrows upwards in plot
  geom_text(data = arrows_df[c(1:3,5),], aes(label = arrows_df$variable[c(1:3,5)]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[c(1:3,5)], y = Y2[c(1:3,5)], 
            vjust = -0.8) +
  #Adding arrow labels for species with arrows upwards in plot
  geom_text(data = arrows_df[4,], aes(label = arrows_df$variable[4]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[4], y = Y2[4], 
            vjust = -0.4) +
  #Adding arrow labels for species with arrows downwards in plot
  geom_text(data = arrows_df[6,], aes(label = arrows_df$variable[6]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[6], y = Y2[6], 
            vjust = 1) +
  #Adding arrow labels for C. melampygus, so it can be read
  geom_text(data = arrows_df[7,], aes(label = arrows_df$variable[7]),
            size = 4, fontface = "bold",
            lineheight = 0.6, 
            x = X2[7], y = Y2[7], 
            vjust = 1.8) +
  xlim(-0.8, 1.15) +
  #scale_color_grey() +
  theme_classic() +
  #moving legend in plot and making box around it
  theme(legend.position = c(0.9, 0.87), 
        legend.box.background = element_rect(size = 0.7, linetype = "solid", colour ="black"), 
        legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))


#remove unnecessary variables
rm(compute_arrows, species_pcoa_arrows, arrows_df, Anchor, K, X2, Y2)
rm(PCO_density, Den_mat_pco, Den_mat_pcoa)


# PERMANOVA density -------------------------------------------------------

#PERMANOVA Main effects and interactions
adonis(Den_mat ~ Method*Fishing*Bioregion,
       data = Factors, method = "bray", permutations = 9999)

#PERMANOVA - method main effect
adonis(Den_mat ~ Method, data = Factors, method = "bray", permutations = 9999)
#Non-significant. Therefore I can exclude it from the final calculations.
#PERMANOVA - method main effect
adonis(Den_mat ~ Fishing, data = Factors, method = "bray", permutations = 9999)
#Non-significant. Therefore I can exclude it from the final calculations.
#Justification - there is no real difference between methods or fishing. 

#PERMANOVA - Bioregion main effect
adonis(Den_mat ~ Bioregion, data = Factors, method = "bray", permutations = 9999)


#Betadispersion Bioregion
dispersion <- betadisper(Den_mat_dist, group = Factors$Bioregion)
permutest(dispersion, permutations = 9999)
#Non-significant
#Plotting dispersion
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse

#Pairwise PERMANOVA to test which of the bioregions differ significantly from each other
pairwise.adonis(Den_mat_dist, Factors$Bioregion, perm = 9999)

#Removing unnecessary variables
rm(dispersion, Den_mat, Den_mat_dist)


# Notes about code and plots ----------------------------------------------

#Boxplots, currently grey colored boxplots are using this code: 
#Plot species richness with significance
ggplot(Richness, aes(x = Fishing, y = Site_sp_500m2, fill = Method)) +
  geom_boxplot(fatten = 3) +
  geom_signif(comparisons = list(c("Closed", "Open")), 
              map_signif_level = TRUE, 
              test = "wilcox.test", 
              test.args = list(alternative = "two.sided", var.equal = FALSE, paired = FALSE)) +
  scale_fill_grey(start = 0.1, end = 0.5) + #For figure of all species, start = 0.1, end = 0.7.
  scale_x_discrete(name = "Zonation") +
  scale_y_continuous(name = "Number of species/500"~m^2) +
  theme_classic()
#However, the figure of all species, as mentioned in the comment, is slightly lighter in the colors, 
#so this figure uses: 
#scale_fill_grey(start = 0.1, end = 0.7)

# Extra tables and figures ------------------------------------------------
#Species list for the study - all the species are present in the UVC data
#This is because the species from the DOVS data that are not in UVC have been removed.
Specieslist <- UVC %>% select(Family, Genus, ValidName) %>% unique()

rm(Specieslist)


