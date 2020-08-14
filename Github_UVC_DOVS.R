#############################################################
# Title: Github data cleaning and analysis UVC and DOVS Galapagos
# Author: Sara Færch Hansen
# Assisting: Denisse Fierro Arcos
# Version: 1
# Date last updated: 2020-08-13
# Aim: Explain what the code does, maybe what project is related to (e.g., Master thesis)
#############################################################

#Github data cleaning and analysis UVC and DOVS Galapagos
#############################################################

# Uploading libraries -----------------------------------------------------
#Remove spaces around text (characters) in DOVS, using mutate function from dplyr and trim from stringr
# library(dplyr)
# library(stringr)
library(tidyverse) #this one contains a bunch of useful packages including the two above


# Uploading data ----------------------------------------------------------
#Set working directory and Load UVC and DOVS data
# setwd("C:/Users/Ejer/Desktop/Github_UVC_DOVS/Data") # I have removed this because you
#have created a project, so only relative paths are needed I have edit below
UVC <- read.csv(file = "Data/UVC_all_clean.csv", header = TRUE, 
                #This number in the square brackets removes the first column
                stringsAsFactors = FALSE)[,-1] 
DOVS <- read.csv(file = "Data/DOVS_clean.csv", header = TRUE, 
                 stringsAsFactors = FALSE)[,-1]


# read.csv(file = "Data/UVC_all_clean.csv", header = TRUE,
#          #This number in the square brackets removes the first column
#          stringsAsFactors = FALSE) %>% select(-X)

#Removing several columns - DOVs: Comments, Stage. UVC: Comments, Sex
#DOVS <- DOVS[, -c("Comments", "Stage")]
#UVC <- UVC[, -c("Comments", "Sex")]



# Tidying up data set ------------------------------------------------------
DOVS %>%
  #First we will check unique values for Family, Genus and Species
  distinct(Family, Genus, Species) %>% 
  arrange(Family, Genus)
#Results suggest that there are a number of Genus with an additional blank space at the
#end. We also have a blank row at the top, which are for unknown species. Check with
#Pelayo if we know what they could be, but otherwise we must remove them

#This takes out white space before or after a word in all columns that are of class
#character. However, because you did not save it, when you merge the columns together
#you still had the spaces. I have included below how to save the outcome, but I have
#rewritten this section so everything appears in just one chunk of code
DOVS_S <- DOVS %>% 
  mutate(across(where(is.character), str_trim))

#Combine Genus and Species into SPECIES for DOVS data
# DOVS_S$SPECIES <- paste0(DOVS$Genus , " ", DOVS$Species) #This includes two spaces because
#the default for paste is to have " " as a separator. Try this instead:
DOVS_S$SPECIES <- paste(DOVS_S$Genus, DOVS_S$Species)

#Rename DOVS Number_individuals to N
names(DOVS_S)[names(DOVS_S) == "Number_individuals"] <- "N"


#Tidying up data - I have done the same as above and saved it as a different data frame
#so you can compare and see which one you would like to keep
DOVS_D <- DOVS %>% 
  #removing columns as you have commented out above
  select(-c(Comment, Method)) %>% 
  #Removing empty rows in Family 
  filter(Family != "") %>% 
  #Rename Number_individuals column
  rename("N" = "Number_individuals") %>% 
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
                                   Genus == "" ~ paste(Family, Species, sep = " ")))
#We can check our progress
DOVS_D %>% 
  distinct(Family, Genus, Species) %>% 
  arrange(Family, Genus)
#Everything looks ok, except the Mycteroperca sp. From memory M. olfax was the only
#species in this genus in the Galapagos. Check with Pelayo and change if needed.

#Checking unique species ID's in DOVS_D and UVC
unique(DOVS_D$SpeciesName) 
DOVS_D %>% distinct(SpeciesName) %>% arrange(SpeciesName)
unique(UVC$SPECIES)

#Correcting misspelled species names in UVC data (bonito, Paralabrax albomaclatus, yellow tail snapper,
# Zalophus wolebacki, Hoplopagrus guenteri, Zalophus wollebackii, Zalophus wollebacki, Heterodonthus quoyi,
# Myvteroperca olfax)
UVC$SpeciesName <- UVC$SPECIES %>%
  recode(., "bonito" = "Sardinops sagax", 
         "Paralabrax albomaclatus" = "Paralabrax albomaculatus",
         "yellow tail snapper" = "Lutjanus argentiventris",
         "Zalophus wolebacki" = "Zalophus wollebaeki",
         "Hoplopagrus guenteri" = "Hoplopagrus guentherii",
         "Zalophus wollebackii" = "Zalophus wollebaeki",
         "Zalophus wollebacki" = "Zalophus wollebaeki",
         "Heterodonthus quoyi" = "Heterodontus quoyi",
         "Myvteroperca olfax" = "Mycteroperca olfax")
#Checking that the unique species are now correct in UVC
unique(UVC$SpeciesName)

# Biomass calculations ----------------------------------------------------
#Getting unique species for finding a and b factors to calculate biomass
library(data.table)
unique_DOVS_D <- data.frame(unique(DOVS_D$SpeciesName))
colnames(unique_DOVS_D) <- "Species"
unique_UVC <- data.frame(unique(UVC$SpeciesName))
colnames(unique_UVC) <- "Species"

#Merging the species lists and keeping only the unique species
species_list <- unique(rbind(unique_DOVS_D, unique_UVC))

#Reading csv file with a and b factors (when I have found all a and b values)
#calculation of biomass


FishDB <- read_csv("https://raw.githubusercontent.com/lidefi87/MangroveProject_CDF/master/Data/FishDB.csv")

x <- UVC
x <- x %>% left_join(FishDB %>% select(ScientificName, ValidName, Family, Genus), 
                by = c("SPECIES" = "ScientificName")) %>% 
  select(-c(SPECIES, SpeciesName))

# NMDS plot for sites and species ------------------------------------------
#Assigning columns with species names and abundance from DOVS_D and UVC
Abun_DOVS <- subset(DOVS_D, select = c("Site", "SpeciesName", "N"))

Abun_DOVS_2 <- Abun_DOVS %>%
  group_by(Site, SpeciesName) %>%
  summarise(Abundance = sum(N, na.rm = T))
# This last line is not working and I do not understand why
#There is something with the mutate and distinct maybe. 
#It works when the line is just: distinct(unique(Sum_N))
#But then I cannot name the column and it is just named "unique(Sum_N)" and I want it to be named "Abundance"


#Making matrix to enable NMDS plot
Abun_DOVS_mat <- Abun_DOVS_2 %>%
  mutate(Abundance = replace_na(Abundance, 0)) %>% 
  pivot_wider(names_from = "SpeciesName", values_from = "Abundance") %>% 
  column_to_rownames("Site") %>% as.matrix()


library(vegan)
x <- sqrt(sqrt(Abun_DOVS_mat))
y <- vegdist(x)
z <- wcmdscale(y, eig = T)
plot(z)
