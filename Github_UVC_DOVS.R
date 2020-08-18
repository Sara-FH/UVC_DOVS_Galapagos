#############################################################
# Title: Github data cleaning and analysis UVC and DOVS Galapagos
# Author: Sara Færch Hansen
# Assisting: Denisse Fierro Arcos
# Version: 1
# Date last updated: 2020-08-17
# Aim: Compare UVC and DOVS data in the Galapagos related to the Master thesis of Sara Færch Hansen
#############################################################

# Uploading libraries -----------------------------------------------------
library(tidyverse) 
library(data.table)
library(vegan)

# Uploading data ----------------------------------------------------------
#Loading UVC and DOVS data
UVC <- read.csv(file = "Data/UVC_all_clean.csv", header = TRUE,
                stringsAsFactors = FALSE) %>% select(-X)

DOVS <- read.csv(file = "Data/DOVS_clean.csv", header = TRUE, 
                 stringsAsFactors = FALSE) %>% select(-X)

# Tidying up data set ------------------------------------------------------

DOVS %>%
  #First we will check unique values for Family, Genus and Species
  distinct(Family, Genus, Species) %>% 
  arrange(Family, Genus)
#Results suggest that there are a number of Genus with an additional blank space at the
#end. We also have a blank row at the top, which are for unknown species. Check with
#Pelayo if we know what they could be, but otherwise we must remove them

#Tidying up data
DOVS <- DOVS %>% 
  #removing Comment column
  select(-c(Comment)) %>% 
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
DOVS %>% 
  distinct(Family, Genus, Species) %>% 
  arrange(Family, Genus)
#Everything looks ok, except the Mycteroperca sp. From memory M. olfax was the only
#species in this genus in the Galapagos. Check with Pelayo and change if needed.

#Checking unique species ID's in DOVS and UVC
DOVS %>% distinct(SpeciesName) %>% arrange(SpeciesName)
UVC %>% distinct(SPECIES) %>% arrange(SPECIES)

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
UVC %>% distinct(SpeciesName) %>% arrange(SpeciesName)


# Biomass calculations ----------------------------------------------------

#Access to the Fish data set with correct names and a/b variables to calculate biomass
FishDB <- read_csv("https://raw.githubusercontent.com/lidefi87/MangroveProject_CDF/master/Data/FishDB.csv")

#Using join to keep correct names of species in UVC data
UVC <- UVC %>% 
  left_join(FishDB %>% select(ScientificName, ValidName, Family, Genus, a, b, LengthType, LenLenRatio), 
            by = c("SPECIES" = "ScientificName")) %>% 
  select(-c(SPECIES, SpeciesName))

#Using join to keep correct names of species in DOVS data
DOVS <- DOVS %>% 
  left_join(FishDB %>% select(ScientificName, ValidName, a, b, LengthType, LenLenRatio),
            by = c("SpeciesName" = "ScientificName")) %>% 
  drop_na(ValidName) %>% #Dropping rows with unidentified species
  filter(!str_detect(ValidName, "Chelonia mydas|Zalophus wollebaeki")) %>% #Dropping non-fish species
  select(-c(SpeciesName))

DOVS %>% distinct(ValidName) %>% arrange(ValidName) #No NA's and the two non-fish species are also removed

#Calculate the biomass for DOVS using FishDB a and b values for FL
str(DOVS) #LenLenRatio is character and needs to be numeric
Biomass_DOVS <- DOVS %>% select(Site, Length_mm, N, Method, ValidName, a, b, LengthType, LenLenRatio) %>%
  mutate(LenLenRatio = as.numeric(LenLenRatio)) %>% 
  mutate(Biomass = (a*LenLenRatio*Length_mm)^b) %>% 
  mutate(Biomass_N = Biomass*N)
#Denisse, could you double check this calculation
#Also what do we do with rows that are missing lengths?

#summing biomass for each species
x <- Biomass_DOVS

################ I cannot make this work. 
#I am trying to sum up all the biomasses per species - so that I can move on to making the matrix to 
#calculate dissimilarity
x <- x %>% group_by(ValidName) %>% 
  mutate(Biomass_sp = sum(Biomass_N))
#summarise(Biomass_sp = sum(Biomass_N))


#Making matrix for dissimilarity calculation
Biomass_DOVS_mat <- Biomass_DOVS %>% select(Site, Method, ValidName, Biomass_N) %>% 
  mutate(SiteMet = paste(Site, Method)) %>% 
  select(-c(Site, Method))



mat <- Biomass_DOVS_mat %>% pivot_wider(names_from = "ValidName", values_from = "Biomass_N") %>% 
  column_to_rownames("SiteMet") %>% 
  as.matrix() #%>% 
  #replace_na(0)

mat %>% head()

#Making matrix to enable NMDS plot
Abun_mat <- Abun %>%
  pivot_wider(names_from = "SpeciesName", values_from = "Abundance") %>%
  #Use the Site column as row names
  column_to_rownames("SiteMet") %>%
  #Turn this tibble into a matrix for further processing
  as.matrix() %>% 
  #replacing NA values with zero
  replace_na(0)

  
################## Data from fishbase ###############
#Getting length data from fishbase.org using rfishbase
library(rfishbase)

#Example from package
## Not run: 
a <- length_length("Oreochromis niloticus", server = "fishbase")
## End(Not run)

#Using species list from UVC to get length data
b1 <- UVC %>% distinct(ValidName) %>% drop_na(ValidName)
b2 <- length_length(b1$ValidName, fields = c("Species", "Length1", "Length2", "a", "b", "Type"), 
                    server = "fishbase")
#a lot of the "a" values are zero - I feel like that is wrong.

b1 %>% distinct(ValidName) %>% arrange(ValidName)
b2 %>% distinct(Species) %>% arrange(Species)

#There are 3 species which are in the species list, but not in the database
b1$ValidName[!(b1$ValidName %in% b2$Species)]
#"Chelonia mydas" = green sea turtle. "Zalophus wollebaeki" = galapagos sea lion.
#"Aetobatus laticeps" = Pacific eagle ray, there is no length weight data on fishbase.
#Aetobatus laticeps is in the FishDB data.
################################################################################

############# Data from FishDB, how many do I actually need to change ##########

x <- UVC
y <- x %>% select(ValidName, a, b, LengthType, LenLenRatio) %>% distinct() %>% 
  drop_na() %>% filter(ValidName != c("Chelonia mydas", "Zalophus wollebaeki"))

y %>% distinct(ValidName) %>% arrange(ValidName)
length(which(y$LengthType == "TL")) #There are 7 that are already TL
#so all I need to find if I make my own column is 11 TL values for the species left.
################################################################################


# Species Richness calculation --------------------------------------------

#Subsetting columns to be used for Species Richness calculations
sric_DOVS <- subset(DOVS, select = c("Site", "ValidName", "Method"))
#Removing ID's that are not made to species level
sric_DOVS_clean <- sric_DOVS %>% filter(!str_detect(ValidName, " sp"))
#This also removes NA values from the column
any(is.na(sric_DOVS$ValidName))
any(is.na(sric_DOVS_clean$ValidName))
#Checking the ValidNames left
sric_DOVS_clean %>% distinct(ValidName) %>% arrange(ValidName) 

#Richness column
#sric_DOVS_clean 
x <- sric_DOVS_clean %>% 
  group_by(Site) %>% 
  mutate(Richness = n_distinct(ValidName)) %>% 
  select(-ValidName) 

#Preparing SRic to be made into dissimilarity matrix
sric_DOVS_mat <- sric_DOVS_clean %>% select(-c(Site, Method))
sric_DOVS_mat$SiteMet <- paste(sric_DOVS$Site, SRic_DOVS$Method) 



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
  summarise(Abundance = sum(N, na.rm = T))

#Assigning columns with species names and abundance from UVC
Abun_UVC <- subset (UVC, select = c("Site", "SpeciesName", "N", "Method"))
#Combine site and methods column
Abun_UVC$SiteMet <- paste(Abun_UVC$Site, Abun_UVC$Method)
Abun_UVC <- Abun_UVC %>% select(-c(Site, Method))
#Summing abundance of each species per site
Abun_UVC <- Abun_UVC %>%
  group_by(SiteMet, SpeciesName) %>%
  summarise(Abundance = sum(N, na.rm = T))

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
#Calculating disimilarity distance using vegan package, the default is Bray Curtis
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



