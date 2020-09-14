#############################################################
# Title: Github data cleaning and analysis UVC and DOVS Galapagos
# Author: Sara Færch Hansen
# Assisting: Denisse Fierro Arcos
# Version: 1
# Date last updated: 2020-09-11
# Aim: Compare UVC and DOVS data in the Galapagos related to the Master thesis of Sara Færch Hansen
#############################################################

# Uploading libraries -----------------------------------------------------
library(tidyverse) 
library(data.table)
library(vegan)

#General question, I have been reading about grouping data and since I use group_by fairly often in this script
#I was considering what it means for the rest of the data analysis.
#Should I use ungroup every time I have used group_by ? To make sure the groups do not create mistakes later on

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
  recode(., "bonito" = "Sardinops sagax", #I think this is incorrect. The bonito in Ecuador
         #refers to a type of tuna rather than a pilchard. I believe the correct species is
         #Sarda orientalis. Check with Pelayo.
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
#Vector containing names of non fish species
NonFish <- c("Eretmochelys imbricata", "Chelonia mydas", "Zalophus wollebaeki", 
             "Phalacrocorax harrisi", "Cardisoma crassum", "Panulirus gracilis",
             "Scyllarides astori", "Spheniscus mendiculus", "Arctocephalus galapagoensis",
             "Lepidochelys olivacea")


#Access to the Fish data set with correct names and a/b variables to calculate biomass
FishDB <- read_csv("https://raw.githubusercontent.com/lidefi87/MangroveProject_CDF/master/Data/FishDB.csv")

#Using join to keep correct names of species in UVC data
#We do not need all columns in the UVC dataset, I am cleaning it up a bit
UVC_clean <- UVC %>% select(-c(Time, Diver, Current, Temperature, Thermocline_depth, 
                  Water_temp_over_therm, Water_temp_below_therm, Vis_over_therm,
                  Vis_below_therm, SPECIES, Comments, Habitat_type_RSM, Rugosity_0_3,
                  Inclination_0_3, Rocky_reef_max_depth))

UVC_clean2 <- UVC_clean %>% 
  left_join(FishDB %>% select(ScientificName, ValidName, Family, Genus, a, b, 
                              LengthType, LenLenRatio),
            by = c("SpeciesName" = "ScientificName")) 
#Prior to removing the SpeciesName column, we should check for NA values in the 
#ScientificName column. This way we can identify the species that are not included in our
#FishDB
#First, let's extract the rows with NA values under ValidName, but keeping all columns
UVC_clean2 %>% filter(is.na(ValidName), .preserve = T) %>% 
  #Now let's extract the unique values of SpeciesNames for which we do not have a valid name
  distinct(SpeciesName)
#We can see that these species either do not appear in the FishDB or they do not have the
#same spelling. I have updated FishDB so we now have these values included
#Check Decapterus sanctaehelenae (now known as D. punctatus) because it is not recorded in
#the Pacific, only the Atlantic. According to the STRI website, there are only three species
#of Decapterus: D. macarellus, D. macrosoma, and D. muroadsi. Talk to Pelayo about this.

#Now that corrections have been made, change remove any non-fish species
UVC_clean <- UVC_clean2 %>% 
  #We keep only observations for which SpeciesNames is not included in the NonFish vector
  filter(!ValidName %in% NonFish) 
  #Remove SpeciesName column when D. santaehelenae is sorted out
  # %>% select(-SpeciesName)
#Remove this variable as it is no longer needed
rm(UVC_clean2)


#Using join to keep correct names of species in DOVS data
DOVS2 <- DOVS %>% 
  #Correcting Mycteroperca sp to M. olfax
  mutate(SpeciesName = case_when(SpeciesName == "Mycteroperca sp" ~ "Mycteroperca olfax",
                                 TRUE ~ SpeciesName)) %>% 
  left_join(FishDB %>% select(ScientificName, ValidName, a, b, LengthType, LenLenRatio),
            by = c("SpeciesName" = "ScientificName")) 
#We do not drop names without first checking why this is the case

#We follow the same steps as before
DOVS2 %>% filter(is.na(ValidName), .preserve = T) %>% 
  #Now let's extract the unique values of SpeciesNames for which we do not have a valid name
  distinct(SpeciesName) # I get a zero here it writes: "[1] SpeciesName" and "<0 rows> (or 0-length row.names)"

#Check with Pelayo if the following genus have more than one species reported in the GMR
#Zanclus genus aside from Z. cornutus
#Auslostomidae family aside from Aulustomus genus and A. chinensis
#Aulostomus genus aside from A. chinensis
#Holacanthus genus aside from H. passer
#Sufflamen genus aside from S. verres
#Uraspis genus aside from U. helvola

#Now we remove non fish species
DOVS <- DOVS2 %>% 
  filter(!ValidName %in% NonFish) %>% 
  select(-c(SpeciesName))
#Remove duplicate variable no longer needed
rm(DOVS2)

#No NAs and the two non-fish species are also removed
DOVS %>% distinct(ValidName) %>% arrange(ValidName) 


# Biomass calculations ----------------------------------------------------
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
  
#We need to ask a few questions before continuing with our calculations:
#1. Is this point data or MaxN data? This is important because we have a stage column and
#if this is a MaxN then we will need to figure out if N is related to MaxN per species or
#MaxN per stage per species
#2. What do we do with data points with no periods attached to it?

#The following code assumes that this is point data and that we need to remove any rows
#with no period
DOVS %>% filter(!Period == "")

#Calculate the biomass for DOVS using FishDB a and b values for FL
str(DOVS) #LenLenRatio is character and needs to be numeric
Biomass_DOVS <- DOVS %>% select(Site, Length_cm, N, Method, ValidName, a, b, 
                                LengthType, LenLenRatio) %>%
  mutate(LenLenRatio = as.numeric(LenLenRatio)) %>% 
  mutate(Biomass = (a*LenLenRatio*Length_cm)^b) %>% 
  mutate(Biomass_N = Biomass*N)
#what do we do with rows that are missing lengths?

#summing biomass for each species in Biomass_sp
Biomass_DOVS <- Biomass_DOVS %>% 
  group_by(Site, ValidName) %>% 
  mutate(Biomass_sp = sum(Biomass_N)) %>%
  ungroup(Site, ValidName)

#Making matrix for dissimilarity calculation
Biomass_DOVS_mat <- Biomass_DOVS %>% select(Site, Method, ValidName, Biomass_sp) %>% #Formerly we used Biomass_N
  #but I changed it to Biomass_sp because that is the biomass for each species at the sites
  mutate(SiteMet = paste(Site, Method)) %>% 
  select(-c(Site, Method))

any(is.na(Biomass_DOVS_mat))

# NA's are introduced, because when there is no species ID e.g. Balistidae sp, there is
#also no length measurement. Should I delete rows with NA's in the abundance columns?

#Dropping rows with NA values and deleting duplicates
DOVS_mat <- Biomass_DOVS_mat %>% drop_na() %>% 
  filter(!duplicated(.))

#Making matrix
DOVS_mat <- DOVS_mat %>% 
  pivot_wider(names_from = "ValidName", values_from = "Biomass_sp") %>% 
  column_to_rownames("SiteMet") %>%
  as.matrix() %>% 
  replace_na(0)

any(is.na(DOVS_mat))
DOVS_mat %>% head()

#Dummy_data to plot two different data sets
Dummy_data <- Biomass_DOVS_mat
Dummy_data$SiteMet <- Dummy_data$SiteMet %>% str_replace_all("DOVS", "Dummy")
#Dropping rows with NA values and deleting duplicates
Dummy_data <- Dummy_data %>% drop_na() %>% 
  filter(!duplicated(.))
Dummy_data$Biomass_sp <- Dummy_mat$Biomass_sp*runif(361, min = 10, max = 100)

#Making matrix dummy_data
Dummy_mat <- Dummy_data %>% 
  pivot_wider(names_from = "ValidName", values_from = "Biomass_sp") %>% 
  column_to_rownames("SiteMet") %>%
  as.matrix() %>% 
  replace_na(0)

#Combining DOVS and dummy_data
DOVS_dummy <- rbind(DOVS_mat, Dummy_mat)

#Applying a 4th root transformation to matrix
x <- DOVS_mat^(1/4)
x2 <- DOVS_dummy^(1/4)
#Calculating dissimilarity distance using vegan package, the default is Bray Curtis
y <- vegdist(x, method = "bray")
y2 <- vegdist(x2, method = "bray")
#Create a PCoA (Principal Co-ordinates Analysis) plot
z <- wcmdscale(y, eig = T)
z2 <- wcmdscale(y2, eig = T)
#Show plot
plot(z)
plot(z2)

#binding PCO coordinates to dataframe
pco1 <- rbind(Dummy_data, Biomass_DOVS_mat) %>% 
  filter(!duplicated(.))


pco2 <- as.data.frame(z2$points[,1:2])
library(data.table)
pco2 <- setDT(pco2, keep.rownames = TRUE)[]
pco2 <- pco2 %>% rename(Sites = rn)
pco2 <- pco2 %>% mutate(Method = case_when(Sites == "DOVS" ~ "DOVS",
                                           Sites == "Dummy" ~ "Dummy"))
#Not working....

(Method = case_when(Sites == "DOVS" ~ "DOVS",
                    Sites == "Dummy" ~ "Dummy"))
str(pco2)
           
           #replace(Sites, c("DOVS", "Dummy"), ""))
  
  Zones <- Sites %>%
  mutate(category = case_when(zoning == "Comparación y protección (2.1)" ~ 2.1, 
                              zoning == "Conservación y uso no extractivo (2.2)" ~ 2.2,
                              zoning == "Conservación y uso extractivo (2.3)" ~ 2.3,
                              zoning == "Manejo especial (2.4)" ~ 2.4)) 




#Now I only need to calculate the biomass for the UVC data, so that I can add these to the plot
ordiplot(z2)
ordihull(z2, groups = c(str_detect("Dummy"), str_detect("DOVS")))





############# Data from FishDB, how many do I actually need to change ##########

x <- UVC
y <- x %>% select(ValidName, a, b, LengthType, LenLenRatio) %>% distinct() %>% 
  drop_na() %>% filter(ValidName != c("Chelonia mydas", "Zalophus wollebaeki"))

y %>% distinct(ValidName) %>% arrange(ValidName)
length(which(y$LengthType == "TL")) #There are 7 that are already TL
#so all I need to find if I make my own column is 11 TL values for the species left.
################################################################################


# Species Richness calculation --------------------------------------------

#Species richness can be calculated with vegan::specnumber() on the density matrix
#While vegan::diversity() will allow you to calculate diversity indices


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



