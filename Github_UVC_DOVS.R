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


# Uploading data ----------------------------------------------------------
#Loading UVC and DOVS data
UVC <- read.csv(file = "Data/UVC.csv", header = TRUE,
                stringsAsFactors = FALSE) %>% select(-X)

DOVS_FullDB <- read.csv(file = "Data/DOVS.csv", header = TRUE, 
                 stringsAsFactors = FALSE) %>% select(-X)

#Checking if the two methods have the same number of sites
unique(DOVS_FullDB$Site)
unique(UVC$Site)
#They do not have the same number of sites - also they have different names.

#Vector containing names of non fish species
NonFish <- c("Eretmochelys imbricata", "Chelonia mydas", "Zalophus wollebaeki", 
             "Phalacrocorax harrisi", "Cardisoma crassum", "Panulirus gracilis",
             "Scyllarides astori", "Spheniscus mendiculus", "Arctocephalus galapagoensis",
             "Lepidochelys olivacea")


#Access to the Fish data set with correct names and a/b variables to calculate biomass
FishDB <- read_csv("https://raw.githubusercontent.com/lidefi87/MangroveProject_CDF/master/Data/FishDB.csv")

#Loading site keys - Spreadsheet matching site names in both methods - DFA
SiteKeys <- openxlsx::read.xlsx("Data/SiteKeys.xlsx") #This is how you load things from Excel

#Loading data for sites open or closed to fishing
Status <- openxlsx::read.xlsx("Data/Bacalao_MagicMysteryTour_GPS_Coordinates_Respaldo.xlsx") %>% 
  select(-c(X9, X10))

#Loading data for site length
Site_length <- openxlsx::read.xlsx("Data/Periods & Transect Lengths_Bacalao Magic mystery tour_2014.xlsx")
#Time and date is read wrong


# Tidying up DOVS data -----------------------------------------------------
#I am removing any unnecessary code which was here as an explanation only. I have also
#merged all data cleaning steps for DOVS into one chunk so it is easier to follow. You can
#do the same for the UVC data - DFA

#Tidying up DOVS data - Saving the clean data set as a different variable - DFA
DOVS <- DOVS_FullDB %>% 
  #Removing columns that are not needed for this analysis - DFA
  select(-c(Comment, Stage, Depth)) %>% 
  #Removing empty rows in Family and period
  filter(Family != "" & Period != "") %>% 
  #Adding 1 when N is NA, as all those with measurements must have 1 individual
  mutate(N = replace_na(N, 1)) %>% 
  #Removing rows with no length information - DFA
  drop_na(Length_mm) %>% 
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
  select(-c(SpeciesName)) %>% 
#We are now going to standardise site names across both methods. I have only included sites
#that are duplicated in both datasets available in the GitHub repo. Go through them and 
#edit if necessary. The column site name is the site with no spaces and sitecode is a 
#shorten version of the site name and island that can be used in graphs - DFA
  select(-Island) %>% 
  #Using inner join so only site names that appear in both dataframes are kept - DFA
  inner_join(SiteKeys %>% select(-UVC), by = c("Site"="DOVS")) %>% 
  #Remove the column Site so only the standardised names remain
  select(-Site) %>% 
  #Remove any accents in the names of sites and islands
  mutate(SiteName = stringi::stri_trans_general(SiteName, "Latin-ASCII"),
         Island = stringi::stri_trans_general(Island, "Latin-ASCII"))

#Make sure site names are the same in DOVS and UVC
#Make sure the species are correct - e.g. from species list
#Maybe delete individuals smaller than 30 cm
#Make column with area of each site in square meters, width always 5 m
#for this use 


# Cleaning multiple individuals with one measurement for raw data ---------


#Note that once the data has been cleaned, most of the rows with multiple individuals have
#disappeared. So I am going to go back to the raw data and show you how we can deal with
#these rows - DFA
MultiLength <- DOVS_FullDB %>% 
  #Removing columns that are not needed for this analysis - DFA
  select(-c(Comment, Stage, Depth)) %>% 
  #Removing empty rows in Family and period
  filter(Family != "" & Period != "") %>% 
  #Removing rows with no length information - DFA
  drop_na(Length_mm) %>% 
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
  #Removing rows with no length information - DFA
  drop_na(Length_mm) %>% 
  #Rename Number_individuals column
  rename("N" = "Number_individuals") %>% 
  #Keep rows with more than one individual
  filter(N == 1) %>% 
  #Remove any instances where the precision is > 10% of length
  filter(Precision_mm < Length_mm*0.1) %>% 
  bind_rows(MLcorr)


# Tidying up UVC data -----------------------------------------------------

#Checking species names in UVC data
UVC %>% distinct(Species) %>% arrange(Species)

#Tidying up UVC data
UVC <- UVC %>% mutate(SpeciesName = 
                        recode(Species, "bonito" = "Sarda orientalis", 
                               #The bonito in Ecuador refers to Sarda orientalis
                               "Paralabrax albomaclatus" = "Paralabrax albomaculatus",
                               "yellow tail snapper" = "Lutjanus argentiventris",
                               "Zalophus wolebacki" = "Zalophus wollebaeki",
                               "Hoplopagrus guenteri" = "Hoplopagrus guentherii",
                               "Zalophus wollebackii" = "Zalophus wollebaeki",
                               "Zalophus wollebacki" = "Zalophus wollebaeki",
                               "Heterodonthus quoyi" = "Heterodontus quoyi",
                               "Myvteroperca olfax" = "Mycteroperca olfax")) %>% 
  #Dropping columns we do not need
  select(-c(Time, Diver, Current, Temperature_unit, Thermocline_depth, 
            Temperature_over_thermocline, Temperature_below_thermocline, Visibility_over_thermocline,
            Visibility_over_thermocline, Visibility_below_thermocline, Species, Comments, 
            Dive_duration, Census_duration, Distance_unit, Sex, Total, Depth)) %>%
  rename(Transect_length_m = Transect_length) %>% #renaming to indicate that length is in meters
  #Joining to keep correct names of species in UVC data from FishDB
  left_join(FishDB %>% select(ScientificName, ValidName, Family, Genus, a, b, 
                              LengthType, LenLenRatio),
            by = c("SpeciesName" = "ScientificName")) %>% 
  #Decide what to do for Decapterus sanctahelenae, e.g. should the name be changed?
  #Now that corrections for D. sanctahelenae have been made
  #Remove any non-fish species
  filter(!ValidName %in% NonFish)
#Remove SpeciesName column when D. santaehelenae is sorted out
# %>% select(-SpeciesName)

#Extracting rows with NA values under ValidName, but keeoing all columns
#to identify if there are any species not included in our FishDB
UVC_clean %>% filter(is.na(ValidName), .preserve = T) %>% 
  #Now let's extract the unique values of SpeciesNames for which we do not have a valid name
  distinct(SpeciesName) #FishDB was updated with Decapterus sanctahelenae (now knoen as D. punctatus)
#Check this ID, because it is not recorded in the Pacific, only the Atlantic. According to the STRI website, 
#there are only three species of Decapterus: D. macarellus, D. macrosoma, and D. muroadsi. 
#Talk to Pelayo about this.


#Correct site names in UVC - so that site names are similar in DOVS and UVC
#Make sure the species are correct - e.g. from species list
#Maybe delete individuals smaller than 30 cm
#Make column with area of each site in square meters, width always 5 m

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

#Calculate the biomass for DOVS using FishDB a and b values for FL
str(DOVS) #LenLenRatio is character and needs to be numeric
Biomass_DOVS <- DOVS %>% select(Site, Length_cm, N, Method, ValidName, a, b, 
                                LengthType, LenLenRatio) %>%
  mutate(LenLenRatio = as.numeric(LenLenRatio)) %>% 
  #The fish biomass equation is W = a*L^b, therefore first transform the length, 
  #then apply the exponent b and finally multiply by a.
  #The equation is therefore: a*((LenLenRatio*Length_cm)^b)
  mutate(Biomass_N = a*((LenLenRatio*Length_cm)^b))

#Calculating mean biomass for each family at each site
mean_bio_family <- Biomass_DOVS %>% 
  filter(!endsWith(ValidName, " sp")) %>% #removing rows only identified to genus or family level
  group_by(Site, Family) %>% 
  mutate(mean_bio_family = mean(Biomass_N)) %>% #calculating mean biomass for each family at each site
  summarise(mean_bio_family) %>% 
  distinct() %>% 
  ungroup(Site, Family)
#Finding the mean biomass for the family balistidae at site Arrecife Antiguo
#as this is where we have the two individuals identified as "Balistidae sp"
mean_bio_family[17,] # 1138.24431

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



