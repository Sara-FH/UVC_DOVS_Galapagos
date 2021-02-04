###############################################################################################################
# Title: Combining UVC data and DOVS data to csv
# Author: Sara Færch Hansen
# Assisting: Denisse Fierro Arcos
# Version: 1
# Date last updated: 2021-01-28
###############################################################################################################


# Loading libraries -------------------------------------------------------
#Loading libraries
library(janitor)
library(tidyverse)
library(xlsx)

# UVC data trip 1-5 -------------------------------------------------------
#Loading data for trip 1
UVC1 <- read.xlsx2("Data/Archive/UVC_Carnivoros_Bacalao MAgic Mystery Tour_ 2014_1st trip_FINAL.xlsx",
                   sheetName = 1, header = TRUE, stringsAsFactors = FALSE) %>% select(2:49) %>%
  rename(
    Transect_code = Trasect.code,
    Dive_duration = Dive.duration,
    Transect_length = Transect.length,
    Census_duration = Census.duration,
    Temperature_unit = Temperature..unit.,
    Distance_unit = Distance..unit.,
    Thermocline_depth = Thermocline.depth,
    Temperature_over_thermocline = Water.temperature,
    Temperature_below_thermocline = X.,
    Visibility_over_thermocline = Visibility,
    Visibility_below_thermocline = X..1,
    Species = SPECIES,
    S20 = Total.length..cms.,
    S30 = X..2,
    S35 = X..3,
    S40 = X..4,
    S50 = X..5,
    S60 = X..6,
    S70 = X..7,
    S80 = X..8,
    S90 = X..9,
    S100 = X..10,
    S110 = X..11,
    S125 = X..12,
    S150 = X..13,
    S175 = X..14,
    S200 = X..15,
    S210 = X..16,
    S225 = X..17,
    S250 = X..18,
    S300 = X..19,
    S325 = X..20,
    S350 = X..21,
    S400 = X..22,
    S450 = X..23) %>%
  #I have renamed S<30 to S20, as I took the average length, as species were between 10 and 30 cm.
  #S>400 renamed to S450, to avoid use of > for later making lengths numeric, 
  #no individuals are detected at this length.
  slice(-1) %>% #Deleting row 1 (to remove extra headers)
  mutate(across(where(is.character), str_trim)) %>% #remove any extra spaces
  mutate_all(na_if, "") %>% #make empty cells NA
  drop_na(Species) %>% #Drop lines with no species in them
  #"Lengthening" data set, so there is one row for each species and size class using
  #the pivot_longer() function. Here, we use all columns except species. The column names will
  #go under the column SizeClass and their values will go under the column N.
  pivot_longer(cols = c(24:46), names_to = "SizeClass", values_to = "N") %>%
  transform(N = as.numeric(N)) %>%
  mutate(Trip = paste("1")) #Adding line with the trip number

#Loading data for trip 2
UVC2 <- read.xlsx2("Data/Archive/UVC_Carnivoros_Bacalao MAgic Mystery Tour_ 2014_2nd trip_FINAL.xlsx", 
                   header = TRUE, sheetName = 1, stringsAsFactors = FALSE) %>% select(2:53) %>%
  rename(
    Transect_code = Trasect.code,
    Dive_duration = Dive.duration,
    Transect_length = Transect.length,
    Census_duration = Census.duration,
    Temperature_unit = Temperature..unit.,
    Distance_unit = Distance..unit.,
    Thermocline_depth = Thermocline.depth,
    Temperature_over_thermocline = Water.temperature,
    Temperature_below_thermocline = X.,
    Visibility_over_thermocline = Visibility,
    Visibility_below_thermocline = X..1,
    Habitat_type = Habitat.type..R..S..M., 
    Rugosity = Rugosity..0.3.,
    Inclination = Inclination..0.3.,
    Rocky_reef_max_depth = Rocky.reef.max.depth,
    Species = SPECIES,
    S20 = Total.length..cms.,
    S30 = X..2,
    S35 = X..3,
    S40 = X..4,
    S50 = X..5,
    S60 = X..6,
    S70 = X..7,
    S80 = X..8,
    S90 = X..9,
    S100 = X..10,
    S110 = X..11,
    S125 = X..12,
    S150 = X..13,
    S175 = X..14,
    S200 = X..15,
    S210 = X..16,
    S225 = X..17,
    S250 = X..18,
    S300 = X..19,
    S325 = X..20,
    S350 = X..21,
    S400 = X..22,
    S450 = X..23) %>%
  slice(-1) %>% #Deleting row 1 (to remove extra headers)
  mutate(across(where(is.character), str_trim)) %>% #remove any extra spaces
  mutate_all(na_if, "") %>% #make empty cells NA
  drop_na(Species) %>% #Drop lines with no species in them
  #"Lengthening" data set, so there is one row for each species and size class using
  #the pivot_longer() function. Here, we use all columns except species. The column names will
  #go under the column SizeClass and their values will go under the column N.
  pivot_longer(cols = c(28:50), names_to = "SizeClass", values_to = "N") %>%
  transform(N = as.numeric(N)) %>%
  mutate(Trip = paste("2")) #Adding line with the trip number

#Loading data for trip 3
UVC3 <- read.xlsx2("Data/Archive/UVC_Carnivoros_Bacalao MAgic Mystery Tour_ 2014_3rd trip_FINAL.xlsx", 
                   header = TRUE, sheetName = 1, stringsAsFactors = FALSE) %>% select(2:53) %>%
  rename(
    Transect_code = Trasect.code,
    Dive_duration = Dive.duration,
    Transect_length = Transect.length,
    Census_duration = Census.duration,
    Temperature_unit = Temperature..unit.,
    Distance_unit = Distance..unit.,
    Thermocline_depth = Thermocline.depth,
    Temperature_over_thermocline = Water.temperature,
    Temperature_below_thermocline = X.,
    Visibility_over_thermocline = Visibility,
    Visibility_below_thermocline = X..1,
    Habitat_type = Habitat.type..R..S..M., 
    Rugosity = Rugosity..0.3.,
    Inclination = Inclination..0.3.,
    Rocky_reef_max_depth = Rocky.reef.max.depth,
    Species = SPECIES,
    S20 = Total.length..cms.,
    S30 = X..2,
    S35 = X..3,
    S40 = X..4,
    S50 = X..5,
    S60 = X..6,
    S70 = X..7,
    S80 = X..8,
    S90 = X..9,
    S100 = X..10,
    S110 = X..11,
    S125 = X..12,
    S150 = X..13,
    S175 = X..14,
    S200 = X..15,
    S210 = X..16,
    S225 = X..17,
    S250 = X..18,
    S300 = X..19,
    S325 = X..20,
    S350 = X..21,
    S400 = X..22,
    S450 = X..23) %>%
  slice(-1) %>% #Deleting row 1 (to remove extra headers)
  mutate(across(where(is.character), str_trim)) %>% #remove any extra spaces
  mutate_all(na_if, "") %>% #make empty cells NA
  drop_na(Species) %>% #Drop lines with no species in them
  #"Lengthening" data set, so there is one row for each species and size class using
  #the pivot_longer() function. Here, we use all columns except species. The column names will
  #go under the column SizeClass and their values will go under the column N.
  pivot_longer(cols = c(28:50), names_to = "SizeClass", values_to = "N") %>%
  transform(N = as.numeric(N)) %>%
  mutate(Trip = paste("3")) #Adding line with the trip number

#Loading data for trip 4
UVC4 <- read.xlsx2("Data/Archive/UVC_Carnivoros_Bacalao MAgic Mystery Tour_ 2014_4thtrip_FINAL.xlsx", 
                   header = TRUE, sheetName = 1, stringsAsFactors = FALSE) %>% select(2:53) %>%
  rename(
    Transect_code = Trasect.code,
    Dive_duration = Dive.duration,
    Transect_length = Transect.length,
    Census_duration = Census.duration,
    Temperature_unit = Temperature..unit.,
    Distance_unit = Distance..unit.,
    Thermocline_depth = Thermocline.depth,
    Temperature_over_thermocline = Water.temperature,
    Temperature_below_thermocline = X.,
    Visibility_over_thermocline = Visibility,
    Visibility_below_thermocline = X..1,
    Habitat_type = Habitat.type..R..S..M., 
    Rugosity = Rugosity..0.3.,
    Inclination = Slope..0.3.,
    Rocky_reef_max_depth = Rocky.reef.max.depth,
    Species = SPECIES,
    S20 = Total.length..cms.,
    S30 = X..2,
    S35 = X..3,
    S40 = X..4,
    S50 = X..5,
    S60 = X..6,
    S70 = X..7,
    S80 = X..8,
    S90 = X..9,
    S100 = X..10,
    S110 = X..11,
    S125 = X..12,
    S150 = X..13,
    S175 = X..14,
    S200 = X..15,
    S210 = X..16,
    S225 = X..17,
    S250 = X..18,
    S300 = X..19,
    S301 = X..20,
    S350 = X..21,
    S400 = X..22,
    S450 = X..23) %>%
  slice(-1) %>% #Deleting row 1 (to remove extra headers)
  mutate(across(where(is.character), str_trim)) %>% #remove any extra spaces
  mutate_all(na_if, "") %>% #make empty cells NA
  drop_na(Species) %>%  #Drop lines with no species in them
  #"Lengthening" data set, so there is one row for each species and size class using
  #the pivot_longer() function. Here, we use all columns except species. The column names will
  #go under the column SizeClass and their values will go under the column N.
  pivot_longer(cols = c(28:50), names_to = "SizeClass", values_to = "N") %>%
  transform(N = as.numeric(N)) %>%
  mutate(Trip = paste("4")) #Adding line with the trip number

#Loading data for trip 5
UVC5 <- read.xlsx2("Data/Archive/DB Transectos visuales Agosto 2014 DW.xlsx", 
                   header = TRUE, sheetName = 1, stringsAsFactors = FALSE) %>% select(1:50) %>%
  rename(
    Transect_code = Trasect.code,
    Dive_duration = Dive.duration,
    Transect_length = Transect.length,
    Census_duration = Census.duration,
    Temperature_unit = Temperature..unit.,
    Distance_unit = Distance..unit.,
    Thermocline_depth = Thermocline.depth,
    Habitat_type = Habitat.type..R..S..M., 
    Rugosity = Rugosity..0.3.,
    Inclination = Inclination..0.3.,
    Rocky_reef_max_depth = Rocky.reef.max.depth,
    S20 = S.30,
    S325 = S300.1,
    S450 = S.400) %>%
  #Dropping columns
  select(-c(Over.thermocline, Below.thermocline, Over.thermocline.1, Below.thermocline.1)) %>% 
  mutate(across(where(is.character), str_trim)) %>% #remove any extra spaces
  mutate_all(na_if, "") %>% #make empty cells NA
  drop_na(Species) %>%  #Drop lines with no species in them
  #"Lengthening" data set, so there is one row for each species and size class using
  #the pivot_longer() function. Here, we use all columns except species. The column names will
  #go under the column SizeClass and their values will go under the column N.
  pivot_longer(cols = c(24:46), names_to = "SizeClass", values_to = "N") %>%
  transform(N = as.numeric(N)) %>%
  mutate(Trip = paste("5")) #Adding line with the trip number

#Renaming the two Fondeadero sites in UVC data to contain Island as well
UVC4 <- UVC4 %>% mutate(Site = 
                       recode(Site, "Fondeadero" = "Santa Fe Fondeadero"))
UVC5 <- UVC5 %>% mutate(Site = 
                          recode(Site, "Fondeadero" = "Wolf Fondeadero"))

#Removing columns with Thermocline, visibility, Total and Comment from UVC1, UVC2, UVC3 and UVC4
#As these are not in UVC5 
#Also removing the columns Habitat_type, Rugosity, Inclination and Rocky_reef_max_depth
#from UVC2-5, since these columns are not in UVC1. To combine all UVC data into one data frame
UVC1 <- UVC1 %>% select(-c("Temperature_over_thermocline", "Temperature_below_thermocline",
                           "Visibility_over_thermocline", "Visibility_below_thermocline", 
                           "Total", "Comments"))
UVC2 <- UVC2 %>% select(-c("Habitat_type", "Rugosity", "Inclination", "Rocky_reef_max_depth", 
                           "Temperature_over_thermocline", "Temperature_below_thermocline",
                           "Visibility_over_thermocline", "Visibility_below_thermocline", 
                           "Total", "Comments"))
UVC3 <- UVC3 %>% select(-c("Habitat_type", "Rugosity", "Inclination", "Rocky_reef_max_depth", 
                           "Temperature_over_thermocline", "Temperature_below_thermocline",
                           "Visibility_over_thermocline", "Visibility_below_thermocline", 
                           "Total", "Comments"))
UVC4 <- UVC4 %>% select(-c("Habitat_type", "Rugosity", "Inclination", "Rocky_reef_max_depth", 
                           "Temperature_over_thermocline", "Temperature_below_thermocline",
                           "Visibility_over_thermocline", "Visibility_below_thermocline", 
                           "Total", "Comments"))
UVC5 <- UVC5 %>% select(-c("Habitat_type", "Rugosity", "Inclination", "Rocky_reef_max_depth"))


#combining UVC1-5 in one data frame
UVC <- rbind(UVC1, UVC2, UVC3, UVC4, UVC5) %>% 
  mutate(Method = "UVC") %>% 
  mutate(N = case_when(is.na(N) ~ 0, 
                       TRUE ~ N))

#checking sites at each trip
unique(UVC1$Site)
unique(UVC2$Site)
unique(UVC3$Site)
unique(UVC4$Site)
unique(UVC5$Site)


# Tidying up UVC data -----------------------------------------------------

#Checking species names in UVC data
UVC %>% distinct(Species) %>% arrange(Species)

#Tidying up UVC data
UVC <- UVC %>% mutate(SpeciesName =
                        #The bonito in Ecuador is Sarda orientalis
                        recode(Species, "bonito" = "Sarda orientalis", 
                               #Correcting misspelled names in UVC data
                               "Paralabrax albomaclatus" = "Paralabrax albomaculatus",
                               "yellow tail snapper" = "Lutjanus argentiventris",
                               "Zalophus wolebacki" = "Zalophus wollebaeki",
                               "Hoplopagrus guenteri" = "Hoplopagrus guentherii",
                               "Zalophus wollebackii" = "Zalophus wollebaeki",
                               "Zalophus wollebacki" = "Zalophus wollebaeki",
                               "Heterodonthus quoyi" = "Heterodontus quoyi",
                               "Myvteroperca olfax" = "Mycteroperca olfax", 
                               "Dermatolepsis dermatolepsis" = "Dermatolepis dermatolepis"))

#Making csv file of UVC data to be used for data analysis
write.csv(UVC, "Data/UVC.csv")

# DOVS data ---------------------------------------------------------------

DOVS <- read.xlsx2("Data/Archive/DOVS_Carnivoros_Bacalao Magic Mystery Tour2014.xlsx", header = TRUE,
                   sheetName = 1, stringsAsFactors = FALSE) %>% 
  rename(
    Site = Sitio, 
    Island = Isla,
    Length_mm = Length..mm.,
    Precision_mm = Precision..mm.,
    RMS_mm = RMS..mm., 
    Range_mm = Range..mm.,
    N = Number.of.Individuals) %>% 
  mutate(Method = "DOVS") %>% 
  mutate(N = as.numeric(N))


# Tidying up DOVS data ----------------------------------------------------
#Checking if there are NA's
str(DOVS)
any(is.na(DOVS$Period)) #Periods has NA's
any(is.na(DOVS$Length_mm)) #Length also has NA's
any(is.na(DOVS$Precision_mm)) #There are precision data for rows with no lengths
any(is.na(DOVS$N)) #There are rows that have no abundance data, but has length data. 

#When there is a length measurement, there is also an individual, therefore N = NA is changed to N = 1.
DOVS <- DOVS %>% 
  mutate(N = replace_na(N, 1))

#Making csv file of DOVS data to be used for data analysis
write.csv(DOVS, "Data/DOVS.csv")


