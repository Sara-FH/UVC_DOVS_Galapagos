
# UVC data trip 1-5 -------------------------------------------------------

#Loading libraries
library(janitor)
library(tidyverse)
library(xlsx)

#Loading data for all 5 trips, however trip 5 looks very strange and does not have a single individual
#in any of the size classes
UVC1 <- read.xlsx2("Data/UVC_Carnivoros_Bacalao MAgic Mystery Tour_ 2014_1st trip_FINAL.xlsx",
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
    S25 = Total.length..cms.,
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
  #I have renamed S<30 to S25, since we don't actually know the lengths of these
  #species, what length do we use?
  #It is also weird that we have a size group of 210, when the others increase by 25
  #There is also two columns with the size group 300, one I had to call 301 since columns
  #need to be unique
  #I renamed S>400 to S450
  slice(-1) %>% #Deleting row 1 (to remove extra headers)
  mutate(across(where(is.character), str_trim)) %>% #remove any extra spaces
  mutate_all(na_if, "") %>% #make empty cells NA
  drop_na(Species) %>% #Drop lines with no species in them
  #"Lengthening" data set, so there is one row for each species and size class using
  #the pivot_longer() function. Here, we use all columns except species. The column names will
  #go under the column SizeClass and their values will go under the column N.
  pivot_longer(cols = c(24:46), names_to = "SizeClass", values_to = "N") %>%
  transform(N = as.numeric(N)) %>%
  group_by(Site, Species, SizeClass) %>% 
  mutate(N = sum(N, na.rm = TRUE)) %>% 
  #summing total number of species per size class at each site
  ungroup(Site, Species, SizeClass) %>% 
  mutate(Trip = paste("1")) #Adding line with the trip number

UVC2 <- read.xlsx2("Data/UVC_Carnivoros_Bacalao MAgic Mystery Tour_ 2014_2nd trip_FINAL.xlsx", 
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
    S25 = Total.length..cms.,
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
  #I have renamed S<30 to S25, since we don't actually know the lengths of these
  #species, what length do we use?
  #It is also weird that we have a size group of 210, when the others increase by 25
  #There is also two columns with the size group 300, one I had to call 301 since columns
  #need to be unique
  #I renamed S>400 to S450
  slice(-1) %>% #Deleting row 1 (to remove extra headers)
  mutate(across(where(is.character), str_trim)) %>% #remove any extra spaces
  mutate_all(na_if, "") %>% #make empty cells NA
  drop_na(Species) %>% #Drop lines with no species in them
  #"Lengthening" data set, so there is one row for each species and size class using
  #the pivot_longer() function. Here, we use all columns except species. The column names will
  #go under the column SizeClass and their values will go under the column N.
  pivot_longer(cols = c(28:50), names_to = "SizeClass", values_to = "N") %>%
  transform(N = as.numeric(N)) %>%
  group_by(Site, Species, SizeClass) %>% 
  #summing total number of species per size class at each site
  mutate(N = sum(N, na.rm = TRUE)) %>% 
  ungroup(Site, Species, SizeClass) %>% 
  mutate(Trip = paste("2")) #Adding line with the trip number

UVC3 <- read.xlsx2("Data/UVC_Carnivoros_Bacalao MAgic Mystery Tour_ 2014_3rd trip_FINAL.xlsx", 
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
    S25 = Total.length..cms.,
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
  #I have renamed S<30 to S25, since we don't actually know the lengths of these
  #species, what length do we use?
  #It is also weird that we have a size group of 210, when the others increase by 25
  #There is also two columns with the size group 300, one I had to call 301 since columns
  #need to be unique
  #I renamed S>400 to S450
  slice(-1) %>% #Deleting row 1 (to remove extra headers)
  mutate(across(where(is.character), str_trim)) %>% #remove any extra spaces
  mutate_all(na_if, "") %>% #make empty cells NA
  drop_na(Species) %>% #Drop lines with no species in them
  #"Lengthening" data set, so there is one row for each species and size class using
  #the pivot_longer() function. Here, we use all columns except species. The column names will
  #go under the column SizeClass and their values will go under the column N.
  pivot_longer(cols = c(28:50), names_to = "SizeClass", values_to = "N") %>%
  transform(N = as.numeric(N)) %>%
  group_by(Site, Species, SizeClass) %>% 
  #summing total number of species per size class at each site
  mutate(N = sum(N, na.rm = TRUE)) %>% 
  ungroup(Site, Species, SizeClass) %>% 
  mutate(Trip = paste("3")) #Adding line with the trip number

UVC4 <- read.xlsx2("Data/UVC_Carnivoros_Bacalao MAgic Mystery Tour_ 2014_4thtrip_FINAL.xlsx", 
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
    S25 = Total.length..cms.,
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
  #I have renamed S<30 to S25, since we don't actually know the lengths of these
  #species, what length do we use?
  #It is also weird that we have a size group of 210, when the others increase by 25
  #There is also two columns with the size group 300, one I had to call 301 since columns
  #need to be unique
  #I renamed S>400 to S450
  slice(-1) %>% #Deleting row 1 (to remove extra headers)
  mutate(across(where(is.character), str_trim)) %>% #remove any extra spaces
  mutate_all(na_if, "") %>% #make empty cells NA
  drop_na(Species) %>%  #Drop lines with no species in them
  #"Lengthening" data set, so there is one row for each species and size class using
  #the pivot_longer() function. Here, we use all columns except species. The column names will
  #go under the column SizeClass and their values will go under the column N.
  pivot_longer(cols = c(28:50), names_to = "SizeClass", values_to = "N") %>%
  transform(N = as.numeric(N)) %>%
  group_by(Site, Species, SizeClass) %>%  
  #summing total number of species per size class at each site
  mutate(N = sum(N, na.rm = TRUE)) %>% 
  ungroup(Site, Species, SizeClass) %>% 
  mutate(Trip = paste("4")) #Adding line with the trip number

#For this one I have an empty sheet, so I need the new excel sheet from Pelayo
UVC5 <- read.xlsx2("Data/UVC_Carnivoros_Bacalao MAgic Mystery Tour_ November 2013_5th trip_FINAL.xlsx", 
                   header = TRUE, sheetName = 1, stringsAsFactors = FALSE) %>% select(2:52) %>%
  rename(
    Transect_code = Código.transecto,
    Census_type = Tipo.de.censo,
    Date = Fecha,
    Year = Año,
    Month = Mes,
    Time = Hora, 
    Station = Estación, 
    Region = Región,
    Island = Isla, 
    Site = Sitio, 
    Diver = Buzo, 
    Bioregion = Bioregión,
    Depth = Profundidad, 
    Dive_duration = Tiempo.de.inmersión, 
    Census_duration = Duración.del.censo,
    Start_time = Start.Time, 
    Longitude_transect = Longitud.transecto,
    Current = Corriente..fuerza., 
    Temperature_unit = Unidad.de.temperatura, 
    Distance_unit = Unidad.de.distancia, 
    Thermocline_depth = Profundidad.termoclina,
    Temperature_over_thermocline = Temperatura.Agua,
    Temperature_below_thermocline = X., 
    Visibility_over_thermocline = Visibilidad,
    Visibility_below_thermocline = X..1, 
    Species = ESPECIE, 
    Sex = Sexo, 
    S25 = Longitud.Total..LT..en.cms, 
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
  #drop_na(Species) %>% #There is no species or abundance data, so dropping na species will leave nothing
  pivot_longer(cols = c(29:51), names_to = "SizeClass", values_to = "N") %>%
  transform(N = as.numeric(N)) %>%
  group_by(Site, Species, SizeClass) %>% 
  #summing total number of species per size class at each site
  mutate(N = sum(N, na.rm = TRUE)) %>% 
  ungroup(Site, Species, SizeClass) %>% 
  mutate(Trip = paste("5")) #Adding line with the trip number

#Removing the columns Habitat_type, Rugosity, Inclination and Rocky_reef_max_depth
#from UVC2-4, since these columns are not in UVC1. To combine all UVC data into one dataframe
UVC2 <- UVC2 %>% select(-c("Habitat_type", "Rugosity", "Inclination", "Rocky_reef_max_depth"))
UVC3 <- UVC3 %>% select(-c("Habitat_type", "Rugosity", "Inclination", "Rocky_reef_max_depth"))
UVC4 <- UVC4 %>% select(-c("Habitat_type", "Rugosity", "Inclination", "Rocky_reef_max_depth"))

#combining UVC1-4 in one data frame
UVC <- rbind(UVC1, UVC2, UVC3, UVC4) %>% 
  mutate(Method = "UVC")

#Making csv file of UVC data to be used for data analysis
write.csv(UVC, "Data/UVC.csv")

#checking sites at each trip
unique(UVC1$Site)
unique(UVC2$Site)
unique(UVC3$Site)
unique(UVC4$Site)
unique(UVC5$Site)

# DOVS data ---------------------------------------------------------------

DOVS <- read.xlsx2("Data/DOVS_Carnivoros_Bacalao Magic Mystery Tour2014.xlsx", header = TRUE,
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

#Checking if there are NA's
str(DOVS)
any(is.na(DOVS$Period)) #Periods has NA's
any(is.na(DOVS$Length_mm)) #Length also has NA's
any(is.na(DOVS$Precision_mm)) #There are precision data for rows with no lengths
any(is.na(DOVS$N)) #There are rows that have no abundance data, but has length data. 
#Here there should be at least 1 individual.

#Making csv file of DOVS data to be used for data analysis
write.csv(DOVS, "Data/DOVS.csv")

#Making excel file
#write.xlsx2(DOVS, "Data/DOVS.xlsx")

