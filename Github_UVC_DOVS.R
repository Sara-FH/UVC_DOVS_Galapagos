#Github data cleaning and analysis UVC and DOVS Galapagos
#############################################################
#Set working directory and Load UVC and DOVS data
setwd("C:/Users/Ejer/Desktop/Github_UVC_DOVS/Data")
UVC <- read.csv(file = "UVC_all_clean.csv", header = TRUE, stringsAsFactors = FALSE)
DOVS <- read.csv(file = "DOVS_clean.csv", header = TRUE, stringsAsFactors = FALSE)

#Removing X column for DOVS and UVC
UVC$X <- NULL
DOVS$X <- NULL

#Removing several columns - DOVs: Comments, Stage. UVC: Comments, Sex
#DOVS <- DOVS[, -c("Comments", "Stage")]
#UVC <- UVC[, -c("Comments", "Sex")]

#Remove spaces around text (characters) in DOVS, using mutate function from dplyr and trim from stringr
library(dplyr)
library(stringr)
DOVS %>%
  mutate(across(where(is.character), str_trim)) # I am not sure this is working, it seems like there are still
                                                # too many spaces between some of the genus and species names
#Combine Genus and Species into SPECIES for DOVS data
DOVS$SPECIES <- paste(DOVS$Genus , " ", DOVS$Species)

#Rename DOVS Number_individuals to N
names(DOVS)[names(DOVS) == "Number_individuals"] <- "N"

#Getting unique species for finding a and b factors to calculate biomass
library(data.table)
unique_dovs <- data.frame(unique(DOVS$SPECIES))# I am not sure why one of the unique values is empty ??
colnames(unique_dovs) <- "species"
unique_uvc <- data.frame(unique(UVC$SPECIES))   # This one does not have an empty value..
colnames(unique_uvc) <- "species"
unique_species <- unique(rbind(unique_dovs, unique_uvc))


#Calculating fish biomass

