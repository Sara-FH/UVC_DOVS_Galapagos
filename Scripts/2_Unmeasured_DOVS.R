###############################################################################################################
# Title: Unmeasured individuals DOVS and precision of measurements
# Author: Sara Færch Hansen
# Assisting: Denisse Fierro Arcos
# Version: 1
# Date last updated: 2021-01-28
###############################################################################################################


# Quality control and adding NA lengths DOVS ----------------------------------------------------------
#Quality Control
#Prior to calculating biomass we need our DOVS measurements to meet two requirements
#1. RMS <= 60 (we have raised it from 20, as some of the videos are very blurry and this affects RMS)
#2. Precision <= 10% estimated Length

#To live up to the precision requirements, I have in the following lines of code removed length measurements
#that do not live up to the length requirements. Afterwards I have replaced them with average lengths for
#each species, based on the length measurements with good precision

#Removing lengths that have bad precision (worse than 10% of length)
DOVS <- DOVS %>% 
  #When precision is worse than 10% of length, I remove the length so an average can be calculated
  mutate(Length_mm = ifelse(!Precision_mm < Length_mm*0.1, NA, Length_mm)) %>% 
  #When precision is worse than 10% of length, the length is removed
  #and when there are no lengths, I remove the precision as well
  mutate(Precision_mm = ifelse(is.na(Length_mm), 0, Precision_mm)) %>% 
  #I also remove RMS, when the length is removed
  mutate(RMS_mm = ifelse(is.na(Length_mm), 0, RMS_mm)) %>% 
  uncount(., N) %>% #uncount from tidyverse to expand N to one row per individual
  #N column is lost in uncount, so it is added here
  mutate(N = 1) %>% 
  #group Site, Period and ValidName
  group_by(Site, ValidName) %>% 
  #Making column with mean length of species per site
  mutate(Length_site = mean(Length_mm, na.rm = TRUE)) %>%
  #grouping species and fishing status
  group_by(ValidName, Fishing) %>% 
  #Making column with mean length per fishing status
  mutate(Length_fishing = mean(Length_mm, na.rm = TRUE)) %>% 
  #Combine lengths in New_length
  mutate(New_length = coalesce(Length_mm, Length_site)) %>% 
  #Combine lengths in New_length
  mutate(New_length = coalesce(New_length, Length_fishing)) %>% 
  #changing order of columns
  select(Site, Period, Length_mm, Length_site, Length_fishing, New_length, everything()) %>% 
  #remove old length columns
  select(-c(Length_mm, Length_site, Length_fishing)) %>% 
  #rename length column
  rename(Length_mm = New_length) %>% 
  ungroup()
  

# Summary of species lengths ------------------------------------------------------
#Formula SE <- sd(samples)/sqrt(sample size)

#Getting length and abundance data for DOVS
lengths_DOVS <- DOVS %>% 
  select(ValidName, N, Length_mm, Method) %>% 
  #grouping by species
  group_by(ValidName) %>% 
  #Abundance for each species
  mutate(N_DOVS = sum(N)) %>% 
  #min length for each species
  mutate(minFL_DOVS = min(Length_mm)) %>% 
  #max length for each species
  mutate(maxFL_DOVS = max(Length_mm)) %>% 
  #mean length for each species
  mutate(meanFL_DOVS = mean(Length_mm)) %>% 
  #Standard error of the mean
  mutate(SE_DOVS = sd(Length_mm)/sqrt(sum(N))) %>% 
  #Removing columns that should not be in the final table
  select(-c(N, Length_mm, Method)) %>% 
  #Getting unique results
  unique() %>% 
  ungroup()

#rounding min, max and mean lengths to 0 decimals
lengths_DOVS[,3:5] <- round(lengths_DOVS[,3:5], digits = 0)
#rounding SE to 2 decimals
lengths_DOVS[,6] <- round(lengths_DOVS[,6], digits = 2)


#Loading file with length ratios to go from TL to FL for species of interest
LengthRatio <- openxlsx::read.xlsx("../Data/LengthRatio.xlsx", sheet = 1) 
str(LengthRatio)

#Getting length and abundance data for UVC
lengths_UVC <- UVC %>% 
  select(ValidName, N, Length_cm, Method) %>% 
  #length from cm to mm
  mutate(Length_mm = Length_cm*10) %>% 
  #Adding length-length ratios calculated from FishBase length-length relationships
  left_join(LengthRatio %>% select(ValidName, LengthRatio), by = "ValidName") %>% 
  #Calculating new length from TL to FL
  mutate(Length = Length_mm*LengthRatio) %>% #Length is FL in mm
  #grouping by species
  group_by(ValidName) %>% 
  #Abundance for each species
  mutate(N_UVC = sum(N)) %>% 
  #min length for each species
  mutate(minFL_UVC = min(Length)) %>% 
  #max length for each species
  mutate(maxFL_UVC = max(Length)) %>% 
  #mean length for each species
  mutate(meanFL_UVC = mean(Length)) %>% 
  
  #Standard error of the mean
  mutate(SE_UVC = sd(Length)/sqrt(sum(N))) %>% 
  #Removing columns that should not be in the final table
  select(-c(N, Length_mm, Length_cm, Length, LengthRatio, Method)) %>% 
  #Getting unique results
  unique() %>% 
  ungroup()

#rounding min, max and mean lengths to 0 decimals
lengths_UVC[,3:5] <- round(lengths_UVC[,3:5], digits = 0)
#rounding SE to 2 decimals
lengths_UVC[,6] <- round(lengths_UVC[,6], digits = 2)


#combining lengths in one dataframe
lengths <- lengths_UVC %>% 
  left_join(lengths_DOVS, by = "ValidName") %>% 
  #Changing order of columns
  select(ValidName, N_DOVS, N_UVC, minFL_DOVS, minFL_UVC, maxFL_DOVS, maxFL_UVC, 
         meanFL_DOVS, SE_DOVS, meanFL_UVC, SE_UVC) %>% 
  #arraning alphabetically
  arrange(ValidName) %>% 
  #renaming species column
  rename(Species = ValidName)


#write to excel table
write.xlsx(lengths, "../Tables/Summary_lengths.xlsx")

#Remove variables after use
rm(lengths_DOVS, lengths_UVC, lengths, LengthRatio)

