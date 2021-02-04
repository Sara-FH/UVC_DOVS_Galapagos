###############################################################################################################
# Title: Calculations for PCO on biomass data
# Author: Sara Færch Hansen
# Assisting: Denisse Fierro Arcos
# Version: 1
# Date last updated: 2021-01-28
###############################################################################################################

# Uploading library -----------------------------------------------------
library(ape) #For principal coordinates analysis
library(vegan)

# Biomass calculations for PCO ---------------------------------------------

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
  #Question - change they way to calculate per 500m2 if we decide to do it for all calculations
  summarise(Gram_500m2 = (Biomass_sp_period/Transect_area)*500, 
            Method, Fishing, SiteCode) %>% 
  #Calculating the biomass of each species per site in g per 500m2 (as average of periods)
  #Question DFA - here I do not use empty periods to calculate mean, I only take the mean
  #for periods were the species is present, is this correct or should I add periods with zero counts?
  summarise(Gram_site_sp = mean(Gram_500m2), 
            ValidName, Method, Fishing, SiteCode) %>%
  #Going from g to kg for the biomass
  mutate(Kg_site_sp = Gram_site_sp/1000) %>% #Biomass in kg
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
  #Question - change they way to calculate per 500m2 if we decide to do it for all calculations
  summarise(Gram_500m2 = (Biomass_sp_period/Transect_area)*500, 
            Method, Fishing, SiteCode) %>% 
  #Calculating the biomass of each species per site in g per 500m2 (as average of periods)
  #Question - check mean calculation
  summarise(Gram_site_sp = mean(Gram_500m2), 
            ValidName, Method, Fishing, SiteCode) %>%
  #Going from g to kg for the biomass
  mutate(Kg_site_sp = Gram_site_sp/1000) %>% #Biomass in kg
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
  mutate(Kg_site_sp = 0.6)
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
  pivot_wider(names_from = "ValidName", values_from = "Kg_site_sp") %>% 
  #Adding dummy species to all sites, to enable dissimilarity calculations later
  mutate(Dummy = 0.6) %>% 
  #As I have some empty sites, where it is important to see how the methods UVC and DOVS differ
  arrange(SiteMet) %>% #Arranging site names
  column_to_rownames("SiteMet") %>% #Making a column into row names for the matrix
  as.matrix() %>% 
  replace_na(0) #Putting 0 instead of NA, when the species was not observed at a site

#Checking ranges for transformations and no transformation
range(Bio_mat)
range(Bio_mat^0.5)
range(Bio_mat^0.25)

#Applying a 4th root transformation to biomass data - and making a matrix
Bio_mat <- Bio_mat^0.25 

#Changing Dummy species data to smallest other biomass data in matrix as suggested by Clarke et al. 2006
Bio_mat[, "Dummy"] <- 0.6 
#Question DFA - the paper below states that for a better community assemblage I should use the smallest 
#biomass value for dummy species, so they are like almost empty sites, do you agree?
#Clarke KR, Somerfield PJ, Chapman MG. On resemblance measures for ecological studies, including
#taxonomic dissimilarities and a zero-adjusted Bray-Curtis coefficient for denuded assemblages. 

#Calculating dissimilarity distance using vegan package, method Bray-Curtis
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
PCO_biomass <- data.table::setDT(PCO_biomass, keep.rownames = TRUE)[]
PCO_biomass <- PCO_biomass %>% 
  rename(Site = rn, PC1 = Dim1, PC2 = Dim2) %>% 
  mutate(Method = case_when(endsWith(Site, "DOVS") ~ "DOVS",
                            endsWith(Site, "UVC") ~ "UVC")) %>% #Getting methods from site name
  mutate(Site = str_remove_all(Site, " DOVS| UVC")) %>% #Removing DOVS and UVC from site names
  left_join(SiteInfo %>% select(Site, Fishing, Bioregion, Island) %>% unique(), 
            by = c("Site")) #Adding site info on fishing (closed or open to fishing), bioregion and island


# Pearson Correlation biomass PCO ------------------------------------------------
# Principal coordinate analysis and simple ordination plot
#We calculate this in the same way you did above
Bio_mat_pcoa <- pcoa(Bio_mat_dist)
#We extract scores from PCoA and calculate correlation with biomass matrix
SpCorr <- envfit(Bio_mat_pcoa$vectors, Bio_mat, permutations = 9999)
#Check out the scores to each dimension from the correlation calculated above 
scores(SpCorr, "vectors")
#Plot your PCoA
plot(Bio_mat_pcoa$vectors, type = "p")
#Overlay species which have significant correlation (p <= 0.05)
plot(SpCorr, p.max = 0.05, col = "red")
#You can check the actual correlation coefficients (Pearson) using the line below
SpCorr$vectors$r

#Species with r value above 0.6
SpCorr$vectors$r > 0.6
#Sphyrna lewini, Lutjanus argentiventris, Carcharhinus galapagensis


# Names on arrows in PCO plot --------------------------------------------------
#Function that computes arrows from a pcoa and a species matrix
compute_arrows <-  function(Bio_mat_pcoa, Bio_mat) {
  
  #Question - change names - depending on pearson values
  #Keeping the species that has the largest arrows (from former PCO plot)
  Bio_mat = Bio_mat[ ,c("Lutjanus argentiventris", "Carcharhinus galapagensis", "Sphyrna lewini")]
  
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

#Question - change names - depending on pearson values
#Naming arrows with short species names
arrows_df$variable <- c("L. argentiventris", "C. galapagensis", "S. lewini")

#Making an anchor for the arrows
Anchor <- c(0.53, -0.15) #upper right corner

#Constant adjusting the size of vectors
K <- 1 #not actually necessary, as it is currently 1, but good for playing around with the code

#define other coordinates for arrows
X2 <- (arrows_df$Axis.1 + Anchor[1])*K
Y2 <- (arrows_df$Axis.2 + Anchor[2])*K


# Preparing data for figure -----------------------------------------------

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

