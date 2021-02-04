###############################################################################################################
# Title: Calculations for PCO on density data
# Author: Sara Færch Hansen
# Assisting: Denisse Fierro Arcos
# Version: 1
# Date last updated: 2021-01-28
###############################################################################################################

# Uploading library -----------------------------------------------------
library(ape) #For principal coordinates analysis
library(vegan)

# PCO plot for density ---------------------------------------------------------

#Calculating average density of each species per site for DOVS
Den_sp_DOVS <- DOVS %>% 
  select(Site, Period, N, Method, ValidName, Fishing, SiteCode, Transect_length_m) %>% 
  mutate(Transect_area = Transect_length_m*5) %>%  #calculating transect area by multiplying with width 5m
  group_by(Site, Period, ValidName) %>% 
  summarise(N_period = sum(N), #Sum of abundance of species within each period
            Method, Fishing, SiteCode, Transect_area) %>% 
  unique() %>%
  #Question - change they way to calculate per 500m2 if we decide to do it for all calculations
  mutate(N_500m2 = (N_period/Transect_area)*500) %>% #Number/500m2
  group_by(Site, ValidName) %>% 
  #Question - check mean calculations, like PCO for biomass
  summarise(N_site_sp = mean(N_500m2), #Calculating average abundance of each species per site
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
  #Question - change they way to calculate per 500m2 if we decide to do it for all calculations
  mutate(N_500m2 = (N_period/Transect_area)*500) %>% #Number/500m2
  group_by(Site, ValidName) %>% 
  #Question - check mean calculations, like PCO for biomass
  summarise(N_site_sp = mean(N_500m2), #Calculating average abundance of each species per site
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
  mutate(N_site_sp = 0.9)
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
  mutate(Dummy = 0.9) %>% #Adding dummy species to all sites, to enable dissimilarity calculations later
  arrange(SiteMet) %>% #Arranging site names
  column_to_rownames("SiteMet") %>% #Making a column into row names for the matrix
  as.matrix() %>% 
  replace_na(0) #Putting 0 instead of NA, when the species was not observed at the site.

#Removing unnecessary variables
rm(Den_sp_DOVS, Den_sp_UVC)

#Checking ranges for transformations and no transformation
range(Den_mat)
range(Den_mat^0.5)
range(Den_mat^0.25)
#I have read online that I should get in the 0-10 range
#To achieve this I am doing a fourth root transformation
#Question DFA - do you agree with the square root transformation - and the reason for doing it?

#Applying a 4th root transformation to matrix
Den_mat <- Den_mat^0.5

#Changing Dummy species data back to 1*10^-5 to avoid it influencing analysis
Den_mat[, "Dummy"] <- 0.9

#Calculating dissimilarity distance using vegan package, the default is Bray Curtis
Den_mat_dist <- vegdist(Den_mat, method = "bray")
#Question - check if wcmdscale calculation can be deleted - I'm pretty sure it can
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
PCO_density <- data.table::setDT(PCO_density, keep.rownames = TRUE)[]
PCO_density <- PCO_density %>% 
  rename(Site = rn, PC1 = Dim1, PC2 = Dim2) %>% 
  mutate(Method = case_when(endsWith(Site, "DOVS") ~ "DOVS",
                            endsWith(Site, "UVC") ~ "UVC")) %>% #Getting methods from site name
  mutate(Site = str_remove_all(Site, " DOVS| UVC")) %>% #Removing DOVS and UVC from site names
  left_join(SiteInfo %>% select(Site, Fishing, Bioregion, Island) %>% unique(), 
            by = c("Site")) #Adding site info on fishing (closed or open to fishing), bioregion and island


# Pearson Correlation Density PCO -----------------------------------------
# Principal coordinate analysis and simple ordination plot
#We calculate this in the same way you did above
Den_mat_pcoa <- pcoa(Den_mat_dist)
#We extract scores from PCoA and calculate correlation with biomass matrix
SpCorr <- envfit(Den_mat_pcoa$vectors, Den_mat, permutations = 9999)
#Check out the scores to each dimension from the correlation calculated above 
scores(SpCorr, "vectors")
#Plot your PCoA
plot(Den_mat_pcoa$vectors, type = "p")
#Overlay species which have significant correlation (p <= 0.05)
plot(SpCorr, p.max = 0.05, col = "red")
#You can check the actual correlation coefficients (Pearson) using the line below
SpCorr$vectors$r

#Species with r value above 0.4
SpCorr$vectors$r > 0.4
#Species are: Sphyrna lewini, Lutjanus argentiventris, Mycteroperca olfax


# PCO density with arrows -------------------------------------------------
#Function that computes arrows from a pcoa and a species matrix
compute_arrows <-  function(Den_mat_pcoa, Den_mat) {
  
  # Keeping the species that has the largest arrows (from former PCO plot)
  Den_mat = Den_mat[ ,c("Lutjanus argentiventris", "Mycteroperca olfax", "Sphyrna lewini")]
  
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
#changing vectors to data.frame before putting it in ggplot2
species_pcoa_arrows$vectors <- as.data.frame(species_pcoa_arrows$vectors)

#making arrows smaller, so they fit better in the PCO
arrows_df <- as.data.frame(species_pcoa_arrows$U/20)
arrows_df$variable <- rownames(arrows_df)

#Naming arrows with short species names
arrows_df$variable <- c("L. argentiventris", "M. olfax", "S. lewini")

#adjusting anchor for the following plot
Anchor <- c(0.55, -0.45)

#Constant adjusting the size of vectors
K <- 1 #not actually necessary, as it is currently 1, but good for playing around with the code

#define other coordinates for arrows
X2 <- (arrows_df$Axis.1 + Anchor[1])*K
Y2 <- (arrows_df$Axis.2 + Anchor[2])*K


# Preparing data for figure -----------------------------------------------

#Renaming methods and fishing status in PCO_biomass for better names on graph
PCO_density <- PCO_density %>% 
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


