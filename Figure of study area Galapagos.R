##################################################################
#
# Figure of study area Galapagos ----------------------------------------------------
#
##################################################################

# Loading libraries -------------------------------------------------------

library(sf)
library(tidyverse) #tidyverse includes ggplot2 so I have removed it
library(sp)
library(janitor)

# Accessing data ----------------------------------------------------------
#Loading shapefiles - DFA
Gal_Isl <- st_read("Shapefiles/GalapagosIslands_WGS84.shp") #Galapagos Islands
GMR <- st_read("Shapefiles/GMRBoundaries_WGS84.shp") #Boundaries of the GMR
GMR_Zones <- st_read("Shapefiles/GMRZones_Lines_WGS84.shp") %>%  #2001 Zoning of the GMR
  #ZONA (zoning type) will be transformed to factor as it is considered as numeric by default
  mutate(ZONA = factor(ZONA))


# Making the map ----------------------------------------------------------
#Putting the map together - DFA
MapGMR <- ggplot() + 
  #First we plot the GMR boundary, I have set it to have a transparent fill, but you can change this under 'fill'
  geom_sf(data = GMR, size = 1, fill = alpha("blue", 0)) + 
  #We now plot the actual island
  geom_sf(data = Gal_Isl, size = 0.5, color = "black", fill = "gray80") + 
  #Finally, we will plot the GMR zones so they appear over the islands contours
  #We will change the colour of the boundaries by zone type (ZONA)
  geom_sf(data = GMR_Zones, aes(color = ZONA), size = 1) +
  #I will use a colorbrewer palette for the different zones, you can change this to whatever you prefer
  #I also changed the title of the legend
  scale_color_brewer(palette = "Dark2", name = "Management Zones")+
  ggtitle("Galapagos Marine Reserve Zoning") + 
  #Applying a black and white theme to the figure
  theme_bw()+
  #Removing grids
  theme(panel.grid = element_blank())
#Checking the map looks ok
MapGMR


# Sites sampled -----------------------------------------------------------
#Reading data from the sites
Sites <- read.csv2("Data/Bacalao_MagicMysteryTour_GPS_Coordinates_Respaldo.csv") %>% 
  select(Site, Island, Bioregion, LAT, LONG, Type, Fishing, Zoning) %>% 
  #I am tidying up the names so they do not look too messy
  janitor::clean_names()
#My microsoft package has been updated, so now it uses comma (,) instead of a dot (.)
#That's why I am using read.csv2.
str(Sites)

#First, we need to change the lat and long columns from factors to characters - DFA
Sites <- Sites %>% mutate(lat = as.character(lat), 
                 long = as.character(long),
                 #I am also correcting that typo under the column 'type' from turism to tourism
                 #We need to convert the column from factor to character
                 type = as.character(type),
                 #Then we correct the mistake
                 type = case_when(type == "Turism" ~ "Tourism",
                                   TRUE ~ type),
                 #Finally we change the column back to factor
                 type = factor(type))

#We will call a library called parzer (I mentioned this in a previous email, check emails for more information) - DFA
library(parzer)

#New columns created so you can check coordinates are correctly transformed - DFA
Sites <- Sites %>% mutate(latDD = parse_lat(lat),
                 lonDD = parse_lon(long))
#You can check coords have been converted correctly using this website: https://www.pgc.umn.edu/apps/convert/

#If you want to show these points on the map we previously created, we first need to create a shapefile - DFA
SiteShp <- Sites %>% 
  #We use the newly created lat and lon columns as coordinates
  st_as_sf(coords = c("lonDD", "latDD"), 
           #We assume the CRS for these coordinates is WGS84, because it uses lat and lon and it is commonly used in 
           #the Galapagos
           crs = 4326)

#Now we simply use the previous map we created and add our data - DFA
MapGMR+
  #I decreased the size of the dots and made them slightly transparent so we can see the management zones clearly
  geom_sf(data = SiteShp, size = 1, colour = alpha("#882255", 0.6))

#Table with overview of areas in each management zone
Zones <- Sites %>%
  mutate(category = case_when(zoning == "Comparación y protección (2.1)" ~ 2.1, 
                              zoning == "Conservación y uso no extractivo (2.2)" ~ 2.2,
                              zoning == "Conservación y uso extractivo (2.3)" ~ 2.3,
                              zoning == "Manejo especial (2.4)" ~ 2.4)) 
Zones <- Zones %>%
  group_by(zoning) %>%
  summarise(count = n())%>% 
  adorn_totals("row") %>% 
  mutate(fishing = case_when(zoning == "Comparación y protección (2.1)" ~ "closed", 
                             zoning == "Conservación y uso no extractivo (2.2)" ~ "closed",
                             zoning == "Conservación y uso extractivo (2.3)" ~ "open",
                             zoning == "Manejo especial (2.4)" ~ "closed"))

  