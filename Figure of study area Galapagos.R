##################################################################
#
# Figure of study area Galapagos ----------------------------------------------------
#
##################################################################

# Loading libraries -------------------------------------------------------

library(ggplot2)
library(sf)
library(tidyverse) #tidyverse includes ggplot2 so I have removed it
library(sp)
library(janitor) # there is no need to include this one as you one use it once - DFA
library(grid) #used for grid in map
library(ggspatial) #used to make north arrow and scale
#loading packages to make world map
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
#Packages used to combine maps
library(raster)
library(ggthemes)

# Accessing data ----------------------------------------------------------
#Loading shapefiles - DFA
Gal_Isl <- st_read("Shapefiles/GalapagosIslands_WGS84.shp") #Galapagos Islands
GMR <- st_read("Shapefiles/GMRBoundaries_WGS84.shp") #Boundaries of the GMR
GMR_Zones <- st_read("Shapefiles/GMRZones_Lines_WGS84.shp") %>%  #2001 Zoning of the GMR
  #ZONA (zoning type) will be transformed to factor as it is considered as numeric by default
  mutate(ZONA = factor(ZONA))
Bioregion <- st_read("Shapefiles/bioregiones.shp") #Bioregions of the GMR


# Bioregions and sites prepare data --------------------------------------------

#Loading data with coordinates for sites and open/closed to fishing status
Sites <- read.csv("Data/GPScoords_BacalaoMMT_Corrected.csv") %>% 
  #Changing first letter of column names to uppercase - DFA
  janitor::clean_names(case = "title") %>%
  #We basically apply the same methods as before to ensure consistency - DFA
  #Remove white space from site names - DFA
  mutate(Site = str_trim(Site, "both")) %>% 
  #Removing accents (e.g., Bahía to Bahia) from site names, islands and zoning - DFA
  mutate_at(vars(c(Site, Island, Zoning)), ~stringi::stri_trans_general(., "Latin-ASCII")) %>% 
  #Changing site names and bioregions to title case (i.e., first letter of each word is capitalised) - DFA
  mutate_at(vars(c(Site, Bioregion)), str_to_title) %>% 
  #remove column zoning
  select(-c(Zoning)) %>% 
  #removing sites that are not used in analysis
  filter(!Site == "Punta Calle 1" & !Site == "Punta Calle 2" & !Site == "Santiago Norte" &
           !Site == "Samtiago Suroeste" & !Site == "Isabela Alcedo" & 
           !Site == "Punta Vicente Roca (No Dovs)")

#Creating data frame only for open sites
open_sites <- Sites %>% 
  filter(Fishing == "Open")
#Creating data frame only for closed sites
open_sf <- st_as_sf(open_sites, coords = c("Lon Dd", "Lat Dd"), 
                     crs = 4326, agr = "constant")

#From the sites data frame creating data frame only for closed sites
closed_sites <- Sites %>% 
  filter(Fishing == "Closed")
#making an sf element for closed sites to add to map
closed_sf <- st_as_sf(closed_sites, coords = c("Lon Dd", "Lat Dd"), 
                    crs = 4326, agr = "constant")

#Changing bioregion for west, removing lines between Elizabeth and Oeste on map
Bioreg_new <- Bioregion %>% 
  filter(!Bioregion$name == "Elizabeth" &
           !Bioregion$name == "Oeste")


# Making the map of GMR bioregions with sites -----------------------------

#Making map with bioregions and sampled sites
MapBio <- ggplot() + 
  #First we plot the GMR boundary, I have set it to have a transparent fill, 
  #but you can change this under 'fill'
  geom_sf(data = GMR, size = 1, color = "black", fill = alpha("blue", 0)) + 
  #We now plot the actual island
  geom_sf(data = Gal_Isl, size = 0.5, color = "black", fill = "gray80") + 
  #Finally, we will plot the GMR zones so they appear over the islands contours
  #We will change the colour of the boundaries by zone type (ZONA) #changed to black
  geom_sf(data = Bioreg_new, color = "black", fill = NA, size = 0.8) +
  #Adding sampled sites open to fishing - circles
  geom_sf(data = open_sf, size = 2.5, shape = 16, fill = "NA") +
  #Adding sampled sites closed to fishing - triangles
  geom_sf(data = closed_sf, size = 2.5, shape = 17, fill = "NA") +
  #Adding names for bioregions
  annotate(geom = "text", x = -90, y = 0, label = "Central South-eastern", 
           fontface = "bold", color = "black", size = 5) +
  annotate(geom = "text", x = -90.2, y = 0.8, label = "Northern", 
           fontface = "bold", color = "black", size = 5) +
  annotate(geom = "text", x = -91.95, y = 1.1, label = "Far Northern", 
           fontface = "bold", color = "black", size = 5) +
  annotate(geom = "text", x = -91.9, y = 0.3, label = "Western", 
           fontface = "bold", color = "black", size = 5) +
  #Adding scale and North arrow
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1) +
  annotation_north_arrow(location = "bl", which_north = "TRUE",
                         pad_x = unit(0.8, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"), width = unit(2, "cm")) +
  #making y-axis longer
  scale_y_continuous(limits = c(-2.2, 2.9)) +
  scale_x_continuous(limits = c(-92.7, -88.5)) +
  #Applying a black and white theme to the figure
  theme_bw()+
  #Removing grids and making axis text and ticks black
  theme(panel.grid = element_blank(), 
        axis.text = element_text(color = "black", size = 16), 
        axis.title = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black", size = 1.1), 
        axis.ticks = element_line(color = "black", size = 1), 
        axis.ticks.length = unit(0.15, "cm"))
#Checking the map looks good
MapBio

#Loading world map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#square around galapagos for ecuador map
temp <- data.frame(long = c(-92.5, -92.5, -88.5, -88.5, -92.5), 
                   lat = c(-2.5, 1.5, 1.5, -2.5, -2.5))

#Line for name of Ecuador
temp2 <- data.frame(long = c(-82, -79), 
                    lat = c(4, -0.5))

#Ecuador map
ggplot(data = world) +
  geom_sf() +
  #Coordinate limits for the world map
  coord_sf(xlim = c(-94, -72), ylim = c(-5, 13), expand = FALSE) +
  #white background in map
  theme_bw() +
  #Adding box around Galapagos
  geom_path(data = temp, aes(x = long, y = lat), size = 0.3) +
  #Adding line for Ecuador name
  geom_path(data = temp2, aes(x = long, y = lat), size = 1) +
  #Adding names of Ecuador and Galapagos
  annotate(geom = "text", x = -82, y = 5, label = "Ecuador", 
           fontface = "bold", color = "black", size = 4) +
  annotate(geom = "text", x = -89, y = 3, label = "Galapagos", 
           fontface = "bold", color = "black", size = 4) +
  #Removing pangel.grid, background, and axes
  theme(panel.grid = element_blank(),
          panel.background = element_rect(fil = NULL),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())

#Ecuador map with ggplotGrob so it can be added to the GMR map
Ecuador <- ggplotGrob(
  ggplot(data = world) +
    geom_sf() +
    #Coordinate limits for the world map
    coord_sf(xlim = c(-94, -72), ylim = c(-5, 13), expand = FALSE) +
    #white background in map
    theme_bw() +
    #Adding box around Galapagos
    geom_path(data = temp, aes(x = long, y = lat), size = 0.3) +
    #Adding line for Ecuador name
    geom_path(data = temp2, aes(x = long, y = lat), size = 1) +
    #Adding names of Ecuador and Galapagos
    annotate(geom = "text", x = -82, y = 5, label = "Ecuador", 
             fontface = "bold", color = "black", size = 4) +
    annotate(geom = "text", x = -89, y = 3, label = "Galapagos", 
             fontface = "bold", color = "black", size = 4) +
    #Removing pangel.grid, background, and axes
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fil = NULL),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  )
#Showing new format
Ecuador

#Combining maps
FinalMap <- MapBio +
  #Choosing placement of Ecuador map on GMR map
  annotation_custom(grob = Ecuador, xmin = -90.45, xmax = -88.24, 
                    ymin = 1.05, ymax = 3.53)
#Check if the map looks good
FinalMap  

#Save figure of study area
ggsave("Figures/Study_area.tiff", FinalMap, device = "tiff", dpi = 300, width = 8, height = 8)

#Delete used variables
rm(Sites, open_sites, open_sf, closed_sites, closed_sf, 
   Bioreg_new, MapBio, world, temp, temp2, Ecuador, FinalMap)

  
# Making the map of GMR ----------------------------------------------------------
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

rm(MapGMR)

# Bioregion map -----------------------------------------------------------

Bioregion <- st_read("Shapefiles/bioregiones.shp") #Bioregions of the GMR

#Making map with bioregions and sampled sites
MapBio <- ggplot() + 
  #First we plot the GMR boundary, I have set it to have a transparent fill, but you can change this under 'fill'
  geom_sf(data = GMR, size = 1, fill = alpha("blue", 0)) + 
  #We now plot the actual island
  geom_sf(data = Gal_Isl, size = 0.5, color = "black", fill = "gray80") + 
  #Finally, we will plot the GMR zones so they appear over the islands contours
  #We will change the colour of the boundaries by zone type (ZONA)
  geom_sf(data = Bioregion, aes(color = name), size = 1) +
  #I will use a colorbrewer palette for the different zones, you can change this to whatever you prefer
  #I also changed the title of the legend
  scale_color_brewer(palette = "Dark2", name = "name")+
  ggtitle("Galapagos Marine Reserve Zoning") + 
  #Applying a black and white theme to the figure
  theme_bw()+
  #Removing grids
  theme(panel.grid = element_blank())
#Checking the map looks ok
MapBio

rm(Bioregion, MapBio)
