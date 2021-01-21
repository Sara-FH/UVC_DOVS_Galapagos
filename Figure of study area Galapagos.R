##################################################################
#
# Figure of study area Galapagos ----------------------------------------------------
#
##################################################################

# Loading libraries -------------------------------------------------------

library(sf)
library(tidyverse) #tidyverse includes ggplot2 so I have removed it
library(sp)
library(janitor) # there is no need to include this one as you one use it once - DFA

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



# Bioregions and sites sampled --------------------------------------------

Bioregion <- st_read("Shapefiles/bioregiones.shp") #Bioregions of the GMR

#Loading data with coordinates and open/closed to fishing status
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

#From the sites data frame creating data frame only for open sites
open_sites <- Sites %>% 
  filter(Fishing == "Open")
#making an sf element for open sites to add to map
open_sf <- st_as_sf(open_sites, coords = c("Lon Dd", "Lat Dd"), 
                     crs = 4326, agr = "constant")

#From the sites data frame creating data frame only for closed sites
closed_sites <- Sites %>% 
  filter(Fishing == "Closed")
#making an sf element for closed sites to add to map
closed_sf <- st_as_sf(closed_sites, coords = c("Lon Dd", "Lat Dd"), 
                    crs = 4326, agr = "constant")

#Changing bioregion for west
Bioreg_eng <- Bioregion %>% 
  filter(!Bioregion$name == "Elizabeth" &
           !Bioregion$name == "Oeste")
  # mutate(name = recode(name, 
  #                      "Elizabeth" = "Oeste"))

library(grid)
library(ggspatial)

#Making map with bioregions and sampled sites
MapBio <- ggplot() + 
  #First we plot the GMR boundary, I have set it to have a transparent fill, 
  #but you can change this under 'fill'
  geom_sf(data = GMR, size = 1, color = "black", fill = alpha("blue", 0)) + 
  #We now plot the actual island
  geom_sf(data = Gal_Isl, size = 0.5, color = "black", fill = "gray80") + 
  #Finally, we will plot the GMR zones so they appear over the islands contours
  #We will change the colour of the boundaries by zone type (ZONA)
  geom_sf(data = Bioreg_eng, color = "black", fill = NA, size = 0.8) +
  #Adding sampled sites open to fishing
  geom_sf(data = open_sf, size = 2.5, shape = 16, fill = "NA") +
  #Adding sampled sites closed to fishing
  geom_sf(data = closed_sf, size = 2.5, shape = 17, fill = "NA") +
  #Adding names for bioregions
  annotate(geom = "text", x = -89.6, y = -0.2, label = "Central South-eastern", 
           fontface = "bold", color = "black", size = 5) +
  annotate(geom = "text", x = -90.2, y = 0.8, label = "Northern", 
           fontface = "bold", color = "black", size = 5) +
  annotate(geom = "text", x = -91.9, y = 1, label = "Far Northern", 
           fontface = "bold", color = "black", size = 5) +
  annotate(geom = "text", x = -91.9, y = 0.3, label = "Western", 
           fontface = "bold", color = "black", size = 5) +
  #Adding scale and North arrow
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1) +
  annotation_north_arrow(location = "bl", which_north = "TRUE",
                         pad_x = unit(0.4, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"), width = unit(2, "cm")) +
  #grobTree(textGrob("Central South-eastern", x = 0.1, y = 0.1)) +
  #ggtitle("Galapagos Marine Reserve Zoning") + 
  #Applying a black and white theme to the figure
  theme_bw()+
  #Removing grids
  theme(panel.grid = element_blank(), 
        axis.text = element_text(color = "black", size = 16), 
        axis.title = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black", size = 1.1), 
        axis.ticks = element_line(color = "black", size = 1), 
        axis.ticks.length = unit(0.15, "cm"))
#Checking the map looks good
MapBio

#loading packages to make world map
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

#Loading world map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#square around galapagos for ecuador map
temp <- data.frame(long = c(-92.5, -92.5, -88.5, -88.5, -92.5), 
                   lat = c(-2.5, 1.5, 1.5, -2.5, -2.5))

#Ecuador map
Ecuador <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-95, -70), ylim = c(-5, 15), expand = FALSE) +
  theme_bw() +
  geom_path(data = temp, aes(x = long, y = lat), size = 0.3) +
  annotate(geom = "text", x = -78, y = -1.2, label = "Ecuador", 
           fontface = "bold", color = "black", size = 5) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fil = NULL), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())
  
library(raster)
library(ggthemes)

#Combining maps
FinalMap <- MapBio +
  annotation_custom(grob = Ecuador, xmin = -90, xmax = -89.5, 
                    ymin = 1.2, ymax = 2.5)
FinalMap  


# Sites sampled -----------------------------------------------------------
#Reading data from the sites
Sites <- openxlsx::read.xlsx("Data/Bacalao_MagicMysteryTour_GPS_Coordinates_Respaldo.xlsx") %>% 
  #I am tidying up the names so they do not look too messy
  janitor::clean_names() %>% 
  select(site, island, bioregion, lat, long, type, fishing, zoning) 
#My microsoft package has been updated, so now it uses comma (,) instead of a dot (.)
#That's why I am using read.csv2.
str(Sites)

#Correcting that typo under the column 'type' from turism to tourism - DFA
Sites <- Sites %>% 
  #We need to convert the column from factor to character
  mutate(type = as.character(type),
         #Then we correct the mistake
         type = case_when(type == "Turism" ~ "Tourism", TRUE ~ type),
         #Finally we change the column back to factor
         type = factor(type))

#We will call a library called parzer (I mentioned this in a previous email, check emails for more information) - DFA
library(parzer)

#New columns created so you can check coordinates are correctly transformed - DFA
Sites <- Sites %>% mutate(latDD = parse_lat(lat),
                 lonDD = parse_lon(long))
#You can check coords have been converted correctly using this website: https://www.pgc.umn.edu/apps/convert/
#Saving corrected Sites file
#write.csv(Sites, "Data/GPScoords_BacalaoMMT_Corrected.csv", row.names = F)

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


#There is an easier way to do the above. Since you already have the codes inside the column
#zoning, you could just extract the numbers using str_extract() - DFA
Zones1 <- Sites %>% 
  #The pattern used means values between 0-9 (essentially numbers), followed by a period (.)
  #and followed by numbers between 0-9.
  mutate(category = str_extract(zoning, "[0-9][.][0-9]"),
         #We can now add opened or closed to fishing really easily
         #Basically if it is not 2.3, then it is closed, if it is is is opened.
         fishing = case_when(category != 2.3 ~ "closed",
                             TRUE ~ "opened")) %>% 
  group_by(category, fishing) %>% 
  summarise(N = n())
  