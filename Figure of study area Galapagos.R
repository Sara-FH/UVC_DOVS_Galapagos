##################################################################
#
# Figure of study area Galapagos ----------------------------------------------------
#
##################################################################

# Loading libraries -------------------------------------------------------

library(sf)
library(ggplot2)
library(tidyverse)
library(sp)

# Making the map ----------------------------------------------------------

#Reading shapefile covering the Galapagos 
Galapagos <- st_read("Data/worldheritagemarineprogramme.shp") #This is only the outline
#I cannot find a shapefile with the Islands
ggplot() + 
  geom_sf(data = Galapagos, size = 0.5, color = "black", fill = "green") + 
  ggtitle("Galapagos") + 
  coord_sf()

Galapagos_zones <- st_read("Data/GMR_Zoning2000_NTZ.shp") #Here I tried to get a shapefile with
#the different zones within the GMR, but this one only has 2.2 and 2.1 zones and I have data for 
#2.1-2.4
ggplot() + 
  geom_sf(data = Galapagos_zones, size = 0.5, color = "black", fill = "cyan1") + 
  ggtitle("Galapagos_zones") + 
  coord_sf()

#Just to try out combining the maps
ggplot() + 
  geom_sf(data = Galapagos, size = 0.5, color = "black", fill = NA) + 
  ggtitle("Galapagos") + 
  coord_sf() + 
  geom_sf(data = Galapagos_zones, size = 0.5, color = "black", fill = "cyan1") + 
  ggtitle("Galapagos_zones") + 
  coord_sf()

#Reading data from the sites
Sites <- read.csv2("Data/Bacalao_MagicMysteryTour_GPS_Coordinates_Respaldo.csv") %>% 
  select(Site, Island, Bioregion, LAT, LONG, Type, Fishing, Zoning)
#My microsoft package has been updated, so now it uses comma (,) instead of a dot (.)
#That's why I am using read.csv2.
str(Sites)

#The coordinates LAT and LONG are in DDM (Degrees, decimal minutes) as far as I can see
#it is also in characters, but it reads the entire cell as 1, not just the first letter, I was
#trying to move N to the back, to try to rearrange to a format that could be changed to 
#DD (decimal degrees).
#Overall I am trying to make the current format into decimal degrees so I can plot it on a map
#hereby, putting the sites on a map of the Galapagos

Sites$LAT2 <- Sites$LAT 
  toString(print(Sites$LAT[1] + Sites$LAT[0]))

Sites$LAT[1]

