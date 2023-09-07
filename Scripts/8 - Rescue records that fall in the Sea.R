####8 - Rescue records that fall in the Sea or other NA pixel####
#Carregar pacotes
library(ellipsenm)
library(raster)
library(dplyr)
library(pbapply)
library(data.table)
library(terra)
library(flexsdm)
library(mapview)


#Import records
occ <- fread("Check_Points/7-Records_in_Neotropics.csv")
occ <- as.data.frame(occ)

#Import raster layer - Raster layer of any variable that you are planning to use in the models
r <- raster("Bio01.tiff")
#Cut raster to area of distribution of the species
ca <- calib_area(occ, x = "decimalLongitude.new1", y = "decimalLatitude.new1",
                 method = c('bmcp', width=10*1000), crs = crs(r))
rca <- mask(crop(rast(r), ca), ca) %>% raster()
plot(rca)

#Extract values
er <- raster::extract(rca, occ[,c("decimalLongitude.new1", "decimalLatitude.new1")])
occ$rastervalue <- er
#Get only values without information
sea <- occ %>% filter(is.na(rastervalue))

#See map with records that fall in the sea or other NA pixel
mapview(st_as_sf(sea, coords = c("decimalLongitude.new1", "decimalLatitude.new1"), crs = 4326)) + mapview(rca)

#Move these records to the closest pixel of the raster layer that contains relevant information
d <- ellipsenm::to_closest(sea,
                           longitude = "decimalLongitude.new1",
                           latitude = "decimalLatitude.new1",
                           raster_layer = rca,
                           limit_distance = 10) #Tmaximun distance in km at which an occurrence could be to be moved

#Join data
occ.no.sea <- occ %>% filter(!is.na(rastervalue)) %>% dplyr::select(-rastervalue) %>%
  mutate(condition = "Correct",
         distance_km = 0,
         initial_lon = NA,
         initial_lat = NA)
d2 <- d %>% dplyr::select(-rastervalue)
#Unir
occ.final <- rbind(occ.no.sea, d2)
#Salvar
fwrite(occ.final, "Check_Points/8-Rescued_from_Sea.csv", row.names = FALSE)