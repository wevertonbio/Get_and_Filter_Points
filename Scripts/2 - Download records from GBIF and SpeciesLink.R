#### 2 - Download records from GBIF and SpeciesLink ####

####Load packages####
library(plantR)
library(pbapply)

#Import list of species
l.species <- read.csv("Examples/List_of_species.csv")
head(l.species)
l.spp <- unique(l.species$species)

####Download from GBIF####
#Create folder to save occurrences from GBIF
dir.create("Examples/GBIF")

#Before download, see number of record of each specie
txkey <- pblapply(seq_along(l.spp), function(i){
  tryCatch({ #Inicio tryCatch
    txkey <- rgbif::name_backbone(l.spp[[i]])
    sp.key <- unique(txkey$usageKey)
    n.occ <- rgbif::occ_count(taxonKey = sp.key)
    df <- data.frame("species" = l.spp[[i]],
                     "n" = n.occ)
    df
  },
  error = function(msg){ #End of trycatch
    message(paste("Error for:", l.spp[i]))
    return(NA) })
})

spp <- txkey %>% map_df(bind_rows)
#Select only species with less than 10,000 points
  #rgbif2 is not be able to download species with more than 10,000 points :(
spp10 <- spp %>% filter(n <=10000) %>% filter(n > 0)
l.spp <- unique(spp10$species)

#As example, let's download the records of a single specie (Araucaria angustifolia)
  #Download from GBIF
oc.gbif <- rgbif2(species = "Araucaria angustifolia", force = TRUE,
                  remove_na = TRUE,
                  n.records = 100000)
#Save file with records downloaded
write.csv(oc.gbif, "Examples/GBIF/Araucaria angustifolia.csv")

####Download from SpeciesLink####
#Create directoryy
dir.create("Examples/SpeciesLink")
#Download from speciesLink
oc.splink <- rspeciesLink(species = "Araucaria angustifolia")
write.csv(oc.splink, "Examples/SpeciesLink/Araucaria angustifolia.csv")

#Check the folders SpeciesLink and GBIF to see the files!#