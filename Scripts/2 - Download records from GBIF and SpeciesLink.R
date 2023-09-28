#### 2 - Download records from GBIF and SpeciesLink ####

####Load packages####
library(plantR)
library(pbapply)
library(florabr)
library(dplyr)

####Araucaria angustifoli as example####
sp <- "Araucaria angustifolia"

#####Get synonyms from Brazilian Flora####
br_dir <- "Other_Files" #Set directory where brazilian flora database was saved
#Import brazilian flora database
bf <- load_florabr(data_dir = br_dir, data_version = "393.370",
                   type = "complete")
#Select synonyms
sp_syn <- bf %>% filter(acceptedName %in% sp) %>% 
  dplyr::select(species, acceptedName) %>% distinct()
  
#Create dataframe with species and synonyms
sp_syn <- sp_syn %>% dplyr::select(species = acceptedName,
                                   synonym = species)
#Create new list of species
l.spp <- unique(c(sp_syn$species, sp_syn$synonym))
  
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

spp <- txkey %>% bind_rows()
#Select only species with less than 10,000 points
  #rgbif2 is not be able to download species with more than 10,000 points :(
spp10 <- spp %>% filter(n <=10000) %>% filter(n > 0)
l.spp <- unique(spp10$species)

#As example, let's download the records of a single specie (Araucaria angustifolia)
  #Download from GBIF
oc.gbif <- rgbif2(species = l.spp, force = TRUE,
                  remove_na = TRUE,
                  n.records = 100000)
#Save file with records downloaded
write.csv(oc.gbif, paste0("Examples/GBIF/", sp, ".csv"))

####Download from SpeciesLink####
#Create directoryy
dir.create("Examples/SpeciesLink")
#Download from speciesLink
oc.splink <- rspeciesLink(species = l.spp)
write.csv(oc.splink, paste0("Examples/SpeciesLink/", sp, ".csv"))

#Check the folders SpeciesLink and GBIF to see the files!#