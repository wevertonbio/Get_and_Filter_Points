#2.1 - Download records from GBIF and SpeciesLink####
            ####IN PARALLEL####

####Load packages####
library(plantR)
library(pbapply)

#Import list of species
l.species <- read.csv("Examples/List_of_species.csv")
head(l.species)
l.spp <- unique(l.species$species)

#As example, let's work only with two species
l.spp <- c("Araucaria angustifolia", "Calibrachoa paranensis")

####Download from GBIF####
#Create folder to save occurrences from GBIF
dir.create("Examples/GBIF")

#####Get synonyms from Brazilian Flora####
br_dir <- "Other_Files" #Set directory where brazilian flora database was saved
#Import brazilian flora database
bf <- load_florabr(data_dir = br_dir, data_version = "393.370",
                   type = "complete")
#Select synonyms
sp_syn <- bf %>% filter(acceptedName %in% l.spp) %>% 
  dplyr::select(species, acceptedName) %>% distinct()

#Create dataframe with species and synonyms
sp_syn <- sp_syn %>% dplyr::select(species = acceptedName,
                                   synonym = species)

all_spp <- unique(c(sp_syn$species, sp_syn$synonym))
#Before download, see number of record of each specie
txkey <- pblapply(seq_along(all_spp), function(i){
  tryCatch({ #Inicio tryCatch
    txkey <- rgbif::name_backbone(all_spp[[i]])
    sp.key <- unique(txkey$usageKey)
    n.occ <- rgbif::occ_count(taxonKey = sp.key)
    df <- data.frame("species" = all_spp[[i]],
                     "n" = n.occ)
    df
  },
  error = function(msg){ #End of trycatch
    message(paste("Error for:", l.spp[i]))
    return(NA) })
})
spp <- txkey %>% bind_rows
#Select only species with less than 10,000 points
#rgbif2 is not be able to download species with more than 10,000 points :(
spp10 <- spp %>% filter(n <=10000) %>% filter(n > 0)
l.spp <- unique(spp10$species)

#Subset dataframe with synonyms
new_sp <- sp_syn %>% filter(species %in% l.spp | species %in% l.spp)
#Get species with accepted name
acc_sp <- unique(new_sp$species)

#Make cluster
library(future.apply)
library(progressr)

plan(future::multisession, workers = 7) #Determinar numero de cores
handlers(handler_progress(format="[:bar] :percent :eta :message")) #Criar
#Baixar ocorrências do GBIS
with_progress({
  p <- progressor(along=seq_along(acc_sp))
  future_lapply(seq_along(acc_sp), FUN = function(i){
    tryCatch(
      {
        sp <- acc_sp[i]
        sp_syn <- new_sp %>% filter(species == sp)
        sp_syn <- unique(c(sp_syn$species, sp_syn$synonym))
        oc.gbif <- rgbif2(species = sp_syn, force = TRUE, remove_na = TRUE, n.records = 100000) #Inicio tryCatch
      write.csv(oc.gbif, paste0("Examples/GBIF/", sp, ".csv"))
      #Progress
      p(sprintf("i=%g", i))}, #Print progress
      error = function(e) NULL)
  }, future.chunk.size=1)
})
#Sometimes you get some warning messages. Just ignore them
#Stop cluster
plan(sequential)

#Obter registros do Species Link
# #Ver espécies que não deram certo no specieslink a primeira vez
# sp.ready <- list.files("SpeciesLink/") %>% gsub("\\.csv", "", .)
# sp.out <- setdiff(l.spp, sp.ready)
# l.spp <- sp.out
# #Make cluster


#Criar pasta para salvar downloads do SpeciesLink
dir.create("Examples/SpeciesLink/")

plan(future::multisession, workers = 3) #Determinar numero de cores
handlers(handler_progress(format="[:bar] :percent :eta :message")) #Criar
with_progress({
  p <- progressor(along=seq_along(l.spp))
  future_lapply(seq_along(l.spp), FUN = function(i){
    tryCatch(
      { #Inicio tryCatch
        sp <- acc_sp[i]
        sp_syn <- new_sp %>% filter(species == sp)
        sp_syn <- unique(c(sp_syn$species, sp_syn$synonym))
        oc.splink <- rspeciesLink(species = sp_syn)
        write.csv(oc.splink, paste0("Examples/SpeciesLink/", sp, ".csv"))
        #Progress
        p(sprintf("i=%g", i))}, #Print progress
      error = function(e) NULL)
  }, future.chunk.size=1)
})

#Check the folders SpeciesLink and GBIF to see the files!