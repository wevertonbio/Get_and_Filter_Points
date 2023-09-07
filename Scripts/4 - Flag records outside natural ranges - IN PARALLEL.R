#### 4 - Flag records outside natural ranges ####
                ####IN PARALLEL####
#Visit: https://wevertonbio.github.io/florabr/articles/Spatialize_florabr.html
#Use filter_florabr function to remove or flags records outside of the species' natural ranges according to information provided by the Brazilian Flora 2020 database

#Load packages
library(florabr)
library(dplyr)
library(data.table)
library(future.apply)
library(progressr)
library(pbapply)

#Set directory
setwd("Examples/")

#Import occurrences of species
lf <- list.files(path = "plantR/", full.names = TRUE)
occ <- pblapply(seq_along(lf), function(i){
  occ_i <- fread(lf[i])
  return(occ_i)
  })
occ <- rbindlist(occ) %>% as.data.frame()

#Get species
l.spp <- occ$species %>% unique()

#Check script 1 to see how to import Brazilian flora database to R 
#Or, check: https://wevertonbio.github.io/florabr/articles/getting_started.html

#Import brazilian flora
bf <- load_florabr(data_dir = "../Other_Files/")


#Start looping
plan(future::multisession, workers = 6) #Set number of cores
handlers(handler_progress(format="[:bar] :percent :eta :message")) #Create progress bar
with_progress({
  p <- progressor(along=seq_along(l.spp))
  occ_flags <- future_lapply(seq_along(l.spp), FUN = function(i){
    tryCatch(
      {#Identify species
  sp <- l.spp[i]
  occ_i <- occ %>% filter(species == sp)
  
  #Flag occurrences
  occ_flag <- filter_florabr(data = bf, occ = occ_i, Species = "species",
                             Long = "decimalLongitude.new1", Lat = "decimalLatitude.new1",
                             by_State = TRUE, buffer_State = 20,
                             by_Biome = TRUE,
                             buffer_Biome = 20,
                             by_Endemism = TRUE,
                             Buffer_Brazil = 20,
                             value = "flag", keep_columns = TRUE,
                             verbose = TRUE)
  return(occ_flag)
  #Print Progress
  p(sprintf("i=%g", i))}, 
  error = function(e) NULL) #Skip errors
  }, future.chunk.size=1)
})
#Rename list of dataframes
names(occ_flags) <- l.spp

#Join results
occ_flag <- rbindlist(occ_flags)

#Save occurrences
dir.create("Check_Points")
fwrite(occ_flag, "Check_Points/4-Flagged_Records.csv", row.names = FALSE)
