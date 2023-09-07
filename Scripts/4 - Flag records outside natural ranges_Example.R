#### 4 - Flag records outside natural ranges ####
#Visit: https://wevertonbio.github.io/florabr/articles/Spatialize_florabr.html
#Use filter_florabr function to remove or flags records outside of the species' natural ranges according to information provided by the Brazilian Flora 2020 database

#Load packages
library(florabr)
library(dplyr)
library(data.table)

#Set directory
setwd("Examples/")

#Import occurrences of Araucaria angustifolia
occ <- fread("plantR/Araucaria angustifolia.csv") %>% data.frame()

#Check script 1 to see how to import Brazilian flora database to R 
#Or, check: https://wevertonbio.github.io/florabr/articles/getting_started.html

#Import brazilian flora
bf <- load_florabr(data_dir = "../Other_Files/")

#Flag occurrences
occ_flag <- filter_florabr(data = bf, occ = occ, Species = "species",
                           Long = "decimalLongitude.new1", Lat = "decimalLatitude.new1",
                           by_State = TRUE, buffer_State = 20,
                           by_Biome = TRUE,
                           buffer_Biome = 20,
                           by_Endemism = TRUE,
                           Buffer_Brazil = 20,
                           value = "flag", keep_columns = TRUE,
                           verbose = TRUE)
#Save occurrences
dir.create("Check_Points")
fwrite(occ_flag, "Check_Points/4-Flagged_Records", row.names = FALSE)
