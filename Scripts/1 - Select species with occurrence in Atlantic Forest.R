#### Select species with occurrence in Atlantic Forest to model####
#Visit: https://wevertonbio.github.io/florabr/

#Install florabr
if(!require(devtools)){
  install.packages("devtools")
}

if(!require(florabr)){
  devtools::install_github('wevertonbio/florabr')}

#Load packages
library(florabr)
library(dplyr)

#Download Brazilian Flora database version 393-370#
br_dir <- "Other_Files" #Set directory to save brazilian flora database

#Get brazilian flora database version 393.370
get_florabr(output_dir = br_dir, data_version = "393.370") 

#Import brazilian flora database
bf <- load_florabr(data_dir = br_dir, data_version = "393.370")

#Select species
# Selected species based on the following criteria: Spermatophytas (Angiosperms and Gymnosperms), natives from Brazil, with correct names, and with occurrences in the Atlantic Forest
af_spp <- select_species(data = bf, include_subspecies = TRUE, include_variety = TRUE, 
                         Kingdom = "Plantae", Group = c("Angiosperms", "Gymnosperms"),
                         Subgroup = "All", Family = "All", 
                         Genus = "All", LifeForm = "All", filter_LifeForm = "in", 
                         Habitat = "All", filter_Habitat = "in",
                         Biome = "Atlantic_Forest", filter_Biome = "in", 
                         State = "All", filter_State = "in", VegetationType = "All", 
                         filter_Vegetation = "in", Endemism = "All", Origin = "Native", 
                         TaxonomicStatus = "Accepted", NomenclaturalStatus = "All")

#Check number of unique species
unique(af_spp$species) %>% length()

#Save list of species
dir.create("Examples")
my_spp <- data.frame(species = unique(af_spp$species))
write.csv(my_spp, "Examples/List_of_species.csv", row.names = F)

