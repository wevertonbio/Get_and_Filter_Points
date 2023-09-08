####3 - Processing records with PlantR####
#Visit: https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13779

####Carregar pacotes####
library(plantR)
library(data.table)
library(dplyr)
library(florabr)

#Set directory
setwd("Examples/")

#Create directory to save processed records
dir.create("plantR")

#We also included records from 3 other sources:
  #Neotropic Tree: http://www.neotroptree.info/
  #Jabot: https://ala-hub.sibbr.gov.br/ala-hub/occurrences/search?q=data_provider_uid:dp28
  #ATLANTIC EPIPHYTES datapaper: https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.2541


#As example, we inclued in our directory only records from Araucaria angustifolia
#Import data from Jabot
jabot.all <- fread("Jabot/JabotFixedFloraDoBrasil.csv",
                   encoding = "UTF-8")
#Import data from Jabot
neotree <- fread("NeotropicTree/NeotropicTreeFixedFloraDoBrasil.csv",
                 encoding = "UTF-8")

#ATLANTIC EPIPHYTES
af_epi <- fread("Atlantic_Epiphytes/Atlantic_Epiphytes.csv", encoding = "UTF-8")

#Load florabr database
bf <- load_florabr("../Other_Files/")
bf <- bf %>% filter(species %in% sp | acceptedName %in% sp)


#Identify species
sp <- "Araucaria angustifolia"

#Check if there is a file downloaded from GBIF. If not, create an empty dataframe
if(file.exists(paste0("GBIF/", sp, ".csv"))) {
  sp.gbif <- fread(paste0("GBIF/", sp, ".csv"), encoding = "UTF-8") } else {
    sp.gbif <- data.frame()
  }

#Check if there is a file downloaded from SpeciesLink. If not, create an empty dataframe
if(file.exists(paste0("SpeciesLink/", sp, ".csv"))) {
  sp.splink <- fread(paste0("SpeciesLink/", sp, ".csv"),
                     encoding = "UTF-8")} else {
                       sp.splink <- data.frame()
                     }

#Subset species data from Jabot
occ.jabot <- subset(jabot.all, jabot.all$species %in% sp)
occ.jabot.dwc <- formatDwc(user_data = data.frame(occ.jabot))
occ.jabot.dwc <- occ.jabot.dwc %>% mutate(data_source = "Jabot")
occ.jabot.dwc$dateIdentified <- as.character(occ.jabot.dwc$dateIdentified)

#Subset species data from Neotropictree
neotree.sp <- subset(neotree, neotree$species %in% sp)

#Convert in DWC format
occ.nt.dwc <- formatDwc(user_data = data.frame(neotree.sp))
occ.nt.dwc$dateIdentified <- as.character(occ.nt.dwc$dateIdentified)
occ.nt.dwc <- occ.nt.dwc %>% mutate(data_source = "NeotropicTree")

#Subset species data from Atlantic_Epiphytes
#Epiphyte specie
epi.sp <- subset(af_epi, af_epi$EPIPHYTE_SPECIES %in% sp)
epi.sp <- epi.sp %>% dplyr::rename(species = EPIPHYTE_SPECIES,
                                   family = EPIPHYTE_FAMILY)
#Forophyte specie
for.sp <- subset(af_epi, af_epi$FOROPYHYTE_SPECIES %in% sp)
for.sp <- for.sp %>% dplyr::rename(species = FOROPYHYTE_SPECIES,
                                   family = FOROPHYTE_FAMILY)
epifor.sp <- bind_rows(epi.sp, for.sp)
#Rename and select columns
epifor.sp <- epifor.sp %>% dplyr::select(
  scientificName = species,
  species,
  Local = REGIONAL_NAME_OF_STUDY_SITE,
  country = COUNTRY,
  locality = REGIONAL_NAME_OF_STUDY_SITE,
  stateProvince = STATE,
  county = STATE,
  decimalLongitude = LONGITUDE_X,
  decimalLatitude = LONGITUDE_X,
  year = YEAR_FINISH,
  municipality = MUNICIPALITY,
  collectionCode = DATASET,
  institutionCode = DATASET_ACRONYM,
  catalogNumber = DATASET,
  recordNumber = RECORD_ID,
  dateIdentified = YEAR_FINISH,
  typeStatus = EPIPHYTE_HABITAT,
  family) %>% mutate(recordedBy = "Datapaper_epiphytes",
                     identifiedBy = "Datapaper_epiphytes",
                     scientificNameAuthorship = "no_info")

#Convert in DWC format
occ.epi.dwc <- formatDwc(user_data = data.frame(epifor.sp))
occ.epi.dwc$dateIdentified <- as.character(occ.epi.dwc$dateIdentified)
occ.epi.dwc <- occ.epi.dwc %>% mutate(data_source = "Datapaper_epiphytes")

#Join data
occ.nt.jabot <- rbindlist(list(occ.nt.dwc, occ.jabot.dwc, occ.epi.dwc),
                          fill = TRUE)

#Remove databases without data
splink_data2 <- if(nrow(sp.splink) <= 1) {NULL} else {sp.splink %>% data.frame()}
gbif_data2 <- if(nrow(sp.gbif) <= 1) {NULL} else {sp.gbif %>% data.frame()}
user_data2 <- if(nrow(occ.nt.jabot) == 0) {NULL} else {(occ.nt.jabot %>% data.frame())}

#Create columns to avoid errors in plantR
col.splink <- c("yearIdentified", "monthIdentified", "year", "country", 
                "municipality", "county", "stateProvince", "identifiedBy",
                "dateIdentified", "typeStatus")
if(!is.null(splink_data2)) {
  splink_data2[col.splink[!(col.splink %in% colnames(splink_data2))]] = "no.info"
  splink_data2}

#Convert date columns to character
gbif_data2[grepl("date|Date", colnames(gbif_data2))] <- as.character(gbif_data2[grepl("date|Date", colnames(gbif_data2))])
splink_data2[grepl("date|Date", colnames(splink_data2))] <- as.character(splink_data2[grepl("date|Date", colnames(splink_data2))])
user_data2[grepl("date|Date", colnames(user_data2))] <- as.character(user_data2[grepl("date|Date", colnames(user_data2))])
gbif_data2[grepl("eventTime", colnames(gbif_data2))] <- as.character(gbif_data2[grepl("eventTime", colnames(gbif_data2))])

#Finally, join occurrences
occ <- formatDwc(splink_data = splink_data2, #SpeciesLink
                 gbif_data = gbif_data2, #Gbif
                 user_data = user_data2) #Jabot, NeotropicTree and Datapaper_epiphytes

#If there is no occurrence, write empty data
if(nrow(occ) == 0) {
  write.csv(occ, paste0("plantR/", sp, ".csv"))
}

#Fix datasource
occ$data_source[which(occ$institutionCode == "NeotropicTree")] <- "NeotropicTree"
occ$data_source[which(occ$recordedBy == "Datapaper_epiphytes")] <- "Datapaper_epiphytes"
occ$data_source[which(occ$data_source == "user")] <- "Jabot"
unique(occ$data_source)

#Fill empty locality with information from verbatimLocality 
occ$locality[which(is.na(occ$locality) & !is.na(occ$verbatimLocality))] <- occ$verbatimLocality[which(is.na(occ$locality) & !is.na(occ$verbatimLocality))]

####Start processing with PlantR####
#Select only necessary columns
col.int <- c("data_source", "species_id", 
             "scientificName", "order", "family", "genus",
             "decimalLatitude", "decimalLongitude", 
             "country", "stateProvince", "municipality", "locality",
             "institutionCode", "collectionCode", "catalogNumber",
             "year", "eventDate", "recordedBy",
             "recordNumber", "identifiedBy", "yearIdentified", "dateIdentified",
             "typeStatus", "basisOfRecord")
#Create columns (if they do not exist yet)
occ[col.int[!(col.int %in% colnames(occ))]] = "no.info"

#Subset columns
occ <- occ[,col.int]

#Fix columns with identifier
occ$identifiedBy <- as.character(occ$identifiedBy)

#Fix column, if necessary
occ$recordedBy[which(occ$recordedBy == "?")] <- "no.info"

#Format Names, Numbers, Dates and Codes
occ <- formatOcc(occ)

#Fix empty columns of municipality
occ$municipality[which(is.na(occ$municipality))] <- "no.info"

#Write emppty file if there is no valid occurrence at this point
if(nrow(occ) == 0) {
  write.csv(occ, paste0("plantR/", sp, ".csv"))
}


#Remove ; . e, from locality
occ$locality <- gsub(",|\\.|:|;|-", " ", occ$locality)
occ$stateProvince<- gsub(",|\\.|:|;|-", " ", occ$stateProvince)


#Standardizes names of administrative levels#
####ATENTION####
#In september 2023, there is an issue related to the unwantedEncoding object imported within the fixLoc function.
#Check the error here: https://github.com/LimaRAF/plantR/issues/100
#To correct the error, open the script formatLoc2 in the Scripts folder and run all the code to get a working version of the function
occ <- formatLoc2(occ)
#If get some error, try scrap = F

# Formats geographical coordinates to decimal degrees and replaces missing coordinates by the coordinates obtained from the gazetteer.
occ <- formatCoord(occ)


#Prepare data to format Taxonomic Information
#Column family must contain a family, even that incorrect
occ$family <- gsub(pattern = "=", replacement = "", occ$family) #Remove "="
occ$family <- gsub(" .*", "", occ$family) #Get only the first word
occ$family <- gsub("\\(.*", "", occ$family) #Delete everything after the pattern "("
occ$family <- gsub("-.*", "", occ$family) #Delete everything after the pattern "-"
occ$family <- gsub("/.*", "", occ$family) #Delete the pattern  "/"
occ$family[which(is.na(occ$family))] <- "Lauraceae" #Generic family if NA - Function will correct it
#If there is two species, get only the first
occ$scientificName <- gsub(".*\\ x ", "", occ$scientificName)
occ$scientificName <- gsub(" var.*", "", occ$scientificName)
occ$scientificName <- gsub(" × s..*", "", occ$scientificName)
#Remove rows without species name
occ <- occ %>% filter(!is.na(scientificName))
#Finally, edits and standardizes the names of plant species and families
occ <- formatTax(occ)

#### START VALIDATION OF DATA ####
# Compares the resolution of the locality information provided in the original record with the resolution retrieved from the gazetteer
occ <- validateLoc(occ)

#Remove records with empty coordinates
occ <- occ %>% filter(!is.na(decimalLatitude.new) &
                        !is.na(decimalLongitude.new)) 

#Write emppty file if there is no valid occurrence at this point
if(nrow(occ) == 0) {
  write.csv(occ, paste0("plantR/", sp, ".csv"))
}

#Remove records with empty locality
occ <- occ %>% filter(!is.na(loc.correct))
#Write emppty file if there is no valid occurrence at this point
if(nrow(occ) == 0) {
  write.csv(occ, paste0("plantR/", sp, ".csv"))
}

#Remove records from timor leste (They cause errors in the function)
occ <- occ %>% filter(country.new != "timor leste")

#Spatial Validation of Species Records
# Checks for coordinates falling near the sea shore, open sea and country boundaries. It also test if problematic coordinates are inverted or swapped. The function also searches for records taken from cultivated individuals and for the presence of spatial outliers for each species
occ <- validateCoord(occ, output = "new.col") #It takes a time (some minutes, maybe hours...)

###If you get a error, this code can help to identify and remove the source of the error###
#Correct error when one records falls in two or more places
#Run only when get a error to identify the error
#Identify countries inside latamMap
# all.countries <- occ$country.new %>% unique()
# countries.plant <- plantR::latamMap[all.countries]
# names_countries <- names(countries.plant) %>% na.omit() %>% as.character()
# #Vectorize 
# list.countries <- pblapply(seq_along(names_countries), function(i){
# name.country <- names_countries[i]
# ct <- countries.plant[[name.country]]
# #Get occurrences
# occ.ct <- occ %>% filter(country.new == name.country)
# #Vectorize data
# ct.vect <- ct %>% vect()
# #Extract points
# ext.ct <- terra::extract(ct.vect, occ.ct[, c("decimalLongitude.new", "decimalLatitude.new")])
# #Identify error
# source_error <- ext.ct$id.y[which(duplicated(ext.ct$id.y))]
# if(length(source_error) >= 1) {
# source_error <- paste(name.country, source_error) }
# if(length(source_error) == 0) {
# source_error <- NULL }
# return(source_error)
# })
# Filter(Negate(is.null), list.countries) #This is the problematics points


#Assigns different categories of confidence level (i.e. high, medium, low or unknown) to the identification of species records
occ <- validateTax(occ)

#Check point! 

#For some species, the validateCoord functions does not work. In these cases, create columns manually
occ.new <- if("decimalLongitude.new.new" %in% colnames(occ)) {
  occ <- occ %>% dplyr::rename(decimalLongitude.new1 = decimalLongitude.new.new,
                               decimalLatitude.new1 = decimalLatitude.new.new) } else{occ}
if("decimalLatitude.new1" %in% colnames(occ.new) == FALSE) {
  occ.new <- occ.new %>% mutate(decimalLongitude.new1 = decimalLongitude.new) %>% 
    mutate(decimalLatitude.new1 = decimalLatitude.new) %>% 
    mutate(country.new1 = country.new) %>%
    mutate(border.check = NA) %>% 
    mutate(scientificName.new1 = scientificName.new) %>% 
    mutate(family.new1 = family.new) %>% 
    mutate(geo.check = NA,
           geo.check1 = NA,
           cult.check = NA,
           loc.check1 = loc.check,
           resolution.gazetteer1 = resolution.gazetteer)
}
occ <- occ.new

#Make sure coordinates are numeric
class(occ$decimalLatitude.new1);class(occ$decimalLongitude.new1)
occ$decimalLatitude.new1 <- as.numeric(occ$decimalLatitude.new1)
occ$decimalLongitude.new1 <- as.numeric(occ$decimalLongitude.new1)
class(occ$decimalLatitude.new1);class(occ$decimalLongitude.new1)
#Remove records with any information of longitude and latitude
occ <- subset(occ, !is.na(occ$decimalLatitude.new1) &
                !is.na(occ$decimalLongitude.new1))

#Get binomial name
if(any(is.na(occ$scientificNameFull))){
occ$scientificNameFull[which(is.na(occ$scientificNameFull))] <- occ$scientificName[which(is.na(occ$scientificNameFull))] }
if(any(is.na(occ$scientificName.new1))){
  occ$scientificName.new1[which(is.na(occ$scientificName.new1))] <- occ$scientificName[which(is.na(occ$scientificName.new1))] }

occ$species <- if("scientificName.new1" %in% colnames(occ)) {
  occ$species <- get_binomial(species_names = occ$scientificName.new1)
} else {
  occ$species <- get_binomial(species_names = occ$scientificNameFull)
}

#Fix column that sometimes becomes a list (?)
occ$border.check <- as.character(occ$border.check)
##Select only necessary columns
colnames(occ)
columns_occ <- c("catalogNumber", "data_source", "species_id", "species", "scientificName.new1", "decimalLatitude", "decimalLongitude", "decimalLatitude.new", "decimalLongitude.new", "decimalLatitude.new1", "decimalLongitude.new1", "country", "country.new", "stateProvince.new", "municipality.new", "locality.new", "year.new", "family.new1", "geo.check", "geo.check1","cult.check", "loc.check1", "recordedBy.new", "identifiedBy.new", "resol.orig", "resolution.gazetteer1")

#Fix columns names
occ <- if("scientificName.new1" %in% colnames(occ)) {occ} else {
  occB <- occ %>% dplyr::rename(scientificName.new1 = scientificNameFull,
                                family.new1 = family.new,
                                geo.check1 = geo.check,
                                loc.check1 = loc.check,
                                resolution.gazetteer1= resolution.gazetteer) %>% 
    mutate(geo.check =geo.check1)
  occB
}
if("year.new" %in% colnames(occ) == FALSE) {
  occ <- occ %>% mutate(year.new = year,
                        recordedBy.new = recordedBy,
                        identifiedBy.new = identifiedBy)
}

#Select columns
oc <- occ[, columns_occ]

#Correct names, if necessary
sp_bf <- bf %>% filter(species %in% sp | acceptedName %in% sp)
all_names_sp <- na.omit(unique(c(sp_bf$species, sp_bf$acceptedName)))
#Rename synonyms, if necessary
oc$species[which(oc$species %in% all_names_sp)] <- sp

#Select only correct species
oc.sp <- subset(oc, oc$species == sp)

#Save data!
saveData(oc.sp, file.name = sp, dir.name = "plantR/",
         compress = FALSE)

#Check the folder plantR to see the files!
