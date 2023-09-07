####5 - Conclude flagging with CoordinateCleaner####

#Load packages
library(data.table)
library(CoordinateCleaner)
library(dplyr)
library(tidyr)


#Set directory
setwd("Examples/")

#Import records
pts <- fread("Check_Points/4-Flagged_Records.csv")

#Starting flagging...

#Radius around country capitals
occ.cap <- cc_cap(pts, lon = "decimalLongitude.new1", lat = "decimalLatitude.new1",
                  species = "species", buffer = 1000, #Buffer in meters
                  value = "flagged")
occ.cap <- cbind(pts, "Country_Capital" = occ.cap)

#Radius around country and province centroids
occ.cen <- cc_cen(occ.cap, lon = "decimalLongitude.new1",
                  lat = "decimalLatitude.new1",
                  species = "species", buffer = 1000, #Buffer in meters
                  value = "flagged")
occ.cen <- cbind(occ.cap, "Province_centroid" = occ.cen)

#Records with identical lon/lat
occ.equ <- cc_equ(occ.cen, lon = "decimalLongitude.new1",
                  lat = "decimalLatitude.new1",
                  test = "absolute",
                  value = "flagged")
occ.equ <- cbind(occ.cen, "IdenticalLongLat" = occ.equ)

#Radius around the GBIF headquarters in Copenhagen
occ.gbif <- cc_gbif(occ.equ, lon = "decimalLongitude.new1",
                    lat = "decimalLatitude.new1",
                    species = "species",
                    buffer = 1000, #Buffer in meters
                    value = "flagged")
occ.gbif <- cbind(occ.equ, "GBIFheadquarter" = occ.gbif)

#Radius around biodiversity institutions
occ.inst <- cc_inst(occ.gbif, lon = "decimalLongitude.new1",
                    lat = "decimalLatitude.new1",
                    species = "species",
                    buffer = 100, #Buffer in meters
                    value = "flagged")
occ.inst <- cbind(occ.gbif, "Biod_inst" = occ.inst)

#Records outside lat/lon coordinate system
occ.val <- cc_val(occ.inst, lon = "decimalLongitude.new1",
                  lat = "decimalLatitude.new1",
                  value = "flagged")
occ.val <- cbind(occ.inst, "Outside_coord" = occ.val)

#Plain zeros in the coordinates and a radius around (0/0)
occ.zero <- cc_zero(occ.val, lon = "decimalLongitude.new1",
                    lat = "decimalLatitude.new1",
                    buffer = 0.1, #Buffer in degrees
                    value = "flagged")
occ.zero <- cbind(occ.val, "zero_coord" = occ.zero)

#Remove duplicates
occ.dup <- cc_dupl(x = occ.zero, lon = "decimalLongitude.new1",
                   lat = "decimalLatitude.new1",
                   species = "species", value = "flagged")
occ.final <- cbind(occ.zero, "Duplicated" = occ.dup)


####Create columns with summary of all problems of the record####
#Check problems identified by plantR (in the column geo.check1)
unique(occ.final$geo.check1)
#Create columns...
occ.final$shore <- ifelse(occ.final$geo.check1 == "shore", "shore", NA)
occ.final$open_sea <- ifelse(occ.final$geo.check1 == "sea", "sea", NA)
occ.final$wrong_country <- ifelse(occ.final$geo.check1 == "bad_country", "wrong_country", NA)
occ.final$wrong_country <- ifelse(occ.final$geo.check1 == "bad_country[border]", "wrong_country[border]", occ.final$wrong_country)
unique(occ.final$loc.check1)
occ.final$Checar_Local <- ifelse(occ.final$loc.check1 == "ok_same_resolution" & occ.final$resolution.gazetteer1 == "locality", NA, "Checar_local")
unique(occ.final$Checar_Local)

#Flag old records (before 1981) or records without year
occ.final$Ano <- ifelse(occ.final$year.new < 1981, "before1981", NA)
occ.final$Ano <- ifelse(occ.final$year.new == "n.d.", "NoYear", occ.final$Ano)

#Update columns
colnames(occ.final)
#Is cultivated? If there is no info, fill with NA
unique(occ.final$cult.check)
occ.final$cult.check <- ifelse(occ.final$cult.check == "", NA, occ.final$cult.check)

#Is inside or outside states with confirmed occurrence? (Flagged with florabr R package)
unique(occ.final$Inside_State)
occ.final$Inside_State[which(occ.final$Inside_State == FALSE)] <- "outside_state"

#Is inside or outside biomes with confirmed occurrence? (Flagged with florabr R package)
unique(occ.final$Inside_Biome)
occ.final$Inside_Biome[which(occ.final$Inside_Biome == FALSE)] <- "outside_bioma"

#Is an endemic species with records outside Brazil? (Flagged with florabr R package)
unique(occ.final$Inside_BR)
occ.final$Inside_BR[which(occ.final$Inside_BR == TRUE |
                                  occ.final$Inside_BR == "Non-endemic")] <- NA
occ.final$Inside_BR[which(occ.final$Inside_BR == FALSE)] <- "Endemic_out_BR"


#Others flags (Flagged with CoordinateCleaner)
occ.final$Country_Capital <- ifelse(occ.final$Country_Capital == FALSE, "capital_centroid", NA)
occ.final$Province_centroid <- ifelse(occ.final$Province_centroid == FALSE, "province_centroid", NA)
occ.final$IdenticalLongLat <- ifelse(occ.final$IdenticalLongLat == FALSE, "identical_LongLat", NA)
occ.final$GBIFheadquarter <- ifelse(occ.final$GBIFheadquarter == FALSE, "gbif_headquarter", NA)
occ.final$Biod_inst <- ifelse(occ.final$Biod_inst == FALSE, "inst_bio", NA)
occ.final$Outside_coord <- ifelse(occ.final$Outside_coord == FALSE, "outside_coord", NA)
occ.final$zero_coord <- ifelse(occ.final$zero_coord == FALSE, "zero_coord", NA)
occ.final$Duplicated <- ifelse(occ.final$Duplicated == FALSE, "duplicated", NA)

#Merge all flags in a unique column
colnames(occ.final)
occ.p <- occ.final %>% 
  mutate(Inside_State2 = Inside_State,
         Inside_Biome2 = Inside_Biome,
         Inside_BR2 = Inside_BR) %>% 
  mutate_at(.vars = c("Inside_State2", "Inside_Biome2", "Inside_BR2"),
                                 ~ ifelse(.x == TRUE, NA, .x)) %>% 
  unite(Flagging, c(shore, open_sea, wrong_country,
                                          Checar_Local, cult.check, Ano,
                                          Inside_State2, Inside_Biome2, Inside_BR2,
                                          Country_Capital,
                                          Province_centroid,IdenticalLongLat,
                                          GBIFheadquarter, Biod_inst,
                                          Outside_coord, zero_coord, Duplicated),
                             sep = "|", na.rm = TRUE, remove = FALSE) %>% 
  dplyr::select(-c("Inside_State2", "Inside_Biome2", "Inside_BR2"))

#See problems
unique(occ.p$Flagging)
#Identify records without any flags
occ.p$Flagging[which(occ.p$Flagging == "")] <- "No flags"
#Identify who processed the records
occ.p$Filtrador <- "Weverton"
##CHECK POINT: save data####
fwrite(occ.p, "Check_Points/5-Flagged_Records_CoordinateCleaner.csv", row.names = FALSE)

#Save metadata
writeLines(c("METADADOS DE E.Ocorrencias_Sinalizadas.csv",
             "Ocorrencias sinalizadas",
             paste("Numero total de registros:", nrow(occ.p), sep = " "),
             paste("Numero total de especies:", length(unique(occ.p$species)), sep = " ")),
           "Check_Points/5-Metadadata.txt")
#Add table to metadata
p.df <- data.frame(table(occ.p$Flagging))
colnames(p.df) <- c("Problems", "n_records")
write.table(p.df, file = "Check_Points/5-Metadadata.txt", append = TRUE, row.names = F)

rm(list = ls());gc() #Free memory


