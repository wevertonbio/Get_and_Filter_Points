####6 - Remove most problematic records####

#Load packages
library(dplyr)
library(data.table)
library(mapview)
library(terra)

#Importar dados
occ <- fread("Check_Points/5-Flagged_Records_CoordinateCleaner.csv")

#See all problems
colnames(occ)
unique(occ$geo.check1)
unique(occ$cult.check)
unique(occ$Inside_State)
unique(occ$Inside_Biome)
unique(occ$Inside_BR)
unique(occ$Country_Capital)
unique(occ$Province_centroid)
unique(occ$IdenticalLongLat)
unique(occ$GBIFheadquarter)
unique(occ$Biod_inst)
unique(occ$Outside_coord)
unique(occ$zero_coord)
unique(occ$Duplicated)
unique(occ$shore)
unique(occ$Ano)
unique(occ$Outl)

#Clean records
occ.clean <- occ %>%
  filter(!(grepl("bad_country", geo.check1))) %>% #Records in wrong countries
  filter(!(grepl("sea", geo.check1))) %>% #Records in open sea
  filter(!(grepl("prob_cultivated", cult.check))) %>% #Records probably cultivated
  filter(!(grepl("cultivated", cult.check))) %>% #Records cultivated
  filter(!(grepl("outside_state", Inside_State))) %>% #Records outside states with confirmed occurrences
  filter(!(grepl("outside_bioma", Inside_Biome))) %>% #Records outside biomes with confirmed occurrences
  filter(!(grepl("Endemic_out_BR", Inside_BR))) %>% #Records of endemic species outside Brazil
  filter(!(grepl("capital_centroid", Country_Capital))) %>% #In centroids of capitals
  filter(!(grepl("province_centroid", Province_centroid))) %>% #In centroids of provinces
  filter(!(grepl("identical_LongLat", IdenticalLongLat))) %>% #With equal Lat-Long
  filter(!(grepl("gbif_headquarter", GBIFheadquarter))) %>% #In the GBIF headquarter
  filter(!(grepl("inst_bio", Biod_inst))) %>% #In biodiversity institutions
  filter(!(grepl("outside_coord", Outside_coord))) %>% #Out of coordinate system
  filter(!(grepl("zero_coord", zero_coord))) %>% #With Lat = 0 and Long = 0
  filter(!(grepl("before1981", Ano))) %>% #Old records (before 1981)
  filter(!(grepl("NoYear", Ano))) #Records without year

#Check number of occurrences before and after cleaning
nrow(occ)
nrow(occ.clean)

#Remove duplicated records
occ.dup <- cc_dupl(x = occ.clean, lon = "decimalLongitude.new1",
                              lat = "decimalLatitude.new1",
                              species = "species", value = "clean")

#Remover outliers)
occ.outl <- cc_outl(occ.dup, lon = "decimalLongitude.new1",
                    lat = "decimalLatitude.new1",
                    species = "species",
                    method = "quantile",
                    mltpl = 5,
                    value = "clean")
#CheckPoints - Salvar dados
fwrite(occ.outl, "Check_Points/6-Cleaned_records.csv", row.names = F)

