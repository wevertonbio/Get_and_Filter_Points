####7-Remove species with broad distribution beyond the Neotropics####

#Load packages
library(terra)
library(dplyr)
library(data.table)

#Import records
occ <- fread("Check_Points/6-Cleaned_records.csv")
#Convert dataframe to spatvector
pts <- vect(occ, geom = c("decimalLongitude.new1", "decimalLatitude.new1"))

#Importa vector of Neotropic
neot <- vect("../Vectors/Neotropic.gpkg")
plot(neot)

#Draw buffer of 250km around neotropics
neot <- terra::buffer(neot, width = 250*1000) %>% terra::aggregate()
neot
plot(neot)

##See how many records fall inside Neotropics
in.neot <- pts[neot]
in.neot$In_neotropic <- TRUE
out.neot <- terra::mask(pts, neot, inverse = TRUE)
out.neot$In_neotropic <- FALSE
final.neot <- rbind(in.neot, out.neot) %>% as.data.frame()


##Summarize by specie
sp.neot <- final.neot %>% 
  group_by(species) %>%
  summarize(In_neotropic = sum(In_neotropic == TRUE, na.rm = TRUE),
            Out_neotropic = sum(In_neotropic == FALSE, na.rm = TRUE)) %>% 
  mutate(Percent_in = In_neotropic/(In_neotropic+Out_neotropic)*100)


#Save data
fwrite(sp.neot, "Check_Points/Number of records inside Neotropics.csv",
       row.names = F)

#Keep only species with at least 95% of records inside Neotropics
sp95 <- sp.neot %>% filter(Percent_in >= 95)
occ95 <- subset(final.neot, species %in% sp95$species)

#Remove records outside neotropics
occ.neot <- occ95 %>% filter(In_neotropic == TRUE)

#Save data
fwrite(occ.neot, "Check_Points/7-Records_in_Neotropics.csv", row.names = FALSE)
