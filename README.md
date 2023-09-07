# Get and Filter Points

This repository serves to store R scripts to download, process and filter occurrence records of species from 4 databases: [GBIF](https://www.gbif.org/), [SpeciesLink](https://specieslink.net/), [Jardim Botânico do Rio de Janeiro (JABOT)](http://jabot.jbrj.gov.br/v3/consulta.php), [NeotropicTree](http://www.neotroptree.info/) and [Atlantic Epiphytes datapaper](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.2541).

Authors: Weverton Carlos Ferreira Trindade\*, Márcia Marques

Contact: [wevertonf1993\@gmail.com](Contact:%20wevertonf1993@gmail.com)

## Description

The 'Scripts' folder contains the code to run the analysis, including both example scripts (indicated by '\_EXAMPLE') and scripts for parallel analysis. The 'Examples' folder contains the data necessary to run the examples with *Araucaria angustifolia*.

## Workflow

Basically, the workflow works through the combination of functions of 3 packages: [florabr](https://wevertonbio.github.io/florabr/), [plantR](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13779) and [CoordinateCleaner](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13152).

A summary of the workflow and codes follows below:

### 1 - Select species with occurrence in Atlantic Forest
We used the florabr R package to select species based on the following criteria: Spermatophytas (Angiosperms and Gymnosperms), natives from Brazil, with correct names, and with occurrences in the Atlantic Forest.

### 2 - Download records from GBIF and SpeciesLink
We used the `rgbif2()` and `rspeciesLink()` functions from plantR R package to retrieve species records from GBIF and SpeciesLink.

### 3 - Processing records with PlantR
We used the functions of plantR R package for merging, processing, cleaning, validating, summarizing and exporting records of plant species occurrences:

#### 3.1 - Merging and standardizing data
* `formatDwc()`: formatting and binding records from differente databases using DarwinCore fields.
* `formatOcc()`: standardizing collector names, determiner names, collection number, dates and collection codes.
* `formatLoc()`: standardizing names of administrative levels (country, state/Province, municipality, and locality).
* `formatCoord()`: formatting geographical coordinates to decimal degrees and replacing missing coordinates by the coordinates obtained from the gazetteer.
* `formatTax()`: editting and standardizing the names of plant species and families.

#### 3.1 - Validating data
* `validateLoc()`: Comparing the resolution of the locality information provided in the original record with the resolution retrieved from the gazetteer.
* `validateCoord()`: Checking for coordinates falling near the sea shore, open sea and country boundaries; testing if problematic coordinates are inverted or swapped; searching for records taken from cultivated individuals and for the presence of spatial outliers.
* `validateTax()`: assigning different categories of confidence level (i.e. high, medium, low or unknown) to the identification of species records
* `saveData()`: saving the processed data.

### 4 - Flag records outside natural ranges
We used the `filter_florabr` to flag records outside of the species' natural ranges according to information provided by the Brazilian Flora 2020 database.

### 5 - Conclude flagging with CoordinateCleaner
We used the functions of CoordinateCleaner R package for flagging common spatial errors:

* `cc_cap()`: records around country capitals.
* `cc_cen()`: records around geographic centroids of political countries and provinces.
* `cc_equ()`: records with equal latitude and longitude coordinates
* `cc_gbif()`: records within 0.5 degree radius around the GBIF headquarters in Copenhagen, DK
* `cc_inst()`: records assigned to the location of zoos, botanical gardens, herbaria, universities and museums.
* `cc_val()`: records with non-numeric and not available coordinates as well as lat >90, la <-90, lon > 180 and lon < -180
* `cc_zero()`: records with either zero longitude or latitude.

### 6 - Remove most problematic records
After flagging the records with plantR and CoordinateCleaner functions, we removed the records with the following problems:
* Records in wrong countries.
* Records in open sea.
* Records of individuals cultivated or probably cultivated.
* Records outside states with confirmed occurrences.
* Records outside biomes with confirmed occurrences.
* Records of endemic species outside Brazil.
* In centroids of capitals.
* In centroids of provinces.
* With equal Lat-Long.
* In the GBIF headquarter.
* In biodiversity institutions.
* Out of coordinate system.
* With Lat = 0 and Long = 0.
* Old records (before 1981).
* Records without year.
* Duplicated records of the same species.
* Outliers in geographic space: records are flagged as outliers if their mean distance to all other records of the same species is larger than 5 * the interquartile range of the mean distance of all records of this species.

### 7 - Remove species with broad distribution beyond the Neotropics
For each species, we counted the number of valid records inside and beyond the Neotropical region. We kept only the species with >95% os the records inside the Neotropical region.

### 8 - Rescue records that fall in the Sea or other NA pixel
We used the function `to_closest()` from [ellipenm](https://github.com/marlonecobos/ellipsenm) to change the longitude and latitude values of occurrences with no environmental data, moving them to the closest pixel of a raster layer that contains relevant information.