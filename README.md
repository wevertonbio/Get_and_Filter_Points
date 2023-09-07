# Get and Filter Points

This repository serves to store R scripts to download, process and filter occurrence records of species from 4 databases: [GBIF](https://www.gbif.org/), [SpeciesLink](https://specieslink.net/), [Jardim Botânico do Rio de Janeiro (JABOT)](http://jabot.jbrj.gov.br/v3/consulta.php), [NeotropicTree](http://www.neotroptree.info/) and [Atlantic Epiphytes datapaper](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.2541).

Authors: Weverton Carlos Ferreira Trindade\*, Márcia Marques

Contact: [wevertonf1993\@gmail.com](Contact:%20wevertonf1993@gmail.com)

## Description

The 'Scripts' folder contains the code to run the analysis, including both example scripts (indicated by '_EXAMPLE') and scripts for parallel analysis. The 'Examples' folder contains the data necessary to run the examples with *Araucaria angustifolia*.

## Workflow

Basically, the workflow works through the combination of functions of 3 packages: [florabr](https://wevertonbio.github.io/florabr/), [plantR](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13779) and [CoordinateCleaner](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13152).

A summary of the workflow and codes follows below:

### 1 - Select species with occurrence in Atlantic Forest

We used the florabr R package to select species based on the following criteria: Spermatophytas (Angiosperms and Gymnosperms), natives from Brazil, with correct names, and with occurrences in the Atlantic Forest.