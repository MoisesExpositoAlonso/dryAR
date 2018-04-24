
##### Note: presubmission version --  does not contain the raw data nor code (links will not work).

The manuscript is in [biorxiv.org](http://www.biorxiv.org/content/early/2017/09/10/186767) with a doi:10.1101/186767

If you would like to use the data, send me an e-mail.

#### Contact

Moises Exposito-Alonso     
moisesexpositoalonso@gmail.com      

------


# dryAR: Companion R package of "Exposito-Alonso et al. (2017) A rainfall-manipulated experiment with 517 *Arabidopsis thaliana* ecotypes"


#### Abstract
The gold standard for studying natural selection is to quantify lifetime fitness in individuals from natural populations that have been grown together under different field conditions. This has been widely done in ecology to measure phenotypic selection in nature for a wide range of organisms -- an evolutionary force that seems to be most determined by local precipitation patterns. Studies that include whole-genome data would enable the translation of coefficients of selection to the genetic level, but such studies are still scarce, even though this type of genetic knowledge will be critical to predict the effect of climate change in natural populations. Here we present such an experiment including rainfall-manipulation with the plant Arabidopsis thaliana. The experiment was carried out in a Mediterranean and a Central European field station with rainout shelters to simulate a high and low rainfall treatment within each location. For each treatment combination, we planted 7 pots with one individual and 5 pots with 30 counted seeds of 517 whole-genome sequenced natural accessions covering the global species distribution. Survival, germination, flowering time, and final seed output were measured for ca. 25,000 pots, which contained ca. 14,500 individual plants and over 310,00 plants growing in small populations. This high-throughput phenotyping was only possible thanks to image analysis techniques using custom-made scripts. To make the data and processing code available, we created an R package dryAR (http://github.com/MoisesExpositoAlonso/dryAR).

#### Geographic distribution of accessions
Location of 517 used ecotypes (red), ecotypes included in the 1001 Genomes Projects (blue), and all locations recorded in gbif.org (grey).
![alt text](https://github.com/MoisesExpositoAlonso/dryAR/blob/master/Figure_gbif_field_occurrence_map.pdf.png)

#### Field experiment setup
Aerial image of the foil tunnel setting in Madrid (up) and photo inside the foil tunnel in Tübingen (down).
![alt text](https://github.com/MoisesExpositoAlonso/dryAR/blob/master/IMG_20151113_154250988.jpg)
![alt text](https://github.com/MoisesExpositoAlonso/dryAR/blob/master/IMG_20151121_162359474_HDR.jpg)



------


### Details about the data and package

For more details on the package, run:
?dryARpackage

For more details in the main dataset, "field", run:
?field

The package structure:

``` sh
dryAR
|- paper/
|--Exposito-Alonso_data_field.pdf
|
|- R/
|  # Functions to do everything
|
|- man/
|  # Documentation of functions in R/
|
|- analyses/
|  # R markdown and corresponding html documents of the  ifferent exploratory analyses.
|
|- data/
|  # R objects produced from /data-raw
|
|- data-raw/
|  # Raw data from field experiment
|
|-- Quickpot_position_who_to_thin_grey.xlsx-Sheet2.tsv
|   # Position of pots and tray numbers in the field experiment
|
|-- Flowering_pheno_Madrid.xlsx-combined.tsv
|-- lowering_pheno_Tuebingen-combined.tsv
|   # Flowering dates in format "monthday" (example 321 is March 21st)
|
|-- GENOTYPES_LIST-1001G_downsample_524_extrabyeye_GREENHOUSE _FIELD_EXPERIMENT_FINAL.csv.tsv
|   # Genotype information: name, latitude, longitude
|
|-- GENOTYPES_REPLICATES_TREATMENTS_POSITIONS-MADRID-2015-9-3_
|    Experiment_1001g_field_design_acc_trays.csv.tsv
|-- GENOTYPES_REPLICATES_TREATMENTS_POSITIONS-TUEBINGEN-2015-9-3_
|    Experiment_1001g_field_design_acc_trays.csv.tsv
|   # The sowing locations of all genotypes and replicates
|
|
|- data-cleaning/
|- # Generating scripts, from data-raw/ to data/
|
|--- gen_accessionlist.R
|--- gen_genotypereplocations.R
|--- gen_quickpot_locations.R
|
|--- gen_floweringdata.R
|--- gen_harvesting.R
|
|--- gen_field.R:
|    # Re-generates the master  field object i

```

------


### Exploratory analyses 

A limited number of analyses are available as html documents.

(1) Growth trajectories exploration:
https://github.com/MoisesExpositoAlonso/dryAR/analyses/growth.html

(2) Flowering time exploration:
https://github.com/MoisesExpositoAlonso/dryAR/analyses/flowering_exploration.html

(3) Survival and seed set exploration:
https://github.com/MoisesExpositoAlonso/dryAR/analyses/fitness_exploration.html

