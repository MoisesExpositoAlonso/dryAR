
##### Note: presubmission version --  does not contain the raw data.

If you would like a copy of the draft manuscript or would like to use the data, send me an e-mail.

#### Contact

Moises Exposito-Alonso     
moisesexpositoalonso@gmail.com      

------


# dryAR: Companion R package of "Exposito-Alonso et al. (2017) A rainfall-manipulated experiment with 517 *Arabidopsis thaliana* ecotypes"


#### Summary info
The gold standard to quantify natural selection are experiments where individuals of natural populations are grown together and lifetime fitness is quantified. These have been widely done in ecology to quantify phenotypic selection in a wide range of organisms, which have uncovered a number of critical patterns, including that local precipitation patterns best correlate with the strength of natural selection. Studies that include whole- genome data are still scarce, but they would inform about the frequency and strength of selection at the genetic level — a critical knowledge that would help to predict the effect of climate change in natural populations. Here we present such an experiment with the plant Arabidopsis thaliana. The experiments were carried out in a Mediterranean and a Central European field station with rainout shelters which enabled to apply a high and low rainfall treatment in each location. For each treatment combination, we planted 7 pots with one individual and 5 pots with 30 counted seeds of 517 whole-genome sequenced ecotype lines covering the global species distribution. Survival, germination, flowering time, and final seed output per plant were measured for ca. 25,000 pots, which contained ca. 14,500 individual plants and over 310,00 plants growing in small populations. Such high-throughput of phenotype production was only possible thanks to image analyses techniques using custom-made scripts. To make the data and processing code available, we created an R package “dryAR” (http://github.com/MoisesExpositoAlonso/dryAR). We believe this could be a useful resource for the evolutionary biology and the Arabidopsis communities.


#### Figure 1
Location of 517 used ecotypes (red), ecotypes included in the 1001 Genomes Projects (blue), and all locations recorded in gbif.org (grey).
![alt text](https://github.com/MoisesExpositoAlonso/dryAR/blob/master/Figure_gbif_field_occurrence_map.pdf.png)

#### Figure 2
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

A limited number of analyses are available as html documents (links might not work until the manuscript is accepted). 

(1) Growth trajectories exploration:
https://github.com/MoisesExpositoAlonso/dryAR/analyses/growth.html

(2) Flowering time exploration:
https://github.com/MoisesExpositoAlonso/dryAR/analyses/flowering_exploration.html

(3) Survival and seed set exploration:
https://github.com/MoisesExpositoAlonso/dryAR/analyses/fitness_exploration.html

