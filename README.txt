[Note: presubmission version -- it does not contain the raw data]


########################################################################################
## dryAR: Companion R pkg of a climatic selection experiment with Arabidopsis thaliana #
########################################################################################
#######################       Moises Exposito-Alonso       #############################
####################      moisesexpositoalonso@gmail.com        ########################
########################################################################################



#########################################################################################
### Details
#########################################################################################

For more details on the package, run:
?fieldpackage

For more details in the main dataset, "field", run:
?field

This is a companion package of the manuscript: Exposito-Alonso et al. (2017) A selection
experiment with 517 Arabidopsis thaliana ecotypes (to be submitted). A copy of the
manuscript comes with the R package:
https://github.com/MoisesExpositoAlonso/dryAR/paper/Exposito-Alonso_data_field.pdf

#########################################################################################
### Package structure
#########################################################################################

field
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
|-- GENOTYPES_REPLICATES_TREATMENTS_POSITIONS-MADRID-2015-9-3_Experiment_1001g_field_design_acc_trays.csv.tsv
|-- GENOTYPES_REPLICATES_TREATMENTS_POSITIONS-TUEBINGEN-2015-9-3_Experiment_1001g_field_design_acc_trays.csv.tsv
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

#########################################################################################
### Exploratory analyses available
#########################################################################################

The analyses are stored under /analyses. They are typically R markdown files (.Rmd) and come
with pre-computed .html report.

The different analyses are:

(1) Growth trajectories exploration:
https://github.com/MoisesExpositoAlonso/dryAR/analyses/growth.html

(2) Flowering time exploration
https://github.com/MoisesExpositoAlonso/dryAR/analyses/flowering_exploration.html

