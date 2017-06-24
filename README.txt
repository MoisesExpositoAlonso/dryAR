########################################################################################
## dryAR: and R pkg for data documentation and analysis of the 1001G field experiment ##
########################################################################################
######################       by Moises Exposito-Alonso       ###########################
####################      moisesexpositoalonso@gmail.com        ########################
########################################################################################


#########################################################################################
### Details
#########################################################################################

For more details on the package, run:
?fieldpackage

For more details in the main dataset, name field, run:
?field

This is a companion package of the manuscript: Exposito-Alonso et al. (2017) A selection
experiment with 517 Arabidopsis thaliana ecotypes (to be submitted); which can be found
under the paper/ folder.


#########################################################################################
### Package structure
#########################################################################################

field
|- paper/
|	Exposito-Alonso_data_field
|
|- R/
|	functions to do everything
|
|- man/
|	documentation of functions in R/
|
|- analyses/
|	R markdown and corresponding html documents of the analyses.
|
|- data/
|	R objects produced from /data-raw
|
|- data-raw/
|
|- data-cleaning/
|
|	 *** data from field experiment ***
|
|--- Quickpot_position_who_to_thin_grey.xlsx-Sheet2.tsv
|		Position of pots and tray numbers in the field experiment
|
|--- Flowering_pheno_Madrid.xlsx-combined.tsv
|--- lowering_pheno_Tuebingen-combined.tsv
|		Flowering dates in format "monthday" (example 321 is March 21st)
|
|--- GENOTYPES_LIST-1001G_downsample_524_extrabyeye_GREENHOUSE _FIELD_EXPERIMENT_FINAL.csv.tsv
|		Genotype information: name, latitude, longitude
|
|--- GENOTYPES_REPLICATES_TREATMENTS_POSITIONS-MADRID-2015-9-3_Experiment_1001g_field_design_acc_trays.csv.tsv
|--- GENOTYPES_REPLICATES_TREATMENTS_POSITIONS-TUEBINGEN-2015-9-3_Experiment_1001g_field_design_acc_trays.csv.tsv
|		The sowing locations of all genotypes and replicates
|
|	 *** generating scripts ***
|
|--- gen_accessionlist.R
|--- gen_genotypereplocations.R
|--- gen_quickpot_locations.R
|		these mostly read csv files
|
|--- gen_floweringdata.R
|--- gen_harvesting.R
|		these are heavy duty scripts that rely on functions in /R. These clean and tidy the data from the field exp.
|
|--- gen_field.R:
		re-generates the field data object in case any of the other datasets have changed

#########################################################################################
### Main analyses
#########################################################################################

The analyses are stored in /analyses are done with R markdown (.Rmd) and then allow to
output the results in .html with some interpretations.
