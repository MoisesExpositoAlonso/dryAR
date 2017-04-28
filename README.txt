########################################################################################
## field: and R pkg for data documentation and analysis of the 1001G field experiment ##
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

For more details in the main dataset, name field, run:
?field


#########################################################################################
### Package structure
#########################################################################################

field
|- R/
|	functions to do everything 
|
|- man/ 
|	documentation of functions in R/
|
|- analyses/
|	R markdown and corresponding html documents of the analyses.
|
|- generate_data.sh
|	Script to generate the data/ from data-raw/ 
|
|- data/
|	R objects produced from /data-raw
|
|- data-raw/
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
|--- GENOTYPES_LIST-1001G_downsample_524_extrabyeye_GREENHOUSE _FIELD_EXPERIMENT_FINAL.csv.tsv|		
|		Genotype information: name, latitude, longitude
|
|--- GENOTYPES_REPLICATES_TREATMENTS_POSITIONS-MADRID-2015-9-3_Experiment_1001g_field_design_acc_trays.csv.tsv
|--- GENOTYPES_REPLICATES_TREATMENTS_POSITIONS-TUEBINGEN-2015-9-3_Experiment_1001g_field_design_acc_trays.csv.tsv	
|		The sowing locations of all genotypes and replicates
|
|
|	 *** extra data from other experiment ***
|
|--- 1165_acc_patrice.tsv
|		flowering time from Patrice experiment
|		1001 Genomes Consortium 2016
|
|--- 1001genomes-FT10-FT16 and 1001genomes-accessions.csv		
|		flowering time from Nordborg lab at two temperatures
|		1001 Genomes Consortium 2016
|
|--- m1diii_effects_29_parameters_prepared.csv		
|		greenhouse drought experiment modeled trajectories
|		Exposito-Alonso et al. 2017
|
|--- new_variables_traject_mean_pergenotype.csv		
|		greenhouse drought experiment raw pixels
|		Exposito-Alonso et al. 2017
|
|--- RAPA_Francois_data.csv		
|		growth room datasets of flowering time and growth parameters
|		Vasseur et al. 2016
|
|
|	 *** generating scripts ***
|	
|--- gen_accessionlist.R
|--- gen_dataextra.R
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


The analyses stored in /analyses are done with R markdown (.Rmd) and then allow to output the results in .html with some 
interpretations. 

So far few analyses are done. Mainly exploration of flowering time.








