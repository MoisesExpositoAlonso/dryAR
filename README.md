#  R data and analysis package field  
##       by Moises Exposito-Alonso       
####       moisesexpositoalonso@gmail.com      
####################################################################################

### package structure

/R: 
	functions to do everything 

/man: 
	documentation of functions in R/

/data: 
	R objects produced from /data-raw

/data-raw: 
	contains raw data of pots, genotype index, flowering records

	> ### data from field experiment

	> /Quickpot_position_who_to_thin_grey.xlsx-Sheet2.tsv
		
		Position of pots and tray numbers in the field experiment

	> /Flowering_pheno_Madrid.xlsx-combined.tsv
	> /Flowering_pheno_Tuebingen-combined.tsv
		
		Flowering dates in format "monthday" (example 321 is March 21st)

	> /GENOTYPES_LIST-1001G_downsample_524_extrabyeye_GREENHOUSE _FIELD_EXPERIMENT_FINAL.csv.tsv
		Genotype information: name, latitude, longitude

	> /GENOTYPES_REPLICATES_TREATMENTS_POSITIONS-MADRID-2015-9-3_Experiment_1001g_field_design_acc_trays.csv.tsv
	> /GENOTYPES_REPLICATES_TREATMENTS_POSITIONS-TUEBINGEN-2015-9-3_Experiment_1001g_field_design_acc_trays.csv.tsv
		
		The sowing locations of all genotypes and replicates

	> ### extra datasets

	> /1165 accessions mean DTF_patrice_16dehg.xlsx
	> /1165_acc_patrice.tsv
		
		flowering time from Patrice experiment
		1001 Genomes Consortium 2016

	> /1001genomes-FT10-FT16 and 1001genomes-accessions.csv
		
		flowering time from Nordborg lab at two temperatures
		1001 Genomes Consortium 2016

	> /m1diii_effects_29_parameters_prepared.csv
		
		greenhouse drought experiment modeled trajectories
		Exposito-Alonso et al. 2017

	> /new_variables_traject_mean_pergenotype.csv
		
		greenhouse drought experiment raw pixels
		Exposito-Alonso et al. 2017

	> /RAPA_Francois_data.csv
		
		growth room datasets of flowering time and growth parameters
		Vasseur et al. 2016

## generating scripts

> /refresh_data.R
	
	to produce the data/ from data-raw/, run source("refresh_data.R")

/analyses

	> R markdown and corresponding html documents of the analyses.


####################################################################################
### Curating flowering time

