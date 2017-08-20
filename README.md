
##### Note: presubmission version --  does not contain the raw data.

------


# dryAR: Companion R pkg of "Exposito-Alonso et al. (2017) A selection
experiment with 517 Arabidopsis thaliana ecotypes"


#### Abstract
To quantify phenotypic and genetic natural selection, the gold standard are evolution experiments. However, studies that include whole-genome data are still scarce. Evolution experiments can be longitudinal, as laboratory experiments continued over many generations, or cross-sectional, as field experiments replicated over many geographic locations or environments. For organisms with generations that span months or years such as \textit{Arabidopsis thaliana}, cross-sectional studies are most feasible. Here we present an experiment carried out in a Mediterranean and a Central European field station with rainout shelters which enabled to apply a high and low rainfall treatment in each location. We planted 12 replicates per treatment combination of 517 whole-genome sequenced \textit{A. thaliana} lines covering the global distribution. To make the data and processing code available, we created a companion R package "dryAR" (\url{http://github.com/MoisesExpositoAlonso/dryAR}). We believe this could be a useful resource for the evolutionary biology and the Arabidopsis communities. 

nline-style: 
![alt text](https://github.com/adam-p/markdown-here/raw/master/src/common/images/icon48.png "Logo Title Text 1")


#### Contact

Moises Exposito-Alonso     
moisesexpositoalonso@gmail.com      


------


### Details about the data and package

For more details on the package, run:
?dryARpackage

For more details in the main dataset, "field", run:
?field

The package structure:

``` sh
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


### Exploratory analyses available

The analyses are stored under /analyses. They are typically R markdown files (.Rmd) and come
with pre-computed .html report.

The different analyses are:

(1) Growth trajectories exploration:
https://github.com/MoisesExpositoAlonso/dryAR/analyses/growth.html

(2) Flowering time exploration
https://github.com/MoisesExpositoAlonso/dryAR/analyses/flowering_exploration.html



* Notes:
The most important phenotypes are the fitness ones. Therefore they are described
more in detail here.

The veg dataset contains all pots that were well sown (errors of sowing removed, red flag removed)

The field dataset was a merge starting from veg with all=T flag, therefore all sown pots are there
even though fitness or other phenotypes might be zero.

Then the existence or not of flowering data indicates survival. Likewise, the existence or not
of harvesting data also indicates survival.

