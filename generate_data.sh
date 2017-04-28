#!/bin/bash

# Generate the data of field package stored under data/ from the source dataset in data-raw/

Rscript  data-raw/gen_quickpot_locations.R
Rscript  data-raw/gen_accessionlist.R
Rscript  data-raw/gen_genotypereplocations.R
Rscript  data-raw/gen_floweringdata.R
Rscript  data-raw/gen_harvesting.R

Rscript  data-raw/gen_dataextra.R

# When there have been changes in some of the above, to refresh the master data, run:
Rscript  data-raw/gen_field.R

