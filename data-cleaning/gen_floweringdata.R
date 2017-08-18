# ---
# title: Read the harvesting data
# author: "Moises Exposito-Alonso (moisesexpositoalonso@gmail.com)"
# date: '`r Sys.Date()`'
# ---

message('-------------------------------------------------')
message('Generating flowering data')
message('-------------------------------------------------')

library(dplyr)
library(tidyr)
library(devtools)

load_all("~/mexposito/moiR/")
# library(moiR)
load_all(".")
# library(field)


#### Flowering data ####
flowering_madrid=make_data_flowering("madrid")
head(flowering_madrid);tail(flowering_madrid)
flowering_madrid$site="madrid"

flowering_tuebingen=make_data_flowering("tuebingen")
head(flowering_tuebingen);tail(flowering_tuebingen)
flowering_tuebingen$site="tuebingen"


devtools::use_data(flowering_madrid,overwrite = TRUE)
devtools::use_data(flowering_tuebingen,overwrite = TRUE)

#### Flowering data merged####

flowering=rbind(flowering_madrid,flowering_tuebingen)
head(flowering);tail(flowering)
devtools::use_data(flowering,overwrite = TRUE)
