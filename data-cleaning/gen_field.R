# ---
# title: Read the harvesting data
# author: "Moises Exposito-Alonso (moisesexpositoalonso@gmail.com)"
# date: '`r Sys.Date()`'
# ---

message('-------------------------------------------------')
message('Generating field data')
message('-------------------------------------------------')

pryr::mem_used()
library(dplyr)
library(tidyr)
library(devtools)
library(ggplot2)
library(cowplot)

load_all("~/moiR/")
pryr::mem_used()
load_all(".")
pryr::mem_used()

#### wannaoverwrite
wannaoverwrite=T

#### Get the datas

data(acc)
data(genoreps)
data(flowering)
data(veg)
data(harvest)

dim(genoreps)
dim(veg)
dim(flowering)
dim(harvest)

head(acc)
head(flowering)
head(veg)
head(harvest)

################################################################################
#### master dataset ####
# acc => contains locations of origin and other information
# veg => contains the sucessfully sown pots
# flowering => contains FT and the survival to reach flowering
# harvest => contains Fitness and the survival to reach reproduction

field<-
  merge(acc,genoreps,by=dupcol(acc,genoreps), all.y=T) %>%
  merge(.,veg,by=dupcol(.,veg), all.y=T) %>%
  merge(.,flowering,by=dupcol(.,flowering), all.x=T) %>%
  merge(.,harvest,by=dupcol(.,harvest), all.x=T)


field %>% names
field %>% head
field %>% tail
field %>% dim


################################################################################
### A couple of quality controls
# There are some flowering unknowns, -9, because it was unclear (only 4, consider dead)
field$FT.q[field$FT.q == -9 & !is.na(field$FT.q)] <- NA

# To be able to calculate later the fitness, I make the harv.num that is NA, zero
field$Harv.num %>% unique

field[is.na(field$Harv.num),'Harv.num']<-0
# and the -9, which are from ind. replicates, I will change to 1 later


################################################################################
### write
summary(field)

devtools::use_data(field,overwrite = T)
