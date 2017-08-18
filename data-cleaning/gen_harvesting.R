# ---
# title: Read the harvesting data
# author: "Moises Exposito-Alonso (moisesexpositoalonso@gmail.com)"
# date: '`r Sys.Date()`'
# ---

message('-------------------------------------------------')
message('Generating harvesting data')
message('-------------------------------------------------')

library(dplyr)
library(tidyr)
library(devtools)

load_all("~/mexposito/moiR/")
load_all(".")

# wannaoverwrite=F
wannaoverwrite=T


# READ
####################################################################################
# these are only the counts
tueh <- read_count(location="tuebingen")
head(tueh)
madh <- read_count(location="madrid")
head(madh)
harvestcount<-rbind(madh,tueh)

if(wannaoverwrite==T ) devtools::use_data(harvestcount,overwrite = T)

####################################################################################
# now the labels of each image
tuel <- read_labels(location="tuebingen")
madl <- read_labels(location="madrid")
harvestlabel<-rbind(madl,tuel) %>% rename(indpop=ptype)
head(harvestlabel)
tail(harvestlabel)

harvestlabel$num %>% unique()
madl$num %>% unique
tuel$num %>% unique

madl$pos %>% unique
tuel$num %>% unique

####################################################################################
# merge both
dim(madh)
dim(tueh)
dim(madl)
dim(tuel)

dim(harvestcount)
harvestcount$pathimage %>% unique
dim(harvestlabel)
harvestlabel$pathimage %>% unique

h<-merge(harvestcount,harvestlabel, by=c('pathimage','site'))  # note this merge will eliminate those images that are not present in both datasets
dim(h)
head(h); tail(h)

# this is to correct some problems in labeling and inconsistent symbols of NA
h<-correct_labels(h) #### EXTREMELY IMPORTANT


# save merge
if(wannaoverwrite==T ) devtools::use_data(h,overwrite = T)
write.tsv(h,file='data-raw/harvestraw.tsv')

####################################################################################
##### re-name some   <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< This is not done yet
# this area is meant to generate a better harvestraw file, where some images are relabeled
# this section should look for weird positions, and add to the tray column, a 'curate' value
# I will use re-label.py for that.
# h=read.table(h,file='../data-raw/harvestraw_relabeled.tsv',sep='\t')

####################################################################################
# ADD GENOTYPE INFORMATION
data("genoreps")
head(genoreps)

head(h); tail(h)
head(genoreps)

h %>% filter(organ=='i')  %>%
  merge(., genoreps,
        by.x=c('tray','pos','site','indpop'),
        by.y=c('qp','pos','site','indpop'),all.y=T) -># note this merge will use the original replicates. therefore the non-existence of data is also a survival measurement
  harvest

# add quality flag
harvest$Harv.q <- 0
harvest$Harv.q[ !is.na(harvest$pathimage) ] <-1

## save but remove some columns
names(harvest)
dim(harvest)

harvest=harvest %>%
  # select(-pathimage, -datelabeled) %>%  # remove these unimportant columns # maybe better to keep it
  rename(Harv.organ=organ,   # rename some
         Harv.num=num,
         Harv.area=totarea,
         Harv.sk=sk,
         Harv.bp=bp,
         Harv.ep=ep)

# Add the -9 to the individual replicates
names(harvest)
harvest$Harv.num [harvest$indpop =='i'] <- (-9)

# Correct the over 30
harvest$Harv.num [harvest$indpop =='p']
harvest$Harv.num [harvest$indpop =='p' & harvest$Harv.num > 30 ]  <-30


# From Francois Vasseur GT01 project, I have counted seeds and image analyses measurements
# data(totbranches2siliques) # waiting for a better estimate from
# summary(totbranches2siliques)
#
# harvest$totbranches=harvest[,'Harv.sk']
# harvest$nsiliques=predict(totbranches2siliques,newdata = harvest)
#
# hist(harvest$nsiliques)
# summary(harvest$nsiliques)
#
# table(is.na(harvest$nsiliques))
# ==> I think this is too bad of an estimate. Not using it


####################################################################################
## Finally output
if(wannaoverwrite==T) devtools::use_data(harvest,overwrite = T) # this includes also path to image


