

### Packages set up

library(knitr)
library(dplyr,tidyr)
library(ggplot2);library(cowplot);library(GGally)
library(devtools)
library(RColorBrewer)
library(stargazer)
library(moiR)
library(lme4)
source("~/ebio/abt6_projects9/ath_1001G_image_pheno/experiment_218_droughtgwa/droughtfunctions.R")
load_all(".") # field


### load data sets

data(field)
dim(field)
names(field)
stargazer(field,type='text')

##

alltreatments<- expand.grid(c("madrid","tuebingen"),c("h","l"),c("i","p"))

for(i in 1:nrow(alltreatments) ){
  print("--------------------")
  line=moiR::fc(alltreatments[i,])
  print (line)
  res<- genotypevar(
            # variable = "FT.q",
            variable = "FT.dif",
            site = line[1],
            water=line[2],
            indpop=line[3],
            applyfilter=T,
            relative=F
            )
  print(res)
}

genotypevar(
            variable = "FT.dif",
            random = c('id','site','water'),
            applyfilter=F,
            relative=F
            )

genotypevar(
            variable = "FT.dif",
            random = c('id','water','indpop'),
            site='madrid',
            applyfilter=T,
            relative=F
            )

genotypevar(
            variable = "FT.dif",
            random = c('id','water','indpop'),
            site='tuebingen',
            applyfilter=T,
            relative=F
            )

