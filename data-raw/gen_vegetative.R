library(dplyr);library(tidyr)
library(devtools)
library(ggplot2);library(cowplot)

load_all("~/mexposito/moiR/")
# library(moiR)
load_all(".")
# library(field)

## read harvesting info

tuev <- make_data_vegetative(location="tuebingen")
madv <- make_data_vegetative(location="madrid")
veggy<-rbind(madv,tuev)

head(veggy)
tail(veggy)
dim(veggy)

# merge

data("genoreps")
head(genoreps)

veggy= merge(veggy,genoreps, by=c('qp','pos','site'),all.y=T)   ### NOTE ALSO
## THAT CAN BE SEVERAL PICTURES FOR A SINGLE POT!
head(veggy)

# correct two folders with a non-standard name
veggy$folder=gsub(veggy$folder,pattern = '2015_12_09y10', replacement = '2015_12_09',fixed=T) # to homogenize
veggy$folder=gsub(veggy$folder,pattern = '2015_11_17_incompleto', replacement = '2015_11_17',fixed = T) # to homogenize

# remove image information
veggy= veggy %>% mutate( pathimage= paste(pathimage,folder,image,sep='/') ) %>%
                mutate(day=as.Date(folder,format= "%Y_%m_%d")) %>%
                select(-folder, -image)
head(veggy)

# save as package database
veggyraw=veggy
devtools::use_data(veggyraw,overwrite = T)


################################################################################


## Get total sum of green pixels to see if something can be inferred

green=veggy %>% group_by(site,qp,pos,trayid,rep,indpop,water,id) %>%
  summarize(greensum=sum(countgreen)) %>%
  mutate(identifier=paste(sep="_",site,qp,pos))

qplot(log10(green$greensum))

badflagsgreen=green %>% filter(greensum < 1e4) %>% select(identifier)

## Get the trajectories

veg<-
  veggy %>%
  head(1e4) %>% # for profiling

  mutate(identifier=paste(sep="_",site,qp,pos)) %>% # get an indentifier variable
  filter( ! identifier  %in% badflags) %>% # remove those pots with bad identifiers
  # filter( ! identifier  %in% badflagsgreen) %>% # filter if there is not enough green


  mutate(starting=startday(site)) %>% # add the start day of the experiment ina per row basis for later calculations
  mutate(daycount= fn(day - as.Date(starting) ) )%>% #  trick to filter the 40 first dates depending on experiment
  filter(daycount <60) %>% # 40 days because we started the thinning like 1 month after they started germination, probably would be better to do it in a per pot basis.

  group_by(site,qp,pos,trayid,rep,indpop,water,id) %>% # group observations by pot to analyse each time series
  summarize(
            ger.a=fitsigmoid(countgreen,daycount,parameter='a'),
            ger.b=fitsigmoid(countgreen,daycount,parameter='b'),
            ger.c=fitsigmoid(countgreen,daycount,parameter='c'),
            firstgreen=firstgreen(y=countgreen,x=daycount),  # first green pixels is kind of arbitrary
            lin.0=fitlinear(countgreen,daycount,'significance')  # Probably the regression one is not so nice.

            )

### join with red and green total count
veg %>%
  full_join(.,red,by=c('site','qp','pos','trayid','rep','indpop','water','id')) %>%
  full_join(.,green,by=c('site','qp','pos','trayid','rep','indpop','water','id')) %>%
  select(-indentifier.y, -identifier.x)


### SAVE DATASET
devtools::use_data(veg,overwrite = T)

