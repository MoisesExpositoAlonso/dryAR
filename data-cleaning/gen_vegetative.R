# ---
# title: Data cleaning and merging of rosette areas from image analysis results
# author: "Moises Exposito-Alonso (moisesexpositoalonso@gmail.com)"
# date: '`r Sys.Date()`'
# output:
#   html_document: default
#   #pdf_document: default
#   #html_notebook: default
# ---
message('-------------------------------------------------')
message('Generating vegetative data')
message('-------------------------------------------------')


####################################################################################

## Packages set up
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

devtools::load_all(".")
wannaoverwrite=T


# DATA READING
## Read the green pixels from image analysis

# ## read harvesting info
# tuev <- make_data_vegetative(location="tuebingen") #### THIS CAN ONLY BE DONE IN THE CLUSTER
# madv <- make_data_vegetative(location="madrid")
# veggyraw<-rbind(madv,tuev)
# # save as package database
# devtools::use_data(veggyraw,overwrite = F)
# write.tsv(veggyraw,file = 'data-raw/veggyraw')

data(veggyraw)

### merge images with genotype replicates

data("genoreps")
head(genoreps)

veggy= merge(veggyraw,genoreps[,-1], by=c('qp','pos','site'),all.dupcoly=T)
## Note that I removed the id. We do not need to work with it all the time
## NOTE ALSO THAT CAN BE SEVERAL PICTURES FOR A SINGLE POT!

head(veggy)

# correct two folders with a non-standard name
veggy$folder=gsub(veggy$folder,pattern = '2015_12_09y10', replacement = '2015_12_09',fixed=T) # to homogenize
veggy$folder=gsub(veggy$folder,pattern = '2015_11_17_incompleto', replacement = '2015_11_17',fixed = T) # to homogenize


# add an index of the pots and days
# veggy=mutate(veggy, indexrep=paste(fc(site),fc(qp),fc(pos),fc(day),sep='_'))
veggy$indexrep=paste(veggy$site ,veggy$qp,veggy$pos,veggy$folder,sep='_')

# remove image information
veggy= veggy %>% mutate( pathimage= paste(pathimage,folder,image,sep='/') ) %>%
                mutate(day=as.Date(folder,format= "%Y_%m_%d")) %>%
                select(-folder, -image)
head(veggy)



################################################################################
## Clean data by removing duplicate records from duplicated images
## Produce cleaner data than veggyraw
## Since there are duplicates but the replicability is >99%, generate average of
## counts for two pots of the same identity (can be due to two pictures per
## tray/day). Necessary for merge with master dataset later

veggy <- veggy %>%
  mutate( indexrep=paste(site,qp,pos,day,sep='_')) %>%
          group_by(indexrep) %>% summarise(site=unique(site),
                                               qp=unique(qp),
                                               pos=unique(pos),
                                               trayid=unique(trayid),
                                               rep=unique(rep),
                                               indpop=unique(indpop),
                                               water=unique(water),
                                               day=unique(day),
                                               pathimage=head(unique(pathimage),n=1),
                                               countgreen=mean(countgreen,na.rm=T),
                                               countred=mean(countred,na.rm=T)
                                           )
veggy %>% head()
veggy %>% tail()

dim(veggy)

veggy= veggy %>% mutate( potindex=paste(site,qp,pos,sep="_")) %>% select(-indexrep)
veggy %>% head()
veggy %>% tail()

if(wannaoverwrite==T) devtools::use_data(veggy,overwrite = T)

################################################################################
# DATA FILTERING
## Get the total number of green pixels

# get total number of green counts
green=veggy %>% group_by(site,qp,pos,trayid,rep,indpop,water,potindex) %>%
  summarize(greensum=sum(countgreen)) %>%
  mutate(identifier=paste(sep="_",site,qp,pos))

p<-ggplot(data=green,aes(x=log10(greensum)),fill='black' )+geom_histogram() + labs(x='log 10 (# green pixels)')
p

################################################################################
## Removing unsuccessful pots - indicated by the red labels
##### == >  RED FLAG
# Cumulative red sum
red=veggy %>%
  group_by(potindex) %>%
  summarize(redsum=sum(countred))

p<-ggplot(data=red,aes(x=log10(redsum+1)),fill='black' )+geom_histogram(binwidth = 0.1) + labs(x='log 10 (# red pixels +1)')
p

################################################################################
#=> only calculated one time
# Calculate variance across groups by establishing different threshold values
## By calculating F statistic, we calculate the variance between two groups of pots
## whose pixels are counted. For that several thresholds are tried, the one that
## separates better the two distribution is the one that will be used

# fvals<-sapply(seq(1e4,1e6,by=1000),function(x){  ## Only need to run once!!!!!
#     summary(aov(red$redsum ~ red$redsum >x))[[1]][["F value"]][1]
# })
# foundthreshold= seq(1e4,1e6,by=1000)[which(fvals==max(fvals,na.rm=T))] # =152000
# print(foundthreshold)
# print(log10(foundthreshold+1))
foundthreshold=152000

# plot over the pixel distribution and draw the line
# p<-p+geom_vline(xintercept =  log10(foundthreshold+1), color='red')
# p
# save_plot(filename="figs/Figure_redcount_histogram.pdf",plot = p, base_width = 5,base_height = 4)

# plot the F values
# qplot(y=fvals ,x= seq(1e4,1e6,by=1000)) + labs(y='F value', x='Threshold of red pixels separating good and bad pots') + geom_vline(xintercept = foundthreshold,color='red')+geom_hline(yintercept = 0)
################################################################################
# generate a bad flag column based on the previous threshold
table(red$redsum > foundthreshold)
redflag = red %>% filter(redsum > foundthreshold) %>% select(potindex)

################################################################################
##### == >  GREEN FLAG

greenflag <- green%>% mutate(isnotgreen = greensum<10) %>%
  group_by(potindex) %>% summarize(greenflag=any(isnotgreen==TRUE)) %>% filter(greenflag==TRUE)

dim(greenflag)
unique(greenflag$potindex) %>% length()

################################################################################
##### == >  FILTER BOTH FLAGS

# select those post that do not have every a red flag or green flag (no pixel)
veggyclean <- veggy %>%
  # do the filtering of red flags
    filter( !(potindex %in% redflag$potindex)) %>% #
  # filtering at least those pots that never had any green pixel
    filter( !(potindex %in% greenflag$potindex))  #
dim(veggyclean)

# hist(log10(veggy$countred+1))
# hist(log10(veggyclean$countred+1),add=T,col='black') # proof of concept that there are no pots with non zero

if(wannaoverwrite==T) devtools::use_data(veggyclean,overwrite = T)


################################################################################
## Model all trajectories as sigmoidal functions


dim(veggyclean)
head(veggyclean)

veg<-  veggyclean %>%
  # select early positions
  mutate(starting=startday(site)) %>% # add the start day of the experiment ina per row basis for later calculations
  mutate(daycount= fn(day - as.Date(starting) ) )%>%
  group_by(site,qp,pos,trayid,rep,indpop,water,potindex) %>% # group observations by pot to analyse each time series
  # calculate several trajectory informations
  summarize(
            ss.a=fitsigmoid(countgreen,daycount,parameter='a'),
            ss.b=fitsigmoid(countgreen,daycount,parameter='b'),
            ss.c=fitsigmoid(countgreen,daycount,parameter='c'),
            spline1=fitspline(y=countgreen,x=daycount),
            green1=firstgreen(y=countgreen,x=daycount),  # first green pixels is kind of arbitrary
            lin0=fitlinear(countgreen,daycount,'significance'),  # Probably the regression one is not so nice.
            avg.green=mean(countgreen,na.rm=T),
            avg.red=mean(countred,na.rm=T)
            ) %>% ungroup %>% as.data.frame()

veg_backup=veg
head(veg)

################################################################################
## some visual checks
names(veg )
names(red)
names(green)

veg %>% dim()
veg %>% head()
veg %>% tail()

table(is.na(veg$spline1))
table(is.na(veg$green1))
table(veg$ss.a=='NA')
table(is.na(veg$lin0))
table(is.na(veg$ss.a))

hist(removetail(veg$ss.a))
hist(removetail(veg$ss.c))

################################################################################
## Write data
if(wannaoverwrite==T) devtools::use_data(veg,overwrite = T)



