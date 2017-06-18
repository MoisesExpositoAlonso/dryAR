---
title: Data cleaning and merging of rosette areas from image analysis results
author: "Moises Exposito-Alonso (moisesexpositoalonso@gmail.com)"
date: '2017-06-02'
output:
  html_document: default
  #pdf_document: default
  #html_notebook: default
---

####################################################################################

## Packages set up




<!-- ## This scripts is meant to read and clean the  -->
<!-- ![Example image of germinating plants and red flags of those unsuccessful pots](../figs/P1030542.JPG) -->

# DATA READING
## Read the green pixels from image analysis


```r
set_project_dir('field')

# ## read harvesting info
# tuev <- make_data_vegetative(location="tuebingen")
# madv <- make_data_vegetative(location="madrid")
# veggyraw<-rbind(madv,tuev)
# # save as package database
# devtools::use_data(veggyraw,overwrite = F)

data(veggyraw)
veggy=veggyraw

head(veggy)
```

```
##              pathimage     folder        image qp pos countgreen countred
## 1 ../data/field_madrid 2015_12_21 P1040635.JPG  1  a1       2598        0
## 2 ../data/field_madrid 2015_12_21 P1040635.JPG  1  b1      12924        0
## 3 ../data/field_madrid 2015_12_21 P1040635.JPG  1  c1      19773        0
## 4 ../data/field_madrid 2015_12_21 P1040635.JPG  1  d1       5124        0
## 5 ../data/field_madrid 2015_12_21 P1040635.JPG  1  e1         75       25
## 6 ../data/field_madrid 2015_12_21 P1040635.JPG  1  a2      12915        0
##     site
## 1 madrid
## 2 madrid
## 3 madrid
## 4 madrid
## 5 madrid
## 6 madrid
```

```r
tail(veggy)
```

```
##                      pathimage     folder        image  qp pos countgreen
## 553085 ../data/field_tuebingen 2015_12_14 P1030608.JPG 176  a8         25
## 553086 ../data/field_tuebingen 2015_12_14 P1030608.JPG 176  b8       2503
## 553087 ../data/field_tuebingen 2015_12_14 P1030608.JPG 176  c8      70623
## 553088 ../data/field_tuebingen 2015_12_14 P1030608.JPG 176  d8       7903
## 553089 ../data/field_tuebingen 2015_12_14 P1030608.JPG 176  e8       1937
## 553090 ../data/field_tuebingen 2015_12_14 P1030608.JPG 176             NA
##        countred      site
## 553085        0 tuebingen
## 553086       20 tuebingen
## 553087        0 tuebingen
## 553088        0 tuebingen
## 553089        0 tuebingen
## 553090       NA tuebingen
```

```r
dim(veggy)
```

```
## [1] 553090      8
```

```r
# merge

data("genoreps")
head(genoreps)
```

```
##     id rep trayid qp indpop water pos   site
## 1 9739   1  1_p_l  1      p     l  a2 madrid
## 2 9910   1  1_p_l  1      p     l  a3 madrid
## 3 9641   1  1_p_l  1      p     l  a4 madrid
## 4 7008   1  1_p_l  1      p     l  a5 madrid
## 5 9714   1  1_p_l  1      p     l  a6 madrid
## 6 9314   1  1_p_l  1      p     l  a7 madrid
```

```r
veggy= merge(veggy,genoreps, by=c('qp','pos','site'),all.y=T)   ### NOTE ALSO
## THAT CAN BE SEVERAL PICTURES FOR A SINGLE POT!
head(veggy)
```

```
##   qp pos   site            pathimage                folder        image
## 1  1  a2 madrid ../data/field_madrid            2016_02_01 P1070837.JPG
## 2  1  a2 madrid ../data/field_madrid            2015_12_14 P1030929.JPG
## 3  1  a2 madrid ../data/field_madrid            2015_12_18 P1040283.JPG
## 4  1  a2 madrid ../data/field_madrid            2015_11_30 P1020514.JPG
## 5  1  a2 madrid ../data/field_madrid            2016_01_22 P1060775.JPG
## 6  1  a2 madrid ../data/field_madrid 2015_11_17_incompleto P1010279.JPG
##   countgreen countred   id rep trayid indpop water
## 1      95174        0 9739   1  1_p_l      p     l
## 2       6412        0 9739   1  1_p_l      p     l
## 3      18040        0 9739   1  1_p_l      p     l
## 4        135        0 9739   1  1_p_l      p     l
## 5      41438        0 9739   1  1_p_l      p     l
## 6         55        0 9739   1  1_p_l      p     l
```

```r
# correct two folders with a non-standard name
veggy$folder=gsub(veggy$folder,pattern = '2015_12_09y10', replacement = '2015_12_09',fixed=T) # to homogenize
veggy$folder=gsub(veggy$folder,pattern = '2015_11_17_incompleto', replacement = '2015_11_17',fixed = T) # to homogenize

# remove image information
veggy= veggy %>% mutate( pathimage= paste(pathimage,folder,image,sep='/') ) %>%
                mutate(day=as.Date(folder,format= "%Y_%m_%d")) %>%
                select(-folder, -image)
head(veggy)
```

```
##   qp pos   site                                    pathimage countgreen
## 1  1  a2 madrid ../data/field_madrid/2016_02_01/P1070837.JPG      95174
## 2  1  a2 madrid ../data/field_madrid/2015_12_14/P1030929.JPG       6412
## 3  1  a2 madrid ../data/field_madrid/2015_12_18/P1040283.JPG      18040
## 4  1  a2 madrid ../data/field_madrid/2015_11_30/P1020514.JPG        135
## 5  1  a2 madrid ../data/field_madrid/2016_01_22/P1060775.JPG      41438
## 6  1  a2 madrid ../data/field_madrid/2015_11_17/P1010279.JPG         55
##   countred   id rep trayid indpop water        day
## 1        0 9739   1  1_p_l      p     l 2016-02-01
## 2        0 9739   1  1_p_l      p     l 2015-12-14
## 3        0 9739   1  1_p_l      p     l 2015-12-18
## 4        0 9739   1  1_p_l      p     l 2015-11-30
## 5        0 9739   1  1_p_l      p     l 2016-01-22
## 6        0 9739   1  1_p_l      p     l 2015-11-17
```

# CONSENSUS OF DUPLICATES
## Verify replicability

```r
set_project_dir('field')

## check duplicated image pots
veggy=mutate(veggy, indexrep=paste(site,qp,pos,day,sep='_'))

## Number of pots used for replicability analysis
dim(veggy[duplicated(veggy$indexrep),]) # There are 790 in total from both experiments
```

```
## [1] 790  13
```

```r
unique(veggy[duplicated(veggy$indexrep),]$day)
```

```
##  [1] "2015-11-16" "2015-11-09" "2015-12-04" "2015-11-13" "2015-12-09"
##  [6] "2016-02-07" "2016-02-09" "2015-11-23" "2015-10-30" "2016-01-30"
## [11] "2015-11-27"
```

```r
unique(veggy[duplicated(veggy$indexrep),]$qp)
```

```
##  [1] 105 109 141 164 168 181 190 193 201 231 232 233 249 257 260  31  33
## [18] 330  45  50  67
```

```r
## Run the test for replicability
library(MCMCglmm)
```

```
## Loading required package: Matrix
```

```
## Loading required package: coda
```

```
## Loading required package: ape
```

```r
lmm=MCMCglmm(data=veggy[duplicated(veggy$indexrep),], countgreen ~1, random = ~ indexrep, family = 'poisson',verbose=F)
print ( h2MCMC(lmm,randomname = 'indexrep') )
```

```
## $Mode
##      var1 
## 0.9975679 
## 
## $HPD
##         lower    upper
## var1 0.995865 0.998453
## attr(,"Probability")
## [1] 0.95
```


## Clean data by removing duplicate records from duplicated images

```r
set_project_dir('field')

## Produce cleaner data than veggyraw
## Since there are duplicates but the replicability is >99%, generate average of
## counts for two pots of the same identity (can be due to two pictures per
## tray/day). Necessary for merge with master dataset later

# veggy <- veggy %>%
#   mutate( indexrep=paste(site,qp,pos,day,sep='_')) %>%
#           group_by(indexrep) %>% summarise(site=unique(site),
#                                                qp=unique(qp),
#                                                pos=unique(pos),
#                                                trayid=unique(trayid),
#                                                rep=unique(rep),
#                                                indpop=unique(indpop),
#                                                water=unique(water),
#                                                id=unique(id),
#                                                day=unique(day),
#                                                countgreen=mean(countgreen),
#                                                countred=mean(countred)
#                                            ) 
# veggy %>% head()
# veggy %>% tail()
# 
# dim(veggy)
# 
# veggy= veggy %>% mutate( potindex=paste(site,qp,pos,id,sep="_")) %>% select(-indexrep)
# 
# devtools::use_data(veggy,overwrite = F) # Save the data for the package

data(veggy)
veggy %>% dim()
veggy %>% head()
veggy %>% tail()
```
This is not run because takes some time, instead I load the data already produced

# DATA VISUALIZATION
## Single pot trends as example


```r
veggy %>% filter(trayid == "27_i_l" ,pos=="a3", site=="madrid") %>%
  ggplot(.) + geom_point(aes(y=countgreen,x=day),color="green") +
  geom_point(aes(y=countred,x=day),color="red")
```

<img src="gen_vegetative_files/figure-html/plot fail pot-1.png" width="672" />

```r
#This is an example of a fail pot, where green pixels are low all the time but a red flag was put to identify them


veggy %>% filter(trayid == "27_i_l" ,pos=="a4", site=="madrid") %>%
  ggplot(.) + geom_point(aes(y=countgreen,x=day),color="green") +
  geom_point(aes(y=countred,x=day),color="red")
```

<img src="gen_vegetative_files/figure-html/plot fail pot-2.png" width="672" />

```r
#The decrease in January of some is due to the thinning of plants for

veggy %>% filter(trayid == "9_p_l" ,pos=="e7", site=="tuebingen") %>%
  ggplot(.) + geom_point(aes(y=countgreen,x=day),color="green") +
  geom_point(aes(y=countred,x=day),color="red")
```

<img src="gen_vegetative_files/figure-html/plot fail pot-3.png" width="672" />

```r
#This plot is a populatino replicate therefore there is no sudden decay, only that due to mortality at the end of the experiment
```


## Plot general trends of green area

```r
set_project_dir('field')

veggy= veggy %>% mutate( potindex=paste(site,qp,pos,id,sep="_"))  %>% mutate(site_water=paste(site,water,sep="_"))

veggy %>% filter(indpop=='i') %>%
ggplot() + geom_line(aes(y=countgreen,x=day,group=potindex, color=factor(site_water)),alpha=0.1 ) +
   labs(
    y = "# green pixels",
    colour = ""
   )#+ theme(legend.position="none")
```

<img src="gen_vegetative_files/figure-html/plot general trends-1.png" width="672" />

```r
veggy %>% filter(indpop=='p') %>%
ggplot() + 
  # geom_line(aes(y=countgreen,x=day,group=potindex, color=factor(site_water)),alpha=0.1 ) +   
  geom_line(aes(y=countgreen,x=day,group=potindex, color=factor(site_water)),alpha=0.05 ) +   
  labs(
    y = "# green pixels",
    colour = ""
   )#+ theme(legend.position="none")
```

<img src="gen_vegetative_files/figure-html/plot general trends-2.png" width="672" />

```r
p1<-
veggy %>% filter(indpop=='p',site=='madrid') %>%
ggplot() + 
  geom_line(aes(y=countgreen,x=day,group=potindex, color=factor(water)),alpha=0.05 ) +   
  scale_color_manual(values = c("blue",  "red"))+
  labs(title='Madrid',
    y = "# green pixels",
    colour = "Watering"
   )+ guides(colour = guide_legend(override.aes = list(alpha = 1)))

p2<-
veggy %>% filter(indpop=='p',site=='tuebingen') %>%
ggplot() + 
  geom_line(aes(y=countgreen,x=day,group=potindex, color=factor(water)),alpha=0.05 ) +   
  scale_color_manual(values = c("blue",  "red"))+
  labs(title='Tübingen',
    y = "# green pixels",
    colour = "Watering"
   )+ guides(colour = guide_legend(override.aes = list(alpha = 1)))


panel<-plot_grid(p1,p2)
save_plot(filename="figs/Figure_green_trajectory.pdf",plot = panel, base_width = 10,base_height = 3.8)
```


## Plot general trends of red area and determine which pots are labeled red

```r
set_project_dir()

veggy %>% 
ggplot() + geom_line(aes(y=countred,x=day,group=potindex, color=factor(site_water)),alpha=0.1 ) #+ theme(legend.position="none")
```

<img src="gen_vegetative_files/figure-html/plot general trend red area-1.png" width="672" />

```r
p<-ggplot(data=veggy,aes(x=log10(countred+1)),fill='black' )+geom_histogram() + labs(x='log 10 (# red pixels +1)')
p
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="gen_vegetative_files/figure-html/plot general trend red area-2.png" width="672" />

```r
save_plot(filename="figs/Figure_redcount_histogram.pdf",plot = p, base_width = 5,base_height = 4)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```


# DATA FILTERING
## Get the total number of red

```r
set_project_dir('field')

red=veggy %>% group_by(site,qp,pos,trayid,rep,indpop,water,id) %>% summarize(redsum=sum(countred))# %>% mutate(identifier=paste(sep="_",site,qp,pos)) 
```


## Get the total number of green

```r
set_project_dir()

# get total number of green counts
green=veggy %>% group_by(site,qp,pos,trayid,rep,indpop,water,id,potindex) %>%
  summarize(greensum=sum(countgreen)) %>%
  mutate(identifier=paste(sep="_",site,qp,pos))
```


## Removing unsuccessful pots

```r
set_project_dir()

########################################################################################################################
##### == >  RED FLAG
# quantile(log10(veggy$countred+1),probs = seq(0,1,0.01))
# whichred= log10(veggy$countred+1) > 1.4
# table(whichred)
# length(whichred)
# nrow(veggy)
# veggy<-veggy %>% mutate(isred=whichred)
# 
# table(veggy$isred)
# lapply(veggy,veggy$potindex,FUN = function      (x) table(x$isred==TRUE))
# 
# 
# dim(veggy)
# redflags <- veggy%>%
#   group_by(potindex) %>% summarize(redflag=table(isred == TRUE)['TRUE']>3) %>% filter(redflag==TRUE)
#   # group_by(potindex) %>% summarize(redflag=any(isred==TRUE)) %>% filter(redflag==TRUE) # i think this is picking many false positives
# dim(redflags)
# unique(redflags$potindex) %>% length()
# 
# redflags <- veggy%>%
#   group_by(potindex) %>% summarize(redflag=table(isred == TRUE)['TRUE']) 
#   # group_by(potindex) %>% summarize(redflag=any(isred==TRUE)) %>% filter(redflag==TRUE) # i think this is picking many false positives
# dim(redflags)
# 
# hist(redflags$redflag)
####  this method was too sensitive. I came back to the previous one of cumulative sum

# Cumulative sum
red=veggy %>% 
  # group_by(site,qp,pos,trayid,rep,indpop,water,id) %>% 
  group_by(potindex) %>% 
  summarize(redsum=sum(countred)) 

# calculate variance across groups by establishing different threshold values
fvals<-sapply(seq(1e4,1e6,by=1000),function(x){
    summary(aov(red$redsum ~ red$redsum >x))[[1]][["F value"]][1]
})
foundthreshold= seq(1e4,1e6,by=1000)[which(fvals==max(fvals,na.rm=T))] # =152000
print(foundthreshold)
```

```
## [1] 152000
```

```r
qplot(y=fvals ,x= seq(1e4,1e6,by=1000)) + labs(y='F value', x='Threshold of red pixels separating good and bad pots') + geom_vline(xintercept = foundthreshold,color='red')+geom_hline(yintercept = 0)
```

```
## Warning: Removed 501 rows containing missing values (geom_point).
```

<img src="gen_vegetative_files/figure-html/count red and green pixels-1.png" width="672" />

```r
#By calculating F statistic, we calculate the variance between two groups of pots whose pixels are counted. For that several thresholds are tried, the one that separates better the two distribution is the one that will be used

# generate a bad flag column
table(red$redsum > foundthreshold)
```

```
## 
## FALSE  TRUE 
## 22780  1967
```

```r
redflag = red %>% filter(redsum > foundthreshold) %>% select(potindex)


########################################################################################################################
##### == >  GREEN FLAG

greenflag <- green%>% mutate(isnotgreen = greensum<10) %>%
  group_by(potindex) %>% summarize(greenflag=any(isnotgreen==TRUE)) %>% filter(greenflag==TRUE)

dim(greenflag)
```

```
## [1] 5 2
```

```r
unique(greenflag$potindex) %>% length()
```

```
## [1] 5
```

```r
# select those post that do not have every a red flag or green flag (no pixel)
veggyclean <- veggy %>%
  # do the filtering of red flags
    filter( !(potindex %in% redflag$potindex)) %>% # 
  # filtering at least those pots that never had any green pixel
    filter( !(potindex %in% greenflag$potindex))  # 
dim(veggyclean)
```

```
## [1] 435020     15
```

```r
# hist(log10(veggy$countred+1))
# hist(log10(veggyclean$countred+1),add=T,col='black') # proof of concept that there are no pots with non zero

devtools::use_data(veggyclean,overwrite = myoverwrite)
```

```
## Saving veggyclean as veggyclean.rda to /Users/moisesexpositoalonso/ebio/abt6_projects7/ath_1001G_field/field/data
```

## Model all trajectories as sigmoidal functions


```r
set_project_dir()

dim(veggyclean)
head(veggyclean)

veg<-  veggyclean %>%
# select early positions
  mutate(starting=startday(site)) %>% # add the start day of the experiment ina per row basis for later calculations
  mutate(daycount= fn(day - as.Date(starting) ) )%>% #  trick to filter the 40 first dates depending on experiment
  # filter(daycount <60) %>% # 40 days because we started the thinning like 1 month after they started germination, probably would be better to do it in a per pot basis.

  group_by(site,qp,pos,trayid,rep,indpop,water,id,potindex) %>% # group observations by pot to analyse each time series

# calculate several trajectory informations
  
  summarize(
            ss.a=fitsigmoid(countgreen,daycount,parameter='a'),
            ss.b=fitsigmoid(countgreen,daycount,parameter='b'),
            ss.c=fitsigmoid(countgreen,daycount,parameter='c'),
            spline1=fitspline(y=countgreen,x=daycount),
            green1=firstgreen(y=countgreen,x=daycount),  # first green pixels is kind of arbitrary
            lin0=fitlinear(countgreen,daycount,'significance')  # Probably the regression one is not so nice.
            # meangreen=mean(countgreen)
            )


## Get the merge with total counts of red and green

veg=veg %>%
  full_join(.,red,by=c('site','qp','pos','trayid','rep','indpop','water','id')) %>%
  full_join(.,green,by=c('site','qp','pos','trayid','rep','indpop','water','id'))
dim(veg)

veg %>% dim()
veg %>% head()
veg %>% tail()

table(is.na(veg$spline1))
table(veg$ss.a=='NA')
table(is.na(veg$lin0))

devtools::use_data(veg,overwrite = myoverwrite)
```

