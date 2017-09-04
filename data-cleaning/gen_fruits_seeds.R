# ---
# title: Estimate seeds for absolute fitness using approximate allometries
# author: "Moises Exposito-Alonso (moisesexpositoalonso@gmail.com)"
# date: '`r Sys.Date()`'
# output:
#   html_document: default
# ---


library(dplyr)
library(tidyr)
library(devtools)

library(moiR)
devtools::load_all('../field/.')



################################################################################
#### load dataset and explore variation ###

set.seed(1)

# get a good representation of images based on the space of skeleton, branching points, etc
data(harvest)


harvest[,c('Harv.sk','Harv.area','Harv.bp','Harv.ep')] %>%
  na.omit() %>%
prcomp(.) -> pcinflorescence

summary(pcinflorescence)


ggpca(pcinflorescence)
ggplot(harvest)+
  geom_point(aes(Harv.area, Harv.sk))
  # this indicates that the most variable is the area


# Subset the relevant variables
harvest[,c('pathimage','Harv.sk','Harv.area','Harv.bp','Harv.ep')] %>%
  na.omit()-> hnona


# # generate groups based on quantiles in the harvest area
# hnona$group=cut(hnona$Harv.area, breaks=
#                   quantile(hnona$Harv.area,probs = c(0,0.2,0.4,0.6,0.8,1)),labels = FALSE)
# hnona$group %>% unique
# # check that it worked
# plot(hnona$Harv.area ~ hnona$group)
#
# # Sample 50 images per harvest area range
# selectionimages=tapply(hnona$pathimage,hnona$group, function(i) sample(i,size=50) ) %>% unlist() %>%
#   sample(.,replace = F)
#   # resample so they are not sorted by size. then I can count any number and will
#   # have a good representation
#
# # write
# write.table(file='data-raw/tocountfruits_n1.tsv',data.frame(pathimage=selectionimages,fruitcount=NA),quote=F, row.names=F)
  ### => this was too many pictures. furtheremore, the large ones cannot be counted
  ### directly in the picture


################################################################################
### Direclty the smallest and the largest can be counted, and then we assume a linear relationship
### I think counting 250 is too much, since some plants have some thousands of fruits.

field.i=filter(field,indpop=='i')

field.i[field.i$Harv.area > quantile(field.i$Harv.area, probs = 0.999,na.rm=T),] %>%
  na.omit %>% select(pathimage) %>%unlist ->
  high
field.i[field.i$Harv.area < quantile(field.i$Harv.area, probs = 0.001,na.rm=T),] %>%
  na.omit%>% select(pathimage) %>%unlist ->
  low

# tocountextremes=c(high,low) %>% sample(.,replace = FALSE)
tocountextremes=c(low) %>% sample(.,replace = FALSE) # high cannot be counted direclty in the image
tocountextremes<-data.frame(pathimage=tocountextremes,nfruits=NA)

### Read the manually counted

# write.table(file='data-raw/tocountfruits_extremes.tsv',tocountextremes,quote=F, row.names=F)
lowcounted=read.table(file='data-raw/tocountfruits_extremes.tsv',header = TRUE,stringsAsFactors = FALSE)
  # there are two that are rosettes
lowcounted=filter(lowcounted, nfruits !='rosette')

### the high needed to be counted manually taking the envelopes
# Just to find those that I counted
# dplyr::filter(field,site=='tuebingen',tray=='321',pos=='a5')
# dplyr::filter(field,site=='tuebingen',tray=='277',pos=='a3')
# dplyr::filter(field,site=='tuebingen',tray=='136',pos=='b6')

highcounted=read.table(file='data-raw/tocountfruits_extremes_manualhandling.tsv',header=TRUE,stringsAsFactors = FALSE)
  # '../data/field_madrid_harvesting/2016_513/P1160487.JPG' this is qp 311, correct it!


### Merge with Field
counted=rbind(lowcounted,highcounted)

fruitcounted<-merge(field, counted, by='pathimage')

# devtools::use_data(fruitcounted,overwrite = FALSE)


################################################################################
#### build relationsihp area to fruit ####
data(fruitcounted)

ggregression<-
function(x,y,color="black",xlab=deparse(substitute(x)),ylab=deparse(substitute(y)), doregression=T,doloess=F,span=if(doloess==T){ c(.5,1,1.5,2)},...){
    require(ggplot2)
    require(cowplot)
    tp=data.frame(x=as.numeric(x),y=as.numeric(y))
    myplot<- ggplot(data=tp)+geom_point(aes(y=y,x=x),colour=color, ...) +    ylab(ylab)+     xlab(xlab)
    if(doregression==T){
    myplot<- myplot+ geom_smooth(aes(y=y,x=x),method="glm",colour=color)+   annotate("text",  x=Inf, y = Inf, label = lm_eq(y,x),parse=FALSE, vjust=1, hjust=1)
    }
    if(doloess==T){
    for(span in span){
    myplot<-  myplot + geom_smooth(aes(y=y,x=x),method="loess",colour=transparent(color),span=span,se=F)
    }
    }
    return(myplot)
}

reg<-ggregression(ylab = 'Manually counted fruits' ,xlab = 'Image skeleton size (pixels)',y=fruitcounted$nfruits ,x= fruitcounted$Harv.sk,doloess = T,span = 2,size=3)
reg
save_plot(filename='figs/seed_regression.pdf',plot = reg,base_height = 4,base_width = 4.4)

plot(fruitcounted$nfruits ~ fruitcounted$Harv.sk ,ylab='Manually counted fruits', xlab='Inflorescence skeleton size (# pixels)')
abline( fruitpredictor()$sk )

fruitpredictor()$sk  %>% summary
fruitpredictor()$all  %>% summary


cor.test(fn(fruitcounted$nfruits) , fn(fruitcounted$Harv.sk))

#

lm(data=fruitcounted, nfruits ~ Harv.sk + Harv.area + Harv.bp + Harv.ep) %>% summary


################################################################################
# manually count seeds
# Took 50 fruits from Col-0 plants at different maturation times and well watered and relatively badly watered.
# seedcounts<-c(21,50,14,27,45,38,44,40,50,50,37)
# mean(seedcounts)
# sd(seedcounts)

# Took fruits from the harvested plants to get an estiamte directly in the field experiment
seedcounts=c(19,17,18,43,48,19,31,22,30,36)
seeds_per_fruit=mean(seedcounts)
sd(seedcounts)

ggplot(data.frame(seeds=seedcounts))+geom_density(aes(x=seeds))


################################################################################
##################### PREDICT ON FIELD ########################################

field.f<-field
field.f$nfruits= usefruitpredictor(dat = field.f,from='all')

field.f$nseeds= field.f$nfruits * seeds_per_fruit

hist(field.f$nfruits)
hist(field.f$nseeds)
summary(field.f$nfruits)
summary(field.f$nseeds)

################################################################################
### write
devtools::use_data(field.f,overwrite = T)


