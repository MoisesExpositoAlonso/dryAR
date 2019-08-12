library(dplyr)

data("g1001")
head(g1001,1)

tablepaper<-g1001
head(g1001)

# Some locations removed by hand
nalocations<-c(440  ,500  ,502  ,504  ,798 ,1119)

## Coverage intersection congruence
cic<-read.table("data-cleaning/id_coverage_intersection_congruence")
cic
covintcro<-data.frame(cic)
colnames(covintcro)="id"

##### who neanderthals  #####
neander<-read.table("data-cleaning/who_are_neanderthals_read.txt",header=T,sep=" ")
# # neander<-read.table("GoogleDrive/RESEARCH/PhD/project_1001G_history/neanderthals/who_are_neanderthals_read.txt",header=T,sep=" ")
colnames(neander)[1]<-"id"
neandernona<-neander[-nalocations,]

GOOD<-merge(neandernona,covintcro,by="id")
length(GOOD[,1])

cat (subset(neander,neander$neanderthal=="YES"&neander$country=="ESP")$id,sep="\n")


#### remove very close genomes  #####

load("data-cleaning/ibs1001g.rda")
kkk[1:5,1:5]
X<-kkk
diag(X)<-rep(NA,1135)

hist(as.matrix(X),breaks=50)
XX<-as.matrix(X)
XX[which(XX < 0.01)]
length((which(XX < 0.01)))

row(XX)[which(XX < 0.02)]
col(XX)[which(XX < 0.02)]

hist(row(X)[which(X < 0.01)],breaks=1000)
plot(row(X)[which(X < 0.01)])
hist(col(X)[which(X < 0.01)],breaks=1000)

# with 0.01 you get 889
# with 0.02 you get 762

# plot(unique(row(XX)[which(XX < 0.01)]),unique(col(XX)[which(XX < 0.01)]))


# newX<-XX[-row(XX)[which(XX > 0.02)], -col(XX)[which(XX > 0.02)]]
# newX<-XX[-unique(row(XX)[which(XX < 0.01)]), -unique(col(XX)[which(XX  < 0.01)])] ## TOO CONSERVATIVE!

newX<-XX[-unique(row(XX)[which(XX < 0.01)]), -unique(row(XX)[which(XX  < 0.01)])] ## I WILL DO IT BY ROW, IF I DO IT BY THE TWO OF THEM ,I MIGHT MISS THOSE OTHER GENOTYPES THAT ARE SIMILAR BUT ARE DIFFERENT AMONG THEM!
# AND REMOVING ONLY THOSE WITH LESS THAN 0.01 DIFFERENCES!

plot(unique(row(XX)[which(XX < 0.01)]),unique(col(XX)[which(XX < 0.01)]))

hist(XX,breaks=50)
hist(newX,breaks=50,col="black",border="white")
length(newX[,1])

noredundant<-data.frame(colnames(newX))
colnames(noredundant)<-"id"
length(noredundant[,1])


######### merging everything  #############
ALLGOOD<-merge(noredundant,GOOD,by="id") # 889 AND 959
length(ALLGOOD[,1]) # 769


length(subset(ALLGOOD,ALLGOOD$neanderthal=="YES")[,1]) # 23 - missing 3!
#I lost 3. Recover them!
length(subset(neander,neander$neanderthal=="YES")[,1]) # 26


FINAL<-subset(ALLGOOD,ALLGOOD$neanderthal!="YES")
length(FINAL[,1]) # 736
FINAL<-rbind(as.matrix(FINAL),as.matrix(subset(neander,neander$neanderthal=="YES")[1:26,]))
length(FINAL[,1])

# rownames(FINAL)<-c(1:length(FINAL[,1]))
# length(FINAL[,1]) # 762 exactly
# FINALDATA<-data.frame(FINAL)
# length(FINALDATA[,1])

cat(FINAL[,1],sep="\n")
# length(FINAL[,1])
finalids<-FINAL[,1]


#what would be the ones to remove
v1<-as.numeric(finalids)
v2<-as.numeric(neander[,1])
setdiff(v2, v1)
length(setdiff(v2, v1))
cat(setdiff(v2, v1),sep="\n")


#### remove  close locations  #####
head(neander,2)
geod<-as.matrix(dist(neander[,4:5],method="euclidean"))
colnames(geod)<-neander[,1]
rownames(geod)<-neander[,1]
dim(geod)

diag(geod)<-NA

cutoff<-0.0001

table(geod<0.001)
table(geod<0.000000000001)
hist(geod,breaks=100)
hist(geod,xlim=c(0,1),breaks=100000) # so there is actually a peak of genomes over 2500 that are very very close (less than 1 km)
#2575
plot(row(geod)[which(geod < cutoff)],
     col(geod)[which(geod < cutoff)])
length( unique(row(geod)[which(geod < cutoff)]) )


newgeod<-geod[-unique(row(geod)[which(geod < cutoff)]) , unique(row(geod)[which(geod < cutoff)])]
dim(newgeod)
table(newgeod<cutoff)
table(geod<cutoff)

length(newgeod[,1])
hist(newgeod,breaks=100)
hist(newgeod,xlim=c(0,1),breaks=100000)

geofilter<-data.frame(id=rownames(newgeod))
#684

######### merging genetics + geo  #############
ALLgengeo<-merge(ALLGOOD,geofilter,by="id") # 889 AND 959
length(ALLgengeo[,1]) # 520


length(subset(ALLgengeo,ALLgengeo$neanderthal=="YES")[,1]) # 23 - missing 3!
#I lost 3. Recover them!
length(subset(neander,neander$neanderthal=="YES")[,1]) # 26


FINAL<-subset(ALLgengeo,ALLgengeo$neanderthal!="YES")
length(FINAL[,1])
FINAL<-rbind(as.matrix(FINAL),as.matrix(subset(neander,neander$neanderthal=="YES")[1:26,]))
length(FINAL[,1])

cat(FINAL[,1],sep="\n")
finalids<-FINAL[,1]
length(FINAL[,1])

FINALdata<-data.frame(FINAL)
head(FINAL)
head(FINALdata)
class(FINALdata$latitude)


FINALdata$latitude<-as.numeric(levels(FINALdata$latitude))[FINALdata$latitude]
FINALdata$longitude<-as.numeric(levels(FINALdata$longitude))[FINALdata$longitude]

# filefinaldata<-"GoogleDrive/RESEARCH/PhD/1001G_downsample_523.csv"
# write.csv(FINALdata,file=filefinaldata)

# ####### merging and writing downsamples  ##########
# GROUPS<-read.table('GoogleDrive/RESEARCH/PhD/project_1001G_history/fineSTRUCTURE/genomeID_RECEIVERS')
# colnames(GROUPS)<-c("id","kgroup")
# FINALwithGROUPS<-merge(FINALdata,GROUPS,by="id")
# write.csv(FINALwithGROUPS,file=filefinaldata)
#
# recoversome<-c(9941,7288,14319,10006,9099,9114,9993,10014,9606)
# torecover<-data.frame(id=recoversome)
# merge(merge(torecover,neander,by="id"), GROUPS,by="id")
# torecover<-merge(torecover,neander,by="id")
# torecover$kgroup<-rep(NA,9)
# torecover[c(2,4),8]<-c(8,5)
# FINALwithGROUPS$id<-as.numeric(levels(FINALwithGROUPS$id))[FINALwithGROUPS$id]
# FINALwithGROUPSwithEXTRA<-rbind(FINALwithGROUPS,torecover)
# head(FINALwithGROUPSwithEXTRA)
# tail(FINALwithGROUPSwithEXTRA)
# # write.csv(FINALwithGROUPSwithEXTRA,file="GoogleDrive/RESEARCH/PhD/1001G_downsample_&extra_533.csv")
#


# GENETICSDOWNSAMPLE<-merge(neander,GROUPS,by="id")
# write.csv(GENETICSDOWNSAMPLE,file="GoogleDrive/INVESTIGACION/PhD/1001G_downsample_genetics_762.csv")

####?? Genetics downsample and only europe
ALLGOOD_europe<-subset(ALLGOOD,ALLGOOD$longitude > -50 & ALLGOOD$longitude<100)
hist(ALLGOOD$longitude)
hist(ALLGOOD_europe$longitude)
dim(ALLGOOD_europe)

# write.csv(ALLGOOD_europe,file="GoogleDrive/INVESTIGACION/PhD/1001G_downsample_genetics_europe_726.csv")

plot(ALLGOOD_europe$latitude~ALLGOOD_europe$longitude)

# write.csv(ALLGOOD_europe,file="ebio/abt6_projects8/ath_1001G_history/1001G_downsample_genetics_europe_726.csv")


########### MAP  ###################

library(ggplot2)
library(cshapes)
require("rgdal")
# require("maptools")
# require("plyr")
library("gpclib")
# install.packages("gpclib")
gpclibPermit()
# library(maptools)
# gpclibPermit() # there was a problem with this fucntion to do the mapping
world <- cshp(date=as.Date("2008-1-1"))
world.points <- fortify(world, region='COWCODE')




#plotit
# dev.off()
ggplot() +
#   geom_polygon(data=world.points, aes(long,lat,group=group) ,fill="white",col="grey")+
  geom_polygon(data=world.points, aes(long,lat,group=group),alpha=0.5,fill="lightgrey",col="grey") +
  geom_point(data=neander,aes(x=longitude,y=latitude),col="black",size=1.2)+
  geom_point(data=FINALdata,aes(x=longitude,y=latitude),col="red",size=1.2)+
  theme(panel.grid.major = element_line(colour = "black"))+
  theme(panel.grid.minor = element_line(colour = "black"))+
  theme(panel.border=element_rect(colour="black",fill=NA))+
  theme(panel.background=element_blank())

# filetosave="/ebio/abt6/mexposito/GoogleDrive/INVESTIGACION/PhD/project_1001G_history/Admixture/all_vs_relicts_bigger_blue_MoiDec2014.pdf"
# filetosave="/ebio/abt6/mexposito/GoogleDrive/INVESTIGACION/PhD/project_1001G_history/Admixture/REDUCEDTO762SAMPLES.pdf"
# filetosave="GoogleDrive/INVESTIGACION/PhD/project_1001G_history/Admixture/REDUCEDTO523SAMPLES.pdf"
# ggsave( filename=filetosave,width=18,height=8.5)




####### CHECK FOR SEED AVAILABILITY   ######
seeds<-read.table("GoogleDrive/INVESTIGACION/PhD/1001_Master_list_May2013",header=T)
colnames(seeds)[2]<-"id"
head(seeds)
length(seeds[,1])
seedsx<-subset(seeds,seeds=="x" | seeds=="mixedup")
length(seedsx[,1])


seedsx<-subset(seeds,seeds=="x" | seeds=="mixedup")
seedsx<-subset(seeds,seeds=="x" )
seeds_available<-merge(FINALdata,seedsx,by="id")
seeds_available
length(seeds_available[,1])

are_relicts<-subset(seeds_available,neanderthal=="YES")
length(are_relicts[,1])
22
