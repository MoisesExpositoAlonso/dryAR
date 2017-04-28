library(dplyr);library(tidyr)
library(devtools)

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
## Produce cleaner data than veggyraw
## Since there are duplicates but the replicability is >99%, generate average of
## counts for two pots of the same identity (can be due to two pictures per
## tray/day). Necessary for merge with master dataset later

names(veggy)
head(veggy)

veggy %>% mutate( indexrep=paste(site,qp,pos,day,sep='_')) %>%
          group_by(indexrep) %>% summarise(site=unique(site),
                                               qp=unique(qp),
                                               pos=unique(pos),
                                               trayid=unique(trayid),
                                               rep=unique(rep),
                                               indpop=unique(indpop),
                                               water=unique(water),
                                               id=unique(id),
                                               day=unique(day),
                                               countgreen=mean(countgreen),
                                               countred=mean(countred)
                                           ) ->veggy
veggy %>% head()
veggy %>% tail()
dim(veggy)

veggy= veggy %>% mutate( potindex=paste(site,qp,pos,id,sep="_")) %>% select(-indexrep)

devtools::use_data(veggy,overwrite = T)

################################################################################

# Generate all trends
ggplot(veggy) + geom_line(aes(y=countgreen,x=day,group=potindex, color=factor(id)),alpha=0.1 ) + theme(legend.position="none")

ggplot(veggy,aes(y=countred,x=day,
                 # group=potindex,
                 group=factor(id),
                 color=factor(id) )) +
  geom_line(alpha=0.1 ) +
  #stat_smooth() +
  theme(legend.position="none")

head(veggy )
names(veggy)

# veggy %>% filter(trayid == "27_i_l") %>%
veggy %>% filter(trayid == "27_i_l" ,pos=="a3", site=="madrid") %>%
  ggplot(.) + geom_point(aes(y=countgreen,x=day),color="green") +
  geom_point(aes(y=countred,x=day),color="red")

veggy %>% filter(trayid == "27_i_l" ,pos=="a4", site=="madrid") %>%
  ggplot(.) + geom_point(aes(y=countgreen,x=day),color="green") +
  geom_point(aes(y=countred,x=day),color="red")

veggy %>% filter(trayid == "27_i_l" ,pos=="a5", site=="madrid") %>%
  ggplot(.) + geom_point(aes(y=countgreen,x=day),color="green") +
  geom_point(aes(y=countred,x=day),color="red")

veggy %>% filter(trayid == "27_i_l" ,pos=="b2", site=="madrid") %>%
  ggplot(.) + geom_point(aes(y=countgreen,x=day),color="green") +
  geom_point(aes(y=countred,x=day),color="red")


filter ( veggyraw , trayid == "27_i_l" ,pos=="b2", site=="madrid") %>%
  select(pathimage) %>%
  write.table(.,file = "../../tmpimg/tmplist.txt",row.names=F, col,names=F)


