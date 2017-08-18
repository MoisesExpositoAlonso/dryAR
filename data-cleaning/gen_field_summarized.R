# ---
# title: Read the harvesting data
# author: "Moises Exposito-Alonso (moisesexpositoalonso@gmail.com)"
# date: '`r Sys.Date()`'
# ---

message('-------------------------------------------------')
message('Generating field data summarized per geno')
message('-------------------------------------------------')

################################################################################
library(dplyr)
library(tidyr)
library(devtools)

load_all("~/moiR/")
load_all(".")

data(field.f)
head(field.f)

################################################################################
# Add the coordinates of the trays, for future mixed models

# either position in the whole tunnel
data(qp)


# but also valid the within block tray numbers. That would be like trays nested within the block
getstart= function(x){
  tmp=lapply(x, function(x){
    strsplit(x, split = "_", fixed = TRUE) [[1]][1]
  }) %>% unlist()
  return(tmp)
}

posintunnel<-function(trays,coord='x'){
  if(coord=='x'){
    tmp=lapply(trays, FUN = function(x){
     return( row(qp)[which(qp==x)] )
    }) %>% unlist()
  }else if(coord=='y'){
    tmp=lapply(trays, FUN = function(x){
     return( col(qp)[which(qp==x)] )
    }) %>% unlist()
  }else{stop('Provide y or x for coord argument')}
  return(tmp)
}


field.f<-mutate(field.f,
                qpblock=getstart(trayid),
                qp_x=posintunnel(qp,'x'),
                qp_y=posintunnel(qp,'y')
)



################################################################################
# Generate a nicer names version of field
names(field.f)
summary(field.f)

field.c<-field.f %>%
  select(-ss.a,-ss.b, -ss.c, -spline1, -lin0,-Harv.sk,-Harv.ep,-Harv.bp,-Harv.organ,
         -tray) %>%
  rename(.,
         pathimage=pathimage,
         Germination_time=green1,
         Green=avg.green,
         Red=avg.red,
         Survival_flowering=FT.q,
         Survival_fruit=Harv.q,
         Survival_num=Harv.num,
         Flowering_date=FT.date,
         Flowering_time=FT.dif,
         Inflorescence_size=Harv.area,
         Fruits=nfruits,
         Seeds=nseeds
)

cat(colnames(field.c))

field.c[,c('latitude','longitude','Germination_time' ,'Green' ,'Red' ,'Survival_flowering' ,
           'Flowering_date' ,'Flowering_time'  ,'Inflorescence_size' ,
           'Survival_num', 'Survival_fruit' ,'Fruits', 'Seeds')]<-
  apply(field.c[,c('latitude','longitude','Germination_time' ,'Green' ,'Red' ,'Survival_flowering' ,
           'Flowering_date' ,'Flowering_time'  ,'Inflorescence_size' ,
           'Survival_num', 'Survival_fruit' ,'Fruits', 'Seeds')],
        2,as.numeric)


head(field.c)

# Survival curate
field.c$Survival_num[field.c$Survival_num == -9] <- 1
field.c$Survival_num[is.na(field.c$Survival_num)] <- 0 # important that survival has all 0
# field.s$Survival_num[field.s$Survival_num == -9] <- 1 # NOT RUN maybe it is good to keep them separate

# Fecundity by individual
field.c$Infloresncence_byind <-ifelse(c(field.c$indpop=='p'),
                               field.c$Inflorescence_size/ field.c$Survival_num,
                               field.c$Inflorescence_size
                               )

field.c$Fruits_byind <-ifelse(c(field.c$indpop=='p'),
                               field.c$Fruits/ as.numeric(field.c$Survival_num),
                               field.c$Fruits
                               )

field.c$Seeds_byind <-ifelse(c(field.c$indpop=='p'),
                               field.c$Seeds/ as.numeric(field.c$Survival_num),
                               field.c$Seeds
                               )

field.c$Fitness <-ifelse(c(field.c$indpop=='p'),
                               field.c$Survival_num/30 * field.c$Seeds_byind,
                               field.c$Survival_fruit* field.c$Seeds_byind
                               )
  #lifetime fitness is calcualted differently for individuals and for populatiosn
  # -> for individuals, it is just the probability to reach repruduction across the
  # different replicates, times the average fecundity
  # -> for population replicates the lifetime fitness is the reals survival knowing
  # that we put 30 seeds per pot, so the ratio of who survived times the fecundity,
  # but in a per individual.

# important check, the overall fitness needs to incorporate survival
field.c$Fitness[field.c$Fitness < 1 & !is.na(field.c$Fitness)]<-1
field.c$Fitness[is.na(field.c$Fitness)] <- 0
field.c$Fitness %>% summary


# WRITE!
summary(field.c)

devtools::use_data(field.c,overwrite = T)

################################################################################
#### Get the per genotype summarized data ####
################################################################################

names(field.c)

meanorvalue=function(x){ # important to use this function in summarize! Otherwise many NAs
  x=na.omit(x)
  if(length(x)>1){
    x=mean(x)
  }else if(length(x)>0){
    x=as.numeric(x)
  }else{
    x=NA
  }
}

field.c %>%
  select(-potindex,-rep,-trayid,-qp,-qpblock,-qp_x,-qp_y,-pos,-pathimage,-datelabeled, # don't want by pot anymore
         -Flowering_date) %>%  # also Date has difficult behaviour
  group_by(id,site,water,indpop,name,country,latitude,longitude,kgroup) %>%
  summarize_all(funs(meanorvalue(.)) )->
field.s

field.s %>% summary

# field.s %>% names
# field.s %>% head
# field.s %>% tail
# field.s %>% dim

# Because I saw a behaviour that many genotype were assigned an NA even though
# they had at least an observation, I do one more check
apply(field.s,2,function(x)length(na.omit(x)))
  # get how many columns values are not NA
field.s %>% filter(site=='madrid' & water=='l' & indpop=='i') %>% select(Inflorescence_size)
  # This was conflictive because there were originally only 4 values


# WRITE!
devtools::use_data(field.s,overwrite = T)
