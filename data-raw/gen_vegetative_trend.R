library(dplyr);library(tidyr)
library(devtools)

load_all("~/mexposito/moiR/")
# library(moiR)
load_all(".")
# library(field)

data(veggyraw)
veggy=veggyraw
###################################################################################################################
## Produce cleaner data than gen_vegetative
## Since there are duplicates but the replicability is >99%, generate average of counts for two pots. Necessary for mergin later on

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
                                               count=mean(count)) ->veggy
veggy %>% head()
veggy %>% tail()

veggypot= veggy %>% mutate( potindex=paste(site,qp,pos,id,sep="_")) %>% select(-indexrep)

devtools::use_data(veggypot,overwrite = T)

