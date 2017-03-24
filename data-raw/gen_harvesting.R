library(dplyr);library(tidyr)
library(devtools)

## read harvesting info

madh<-read.csv('data-raw/madharvest_image_index.csv',header=T) %>% mutate(site='madrid')
head(madh)

tueh<-read.csv('data-raw/tueharvest_image_index.csv',header=T) %>% mutate(site='tuebingen')
head(tueh)

harvest<-rbind(madh,tueh)


## add info of the replicates
data(genoreps)


harvest %>% select(-pathimage,-datelabeled)  -> harvest
genoreps %>%filter(quality %in% c('yes','maybe') ) %>% select(-otherpos,-othergeno,-quality)  -> harvest
harvest

%>%
  merge(.,genoreps, by.x=c('tray','pos','site'),by.y=c('qp','pos','site')) -> harvest

··············· CHECK MERGE CORRECT. KEEP THOSE THAT DID NOT SURVIVE!

## save
print('The first lines of data index of harvest:')
head(harvest,n=5)

devtools::use_data(harvest,overwrite = T)


