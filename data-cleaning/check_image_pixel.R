devtools::load_all()
library(dplyr)

data(veggyraw)
data(veggy)

head(veggyraw)

# View(veggyraw)

# therow = commandArgs(trailingOnly=TRUE)

therow=166

veggyraw[therow,]
veggyraw[therow,1:3] %>% mutate_all(as.character) %>%
  paste(.,collapse='/') %>%
  paste('eog',.) %>%
  system()

# openimage<-function(veggyrawline){
#
#   veggyrawline[,1:3] %>% mutate_all(as.character) %>%
#   paste(.,collapse='/') %>%
#   paste('eog',.) %>%
#   system()
# }

names(veggyraw>5000)

tmp<-veggyraw %>% filter(countred >5000 & countgreen >5000)

tmp2<-tmp %>%
  sample_n(.,size = 1)
tmp2
tmp2 %>%
  select(pathimage,folder,image) %>%
  mutate_all(as.character) %>%
  paste(.,collapse='/') %>%
  paste('eog',.) %>%
  system()
