library(dplyr,tidyr)
library(ggplot2);library(cowplot)
library(devtools)
library(RColorBrewer)
# library(moiR)
load_all("~/moiR")
load_all(".")


data(veggyraw)
data(genoreps)


# Prepare
veggy= merge(veggyraw,genoreps[,-1], by=c('qp','pos','site'),all.dupcoly=T)

# correct two folders with a non-standard name
veggy$folder=gsub(veggy$folder,pattern = '2015_12_09y10', replacement = '2015_12_09',fixed=T) # to homogenize
veggy$folder=gsub(veggy$folder,pattern = '2015_11_17_incompleto', replacement = '2015_11_17',fixed = T) # to homogenize

# add an index of the pots and days
veggy$indexrep=paste(veggy$site ,veggy$qp,veggy$pos,veggy$folder,sep='_')

# remove image information

head(veggy)

# CONSENSUS OF DUPLICATES
## Verify replicability, find duplicates

vegdup=veggy %>%
  group_by(site,qp,pos,folder) %>%
  filter(n()>1)

View(vegdup)

# vegdup$indexrep_rank=rank(vegdup$indexrep)
# vegdup$repimage= rep(c(1:2),nrow(vegdup)/2)
#

left<-vegdup %>%
  group_by(indexrep) %>%
  summarise(pixels1=head(countgreen,1))
right<-vegdup %>%
  group_by(indexrep) %>%
  summarise(pixels2=tail(countgreen,1))

vegdup.clean=cbind(left,right)

plot(
  vegdup.clean$pixels1, vegdup.clean$pixels2
     )

cor.test(
    log10(vegdup.clean$pixels1+1), log10(vegdup.clean$pixels2+1)
)

cor.test(
    (vegdup.clean$pixels1), (vegdup.clean$pixels2), method='spearman'
)

modtest<-lm(
    log10(vegdup.clean$pixels1+1) ~ log10(vegdup.clean$pixels2+1)
)
t.test.custom(modtest,val=0)
t.test.custom(modtest,val=0.99) # yes it is different from 1
coef(summary(modtest))[2,4]

# r=cor(
#     log10(vegdup.clean$pixels1+1), log10(vegdup.clean$pixels2+1)
# )
# n= length(log10(vegdup.clean$pixels1+1) )
#
# psych::r.test(30,r,0.094)
# psych::r.test(30,r,0.99999)

## Run the test for replicability
library(MCMCglmm)
lmm=MCMCglmm(data=vegdup, countgreen ~1, random = ~ indexrep, family = 'poisson',verbose=F)
print('Variance accumulated by the same pot taken pictures multiple times (replicability)')
print ( h2MCMC(lmm,randomname = 'indexrep') )

