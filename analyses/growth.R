data(veggy)
data(veg)


## First visualze

data(veggy)
head(veggy )
names(veggy)


# Generate all trends
veggy %>% filter(indpop=='i') %>%
ggplot() + geom_line(aes(y=countgreen,x=day,group=potindex, color=factor(id)),alpha=0.1 ) + theme(legend.position="none")

veggy %>% filter(indpop=='p') %>%
ggplot() + geom_line(aes(y=countgreen,x=day,group=potindex, color=factor(id)),alpha=0.1 ) + theme(legend.position="none")


# single trends
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



# check red flags

# Get the ones with red flag
ggplot(veggy) + geom_line(aes(y=countred,x=day,group=potindex, color=factor(id)),alpha=0.1 ) + theme(legend.position="none")
ggplot(veggy) +geom_histogram(aes(x=log10(countred)))

# Check what happens with the up and down of green pixels
filter ( veggyraw , trayid == "27_i_l" ,pos=="b2", site=="madrid") %>%
  select(pathimage) %>%
  # print()
  write.table(file = "../tmpimg/tmplist.txt",row.names=F, col.names=F,quote=F)


# (
# 'madrid     1    a2  1_p_l     1      p     l  9739'
# # 'madrid     1    a3  1_p_l     1      p     l  9910'
# %>% strsplit(split = ' ')
# ) [[1]] -> xvec
# xvec=xvec[xvec!='']
# filter(veggyraw,site==xvec[1],qp==xvec[2],pos==xvec[3],trayid==xvec[4],rep==xvec[5],indpop==xvec[6],water==xvec[7],id==xvec[8])  %>% arrange(day) %>% select(pathimage) %>%
# print()
write.table(file = "../tmpimg/tmplist.txt",row.names=F, col.names=F,quote=F)




# some exploratory outputs

cor.test(veg$firstgreen,veg$germi) # good point
qplot(veg$firstgreen,veg$germi)

qplot(veg$firstgreen,veg$redsum)

hist(log10(newveg$redsum.y))

summary( newveg$redsum.y - newveg$redsum.x )
hist(log10(newveg$redsum.y - newveg$redsum.x))

summary(veg$firstgreen)
hist (fn(veg$firstgreen))

table(is.na(veg$germi))
hist(x=veg$germi)

plot(veg$germi~veg$germip)
plot(veg$germi~veg$germir2)

# quality filter
table(veg$germip >0.05)
table(veg$germir2 <0.2)
table(veg$germir2 <0.2 | veg$germip >0.05)
veg$germi[veg$germir2 <0.2 | veg$germip >0.05]<-NA

library(lme4) ## check just that there is some heritability at least
library(MCMCglmm)

# newveg %>%
  # filter(redsum<1e4) %>%
veg %>%
  # filter(site=='tuebingen',water=='l') %>%
  # select(id,firstgreen,germi) %>%
  # na.omit() %>%
# lmer(formula = firstgreen ~ (1|id)) %>%
# lmer(data = .,formula = firstgreen ~ (1|id)   ) -> lmod
# lmer(data = .,formula = firstgreen ~ (1|id)   ) -> lmod
# MCMCglmm(data = .,fixed = firstgreen ~ 1, random= ~id)    -> lmod
lmer(data = .,formula = firstgreen ~ (1|id) + (1|water)+ (1|site)  ) -> lmod
# randomvariance(lmod)
randomvariance(lmod,var =c('id','water','site') )


names(veg)

library(lme4)
names(veg)

veg %>%
  mutate(
  a= fn(substrRight(ger.a,lastpos = 4,giveright = F)),
  b= fn(substrRight(ger.b,lastpos = 4,giveright = F)),
  c= fn(substrRight(ger.c,lastpos = 4,giveright = F))
  ) %>%
# lmer(formula = a ~ (1|id) + (1|water)+ (1|site)  ) -> lmod
# lmer(formula = b ~ (1|id) + (1|water)+ (1|site)  ) -> lmod
lmer(formula = c ~ (1|id) + (1|water)+ (1|site)  ) -> lmod
# randomvariance(lmod)
randomvariance(lmod,var =c('id','water','site') )


#
# ```{r}
names(veg)
data(veg)
veg=veg %>%
  full_join(.,red,by=c('site','qp','pos','trayid','rep','indpop','water','id'))

veg=veggy %>% group_by(site,qp,pos,trayid,rep,indpop,water,id) %>% summarize(greensum=sum(countgreen)) %>%
  full_join(.,veg,by=c('site','qp','pos','trayid','rep','indpop','water','id'))

library(lme4)

veg %>%
  filter(redsum < 1e4) %>%
  filter(greensum >1e4) %>%
  mutate(
  a= fn(substrRight(ger.a,lastpos = 4,giveright = F)),
  b= fn(substrRight(ger.b,lastpos = 4,giveright = F)),
  c= fn(substrRight(ger.c,lastpos = 4,giveright = F))
  ) %>%
# lmer(formula = a ~ (1|id) + (1|water)+ (1|site) + (1|indpop)  ) -> lmod
# lmer(formula = b ~ (1|id) + (1|water)+ (1|site) + (1|indpop)  ) -> lmod
# lmer(formula = c ~ (1|id) + (1|water)+ (1|site) + (1|indpop)  ) -> lmod
# randomvariance(lmod)
randomvariance(lmod,var =c('id','water','site',"indpop") )

#
# ```
#
