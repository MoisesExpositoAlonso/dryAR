####@ generate the original datasets 

GENERATE_MASTER_DATA_FROMEXPERIMENT <- function(){

# setwd("~/GoogleDrive")
# setwd("/media/moisesexpositoalonso/Data/GoogleDrive")

source("R/MOI_FUNCTIONS.R")

#### from drought experiment raw data

pergeno<-read.csv("~/ebio/abt6_projects9/ath_1001G_image_pheno/experiment_218_drought/new_variables_traject_mean_pergenotype.csv")
genotypes=pergeno$id
pergeno<-pergeno[,-which(colnames(pergeno)%in% c("latitude.x","X","longitude.x","kgroup","exp.r","exp.a","exp.b","lm.r","pol.r","endday","endvalue") )]

#### from drought experiment models effects

#m2
m2<-read.csv("~/ebio/abt6_projects9/ath_1001G_image_pheno/experiment_218_drought/m2_effects_perday_prepared.csv")
m2<-m2[,-1]
head(m2)
colnames(m2)<-c("id",paste("m2",colnames(m2)[-1],sep=""))  

#m1b
m1b_idxday<-read.csv("~/ebio/abt6_projects9/ath_1001G_image_pheno/experiment_218_drought/m1b_effects_idxday_prepared.csv")
m1b_idxday<-m1b_idxday[-c(1:2)]
head(m1b_idxday)

m1b_idxday.df<-reshape2::dcast(m1b_idxday,formula=id ~ day,value.var="idxdayeffect",fun.aggregate=mean)
colnames(m1b_idxday.df)[-1] <-paste("m1b_idxday_",colnames(m1b_idxday.df)[-1],sep="")
head(m1b_idxday.df)

### NOT WORKING, NEED THE PACKAGE RESHPAE2

m1b_id<-read.csv("~/ebio/abt6_projects9/ath_1001G_image_pheno/experiment_218_drought/m1b_effects_id_prepared.csv")
m1b_id<-m1b_id[-1]
colnames(m1b_id)<-c("id","m1b_id")

#m1d
m1d<-read.csv("~/ebio/abt6_projects9/ath_1001G_image_pheno/experiment_218_drought/m1diii_effects_29_parameters_prepared.csv")
m1d<-m1d[-1]
head(m1d)
colnames(m1d)<-c("id","m1d_polint","m1d_polqua","m1d_polslo" )

############ population structure  ####

workingdir="INVESTIGACION/PhD/project_1001G_history/Admixture/762g"
workingdir="~/ebio/abt6_projects8/ath_1001G_history/admixture_miss_qualred"

fileQ="762g_miss01maf0001.11.Q"               
filefam="762g_miss01maf0001.fam"          

knum=11

# read
admix<-read.table(paste(workingdir,"/",fileQ,sep="") )
idorder<-read.table(paste(workingdir,"/../",filefam,sep="") )[,1]

rownames(admix)<-idorder
colnames(admix)<-gsub(x=colnames(admix),pattern="V",replacement="k")
admix$id<-rownames(admix)

############ from rapa ####

rapa<-read.csv("~/ebio/abt6_projects9/ath_1001G_image_pheno/experiment_218_drought/RAPA_Francois_data.csv",sep=";")

rapa$nSiliques=as.numeric(levels(rapa$nSiliques))[rapa$nSiliques]
toremove<-c("idFlat","Pot_position","Pot_row","Pot_column","Flat_position_16LD")
rapa<-rapa[-which(colnames(rapa) %in% toremove)]
colnames(rapa)[which(colnames(rapa)=="accessionid")]<-"id"

rapa_mean<-aggregate(rapa[,2:dim(rapa)[2] ],list(id=rapa$id) , FUN=function(x){mean(x,na.omit=T)} )

########### from genotype list ####

glist<-read.csv("~/ebio/abt6_projects9/ath_1001G_image_pheno/experiment_218_drought/greenhousedrough_218_genotype_list.txt",sep="\t")
glist=glist[, -which(colnames(glist) %in% c("yes","found_patrice","found_mother","comment", "name","country"))] # note that I remove country here

## Careful I need to include the NA from k group in a new category, -9
glist[which(is.na(glist$kgroup)),"kgroup"]<- (0)

########### from bioclim ####


clim<-read.csv("~/ebio/abt6_projects8/ath_1001G_history/1001genvironment/1135_master_table_admix_pop_bioclim.csv")

toremove<-c("latitude","longitude","ecotypeids_merged","name","ecotypeid","res","country")
clim<-clim[-which(colnames(clim) %in% toremove)]
colnames(clim)[which(colnames(clim)=="tg_ecotypeid")]<-"id"

############ merge  ##############

# listanalyses<-listanalyses[1]
listanalyses<-list(pergeno,m2,m1b_id,m1b_idxday.df,m1d)
nameofanalyses<-c("pergeno","m2","m1b_id","m1b_idxday.df","m1d")

listanalyses<-list(pergeno,m2,m1b_id,m1d) ## removed the m1b because could not work the data without reshpae2
nameofanalyses<-c("pergeno","m2","m1b_id","m1d")

for (x in 1:length(listanalyses) ) {

print(nameofanalyses[x])

listomerge<-list(listanalyses[x],admix,glist,clim,rapa_mean)
dataMerge <- data.frame(id=genotypes)
for(ReadInMerge in listomerge){ 
  #   print (ReadInMerge)
#   ReadInMerge <- read.csv(file=f, header=T, na.strings="NULL")
  dataMerge <- merge(dataMerge, ReadInMerge,by="id")
} }
# head(dataMerge)
colnames(dataMerge)

# save(dataMerge,file="{/ebio/abt6_projects9/ath_1001G_image_pheno/experiment_218_droughtgwa/MASTER_DATA.RData")
save(dataMerge,file="~/ebio/abt6_projects9/ath_1001G_image_pheno/experiment_218_droughtgwa/MASTER_DATA_V2.RData")

}

GENERATE_MASTER_DATA_pca<- function(){

###  762 population structure
accpca<-read.table('762analysis/genomepca.eigenvec.762clean.tsv',sep='\t')
head(accpca)

### only list 762
acc<-read.csv(file = "~/ebio/abt6_projects8/ath_1001G_history/p1001_MOTHER_DATABASE_DOWNSAMPLING/1001G_downsample_genetics_762.csv")
master<-read.csv('~/ebio/abt6_projects8/ath_1001G_history/p1001_MOTHER_DATABASE_DOWNSAMPLING/1135_master_table_admix_pop_bioclim.csv',header=T)

accmaster<-merge(acc, by.x="id",master[,c('tg_ecotypeid','alt',paste('bio',c(1:19),sep="_"))],by.y="tg_ecotypeid")

# add phenotypes of interest

# load('MASTER_DATA_pca.RData')
load('MASTER_DATA.RData')

accmaster_mer<-merge(accmaster,by.x='id',dataMerge[,c('id','m1d_polqua')],all.x=T)
dim(accmaster_mer)
head(accmaster_mer)

accmaster<-accmaster_mer
head(accmaster)

accmaster$PC1<-accpca$PC1
accmaster$PC2<-accpca$PC2
accmaster$PC3<-accpca$PC3

write.table(accmaster,file="752_accmaster_env_str_drough.tsv",sep="\t",quote = F,row.names = F)
}

####@ read datasets

selection_profiles_load <- function(path="~/ebio/abt6_projects9/ath_1001G_image_pheno/experiment_218_droughtgwa/selectionscreens/"){
  
fstfile=paste0(path,"218_fst.fst")
pifile=paste0(path,"218_pi.sites.pi")
tajimafile=paste0(path,"218_tajima.Tajima.D_modpersite")
swepfile=paste0(path,"SweeD_grid100000_to800kSNPs")
freqfile=paste0(path,"../218gdrought_freq.frq")

signiname="signi.RObject"
signifile=paste0(path,"signi.RObject")
  
if(signiname %in% list.files(path)){
  message("loading the found signi.RObject")
  load(signifile)
}else{
message("signi.RObject not found ... generating it")


## load signatures
fst<-read.table(fstfile,header=T)
  fst$SNP<-paste(fst$CHR,fst$POS,sep="_")
pi<-read.table(pifile,header=T)
  pi$SNP<-paste(pi$CHROM,pi$POS,sep="_")
tajpersnp<-read.table(tajimafile,header=T)
swepersnp<-read.table(swepfile,header=T)  
swepersnp<-swepersnp[,c("SNP","Alpha","Likelihood")]

# Load frequency to match equal frequency SNPs

freq<-read.table(freqfile,header=T)

#### mergeall
dataset<-list(fst,tajpersnp,pi,swepersnp)
merged<-freq

message("signi.RObject not found ... generating it")
for (i in dataset){
  merged<-merge(merged,i[, c(which( !(names(i) %in% names(merged))) ,which(names(i) =="SNP") ) ],by="SNP")
  
}

#### add last details

merged$fqround<-round(merged$MAF,digits=2)
merged$cumulativepos<-c(1:nrow(merged))

merged<-arrange(merged,"CHR","POS")

#### save

signi<-merged
save(signi,file='signi.RObject')
message("saved signi.RObject")

}# end of if conditions

return(signi)
}

readgwaout<-function(map,out,format='emmax'){
#' @parameter format can be either emmax or gemma
#' @parameter map file used to feed emmax or gemma

print('reading map file...')    
mappy<-read.table(map)

if(format=="emmax"){
print('reading emmax out file...')    
    ps<-read.table(ps)
  res<-cbind(mappy,ps)
  colnames(res)<-c("Chr","nada1","nada2","Pos","nada3","beta","pval")
  res<-res[,-grep("nada",colnames(res))]}
else if(format=="gemma"){
print('reading gemma out file...')    

}

}

parsePS<-function(mappy, ps){

  res<-cbind(mappy,ps)
  colnames(res)<-c("Chr","nada1","nada2","Pos","nada3","beta","pval")
  res<-res[,-grep("nada",colnames(res))]
  return(res)

}

mapSNPs <- function(map="218gdrought.map"){
mappy<-read.table(map)
head(mappy)
dim(mappy)
return(mappy)
}

gwadrought_load <- function(file="polquagwa/m1d_polqua.ps",map){

# ps="polquagwa/m1d_polqua.ps"
# droughtuncorrected="polquagwa/m1d_polqua.csv_nullkinship.ps"

#### read and merge w map
psread<-read.table(file)
psy<-parsePS(map,psread )

#### add important information
psy$cumulativepos<-rownames(psy)
psy$SNP<-paste(psy$Chr,psy$Pos,sep="_")

return(psy)
}

####@ plotting  

corrandplot<-function(dataset=dm,var1name=NULL,var2name=NULL) {
var1=dataset[,var1name]
var2=dataset[,var2name]

  # ct<-cor.test(var1,var2,method='spearman')
  # rho<-ct$estimate
  # pval<-ct$p.value
pdf(height = 6,width = 6,file = paste(var1name,"_-_",var2name,".pdf",sep=""),useDingbats = F)

plot(pch=19,x = var2,y = var1,ylab=var1name,xlab=var2name,col=colors11[dataset$kgroup])
lmod<-lm(var1~var2)
abline(lmod)
pval<-summary(lmod)$coefficients[2,4]
beta<-summary(lmod)$coefficients[2,1]
r2<-summary(lmod)$r.squared

Corner_text <- function(text, location="topright"){
legend(location,legend=text, bty ="n", pch=NA)
}

mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
Corner_text(mylabel,location='topright')
mylabel = bquote(italic(p) == .(format(pval, digits = 3)))
Corner_text(mylabel,location='top')

dev.off()
}

knames=function(){
  knames<-c("NEEurope","EEurope","SESweden","NWSpain","Relicts","SWSweden","Asia","SMediter","CEurope","NESpain","NSweden")
}

colors11=function(){
  colors11=c("#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","gold","#a6cee3")
}

plotand3loessline<-function(x,y1,y2,y3,name='',ylim=NULL){
  plot(  y1~newsigniline$cumulativepos ,xlab="", ylab=name ,xaxt="n",type='n',ylim=ylim)
lines(y=predict(loess(y1~x, span=0.02),newdata=x ), x= x)
lines(y=predict(loess(y2~x, span=0.02),newdata=x ), x= x)
lines(y=predict(loess(y3~x, span=0.02),newdata=x ), x= x)

}

addsignipoints<-function(x,y,cexes=c(1,0.75,0.5)){
points(y~ x , pch=16,cex=cexes[1],col="black")
points(y~ x , pch=16,cex=cexes[2],col="white")
points(y~ x , pch=16,cex=cexes[3],col="red")
}


plotgwa<-function(res,add=F){

  res [ which ( res$pval == min(res$pval,na.rm=T) ) , ]
  # print( res [ which ( res$pval < 8e-6 ) , ] )
  res$pval[res$pval==0]<-1e-10

  if (add==T){
    points(-log10(res$pval),col=rainbow(5)[res$Chr ],pch=19)

  }
  else if (add==F)
    plot(-log10(res$pval),col=rainbow(5)[res$Chr ])

}

qqGWA<-function(pvals,dir=getwd(),name="",save=F){

  observed <- sort(pvals)
  observed[observed==0] <- 1e-10
  lobs <- -(log10(observed))

    expected <- c(1:length(observed))
  lexp <- -(log10(expected / (length(expected)+1)))

  if(save==T){
    pdf(paste("qqplot",name,".pdf",sep=""), width=6, height=6)
  }

  plot(c(0,7), c(0,7), col="red", lwd=3, type="l", xlab="Expected (-logP)", ylab="Observed (-logP)", xlim=c(0,max(lobs)+1), ylim=c(0,max(lobs)+1), las=1, xaxs="i", yaxs="i", bty="l")
  points(lexp, lobs, pch=23, cex=.4, bg="black")
  abline(lm(lobs~lexp ),col="black",lty="dashed")
  title( paste("lambda",round(coefficients(lm(lobs~lexp ))[2],digits=3) ) )

  if(save==T){
    print(paste("qqplot printed to: ",dir))
    dev.off()
  }}

frequencypallete<-function(sequencebins){
  mycol<-gray.colors(length(sequencebins))
  return(mycol)
}

dens_signi_bins<-function(data=signi,namesigni="FST",namefreq="MAF",sequencebins=seq(0.0,0.45,by=0.01),samplesize=150,sampletimes=10){

dback<-density(na.omit(data[,namesigni]))
plot(dback$y/max(dback$y)~dback$x, type='l', xlab=namesigni,ylab='Density')

for (sample in sampletimes){
for (bin in sequencebins){
sub<-subset(signi,signi[,namefreq] >= bin)
subsample<-sub[sample(row.names(sub),size=samplesize),]
dback<-density(na.omit(subsample[,namesigni]))
lines(dback$y/max(dback$y)~dback$x)

# pval<-.test(sub$FST,topsubset$FST)$p.value
# text(y =bin,x=1,labels = pval)
}
}
}

dens_top<-function(data=topsubset,namesigni="FST"){

dtop<-density(na.omit(data[,namesigni]))
lines(dtop$y/max(dtop$y)~dtop$x,col="blue",lwd=3)

}

dens_normal<-function(data=subdata,namesigni="FST"){

dback<-density(na.omit(data[,namesigni]))
plot(dback$y/max(dback$y)~dback$x, type='l', xlab=namesigni,ylab='Density')

}

manhattan<-function(psydata,minpval=2,maxpval=max(-log10(psydata$pval)),colvec=transparent(c('black','darkgrey','black','darkgrey',"black"),alpha=0.5),addplot=F,mypch=16){
  if(addplot==F){
  plot( -log10(psydata$pval)~ psydata$cumulativepos , col=colvec[psydata$Chr] ,pch=mypch,cex=(-log10(psydata$pval)) / max(-log10(psydata$pval)) ,xlab="cumulative position", ylab="-log10(p-value)", ylim=c(minpval,maxpval),xaxt="n" )
  } else{
points( -log10(psydata$pval) ~psydata$cumulativepos , pch=mypch,cex=(-log10(psydata$pval)) / max(-log10(psydata$pval)),col=colvec[psydata$Chr])
  }
}

####@ managing the datasets

matchfrequencydata<-function(mydata=signi_backup,freqname="MAF",interesteddata="SNP",snpstomatch=topsubset$SNP,resolution=2){

toydata<-subset(mydata,mydata$SNP %in% snpstomatch)
toydata$hitfreq<-round(toydata$MAF,digits = resolution)
toydata$thehit=paste("hit_",as.character.factor(toydata$SNP),sep = "")

mydata$hitfreq<-NA
mydata$hitfreq<-round(mydata$MAF,digits = resolution)

merged<-merge(toydata[,c("hitfreq","thehit")],mydata,by="hitfreq",all.x=T)
head(merged)

merged_sample<-tapply(as.character.factor(merged[,interesteddata]),INDEX=merged[,"thehit"],FUN = function(x){sample(x = x,size = 1)})

head(merged_sample)
dim(merged_sample)
merged_sample<-as.character(merged_sample)

sampleddata<-subset(mydata,mydata[,interesteddata] %in% merged_sample)
return(sampleddata)
}

samplegivenprob<-function(mydata=signi,snpstomatch=topsubset$MAF,sizesample=length(snpstomatch),resolution=1/10){

  snpstomatch<-round(snpstomatch * (1/resolution)) / (1/resolution)
  tableprob<-data.frame(table(snpstomatch))
  colnames(tableprob)=c("snpstomatch","probability")

  mydata$hitfreq<-round(mydata[,"MAF"] * (1/resolution)) / (1/resolution)

merged<-merge(mydata,by.x="hitfreq",tableprob,by.y="snpstomatch")

merged_sampled<-sample(merged$SNP, size=sizesample, prob=merged$probability,replace=F)

merged_sampled_subset<-subset(signi,signi$SNP %in% merged_sampled)
return(merged_sampled_subset)
}

applypvalue_given_distribution<-function(dataset,datasubset,variable='pval'){
 pval_rank<- apply(datasubset,1,FUN=function(x){   pvalue_given_distribution(value=as.numeric(x[which(colnames(dataset)==variable)]) , distribution=dataset[,variable]
    )
   })
 pval_rank=as.matrix(pval_rank,ncol=1)
 colnames(pval_rank)=paste(variable,"_rank",sep="")
 datasubset<-cbind(datasubset,pval_rank)
return(datasubset)
}

pvalue_given_distribution<-function(value,distribution){
  # sorted<-sort(distribution)
  pemp<-as.numeric( table(distribution<value)['TRUE'] / length(distribution) )
  if(is.na(pemp)){pemp=0}
return(pemp)
}

renamebetaandpval<-function(dataset=puncorrected,name='puncorrected'){
  colnames(dataset)[which(colnames(dataset)=='beta')] = paste(name,'beta',sep="_")
  colnames(dataset)[which(colnames(dataset)=='pval')] = paste(name,'pval',sep="_")
  colnames(dataset)[which(colnames(dataset)=='pval_rank')] = paste(name,'pval_rank',sep="_")
  return(dataset)
}

####@ annotations 

findSNPname <- function(i,snptable,ann){

  print(i)
  annsub<-subset(ann,ann$V1 == paste("Chr",snptable[i,"chr"],sep='') & ann$V4<snptable[i,"pos"] & ann$V5> snptable[i,"pos"]  )
  annsub_select<-subset(annsub,annsub$V3 %in% c('gene'))
  annsub_select_an<-as.character.factor(annsub_select$V9)
  if(length(annsub_select_an) != 0L){
  genename<-strsplit(annsub_select_an,split = 'Name=')[[1]][2]
  # annsub_collapse<-paste(annsub$V9,collapse = "|")
  # annsub_collapse_1<-gsub(annsub_collapse,pattern = "Name=",replacement = "")
  # annsub_collapse_2<-gsub(annsub_collapse_1,pattern = "ID=",replacement = "")
  # annsub_collapse_3<-gsub(annsub_collapse_2,pattern = "Parent=",replacement = "")
  # strsplit(annsub_collapse_3,split = "|",fix=T)[[1]]
  # grep(pattern = "AT[1-5]G*",x = strsplit( annsub_collapse,split = c(";","=","|") ) )
  # genename<-as.character.factor(annsub$V9[grep("ID=",annsub$V9)] )
  # genename_list<-strsplit(genename,split = ";",fixed = T)[[1]]
  # genename_list_parse<-genename_list[grep("ID=",genename_list)]
  # genename_list_parse_clean<-sub(x = genename_list_parse,pattern = "ID=",replacement = "")
  # newsnptable$genenames[i]<-genename_list_parse
  return(genename)
  }else{
    print('not found')
    return(NA)
    }

}

findgenename<-function(snptable=genelist,ebioroot='~/ebio/') {
  
  if(colnames(snptable)[1] != 'chr' | colnames(snptable)[2] != 'pos')  {
    stop(' please rename your snptable as two columns: chr pos')
        }
  
  message("reading the TAIR annotations file")
  ann<-read.table(paste(ebioroot,'abt6_projects8/ath_1001G_history/TAIR10_GFF3_genes_transposons.gff.txt',sep=''))
  head(ann)
  
  message("looking for genes hit by the SNPs")
  genenames=sapply(1:nrow(snptable), function(i) findSNPname(i,snptable,ann) )
  
  snptable$genenames=genenames
  return(snptable)
}

findgenename_SNPeff<-function(snptable=genelist,ebioroot='~/ebio/') {
  
  #### check for the column names
  if(colnames(snptable)[1] != 'chr' | colnames(snptable)[2] != 'pos')  {
    stop(' please rename your snptable as two columns: chr pos')
        }
  
  #### write tmp file to call the python script
  write.table(file = ".tmp.tsv", sep="\t" , snptable,col.names = F, row.names=F)
  tmpfile= paste0(getwd(), "/.tmp.tsv")
  
  command <- paste0("python ",ebioroot,"abt6_projects9/ath_1001g_foreverybody/minivcftools/findSNPeff_fromlist.py ", tmpfile)
  system(command)
  
  anfile= paste0(".tmp.tsv_genearound")
  anadd=read.table(anfile,header=F)
  return(anadd)
}

.parser_SNPeff <-  function(x){
tmp<- strsplit( strsplit(as.character(x),split = "EFF=")[[1]][2] ,split =  "(" ,fixed = T) [[1]] [1]
return(as.character(tmp))
}

parser_SNPeff <- function(anadd){
  as.character(sapply(anadd[,3], FUN = .parser_SNPeff) )
}

genes_SNPeff <- function(anadd){
  tmp<- sapply(anadd[,3],.genes_SNPeff )
  # print(tmp)
  # tmp2 <- paste0(as.character(tmp), collapse = " | ")
  # print(tmp2)
  # return(tmp2)
  # reduce(tmp,paste(collapse = "|"))
  # return(as.character(tmp))
  sapply(tmp,FUN = function(x) paste(x, collapse = " "))
}
.genes_SNPeff <- function(x){
x=as.character(x)
splitted<- strsplit(x, split = "|",fixed=T)
locations<- grep(pattern = "AT[1-5]G",perl = T, splitted[[1]])
parsed<- splitted[[1]] [locations]
parsed2 <-gsub(parsed,pattern = "\\..",fixed = F, replacement = "") # this replaces a dot (\\.) and any character afterwards(.). The backslash is necessary so it knows it is an actual dot.
parsed2 <- unlist(unique(parsed2))
return(parsed2)
}

loadgoslim<-function(){
# gocat<-read.table('~/ebio/abt6_projects8/ath_1001G_history/GO_Ath/TAIR_GO_slim_categories.txt',fill=T,header=T)
# head(gocat)
print("loading goslim datasets stored at:")
filegoslim<-'~/ebio/abt6_projects8/ath_1001G_history/GO_Ath/ATH_GO_GOSLIM.txt'
print(filegoslim)
goslim<-read.delim(filegoslim,fill=T,header=F)
head(goslim)


goslim$V1 # at gene name
goslim$V2 # locus id
goslim$V3 # at gene name or splicing form
levels(goslim$V4) # preambule
levels(goslim$V5) # word description
levels(goslim$V6) # actual go term
(goslim$V7) # tair keyword
(goslim$V8) #  F=molecular function, C=cellular component, P=biological process
levels(goslim$V9)  # classification description
# (goslim$V10)

# source("https://bioconductor.org/biocLite.R")
# biocLite("GEOsearch")

# https://www.arabidopsis.org/servlets/Search?action=new_search&type=keyword
# https://www.arabidopsis.org/servlets/Search?action=new_search&type=gene
# https://www.arabidopsis.org/servlets/Search?type=general&action=new_search
# return(goslim)
assign("goslim", goslim, envir=globalenv())
}

checkgoslim <- function(){
  if( !("goslim" %in% ls(envir=globalenv()))) {
  message("goslim not found, loading...")
  loadgoslim() 
}else{
  # message("goslim found")
  } 
}

humanname <- function(genes,columngenes="genenames", goodcolumns=c("genenames","chr","pos","V5","V9")){
checkgoslim()

if(is.null(columngenes)){message("you need to provide a data frame for genes, and the column name that you want to merge with the gene names, normally V5 and V9 that are annotations")}

#### merge with goslim annotations
merged<-merge(genes,by.x=columngenes,goslim,by.y="V1")

#### get important column and remove duplicates
namesan.unique=unique(merged[,goodcolumns])

#### summarize the 

go_broad <- as.matrix(tapply(namesan.unique$V9,namesan.unique$genenames,FUN = function(x){paste(unique(x),collapse = " | ")}))
go_broad <- data.frame(genenames=rownames(go_broad), go_broad=go_broad)
go_narrow <-as.matrix(tapply(namesan.unique$V5,namesan.unique$genenames,FUN = function(x){paste(unique(x),collapse = " | ")})) 
go_narrow<- data.frame(genenames=rownames(go_narrow), go_narrow=go_narrow)

newgenelist_go <-left_join(newgenelist, go_broad,by=c("genenames")) %>% left_join (.,go_narrow,by=c("genenames"))

print("the resulting table has the next structure")
print(str(newgenelist_go))

return(newgenelist_go)

}

humanname_specific <- function(genes,columngenes="genenames"){

library(org.At.tair.db)
library(DBI)

geneinfolist<-select(org.At.tair.db, keys=na.omit(genes[,columngenes]),columns=c("SYMBOL","GENENAME"))
geneinfolist <- unique(geneinfolist)


geneinfolist_tmp<-as.matrix(tapply(geneinfolist$SYMBOL,geneinfolist$TAIR,FUN = function(x){paste(unique(x),collapse = " | ")}))
geneinfolist_wordy_tmp<-as.matrix(tapply(geneinfolist$GENENAME,geneinfolist$TAIR,FUN = function(x){paste(unique(x),collapse = " | ")}))

newgeneinfolist<-data.frame(TAIR=row.names(geneinfolist_tmp),SYMBOL=geneinfolist_tmp[,1],DESCRIP=geneinfolist_wordy_tmp)

print("the resulting table has the next structure")
print(str(newgeneinfolist))

return(newgeneinfolist)

}

grepgenes<-function(genes=myhits,togrep="port",columngo="V9",type="union",wannaprint=F,columngenes="genenames"){
checkgoslim()
  
merged<-merge(genes,by.x=columngenes,goslim,by.y="V1")
head(merged)
dim(merged)

if(type=="union"){
found<-unique(unlist(sapply(togrep,FUN=function(x) grep(x, merged$V5))))

}else if(type=="intersection"){
found1<-unlist(sapply(togrep,FUN=function(x) grep(x, merged$V5)))
found<-found1[duplicated(found1)]

}else{ print ("need to provide intersection or union in type flag!")}

# message("returned all SNPs merged and those grepped by the keyword")
found<-merged[found, ] 
return(list(merged=merged,found=found))
# return(searched)
}

count_grep<-function(genes=myhits,togrep="port",columngo="V9",type="union",wannaprint=F, columngenes="genenames",searched=NULL){

if(is.null(searched)){
searched=grepgenes(genes=genes,togrep=togrep,columngo=columngo,type=type,wannaprint=wannaprint,columngenes=columngenes)
# searched<-merged$V5[found ] 
}

annot=searched$found[,columngo]


total_genes<-length(na.omit(unique(genes[,columngenes])))
total_merged <- nrow(searched$merged)
total_unique_merged <- length(unique(searched$merged[,1]))

wordhits<-length(annot)
uniquegeneword <- length(unique(searched$found[,1]))

results<-list( unique_word_genes= uniquegeneword,
                    all_word_found=wordhits,
                    unique_merges= total_unique_merged,
                    all_merges=total_merged,
                    all_genes_provided= total_genes
                    )

if(wannaprint==T){print(results) }

return(results)
}


plotcounts<-function(test,neutral,name,...){
require(cowplot)
require(ggplot2)
p<-ggplot(data.frame(hits=neutral))+geom_histogram(aes(x = hits),fill="black",color="white",...) + ggtitle(name)+ geom_vline( aes(xintercept= test),color="red" )
print(p)
}

genepoisson <- function(test,neutral){
  
print(paste("average # found in neutral sets: ",mean(neutral)) )
print(paste("total # genes in my hits: ",test) )
print (paste("the p-value under Poisson:", ppois(test,lambda = mean(neutral),lower.tail = F) ) )

}

genetstudent <- function(test,totaltest,neutral,totalneutral){
  
print(paste("average proportion in neutral sets: ", mean(neutral/totalneutral)) )
print(paste("total # genes in my hits: ",mean(test/totaltest)) )
print (paste("the p-value under t distribution:", t.test(neutral/totalneutral, mu= test/totaltest,alternative="less" )$p.val ) )

}


neutral_go<-function(times=10,togrep="port",size=30,sizem=size*2,category=NULL,type="union",columngo ="V9"){
print(paste("generating empirical distribution of",times, "random draws...") )
  
  thecounts<- sapply(1:times, FUN = function(x){
    
    # message(dim(goslim))
    # message(size)
    
    #### first sample a bit higher in case some are not found
    # samplenames<-sample(as.character.factor(goslim[,'V1']), size=sizem,replace = F)
    samplenames<-sample(as.character.factor(goslim[,'V1']), size=size,replace = F)
    
    # searched <- grepgenes(genes=data.frame(genenames=samplenames),togrep = togrep,type = type,columngo = columngo )
    # searched$merged$genenames <- as.character(searched$merged$genenames)
    # searched$found$genenames <- as.character(searched$found$genenames)
    # 
    # unique(searched$merged$genenames)
    # unique(searched$found$genenames)
    # 
    # #### secondly subsample. 
    # subsample<- sample(searched$merged$genenames, size = size)
    # 
    # searched$merged <-filter(searched$merged, genenames %in% subsample)
    # searched$found <- filter(searched$found, genenames %in% subsample)
    
      
    found<-count_grep(genes = data.frame(genenames=samplenames),wannaprint = F )#,searched = searched)
    
    if(!is.null(category)){counted<-found[[category]]}
    else{counted<-found}
    
    
    return(as.numeric(counted))
  }
  )
  
return(thecounts)
}



wrap_annotation_test<-function(genes,togrep,type="union",columngo="V5",wannaprint=T,times=1000){

message("searching for type: ",type)
message("of the keywords: ", paste(togrep,collapse= " "))

searched <- grepgenes(genes = genes,columngo = columngo,togrep = togrep,wannaprint = T,type = type)
# unique(as.matrix(searched$found$V5))
message(unique(as.matrix(searched$found$V5)))

message("THE FOCAL GENE SET COUNT")
counted<- count_grep(genes = genes,wannaprint = T,columngo = columngo,searched = searched,columngenes = "genenames")
coun=as.numeric(counted)

message("THE NEUTRAL GENE SET COUNT")
neut <- neutral_go(times = times,togrep = togrep,columngo = columngo,  size = counted$all_genes_provided,  type = type)
message(paste("average size of random sample:", mean(neut[3,])))

genepoisson(coun[1],neutral = neut[1,] )
genetstudent(coun[1],coun[3],neut[1,],neut[3,] )

return(searched)
}
