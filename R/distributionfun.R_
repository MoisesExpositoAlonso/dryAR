
message("loading distribution_model_functions.R ...")
set.seed(1)

####@ special operators

`%%` <- function(e1, e2) e1 %>% e2


####@ diversity functions

nucdiv<-function(alldifferences,numsites=4004754,totcompar=NULL){
  if(is.null(totcompar)) {
  totcompar=combn(length(alldifferences),m = 2)
  return(sum(alldifferences/(numsites*totcompar)))
  }else {
     return(sum(alldifferences/(numsites*totcompar)))
  }
}

whichclosest<-function(vectordist,numberneighbors=10,k=NULL){
  distthreshold<-sort(vectordist)[numberneighbors+1]
  who<-as.numeric(which(vectordist <distthreshold & vectordist >0))
 if(!is.null(k) & length(who)<k){
 who<-as.numeric(names(sort(vectordist))[2:numberneighbors+1])

 }
   return(who)
}

neighbordiversity<-function(hammingm=ham,geographicm=geodist,numberneighbors=10, k=2){
  newdiv<-c()
  for (i in 1:dim(geographicm) [1]){
    who<-whichclosest(geographicm[i,],numberneighbors = numberneighbors,k=k)

    # print(who)
        if (k==2){
        nearbydistance<-ham[i,who]
        newdiv[i] <- nucdiv(alldifferences = sample(nearbydistance,size=1), totcompar = 1,numsites = 119e6) # be careful with more than 2 you have to find the diversity of all comparisons, not only from a focal genome
        }else if (k>2){
          samplewho<-sample(who,size=k,replace = F)
        nearbydistance<-ham[  samplewho,  samplewho]
        nearbydistance_compar<-nearbydistance[lower.tri(nearbydistance)]
        totcompar=length(nearbydistance_compar)
        newdiv[i] <- nucdiv(alldifferences = nearbydistance_compar, totcompar = totcompar,numsites = 119e6)
        }
  }
  hist(newdiv,col="black",border="white",main = paste("nei=",numberneighbors,"|","k=",k),breaks=20)
  return(newdiv)
}


div_perpop<-function(acclist,bigmatrix){

submat=bigmatrix[as.character(acclist) ,as.character(acclist)]

nucdiv(submat[lower.tri(submat)==T],totcompar = length(submat[lower.tri(submat)==T]) , numsites = 119e6)

}

####@ random forest functions

forcecombine_randomforest <- function (...) {
  require("randomForest")
    pad0 <- function(x, len) c(x, rep(0, len - length(x)))
    padm0 <- function(x, len) rbind(x, matrix(0, nrow = len -
        nrow(x), ncol = ncol(x)))
    rflist <- list(...)
    areForest <- sapply(rflist, function(x) inherits(x, "randomForest"))
    if (any(!areForest))
        stop("Argument must be a list of randomForest objects")
    rf <- rflist[[1]]
    classRF <- rf$type == "classification"
    trees <- sapply(rflist, function(x) x$ntree)
    ntree <- sum(trees)
    rf$ntree <- ntree
    nforest <- length(rflist)
    haveTest <- !any(sapply(rflist, function(x) is.null(x$test)))
    vlist <- lapply(rflist, function(x) rownames(importance(x)))
    numvars <- sapply(vlist, length)
    if (!all(numvars[1] == numvars[-1]))
        stop("Unequal number of predictor variables in the randomForest objects.")
    for (i in seq_along(vlist)) {
        if (!all(vlist[[i]] == vlist[[1]]))
            stop("Predictor variables are different in the randomForest objects.")
    }
    haveForest <- sapply(rflist, function(x) !is.null(x$forest))
    if (all(haveForest)) {
        nrnodes <- max(sapply(rflist, function(x) x$forest$nrnodes))
        rf$forest$nrnodes <- nrnodes
        rf$forest$ndbigtree <- unlist(sapply(rflist, function(x) x$forest$ndbigtree))
        rf$forest$nodestatus <- do.call("cbind", lapply(rflist,
            function(x) padm0(x$forest$nodestatus, nrnodes)))
        rf$forest$bestvar <- do.call("cbind", lapply(rflist,
            function(x) padm0(x$forest$bestvar, nrnodes)))
        rf$forest$xbestsplit <- do.call("cbind", lapply(rflist,
            function(x) padm0(x$forest$xbestsplit, nrnodes)))
        rf$forest$nodepred <- do.call("cbind", lapply(rflist,
            function(x) padm0(x$forest$nodepred, nrnodes)))
        tree.dim <- dim(rf$forest$treemap)
        if (classRF) {
            rf$forest$treemap <- array(unlist(lapply(rflist,
                function(x) apply(x$forest$treemap, 2:3, pad0,
                  nrnodes))), c(nrnodes, 2, ntree))
        }
        else {
            rf$forest$leftDaughter <- do.call("cbind", lapply(rflist,
                function(x) padm0(x$forest$leftDaughter, nrnodes)))
            rf$forest$rightDaughter <- do.call("cbind", lapply(rflist,
                function(x) padm0(x$forest$rightDaughter, nrnodes)))
        }
        rf$forest$ntree <- ntree
        if (classRF)
            rf$forest$cutoff <- rflist[[1]]$forest$cutoff
    }
    else {
        rf$forest <- NULL
    }
    #
    #Tons of stuff removed here...
    #
    if (classRF) {
        rf$confusion <- NULL
        rf$err.rate <- NULL
        if (haveTest) {
            rf$test$confusion <- NULL
            rf$err.rate <- NULL
        }
    }
    else {
        rf$mse <- rf$rsq <- NULL
        if (haveTest)
            rf$test$mse <- rf$test$rsq <- NULL
    }
    rf
}

randomforestk <- function(data,response, predictors,k=5,ntree=500,type="regression"){
print(paste("calculating random forest of ...",response))

makeformula

# Generate group
group <- dismo::kfold(data, k=k)

# Run for each K group
rfmodels<-lapply(1:k,FUN =function(l){
  train <- data[group != l,]
      if(type=="classification"){
          myformula<-makeformula(paste("factor ( ",response,")") ,predictors)
      }
      else if(type=="regression"){
          myformula<-makeformula(paste("as.numeric ( ",response,")") ,predictors)
      }
  fit <- randomForest(myformula, data=data,ntree=ntree,na.action=na.omit,keep.inbag = TRUE,importance=TRUE) # removed proximity
  return(fit)}
)

rfall<-randomForest::combine(rfmodels[[1]],rfmodels[[2]],rfmodels[[3]],rfmodels[[4]],rfmodels[[5]])

}

run_randomforest<-function(mydata,cross=5,seed=1, myfilename="",phenoname,type="regression",sink=T){

  # This function computes a random forest from a training dataset. To have replicable data we set seed at 1. Use something else if you want
  # If cross=0, then no cross validation is done.
set.seed(seed)
require(randomForest)
require(reshape)

group <- dismo::kfold(mydata, k=cross)
print(paste("calculating random forest of ...",phenoname))

rfmodels<-lapply(1:cross,FUN =
function(l){
  # train,mydata,mydata
  train <- mydata[group != l,]
  if(type=="classification"){
    myformula<-formula(paste("factor(", phenoname,") ~ ."))
  } else if(type=="regression"){ # so regression
      #myformula<-formula(paste(phenoname," ~ ."))
      myformula<-formula(paste('as.numeric(',phenoname,") ~ .")) # problems running regression with alleles, likely because they are characters 0 1 NA. need to put as.numeric
      } else{ print("need to provide a valid type of random forest: regression or classification")}
  fit <- randomForest(myformula, data=mydata,ntree=500,na.action=na.omit,keep.inbag = TRUE,importance=TRUE) # removed proximity
return(fit)
} )


print("join random forest cross validations...")
# merge the cross validated random forests
# pasmodel<-paste0("rfmodels [[",c(1:cross),"]]", collapse = ",")
# rfall<-evalparse(paste("forcecombine_randomforest(",pasmodel,")") )
# rfall<-evalparse(paste("combine(",pasmodel,")") )
rfall<-randomForest::combine(rfmodels[[1]],rfmodels[[2]],rfmodels[[3]],rfmodels[[4]],rfmodels[[5]])


### evaluate the prediction of rf regression predicted and observed.
# ypredict<-randomForest::predict(object = rfall,newdata=mydata)
ypredict<-predict(object = rfall,newdata=mydata)


if(type=="classification"){
print("as binary classification ...")
myr2= table(ypredict == mydata[,phenoname])["TRUE"] / sum(table(ypredict == mydata[,phenoname]))
print(paste("predictive power, proportion of right predictions=",myr2))
}else{
print ("assumed regression, continuous variable, change type flag to type classification if your response variable is discrete")
myr2<-summary(lm(ypredict~mydata[,phenoname]))$r.squared
print(paste("predictive power, R2=",myr2))
}

rfall$custom.err.rate<-myr2 # Important step, to store my computed r2

if(sink==T){
sortedimportance<-data.frame(importance(rfall) )
sortedimportance$r2<-myr2
sortedimportance$var<-row.names(sortedimportance)
# toreturn<-list(rfobject=rfall,importance=sortedimportance,myr2)
write.tsv(sortedimportance,file = paste0("tables/",phenoname,"_",myfilename,"randomforest.tsv"))
# print(sortedimportance)
}

return(rfall)
}

run_randomforest_withtraining<-function(mydata,cross=5,seed=1, myfilename="",phenoname,type="regression",sink=T){

  # This function computes a random forest from a training dataset. To have replicable data we set seed at 1. Use something else if you want
  # If cross=0, then no cross validation is done.
set.seed(seed)
require(randomForest)
require(reshape)

group <- dismo::kfold(mydata, k=cross)
print(paste("calculating random forest of ...",phenoname))

rfmodels<-lapply(1:cross,FUN =
function(l){
  # train,mydata,mydata
  train <- mydata[group != l,]
  if(type=="classification"){
    myformula<-formula(paste("factor(", phenoname,") ~ ."))
  } else if(type=="regression"){ # so regression
      #myformula<-formula(paste(phenoname," ~ ."))
      myformula<-formula(paste('as.numeric(',phenoname,") ~ .")) # problems running regression with alleles, likely because they are characters 0 1 NA. need to put as.numeric
      } else{ print("need to provide a valid type of random forest: regression or classification")}
  fit <- randomForest(myformula, data=mydata,ntree=500,na.action=na.omit,keep.inbag = TRUE,importance=TRUE) # removed proximity
return(fit)
} )


print("join random forest cross validations...")
# merge the cross validated random forests
# pasmodel<-paste0("rfmodels [[",c(1:cross),"]]", collapse = ",")
# rfall<-evalparse(paste("forcecombine_randomforest(",pasmodel,")") )
# rfall<-evalparse(paste("combine(",pasmodel,")") )
rfall<-combine(rfmodels[[1]],rfmodels[[2]],rfmodels[[3]],rfmodels[[4]],rfmodels[[5]])

### evaluate the prediction of rf regression predicted and observed.
ypredict<-predict(object = rfall,newdata=mydata)


if(type=="classification"){
print("as binary classification ...")
myr2= table(ypredict == mydata[,phenoname])["TRUE"] / sum(table(ypredict == mydata[,phenoname]))
print(paste("predictive power, proportion of right predictions=",myr2))
}else{
print ("assumed regression, continuous variable, change type flag to type classification if your response variable is discrete")
myr2<-summary(lm(ypredict~mydata[,phenoname]))$r.squared
print(paste("predictive power, R2=",myr2))
}


if(sink==T){
sortedimportance<-data.frame(importance(rfall) )
sortedimportance$r2<-myr2
sortedimportance$var<-row.names(sortedimportance)
# toreturn<-list(rfobject=rfall,importance=sortedimportance,myr2)
write.tsv(sortedimportance,file = paste0("tables/",phenoname,"_",myfilename,"randomforest.tsv"))
# print(sortedimportance)
}

return(rfall)
}

report_allelemod <- function(modstack, var="combined" ){

  rep= sapply(names(modstack),FUN = function(i){
         tmp=modstack[[i]]$importance[,'MeanDecreaseAccuracy']
          mostimportant=names(which(tmp==max(tmp)))
          accu=modstack[[i]]$custom.err.rate %>% format(digits=3)
          combined=paste(accu,mostimportant,sep='_')

          if(var=="r2"){accu}
          else if(var=='importance'){mostimportant}
          else{combined}

  })
return(rep)
}

### get phenotype data

getmydata<-function(data,phenoname,PCA=F, id=T, altitude=F, geo=T, bio=T){

mycolumns=phenoname

if(PCA==T){ mycolumns=c(mycolumns,"PC1","PC2","PC3")  }
if(geo==T){ mycolumns=c(mycolumns,"latitude","longitude")  }
if(bio==T){ mycolumns=c(mycolumns, paste("bio",c(1:19),sep=""))}
if(id==T){ mycolumns= c("id",mycolumns)}


mydata <- data[, mycolumns]
colnames(mydata)<-mycolumns

# print(mycolumns)

return(mydata)
}

envir_predict<-function(model,EuropeClim,type="rf") {
  if(type=="rf"){
   predict(model,EuropeClim)

  }
  return(predict)
}

get_randomforest_prediction<-function(accmaster,divvar,EuropeClim=EuropeClim,skipplot=T,PCA=F,name="",vecol=c("blue3","blue3","red2","red2"), type ){

message(paste("working in variable:", divvar))

mydata<-getmydata(accmaster,phenoname = divvar,PCA=PCA)

mod_forest<-run_randomforest(mydata=na.omit(mydata)[,-c(1,3,4,5)],phenoname = divvar,type = type)

prf <- predict(EuropeClim, mod_forest)

return(prf)

if(skipplot!=T){
envir_plot(prediction = prf,phenoname = divvar,name = name,vecol = vecol,rangecol = c(0,1) ) #diversity
}

}

read_accmaster_plus<-function(divvar="T_repro",dosubset=T){
accmaster<-read.table("../762_accmaster_env_str_drough.tsv",sep="\t",header=T)
load('../MASTER_DATA_pca.RData')

accmaster_mer<-merge(accmaster,by.x='id',dataMerge[,c('id',divvar)],all.x=T)
head(accmaster_mer)


if(dosubset==T){
accmaster_mer<-subset(accmaster_mer, accmaster_mer$longitude > -50 & accmaster_mer$longitude < 100 )
}
return(accmaster_mer)
}

subset_accmastergeo=function(accmaster){
  subset(accmaster, accmaster$longitude > -50 & accmaster$longitude < 100 )

}


## read and manipulate SNP stuff

generate_SNP_matrix<-function(thefile){
snpsubset<-read.table(thefile,fill=T,header=F)
snpsubset[1:10,1:10]
dim(snpsubset)

snpsubset[,4:dim(snpsubset)[2]]<-apply(snpsubset[,4:dim(snpsubset)[2]], c(2),FUN =
function(x) {
  sapply(x, FUN = function(x) substr(x ,start = 1,stop = 1))

  }
)

snpsubset[1:10,1:10]
colnames(snpsubset)[1:3]<-c("chr","pos","ref")

SNPnames<-paste0("chr",snpsubset$chr,"_",snpsubset$pos)

transposed<-rbind( SNPnames,t(snpsubset[,4:dim(snpsubset)[2]]))
transposed<-t(snpsubset[,4:dim(snpsubset)[2]])
colnames(transposed)=SNPnames
transposed[1:10,1:10]

header<-read.table("../subsetvcf/SNPsubset.header")  ## THIS IS DANGEROUS TO PRODUCE BUGS
head(header)
header_parsed<-as.character(as.matrix(header))
header_parsed<-sapply(header_parsed,FUN =
function(x) as.character(strsplit(x ,split = "_")[[1]][1])
  )
# probably need to transform this into columns
header_parsed<-data.frame(id=as.matrix(header_parsed))
gm<-cbind(as.matrix(header_parsed),transposed)

gm[1:10,1:10]

return(gm)
}

read_alleles<-function(name="add",  file=if(name=="genome"){'../subsetvcf/genome/tapbiggenome.tab'}else if(name=="add" | name=="gene"){"../subsetvcf/SNPsubset.tab"},verbose=F){

# read master table
accmaster<-read_accmaster_plus()

# get alleles and merge
gm<-generate_SNP_matrix(file)
gm[1:10,1:10]

head(gm)

accmaster_add<-merge(accmaster,gm,by="id")
head(accmaster_add)
dim(accmaster_add)

accmaster_add
accmaster_add_backup<-accmaster_add

accmaster_add[,grep("chr",names(accmaster_add)) ] <-sapply(names(accmaster_add)[grep("chr",names(accmaster_add))] ,FUN=function(x){

  mycolumn<-as.character.factor(accmaster_add[,x])
  mycolumn[mycolumn=="."]<-NA
  mysnps<-c(na.omit(unique(mycolumn)))

  printverbose(x,verbose)
  printverbose(mysnps,verbose)

  if(length(mysnps) == 1){
    newcolumn<-rep("Invariant",length(mycolumn))
    printverbose("Invariant SNP in this set!",verbose)
  }else{
  mymeans<-tapply(accmaster_add[,"bio18"],mycolumn,function(x)mean(x,na.rm=T)) #[mysnps]
  mymeans
  mytranslation<-c(2,3)
  names(mytranslation)<-mysnps
  if(any(is.na(mymeans))){
    newcolumn<-mytranslation[mycolumn]

  } else{

  if(mymeans[mysnps[1]] < mymeans[mysnps[2]]) { mytranslation[1]<-1;mytranslation[2]<-0  }else if (mymeans[mysnps[1]] >     mymeans[mysnps[2]]) {mytranslation[1]<-0;mytranslation[2]<-1  }else{print("there was an error orienting alleles")}

  newcolumn<-mytranslation[mycolumn]
  }
  }

return(newcolumn)  }
)

# names(accmaster_add)[grep("chr",names(accmaster_add))]
filtered_snps<-sapply(colnames(accmaster_add),FUN = function(x){ if("Invariant" %in% accmaster_add[,x]){}else{return(x)}  })
accmaster_add<-accmaster_add[,unlist(filtered_snps)]

if( any((dim(accmaster_add_backup) != dim(accmaster_add))==F  )) {"Filtered for invariant sites!"}

apply(accmaster_add,2,function(x) unique(x))


save(accmaster_add, file=paste0("accmaster_",name,".RDATA"))

return(accmaster_add)
}

getindex<-function(whatsnps,wheresnps,totlength=length(wheresnps)){
if(whatsnps=="gwa"){ return(1:151)}
if(whatsnps=="agwa"){return(152:totlength)}
if(whatsnps=="genome"){return(1:wheresnps)}
if(whatsnps=="all"){return(1:wheresnps)}
}

count_alleles <- function(accmaster_add,SNPs,allelecode=1){
  wheresnps<-grep("chr", colnames(accmaster_add))
  accmaster_add$countdrought=apply(accmaster_add[,getindex(whatsnps = SNPs,wheresnps = wheresnps)],1,FUN = function(x) table( x== allelecode)["TRUE"] )
return(accmaster_add)
}

read_kinshipemmax=function(file){

K<-read.table(file,fill=T,header=F)
K[1:15,1:15]
dim(K)

save(K,file=paste0(file,"kinship.RData"))
return(K)
}

h2MCMC=function(mcmcglmmobject, randomname){

allvariance=apply(mcmcglmmobject$VCV, 1, sum)
posterior<-mcmcglmmobject$VCV[,randomname]/ allvariance

HPDinterval(posterior,0.95)
posterior.mode(posterior)

}

H.diversity.bi <- function(f){
  2*(f * (1-f))
}


## load and manipulate bioclim

createmapbase<-function(mapcol="seashell3",backcol="white",countrycol=mapcol,xlim=c(-200,+200),ylim=c(-60,+80)){
library(ggplot2)
library(cshapes)
library("gpclib")
# install.packages("gpclib")
gpclibPermit() # there is a problem in one package if you don't allow this
world <- cshp(date=as.Date("2008-1-1"))
world.points <- fortify(world, region='COWCODE')

xticks=round( seq(xlim[1],xlim[2],length.out = 10 ),digits=0)
yticks=round( seq(ylim[1],ylim[2],length.out = 10 ),digits=0)

p1<-ggplot()
p1<-p1+ geom_polygon(data=world.points, aes(long,lat,group=group) ,fill=mapcol,col=countrycol)
p1<-p1+   coord_cartesian(xlim = xlim,ylim=ylim)
p1<-p1+ xlab("")+ylab("")
p1<-p1+ scale_x_discrete(breaks=xticks, labels=paste(xticks,"E",sep=" ") )
p1<-p1+ scale_y_discrete(breaks=yticks ,labels= paste(yticks ,"N",sep=" ") )
# p1<-p1+ theme(panel.grid.major = element_line(colour = "white",size=0,linetype="dashed"))
# p1<-p1+ theme(panel.grid.minor = element_line(colour = "white",size=0,linetype="dashed"))
# p1<-p1+ theme(panel.border=element_rect(colour="black",fill=NA))
p1 <- p1+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = backcol, colour = 'white'),
                panel.border=element_rect(colour="black",fill=NA),legend.title = element_blank(),legend.background=element_blank(),legend.key = element_blank())
p1

return(p1)
}

load_rbioclim<-function(xlim =  c(-10.5,+ 53),ylim=c(32,65),layers=names(bioclim),bioclimvars=c(1:19) ) {
library(raster)

load("~/rbioclim/bioclim.RDATA")
# bioclim
# names(bioclim[1][[1]])

EuropeClim_all<-sapply(layers,FUN = function(x)
  cropenvironment(mybioclim = bioclim[[x]][[bioclimvars]],xlim,ylim,replace = T)
  )

}

read_bioclimlayers <- function(limitlayers=F){
  layernames<- as.character(as.matrix(read.table("~/rbioclim/bioclimlayers.txt")))
  if(limitlayers==T){ layernames=c("bio_2-5m_bil","cc26bi70","cc85bi70","cclgmbi_2-5m") }
  return(layernames)
}

define_tundra_icesheet<-function(){
  ##### GENERATE LAST GLACIAL MAXIMA REGIONS W PLANTS
# get a dummy variable of the last ice sheet
# icesheet<- EuropeClim_all[mylayers[2]][[1]] [["bio1"]] <50
icesheet<- EuropeClim_all[mylayers[2]][[1]] [["bio1"]] <0
# plot(icesheet)
tundra<- EuropeClim_all[mylayers[2]][[1]] [["bio1"]] <50
# plot(icesheet)

lgmsum<-icesheet+tundra
lgmsum[lgmsum==0]<-NA


save(lgmsum,file="lgmsum.RDATA")
save(icesheet,file="icesheet.RDATA")

pdf("icesheet.pdf",useDingbats=F,7,7)
plot(icesheet,col=c("white","grey"),at=c(1,2))
dev.off()

pdf("tundrasheet.pdf",useDingbats=F,7,7)
plot(lgmsum,col=c("NA","grey","grey95"))
dev.off()
}

intersect_climate<-function(master,EuropeClim_all){

 # add the climate
toadd3<- lapply(names(EuropeClim_all)[c(1,10,3,9)], FUN=function(x){
  mytmp2<-sapply( names(EuropeClim_all[[x]] ) , FUN= function(y) {
  mytmp<-data.frame( extract(EuropeClim_all[[x]][[y]],master[,c("longitude","latitude")] ) )
  # names(mytmp)<-paste0(x,".",y)
  names(mytmp)<-paste0(x)
  return(mytmp)} )
  mytmp2<- do.call(cbind,mytmp2)
  # print(head(mytmp2,n=3)[,1:5])
  return(mytmp2)
  } )
toadd3<-do.call(cbind, toadd3)
dim(toadd3)
head(toadd3,n=3)

return(cbind(master,toadd3))
}

make_georaster<-function(motherraster){
d.lat <- rasterToPoints(motherraster)
d.lat[,3] =d.lat[,2]
colnames(d.lat)[3] ="latitude"

d.lon <- rasterToPoints(motherraster)
d.lon[,3] =d.lon[,2]
colnames(d.lon)[3] ="latitude"

r.lat=rasterFromXYZ(d.lat)
r.lon=rasterFromXYZ(d.lon)

projection(r.lat)=projection(motherraster)
projection(r.lon)=projection(motherraster)

return(stack(list(latitude=r.lat,longitude=r.lon)))
}

morelayers_2_bioclim<-function(bioclim,newlayerslist){

CLIMPC<-stack(bioclim,newlayerslist)
names(CLIMPC)<-c(names(bioclim),names(newlayerslist) )

return(CLIMPC)
}

bio<-function(layer='now',bioclim=c(1:19),values=F){

  translation<-c("bio_2-5m_bil" ,"cclgmbi_2-5m", "ccmidbi_2-5m" ,
                 "cc26bi50"   ,  "cc26bi70"  ,
                 "cc45bi50"   ,  "cc45bi70"  ,
                 "cc60bi50"   ,  "cc60bi70"  ,
                 "cc85bi50"  ,   "cc85bi70"    )
  names(translation)<-c('now','lgm','mid',
           '2050l','2070l',
           '2050lm','2070lm',
           '2050hm','2070hm',
           '2050l','2070h')

  extracted<-EuropeClim_all[[translation[layer]]] [[bioclim]]
  if(values==F){  return(extracted)} else{return(values(extracted))}
}

rasdif<-function(layer1,layer2,values=F){
  differ<-layer1-layer2

  if(values==F){return(differ)}else{return(values(differ))}
}

cropenvironment <- function(mybioclim,xlim=c(-10.5,+ 53),ylim=c(32,65),replace=F , addPCA=F)  {
# xlim=c(-10.5,+ 35),ylim=c(34,65)
Range=extent(c(xlim,ylim))
EuropeClim = crop(mybioclim, Range)
if(addPCA==T){
  EuropeClim<-morelayers_2_bioclim(bioclim = EuropeClim,newlayerslist = stack(list(PC1=PC1,PC2=PC2,PC3=PC3)) )

}

return(EuropeClim)
}

maskif <- function(myraster){
load("datas/reference_raster.RDATA")

myraster[is.na(ref)] <- NA
return(myraster)
}

maskpresence <- function(myraster,code=NA){
# load("datas/reference_raster.RDATA")
load("datas/density_raster.RDATA")
ara=aradensity
ara[ara <= 0]<-NA
myraster[is.na(ara)] <-  code
return(myraster)
}

makeglacial <- function(col=c("grey","grey90")){
if(!"lgmsum" %in% ls(envir = .GlobalEnv) ){ load("datas/lgmsum.RDATA") }

envirplot(lgmsum,vecol =col,plotlegend = F,add=T,discrete = T)

}

makebackground <- function(col="lightgrey"){
if(!"rf" %in% ls(envir = .GlobalEnv)){ load("datas/reference_raster.RDATA") }

envirplot(ref,vecol = col,plotlegend = F)

}

getdensityraster <- function(longitude,latitude,refraster=NULL,dilute=NULL){
  if(is.null(refraster)){
    world<- rbioclim::getData(name = "worldclim",var='bio', res=2.5)
    world[!is.na(world)]=1
  }
  if(is.null(dilute)){
  world<-aggregate(x = world,fact=dilute)
  }

coords = cbind(longitude,latitude)
sp = SpatialPoints(coords)

x <- rasterize(sp, world, fun='count')

return(x)
}

# raster to get periods of dryness
getlongestdryperiod<-function(rasterst,thres=16){
# get the grid below threshold
ep<-rasterst < thres

# gode readable 1/0
epval<-values(ep)
epval[is.na(epval)]<- (-9)
epval[epval==T]<-1
epval[epval==F]<-0

# get the longest period without water
epval2<-apply(epval,1,FUN=function(x) if(all(x ==-9)){NA}else{ findlongest(x) } )
summary(epval2)

# fill in values
ep2<-ep[[1]]
values(ep2)<-epval2

return(ep2)
}

findlongest<-function(x){
l=rle(x)$lengths
v=rle(x)$values
lv<-l[which(v==1) ]
if(length(lv)==0){NA}
else{max(lv,na.rm=T)}
}

#### model alleles with environment

model_alleles<-function(EuropeClim,snpnames,layername,endingname,cluster=F,runtype="classification",PCA=F,geo=F,accmaster_add){

message(paste("runtype=",runtype))
message(paste("PCA=",PCA))
message(paste("geo=",geo))
message(paste("cluster=",cluster))

#### run it
if(cluster==T){
cl<-startcluster()
clusterExport(cl=cl, list("EuropeClim","snpnames","accmaster_add","PCA","geo"),envir=environment())
modstack<-parLapply(cl, snpnames,fun = function(x){run_allele_randomforest(x,accmaster_add,PCA,geo) } )
stopCluster(cl)
}else{
modstack<-lapply(snpnames,FUN = function(x){run_allele_randomforest(x,accmaster_add,PCA,geo)} )
}

#### save the result

names(modstack) = snpnames
save(modstack,file=paste0("results/",layername,"_",endingname,".RDATA"))
message(paste("modstack stored in ", paste0("results/",layername,"_",endingname,".RDATA")))
return(modstack)
}

startcluster<-function(){
require("parallel")
print("starting cluster for parallel operations")

# Calculate the number of cores
no_cores <- detectCores() - 1
if(no_cores > 4){ no_cores=10}
 # Initiate cluster
cl <- makeCluster(no_cores,type="FORK")
# Now we just call the parallel version of lapply, parLapply:

return(cl)
}

stopcluster<-function(cl=cl){
  stopCluster(cl)
  print("cluster closed")

}

run_allele_randomforest=function(x,accmaster_add,PCA,geo){
  mydata<-getmydata(data=accmaster_add,phenoname = x,PCA=PCA,geo=geo,id=F)
  # print(head(mydata))
  mod_forest<-run_randomforest(mydata=mydata,phenoname = x ,myfilename = endingname,type = runtype,sink=F)
return(mod_forest)}

predict_alleles<-function(EuropeClim,modstack,snpnames,layername,endingname,cluster=F){
print(paste("start raster predictions of layer ",layername))

predict_allele=function(x,modstack,EuropeClim){
prf <- predict(EuropeClim,modstack[[x]])
return(prf)}

if(cluster==T){

cl<-startcluster() ##
clusterExport(cl=cl, list("EuropeClim","modstack","snpnames"),envir=environment())
allelstack<-parLapply(cl, snpnames,fun = function(x){predict_allele(x,modstack,EuropeClim)})
stopCluster(cl) ##

}else{
allelstack<-lapply( snpnames,FUN = function(x){predict_allele(x,modstack,EuropeClim) })
}


allelstack= stack(allelstack)
names(allelstack)=snpnames
save(allelstack,file=paste0("results/",layername,"_",endingname,".RDATA"))
message(paste("predicted alleles stored in ", paste0("results/",layername,"_",endingname,".RDATA")))
return(allelstack)
}

#### read alleles predictions

getindex.allstack<-function(allstack,SNPs){
possibilities <- c("gwa","agwa","genome","all")
if( !(SNPs %in%  possibilities)){
  cat("The SNPs name provided does not match any of these: "); cat(possibilities)
  stop("the SNPs category does not exist! specify one of the above" )
  }

if(SNPs=="gwa"){ return(1:151)}
if(SNPs=="agwa"){return(152:length(names(allstack[[1]])))}
if(SNPs=="genome"){return(1:length(names(allstack[[1]])))}
if(SNPs=="all"){return(1:length(names(allstack[[1]])))}
if(SNPs=="benchmark"){return(1:5)} # just to check the
}

get_allstack_old=function(nameanalysis){

endingname=paste0("allelstack_",nameanalysis)

# Read the stack
message("load allele stack...")
thefile=paste0("allstack_",nameanalysis,".RDATA")

if(thefile %in% list.files()){
load(file =thefile)
message(paste("found file, then only loading:",thefile))
}
else{
allstack<-read_stack_predictions(layernames = layernames,endingname = endingname)
save(allstack,file =thefile)

message(paste("finished loading allele stack...",nameanalysis))
message(paste("stored here:",thefile))
}

return(allstack)
}

allstack_load_or_read <- function(nameanalysis,layernames,SNPs,path="/results",force=F){

  load_allstack(nameanalysis,layernames,path,force)

    substack<-sapply(names(allstack),FUN = function(x){
    allstack[[x]] [[getindex.allstack(allstack=allstack,SNPs=SNPs)]]
    })

  # assign(x = "substack",value = substack,envir = .GlobalEnv)
  return(substack)
}

load_allstack<- function(nameanalysis,layernames, path="results/",force){

  endingname=paste0("allelstack_",nameanalysis)

  if( !("allstack" %in% ls(envir=.GlobalEnv)) | force==T){
    cat("\n--> allstack not found in the environment")  ; if(force==T){cat(" ...that is a lie, cause force==T") }
        bfile=paste0(path,endingname,".RDATA")

        if( (basename(bfile) %in%  list.files(path) ) ){
          cat(paste("\n--> binary .RDATA found in directory, proceed to load", bfile))
          load(file =bfile)
        }
        else{
          cat("\n--> binary .RDATA not found in the directory, calling reading function...")
          allstack=read_stack_predictions(layernames=layernames,endingname=endingname,path=path)
        }

  assign(x = "allstack",value = allstack,envir = .GlobalEnv)
  }else{  cat("\n--> allstack found in the environment") }
}

read_stack_predictions<-function(layernames,endingname,path="results/"){
  message(paste("\n    Reading stack of predictions from", endingname,":"))

  allstack<-sapply(layernames,FUN = function(layername) read_prediction(layername=layername,endingname=endingname,path))

  allstackfile <- paste0(path,endingname,".RDATA")
  message(paste("\nsaving binary to", allstackfile))
  save(file=allstackfile,allstack)

  return(allstack)
}

read_prediction <- function(layername,endingname,path){
  focalfile<-paste0(path,layername,"_",endingname,".RDATA")
  message(paste("    ...reading predictions of layer", focalfile))

  load(file=focalfile)
  return(allelstack)
}

plotallstack_design <- function(layername){
  # The lists coinsists in mapping
  # Layer to pot | Substracting layer | Substracted layer
  # Essentially column 3-2 except for the first row
  designlist <- matrix(c(
    "bio_2-5m_bil", "bio_2-5m_bil","0",
    "cc26bi50","bio_2-5m_bil","+",
    "cc26bi70","bio_2-5m_bil","+",
    "cc45bi50","bio_2-5m_bil","+",
    "cc45bi70","bio_2-5m_bil","+",
    "cc60bi50","bio_2-5m_bil","+",
    "cc60bi70","bio_2-5m_bil","+",
    "cc85bi50","bio_2-5m_bil","+",
    "cc85bi70","bio_2-5m_bil","+",
    "cclgmbi_2-5m","bio_2-5m_bil","-",
    "ccmidbi_2-5m","bio_2-5m_bil","-"
    ),nrow= 11,ncol = 3 ,byrow = T)
  row <- designlist[which(designlist[,1]==layername),]
  return(row)
}

plotallstack <- function(a, layernames, numbreak=10){
  sapply(layernames,function(layer) {
    des <- plotallstack_design(layer)
    # print(des)
    # plotalleleprediction(a=a,layer=des[1], reference=des[2], type=des[3], numbreak=numbreak)
    plotalleleprediction(layer=a[[des[1]]], reference=a[[des[2]]], type=des[3], numbreak=numbreak)
  })
}

plotalleleprediction <- function(layer,reference=NULL,type="0",numbreak=10, colorpal=(brewer.pal(n = 9,name = "Greys"))){

  if(type =="0"){
  envirplot(
              maskif(sum(layer) ) , # sometimes there are problems. the sea is colored. to avoid that I mask from the info in a bioclim layer
              # (sum(a[[ layer ]]) ) ,
              vecol=colorpal,
              numbreak = numbreak,  contrast=0)
  }else if(type=="+"){
  envirplot(
              sum(layer) - sum(reference) ,
              vecol=rev(brewer.pal(n = 10,name = "RdYlBu")), midpoint = 0,
              numbreak = numbreak,  contrast=4)
  }else if(type=="-"){
  envirplot(
              sum(reference) - sum(layer)  ,
              vecol=rev(brewer.pal(n = 10,name = "RdYlBu")), midpoint = 0,
              numbreak = numbreak,  contrast=4)
    makeglacial()
  }
  # return(ab)
}


####@ allele frequency calculations

frequency_perpop <- function(genmatrix,popindex, popnames=NULL, allelecode=1, return="freq"){
# The genmatrix has to be rows individuals and columns SNPs
# The population inddex has to be the same order as the rows in genmatrix

  freqpop<-sapply(sort(unique(popindex)), function(x) {
  	tmp=subset(genmatrix, popindex==x)
      	tmpfq=apply(tmp,2,function(SNP) {
          nsize=length(na.omit(SNP))

    		  if(is.na(table(SNP == allelecode)["TRUE"] )){
    		      freq <- 0
    		      freq_se <- 0
    		      }
    		  else{
    		      freq <-  table(SNP == 1)["TRUE"] /  nsize
    		      freq_se <- sqrt(  (freq * (1-freq)) / nsize )
    		  }

          if(return=="freq"){ return(freq)}
          if(return=="freq_se"){ return(freq_se)}
  		})
  	})

if(!is.null(popnames)){ colnames(freqpop) <- popnames}

return(freqpop)
}

freq_test_data <- function(present_freq,present_se,future_freq,future_se, population, n1=40,n2=40){
  f1=present_freq[,population]
  se1=present_se[,population]
  f2=future_freq[,population]
  se2=future_se[,population]
  n1=rep(n1,length(f1))
  n2=rep(n2,length(f2))

  freq_test(f1,se1,f2,se2,n1,n2)

}


freq_test <- function(f1,se1,f2,se2,n1,n2){
  poolse <- sqrt( ((n1-1) * se1^2 + (n2-1) * se2^2 )/(n1+n2-2) ) * sqrt( (1/n1) +(1/n2) )
    poolse[poolse==0] <- 10-6
  fdif <- f1-f2
  t.val <-  fdif / poolse
    t.val[is.na(t.val)] <- 0
    t.val[is.infinite(t.val)] <- 0 # to avoid having Nas all over the place
  thedf= n1+n2-2
  p.val<- pt(abs(t.val),df=thedf,lower.tail = F) # this is necessary cause all changes in frequency to high will be p=1
  return(list(t.val=t.val,p.val=p.val))
}

t.extract <- function(thet){
  # pval<-paste( "p=",format(thet$p.value,digits = 3,scientific = T) )
  pval=starpvalue(thet$p.value)
  # tval <-paste( "t=",round(thet$statistic ,3) )
  confi <- paste("",  paste(round(as.numeric(thet$conf.int),3),collapse=", ") )
  # return(list(pval,tval,confi))
  # return(paste(c(confi,tval,pval),collapse = ", "))
  return(paste(c(confi,pval),collapse = " "))
}

####@  simulation

genofreq<-function(genoid=paste0('g',c(1:length(genofit))) ,
                   genofit=apply(allelemat,1,function(x) sum( as.numeric(x) * s ) ) ,
                   genofq=rep(1/length(genofit), length(genofit)),
                   n=100000,
                   gen=50,
                   s=0.01){

g=matrix(ncol=gen+1,nrow=length(genofit))
g[,1]<-genofq

for( t in 1:gen){
# print(t)
inds=round(n*g[,t])

# sel
nesinds=round(inds*  ( (1+genofit) /mean(1+genofit,na.rm=T) )  )

g[,t+1] <- nesinds / sum(nesinds)

}
return(g)
}


alleleFreq <- function(mu, nu, m, wAA, wAa, waa, p0, psource, tmax, d, Fi, N,rep) {
sapply( 1:rep , FUN=function(rep){
    p <- c()
    p[1] <- p0

    for (t in 1:(tmax-1)) {
      # mutation first
      pp <- (1-mu)*p[t] + (1-p[t])*nu

      # next, migration
      if(m!=0) ppp <- (1-m)*pp + m*psource
      else ppp=pp

      # then selection
      meanfit=( wAA*ppp^2 + wAa*2*ppp*(1-ppp) + waa*(1-ppp)^2 )

      if (is.na(meanfit)){  p[t+1] <- NA
      }else if (meanfit  > 0) {
        p[t+1] <- ( wAA*ppp^2 + wAa*ppp*(1-ppp) ) / ( wAA*ppp^2 + wAa*2*ppp*(1-ppp) + waa*(1-ppp)^2 )
      }
      else {
        p[t+1] <- NA
      }
      # then imbreeding (this equation is general)
      fAA <- (p[t+1]^2 * (1-Fi)) + p[t+1]*Fi
      fAa <- 2*p[t+1]*(1-p[t+1]) * (1-Fi)
      faa <- (1-p[t+1])^2 * (1-Fi) + (1-p[t+1])* Fi
      # no imbreeding
      # fAA <- p[t+1]^2
      # fAa <- 2*p[t+1]*(1-p[t+1])
      # faa <- (1-p[t+1])^2

      if(d!=0){
      # then drift
      NAA <- round(N*fAA)
      NAa <- round(N*fAa)
      Naa <- round(N*faa)

      if (NAA <= 0) {
        NAA <- 0
        NAA.prime <- 0
      } else {
        NAA.prime <- sum(rbinom(n=NAA, size=1, prob=d))
      }
      if (NAa <= 0) {
        NAa <- 0
        NAa.prime <- 0
      } else {
        NAa.prime <- sum(rbinom(n=NAa, size=1, prob=d))
      }
      if (Naa <= 0) {
        Naa <- 0
        Naa.prime <- 0
      } else {
        Naa.prime <- sum(rbinom(n=Naa, size=1, prob=d))
      }
      N.prime <- NAA.prime + NAa.prime + Naa.prime

      if (N.prime <= 0) {
        p[t+1] <- NA
      } else {
        p[t+1] <- (2*NAA.prime + NAa.prime) / (2*N.prime)
      }
      }
    } #end t loop


    return(p)
}) #end sapply
}

sim_one<-function(p0,s,m=0,psource=0){
tmax=50
mu= 7e-9
nu=7e-9 * 7e-9
Fi=0.99
N=150000
rep=1
d=0.999
wAA=1
waa= wAA-s
wAa= (wAA+waa)/2
142608
return(
 alleleFreq(mu, nu, m, wAA, wAa, waa, p0, psource, tmax, d, Fi, N,rep)
)
}

sim_array<-function( freqs,sels){
  comb<-expand.grid(freqs,sels)
  colnames(comb)=c('p0','s')

  f.prim<-apply(comb,1,FUN=function(x){

        sim_one(p0 = x[1],s = x[2])[50,]

    } )

  comb$fprim=f.prim
  return(comb)
}

alleleFreq.vec <- function(mu, nu, m, wAA, wAa, waa, p0=c(0.1,0.5,0.9), psource, tmax=50, d, Fi, N,rep) {
# sapply( 1:rep , FUN=function(rep){

    p <- matrix(ncol=tmax,nrow=length(p0))
    p[,1] <- p0

    for (t in 1:(tmax-1)) {
      # mutation first
      pp <- (1-mu)*p[,t] + (1-p[,t])*nu

      # next, migration
      if(m!=0) {ppp <- (1-m)*pp + m*psource
      }else{ ppp=pp}

      # then selection
      meanfit=( wAA*ppp^2 + wAa*2*ppp*(1-ppp) + waa*(1-ppp)^2 )

      # if (is.na(meanfit)){  p[t+1] <- NA
      # }else if (meanfit  > 0) {
        p[,t+1] <- ( wAA*ppp^2 + wAa*ppp*(1-ppp) ) / ( wAA*ppp^2 + wAa*2*ppp*(1-ppp) + waa*(1-ppp)^2 )
      # }
      # else {
        # p[t+1] <- NA
      # }

      # then imbreeding (this equation is general)
      fAA <- (p[,t+1]^2 * (1-Fi)) + p[,t+1]*Fi
      fAa <- 2*p[,t+1]*(1-p[t+1]) * (1-Fi)
      faa <- (1-p[,t+1])^2 * (1-Fi) + (1-p[,t+1])* Fi
      # no imbreeding
      # fAA <- p[t+1]^2
      # fAa <- 2*p[t+1]*(1-p[t+1])
      # faa <- (1-p[t+1])^2

      if(d!=0){
      # then drift
      NAA <- round(N*fAA)
      NAa <- round(N*fAa)
      Naa <- round(N*faa)

      driftsample<-function(ind){
        Nprime=sum(rbinom(n=ind, size=1, prob=d))
      }
      NAA.prime <-sapply(NAA, driftsample )
      NAa.prime <-sapply(NAa, driftsample )
      Naa.prime <-sapply(Naa, driftsample )
      N.prime <- NAA.prime + NAa.prime + Naa.prime


      p[,t+1] <- (2*NAA.prime + NAa.prime) / (2*N.prime)

      # if (NAA <= 0) {
      #   NAA <- 0
      #   NAA.prime <- 0
      # } else {
      #   NAA.prime <- sum(rbinom(n=NAA, size=1, prob=d))
      # }
      # if (NAa <= 0) {
      #   NAa <- 0
      #   NAa.prime <- 0
      # } else {
      #   NAa.prime <- sum(rbinom(n=NAa, size=1, prob=d))
      # }
      # if (Naa <= 0) {
      #   Naa <- 0
      #   Naa.prime <- 0
      # } else {
      #   Naa.prime <- sum(rbinom(n=Naa, size=1, prob=d))
      # }
      # N.prime <- NAA.prime + NAa.prime + Naa.prime

      # if (N.prime <= 0) {
      #   p[t+1] <- NA
      # } else {
      #   p[t+1] <- (2*NAA.prime + NAa.prime) / (2*N.prime)
      # }
      }
    } #end t loop


    return(p)
# }) #end sapply
}

sim_array.vec<-function( freqs,sels,m=0, psource=0){

tmax=50
mu= 7e-9 * 7e-9
nu=1e-20
self=.99
Fi= self/(2-self)
N=285000
rep=1
d=0.999


cl<-startcluster() ##
f.prim<-parSapply(cl,sels,FUN = function(s){
    wAA=1
    waa= wAA-s
    wAa= (wAA+waa)/2

  alleleFreq.vec(mu, nu, m, wAA, wAa, waa, p0=freqs, psource, tmax, d, Fi, N,rep)[,tmax]

  })
stopCluster(cl) ##

return(f.prim)
}

####@  my dissimilarity modeling


make_gdmDiss<-function(accmaster_add,SNPs="gwa"){

if(SNPs=="gwa"){
wheresnps<-grep("chr", colnames(accmaster_add))
row.names(accmaster_add) = accmaster_add$id
distmat<-as.matrix(dist(accmaster_add[,wheresnps]))
distmat / (2*length(wheresnps))
hist(distmat,main="gdmDiss")
mygdmDissim<-data.frame(cbind(as.numeric(row.names(distmat)),distmat))
mygdmDissim[1:5,1:5]
}
if(SNPs=="genome"){

kinship<-read.table('../762analysis/762_4m.hIBS.kinf')
hh(kinship)
# hist( as.matrix((1-kinship) / (1- min(kinship))) )
fam<-data.frame(genomes=read.table('../762analysis/762_4m.fam')$V1)
mygdmDissim<-cbind(fam, as.matrix((1-kinship) / (1- min(kinship)))) # with transformation to orient and express from 0 to 1
mygdmDissim[1:5,1:5]
hist(fn(mygdmDissim[,-1]),main="gdmDiss")

}
colnames(mygdmDissim)[1]<-"genomes"
return(gdmDissimsnps)
}

make_snpTab<-function(accmaster_add,wheresnps=NULL){

if(is.null(wheresnps)){ wheresnps<-grep("chr", colnames(accmaster_add)) }

snpTab<-data.frame(matrix(nrow=0,ncol=4))
colnames(snpTab)<-c("snps","genomes","Lat","Long")
for(id in accmaster_add$id){
# print(id)
 mysub<-accmaster_add[which(accmaster_add$id ==id),]
 mysnps<-colnames(accmaster_add)[wheresnps][ which(mysub[wheresnps] ==1) ]
 if(any(length(mysnps) ==0) | is.null(length(mysnps)))
  { print("zero SNPs!")}
  else{
  thedat<-data.frame(snps=mysnps,genomes=id,Lat=mysub$latitude,Long=mysub$longitude)
  snpTab<-rbind(snpTab,thedat)
  }
}

head(snpTab);tail(snpTab)
return(snpTab)
}

make_envTab<-function(accmaster_add){

envTab<-accmaster_add[,c("id",colnames(accmaster_add)[grep("bio",colnames(accmaster_add))], "latitude","longitude" )]

colnames(envTab)[which(colnames(envTab) %in% c("id","latitude","longitude"))]<-c("genomes","Lat","Long")

head(envTab,n=3)
return(envTab)
}


rf_gdm <- function(mygdmdata){
  pulmod<-randomForest(formula(gene~. ),data=pulsub ,ytest=pulsub2[,1],xtest=data.frame(pulsub2[,-1]),keep.inbag = TRUE,importance=TRUE)

}

dissimilarity<-function(biodata){
  sapply(colnames(biodata),FUN=function(n){
    tmp<-( dist(biodata[,n]) )
    tmp <-tmp[lower.tri(tmp)]
    tmp=fn(tmp)
    return(tmp)
})}

make_gdmDiss<-function(accmaster_add,wheresnps=NULL){

if(is.null(wheresnps)){ wheresnps<-grep("chr", colnames(accmaster_add)) }

row.names(accmaster_add) = accmaster_add$id
distmat<-as.matrix(dist(accmaster_add[,wheresnps]))
distmat / (2*length(wheresnps))
# distmat[1:10,1:10]
hist(distmat,main="gdmDiss")

gdmDissimsnps<-data.frame(cbind(as.numeric(row.names(distmat)),distmat))
gdmDissimsnps[1:10,1:10]
colnames(gdmDissimsnps)[1]<-"genomes"
return(gdmDissimsnps)
}

bio_dissimilarity<-function(biodata){
  sapply(colnames(biodata),FUN=function(n){
    tmp<-fn( dist(biodata[,n]) )
    return(tmp)

           })
}

####@ plotting stuff

envir_plot_newerbutold<-function(prediction,phenoname,dimcol=100,name="",vecol=NULL,rangecol=NULL,pdf=F,numbreak=11,contrast=0){

if(pdf == T ){
pdf(paste0("plots/prediction_",phenoname,"_",name,".pdf"),width=8,height =
8,useDingbats = F) }


if( is.null(vecol ) ){
mypalette<-colorRampPalette(c('cyan','dodgerblue1','blue','forestgreen','orange','red','purple'))

}else{ if(contrast==0){ mypalette<-colorRampPalette(vecol) }else{ for(i in
1:contrast){ vecol<-c(vecol[1],vecol,vecol[length(vecol)])
mypalette<-colorRampPalette(vecol) } } }

if(is.null(rangecol)){ plot(prediction, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), axes=F) }
else{ mysequence<-seq(rangecol[1],rangecol[2],len = dimcol-1)

if(is.null(numbreak)) {
plot(prediction, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), axes=F,breaks=mysequence) }
else{ arg <-
list(at=seq(rangecol[1],rangecol[2],len = numbreak-1))
plot(prediction,xaxt="n", yaxt="n", main="", col=mypalette(dimcol),axes=F,breaks=mysequence,axis.args=arg)

}
}

##Now plot
  plot(prediction, xaxt="n", yaxt="n", main="",col=mypalette(dimcol), axes=F,breaks=mysequence,axis.args=arg)
  # points(###need to add a call for latitude and longitude of points####, col="white", pch=20, cex=0.15)

# box() axis(1,las=1) axis(2,las=1)

# mtext(paste("prediction",phenoname,"",name), 3, outer=T, line=-2, cex=1.3)

if(pdf == T ){ dev.off() }

}

envir_plot_old<-function(prediction,phenoname,dimcol=100,name="",vecol=NULL,rangecol=NULL,pdf=F){

if(pdf == T ){
pdf(paste0("plots/prediction_",phenoname,"_",name,".pdf"),width=8,height = 8,useDingbats = F)
}


if( is.null(vecol ) ){
mypalette<-colorRampPalette(c('cyan','dodgerblue1','blue','forestgreen','orange','red','purple'))

}else{
  mypalette<-colorRampPalette(vecol)
}

if(is.null(rangecol)){
  mysequence=NULL
}else{
  #mysequence<- seq(rangecol[1],rangecol[2],len = dimcol-1)
  mysequence<- seq(rangecol[1],rangecol[2],len = 10)
}

# plot(prediction, xaxt="n", yaxt="n", main="", col=mycolors, axes=F)
# plot(prediction, xaxt="n", yaxt="n", main="", col=mypallete(dimcol), axes=F)
plot(prediction, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), axes=F,breaks=mysequence)

# points(###need to add a call for latitude and longitude of points####, col="white", pch=20, cex=0.15)
box()
axis(1,las=1)
axis(2,las=1)
# mtext(paste("prediction",phenoname,"",name), 3, outer=T, line=-2, cex=1.3)

if(pdf == T ){
dev.off()
}

}

plotwithcolors<-function(x,y,mycol,palette=c("red","orange","yellow","green","blue"),pch=19,alpha=0.5){

  colfun<-colorRampPalette(palette) #paleta de colores de azul(pocos trich) a rojo (muchos)
  colores<-colfun(length(mycol))[rank(mycol)]

  plot(x=x,y=y,col=transparent(colores,alpha=alpha),breaks=seq(range(mycol,na.rm = T)[1],range(mycol,na.rm = T)[2],10),pch=19)
}
