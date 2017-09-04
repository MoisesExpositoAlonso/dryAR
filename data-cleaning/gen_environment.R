### Script to read all environmental data from my FLower Power sensors ###
set.seed(1)
devtools::load_all('.')
setup_dryAR()

################################################################################
### (1) Define information of sensors ####

working_directory="data-raw/climatesensors/"

data(sensorinfo)
sensorinfo

### (2) Read all sensors ####

# allsensors<-
# lapply(1:nrow(sensorinfo),
#        function(sensnum){
#        myfilename=sensorinfo[sensnum,'filename']
#        tmp<-data.frame(read.csv(paste0(working_directory,myfilename),fill = T,head=T))
#        tmp$sensor=sensorinfo[sensnum,'name']
#        tmp
#        }
#        )
# allsensors.df=do.call(rbind,allsensors)
#
# head(allsensors.df)
# tail(allsensors.df)
#
# devtools::use_data(allsensors.df)  ## Only done once

data(allsensors.df)

columnames<-c("par_umole_m2s","vwc_percent","air_temperature_celsius")

### (3) merge and clean ####

env=merge(allsensors.df,by.x='sensor',sensorinfo,by.y='name',all.x = T)
head(env)

### manipulate time class
env$capture_time= strptime(env$capture_ts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
env = rename(env , datestart= Date)
env$capture_time <- as.POSIXct(env$capture_time)
env$datestart <- as.POSIXct(env$datestart)
env$group = paste(env$Site, env$Treatment, sep="_")

head(env)
dim(env)

### subset those pots whose start and end are over?

envsub= env %>% filter(capture_time >= datestart)
dim(envsub)
dim(env)

################################################################################
### Solve the mess that some sensors stopped working

# toy=envsub %>% sample_n(size=200)
#
# ## first round the dates to 5 per day or sth
# toy$capture_time %>% round()
#
# ## second, spread to short format to predict one pot from the rest
# # library(reshape2)
# ## dcast(toy,formula= capture_time ~ sensor , value.var="vwc_percent")
# toy %>% spread(key='sensor',value='vwc_percent')


################################################################################

envsample=envsub %>% sample_n(.,size=10000)

histwater=ggplot(envsample,aes(y=vwc_percent, x=Treatment,fill=Treatment)) +
  geom_violin(alpha=0.4,na.rm=T) +
    # geom_boxplot(width=.01,alpha=0.5) +
    facet_wrap( ~Site,nrow=1)+
    # scale_color_manual('',values=c('blue', 'red', 'black'))+
    scale_fill_manual('',values=transparent(c('blue', 'red', 'black')))+
    ylab('Soil water content (%)') + xlab('')+
    stat_summary(fun.data=data_summary_median)
histwater

histwater=ggplot(envsample,aes(y=vwc_percent, x=Site,fill=Treatment)) +
  geom_violin(alpha=0.4,na.rm=T) +
    # facet_wrap( ~Site,nrow=1)+
    facet_wrap( ~Treatment,nrow=1)+
    # scale_color_manual('',values=c('blue', 'red', 'black'))+
    scale_fill_manual('',values=transparent(c('blue', 'red', 'black')))+
    ylab('Soil water content (%)') + xlab('')+
    stat_summary(fun.data=data_summary_median)
histwater


histtemp=ggplot(envsample,aes(y=air_temperature_celsius, x=Treatment,fill=Treatment)) +
  geom_violin(alpha=0.4,na.rm=T) +
    facet_wrap( ~Site,nrow=1)+
    scale_color_manual('',values=c('blue', 'red', 'black'))+
    scale_fill_manual('',values=c('blue', 'red', 'black'))+
    ylab('Soil surface temperature (ºC)') + xlab('')+
  stat_summary(fun.data=data_summary_median)
histtemp

histtemp=ggplot(envsample,aes(y=air_temperature_celsius, x=Site,fill=Treatment)) +
  geom_violin(alpha=0.4,na.rm=T) +
    facet_wrap( ~Treatment,nrow=1)+
    scale_color_manual('',values=c('blue', 'red', 'black'))+
    scale_fill_manual('',values=c('blue', 'red', 'black'))+
    ylab('Soil surface temperature (ºC)') + xlab('')+
  stat_summary(fun.data=data_summary_median)
histtemp

envpanel=plot_grid(histwater,histtemp,ncol = 1, nrow=2)
envpanel
save_plot(filename="figs/Figure_watering_temperature.pdf",plot = envpanel, base_width = 7,base_height = 7)

### Tests
envsample=envsub %>% sample_n(.,size=100)

lm(
  data=filter(envsample, group %in% c("Tuebingen_low","Madrid_low","Madrid_out")),
  vwc_percent ~group
   ) %>% anova

lm(
  data=filter(envsample, group %in% c("Tuebingen_high","Madrid_high","Tuebingen_out")),
  vwc_percent ~group
   ) %>% anova

wilcox.test(
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='out' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample, Site=='Madrid' ,Treatment=='high' ) %>% select(vwc_percent) %>% fn() )
)

wilcox.test(
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='out' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='high' ) %>% select(vwc_percent) %>% fn() )
)


wilcox.test(
(dplyr::filter(envsample, Site=='Madrid' ,Treatment=='out' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='low' ) %>% select(vwc_percent) %>% fn() )
)

wilcox.test(
(dplyr::filter(envsample, Site=='Madrid' ,Treatment=='out' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='low' ) %>% select(vwc_percent) %>% fn() )
)

wilcox.test(
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='high' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='low' ) %>% select(vwc_percent) %>% fn() )
)

wilcox.test(
(dplyr::filter(envsample, Site=='Madrid' ,Treatment=='high' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample, Site=='Madrid' ,Treatment=='low' ) %>% select(vwc_percent) %>% fn() )
)


removesome=which(envsample$Treatment =="out" & envsample$Site =="Madrid" & envsample$vwc_percent <10)
envsample2=envsample[-removesome,]

t.test(
(dplyr::filter(envsample2, Site=='Madrid' ,Treatment=='out' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample2, Site=='Tuebingen' ,Treatment=='low' ) %>% select(vwc_percent) %>% fn() )
)

boxplot(envsample2$vwc_percent ~ envsample2$Treatment + envsample2$Site)
boxplot(envsample$vwc_percent ~ envsample$Treatment + envsample$Site)

t.test(
(dplyr::filter(envsample2, Site=='Tuebingen' ,Treatment=='out' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample2, Site=='Madrid' ,Treatment=='low' ) %>% select(vwc_percent) %>% fn() )
)


### bigplot
names(envsub)

envsample=envsub %>% sample_n(.,size=10000) %>%
  rbind(.,
        filter(envsub,Treatment=='out'))

ggplot(data=envsample) + xlab('')+ ylab('Soil water content (%)')+
  geom_line(aes(y=vwc_percent,x = capture_time,group=Treatment,color=Treatment ,size=Treatment)) +
  facet_wrap( ~Site,nrow=2)+
  scale_color_manual(values=c('blue3', 'red3', 'black'))+
  scale_size_manual(values= c(0.5,0.51,2)) +
  #theme_bw()
  scale_x_date(breaks =
    as.Date(
    seq(min(unique(envsub$capture_time) ),
    max(unique(envsub$capture_time) ),
    length.out = 8) )
    )

################################################################################
### ANOVA
envsub %>% head

envsub %>% head

envsub$group=paste(envsub$Site, envsub$Treatment, sep="_")
boxplot(envsub$vwc_percent ~ envsub$group)

# kruskal.test(envsub$vwc_percent ~envsub$group)
lmmod = lm(envsub$vwc_percent ~ (envsub$group) )
summary(lmmod)



################################################################################


downy=-10
upy=55
if (counter==1){
library(lubridate)


par(mar = c(8, 4, 4, 4) + 0.1)
plot(y = data[,columnames[2]] ,x=data$capture_ts, ylab=columnfinalnames[2], frame.plot=T,     cex=0,xlab="",axes=F,col="blue",xlim = as.POSIXct(c(range(data$capture_ts) )+ c(hours(0),hours(3))) ,ylim=c(downy, upy), xaxt="n")

  # lines(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,lty=as.numeric(shapewater))
  # lines(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,lty=as.numeric(shapetemp))
  points(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,pch=as.numeric(shapewater))
  points(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,pch=as.numeric(shapetemp))
# add  axis and grid

    axis(2, ylim=c(downy,upy),at=seq(downy,upy,5),col="black",las=1)
  axis(4, ylim=c(downy,upy),at=seq(downy,upy,5),col="black",las=1)
  mtext("Temp (C)",side=4,line=2.5)
  labelspretty=pretty(range(data$capture_ts),15)

  abline(h=seq(downy,upy,5), v=labelspretty, col="gray", lty=3)

# add the sensor name in the water lines
text(y = tail(data[,columnames[2]], n=1) ,x= as.POSIXct(tail(data$capture_ts, n=1)+hours(3))   ,paste(posnumber,name,sep="-"))

# labels axis
  labelspretty=paste( format(labelspretty,format="%Y-%m-%d %H"), "H",sep="" )
  axis.POSIXct(1,at=pretty(range(data$capture_ts),15),labels=labelspretty,las=2)

# legend
legend("topleft",legend = c("LOW_water","LOW_temp","HIGH_water","HIGH_temp"),col=c(colorfile[1,2:3],colorfile[2,2:3]) ,bty="n",title="")

}
else{
  # lines(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,lty=as.numeric(shapewater))
  # lines(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,lty=as.numeric(shapetemp))
  points(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,pch=as.numeric(shapewater))
  points(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,pch=as.numeric(shapetemp))

  # add the sensor name in the water lines
text(y = tail(data[,columnames[2]], n=1) ,x= as.POSIXct(tail(data$capture_ts, n=1)+hours(3))   ,paste(posnumber,name,sep="-"))


}
} # end file


################################################################################
### Info on columns of spreadsheet

columnpositions<-c(2,3,4)
columnames<-c("par_umole_m2s","vwc_percent","air_temperature_celsius")
columnfinalnames<-c("PAR (photons /sec m2 ) ", "Water content (%)","air Temp (C)")

# collumnames<-c("par_umole_m2s","vwc_percent","ecp_ms_cm","air_temperature_celsius")
# "Fertilizer (conductivity mS/cm)","


### Merge files and information of sensors ####

sensor_files_merge<-merge(sensor_files,by.x="name",  sensor_info, by.y="Sensor_name" )
# sensor_files$name
# sensor_info$Sensor_name

setdiff(sensor_files$name,sensor_info$Sensor_name)
print(paste("total number of sensors:"  ,as.character(dim(sensor_files_merge)[1 ]) ) )

#### Colors & shapes of sensors  ####

# colorfile<-matrix(c('low','dodgerblue2','indianred1',
#                     'high','navy','darkred',
#                     "out","grey","grey") ,ncol=3,nrow=3,byrow=T)
# colnames(colorfile)<-c("Treatment","water","temp")
#
# shapefile<-matrix(c('low',1,1,
#                     'high',1,1,
#                     "out",1,1) ,ncol=3,nrow=3,byrow=T)
# colnames(shapefile)<-c("Treatment","water","temp")

### Parse files and plot ### Tuebingen #####
site="Tuebingen"
for (site in unique(sensor_files_merge$Site) ) {

finalfilelist= subset( sensor_files_merge, sensor_files_merge$Site==site)
rownames(finalfilelist)=finalfilelist$name

numberfiles =dim(finalfilelist)[1]
print (paste("Sensors in",site,sep=" ") )
numberfiles


# working_directory="~/GoogleDrive/PhD_docs/experiment_1001G/experiment_field_1001g_share/climate_sensors/"
working_directory="../environment/"

# pdf(paste(working_directory,"water_temp_series_TUEBINGEN.pdf",sep="/"),height =8 ,width = 15,useDingbats = F)
pdf(paste(working_directory,"/water_temp_series_",site,".pdf",sep=""),height =8 ,width = 15,useDingbats = F)

counter=0
for (sensor in finalfilelist$name )   {
  file=as.character(finalfilelist[sensor,"filename"])
  name=as.character(finalfilelist[sensor,"name"])
  posnumber= as.character(finalfilelist[sensor,"Position"])
  datestart= as.character(finalfilelist[sensor,"Date"])

  data<-read.csv(file)
  head(data);tail(data)

  if( 0 %in% dim(data) ){
    # break
    print (paste(name," Flower Power data EMPTY ! "))
    next
  }else{
    print (paste(name," Plotting Flower Power time series"))
    counter=counter+1
  }

  # make the date series type
  data$capture_ts<-strptime(data$capture_ts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  class(data$capture_ts)
  data<-subset(data,data$capture_ts >= as.POSIXct(datestart) )

    ## summary data
#   library(plyr)
#   data$week <- format(data$capture_ts, format="%Y-%U")
#   ddply(data, .(week), summarize, air_temperature_celsius=mean(air_temperature_celsius))
#   ddply(data, .(week), summarize, vwc_percent=mean(vwc_percent))
#   summary(data$vwc_percent)

colortemp= as.character(finalfilelist[sensor,"Color_temp"])
colorwater= as.character(finalfilelist[sensor,"Color_water"])

shapetemp<-1
shapewater<-1

downy=-10
upy=55
if (counter==1){
library(lubridate)


par(mar = c(8, 4, 4, 4) + 0.1)
plot(y = data[,columnames[2]] ,x=data$capture_ts, ylab=columnfinalnames[2], frame.plot=T,     cex=0,xlab="",axes=F,col="blue",xlim = as.POSIXct(c(range(data$capture_ts) )+ c(hours(0),hours(3))) ,ylim=c(downy, upy), xaxt="n")

  # lines(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,lty=as.numeric(shapewater))
  # lines(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,lty=as.numeric(shapetemp))
  points(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,pch=as.numeric(shapewater))
  points(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,pch=as.numeric(shapetemp))
# add  axis and grid

    axis(2, ylim=c(downy,upy),at=seq(downy,upy,5),col="black",las=1)
  axis(4, ylim=c(downy,upy),at=seq(downy,upy,5),col="black",las=1)
  mtext("Temp (C)",side=4,line=2.5)
  labelspretty=pretty(range(data$capture_ts),15)

  abline(h=seq(downy,upy,5), v=labelspretty, col="gray", lty=3)

# add the sensor name in the water lines
text(y = tail(data[,columnames[2]], n=1) ,x= as.POSIXct(tail(data$capture_ts, n=1)+hours(3))   ,paste(posnumber,name,sep="-"))

# labels axis
  labelspretty=paste( format(labelspretty,format="%Y-%m-%d %H"), "H",sep="" )
  axis.POSIXct(1,at=pretty(range(data$capture_ts),15),labels=labelspretty,las=2)

# legend
legend("topleft",legend = c("LOW_water","LOW_temp","HIGH_water","HIGH_temp"),col=c(colorfile[1,2:3],colorfile[2,2:3]) ,bty="n",title="")

}
else{
  # lines(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,lty=as.numeric(shapewater))
  # lines(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,lty=as.numeric(shapetemp))
  points(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,pch=as.numeric(shapewater))
  points(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,pch=as.numeric(shapetemp))

  # add the sensor name in the water lines
text(y = tail(data[,columnames[2]], n=1) ,x= as.POSIXct(tail(data$capture_ts, n=1)+hours(3))   ,paste(posnumber,name,sep="-"))


}
} # end file loop

dev.off()

}
