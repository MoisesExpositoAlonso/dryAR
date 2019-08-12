### Script to read all environmental data from my FLower Power sensors ###
devtools::load_all('.')
setup_dryAR()

################################################################################
### (1) Define information of sensors ####

working_directory="data-raw/climatesensors/"

data(sensorinfo)
sensorinfo

### (2) Read all sensors ####

# allsensors<-
#     lapply(1:nrow(sensorinfo),
#            function(sensnum){
#            myfilename=sensorinfo[sensnum,'filename']
#            tmp<-data.frame(read.csv(paste0(working_directory,myfilename),fill = T,head=T))
#            tmp$sensor=sensorinfo[sensnum,'name']
#            tmp
#            }
#            )
# allsensors.df=do.call(rbind,allsensors)
#
# head(allsensors.df)
# tail(allsensors.df)

# devtools::use_data(allsensors.df,overwrite = FALSE)  ## Only done once

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

# Add also the date of sowing
env$sowing <- ifelse(env$Site =='Madrid', sowingday()$madrid,sowingday()$tuebingen)

################################################################################
### Solve the mess that some sensors stopped working ###### !!!!!!!!!!!!!!!!!!!

#>> (A) subset those pots whose start and end are over?
    # removing the 3 first days help to remove those days until when trays were completely wet
envsub= env %>% filter(capture_time >= datestart + days(3))

#>> (b) The experiment in Tubingen finished 1 June  2016-06-01 need to remove that data (this is conservative, as everything was dry already)
    # a less conservative is the 2016-05-15, where most plants were dry, and 2016-05-01 where everything had fruits
    # and about mid april everything had flowered already 2016-04-15 (indeed Madrid drought was stopped)
envsub= envsub %>% filter(capture_time < as.Date('2016-05-1'))

    # Actually for Tuebingen the 15 of April everything was done
    # And the 25 of April in Madrid more watering was started

# envsub= envsub %>% filter(capture_time < as.Date('2016-05-1'))
envsub=envsub[-(which(
  (envsub$Site=='Madrid' &envsub$capture_time > as.Date('2016-04-25') ) |
  (envsub$Site=='Tuebingen' &envsub$capture_time > as.Date('2016-04-15') )
   )) , ]


#>> (c) pots with zero values are rare.
envsub= envsub %>% filter(vwc_percent > 0)


################################################################################
### (4) histogram plots ####

envsample=envsub %>% sample_n(.,size=10000)

histwater=ggplot(envsample,aes(y=vwc_percent, x=Treatment,fill=Treatment)) +
  geom_violin(alpha=0.7,na.rm=T, color=transparent('white',1)) +
    # geom_boxplot(width=.01,alpha=0.5) +
    facet_wrap( ~Site,nrow=1)+
    # scale_color_manual('',values=c('blue', 'red', 'black'))+
    scale_fill_manual('',values=transparent(c(watercolors()[['h']], watercolors()[['l']], 'black')))+
    ylab('Soil water content (%)') + xlab('')+
    stat_summary(fun.data=data_summary_median)
histwater

histwater=ggplot(envsample,aes(y=vwc_percent, x=Site,fill=Treatment)) +
  geom_violin(alpha=0.7,na.rm=T, color=transparent('white',1)) +
    # facet_wrap( ~Site,nrow=1)+
    facet_wrap( ~Treatment,nrow=1)+
    # scale_color_manual('',values=c('blue', 'red', 'black'))+
    scale_fill_manual('',values=transparent(c(watercolors()[['h']], watercolors()[['l']], 'black')))+
    ylab('Soil water content (%)') + xlab('')+
    stat_summary(fun.data=data_summary_median)
histwater


histtemp=ggplot(envsample,aes(y=air_temperature_celsius, x=Treatment,fill=Treatment)) +
  geom_violin(alpha=0.7,na.rm=T, color=transparent('white',1)) +
    facet_wrap( ~Site,nrow=1)+
    scale_color_manual('',values=c('blue', 'red', 'black'))+
    scale_fill_manual('',values=transparent(c(watercolors()[['h']], watercolors()[['l']], 'black')))+
    ylab('Soil surface temperature (ºC)') + xlab('')+
  stat_summary(fun.data=data_summary_median)
histtemp

histtemp=ggplot(envsample,aes(y=air_temperature_celsius, x=Site,fill=Treatment)) +
  geom_violin(alpha=0.7,na.rm=T, color=transparent('white',1)) +
    facet_wrap( ~Treatment,nrow=1)+
    scale_color_manual('',values=c('blue', 'red', 'black'))+
    scale_fill_manual('',values=transparent(c(watercolors()[['h']], watercolors()[['l']], 'black')))+
    ylab('Soil surface temperature (ºC)') + xlab('')+
  stat_summary(fun.data=data_summary_median)
histtemp

envpanel=plot_grid(histwater,histtemp,ncol = 1, nrow=2)
envpanel
save_plot(filename="figs/Figure_watering_temperature.pdf",plot = envpanel, base_width = 7,base_height = 7)


### (4b) Trajectory pots ####

names(envsub)

envsample=envsub %>% sample_n(.,size=10000) %>%
  filter(capture_time < as.Date('2016-04-25')) %>%
  rbind(.,
        filter(envsub,Treatment=='out'))

ggplot(data=envsample) + xlab('')+ ylab('Soil water content (%)')+
  # geom_line(aes(y=vwc_percent,x = capture_time,group=Treatment,color=Treatment ,size=Treatment)) +
#   stat_summary(aes(y=vwc_percent,x = capture_time,group=Treatment,color=Treatment ,size=Treatment),geom = "line") +
#   stat_smooth(aes(y=vwc_percent,x = capture_time,group=Treatment,color=Treatment ,size=Treatment)) +
  geom_line(aes(y=vwc_percent,x = capture_time,group=sensor,color=Treatment )) +
  facet_wrap( ~Site,nrow=2)+
  scale_color_manual(values=c("#2166ac", "#b2182b" ,"black" ))+
  scale_size_manual(values= c(0.5,0.51,2)) +
  theme_bw()

###############################################################################
### (5a) average values  ####


e=envsub

head(e)

matrix( byrow = FALSE, ncol=4,
  c(
c('Madrid','Madrid','Tuebingen','Tuebingen','Tuebingen','Madrid'),
c('out','low','low','out','high','high'),

(dplyr::filter(e, Site=='Madrid' ,Treatment=='out' )$vwc_percent %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Madrid' ,Treatment=='low' )$vwc_percent %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Tuebingen' ,Treatment=='low' )$vwc_percent %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Tuebingen' ,Treatment=='out' )$vwc_percent %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Tuebingen' ,Treatment=='high' )$vwc_percent %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Madrid' ,Treatment=='high' )$vwc_percent %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),


(dplyr::filter(e, Site=='Madrid' ,Treatment=='out' )$air_temperature_celsius %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Madrid' ,Treatment=='low' )$air_temperature_celsius %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Tuebingen' ,Treatment=='low' )$air_temperature_celsius %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Tuebingen' ,Treatment=='out' )$air_temperature_celsius %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Tuebingen' ,Treatment=='high')$air_temperature_celsius %>%quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Madrid' ,Treatment=='high' )$air_temperature_celsius %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) )


)) -> summaryenv
write.tsv(file='tables/summary_environment.tsv',summaryenv)

head(e)

e_par<-

c(
(dplyr::filter(e, Site=='Madrid' ,Treatment=='out' )$par_umole_m2s %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Madrid' ,Treatment=='low' )$par_umole_m2s %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Tuebingen' ,Treatment=='low' )$par_umole_m2s %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Tuebingen' ,Treatment=='out' )$par_umole_m2s %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Tuebingen' ,Treatment=='high')$par_umole_m2s %>%quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e, Site=='Madrid' ,Treatment=='high' )$par_umole_m2s %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) )
)


require( lubridate )
e_par<-with(e, e[ hour( capture_ts ) >= 11 & hour( capture_ts ) < 13 , ] )

c(
(dplyr::filter(e_par, Site=='Madrid' ,Treatment=='out' )$par_umole_m2s %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e_par, Site=='Madrid' ,Treatment=='low' )$par_umole_m2s %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e_par, Site=='Madrid' ,Treatment=='high' )$par_umole_m2s %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e_par, Site=='Tuebingen' ,Treatment=='low' )$par_umole_m2s %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e_par, Site=='Tuebingen' ,Treatment=='out' )$par_umole_m2s %>% quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) ),
(dplyr::filter(e_par, Site=='Tuebingen' ,Treatment=='high')$par_umole_m2s %>%quantile(probs=c(0.5, 0.25,0.75)) %>% mean.ci(2) )
)


###############################################################################
### (5) statistical tests ####


# envsub %>% sample_n(.,size=100) # wrong!
# e %>% filter(envsub,) # wrong!

e=envsub

ggplot(data = field)+
  geom_density(aes(x=FT.date,group=water,fill=water,alpha=0.3)) + scale_fill_manual(values = c("h"="navy","l"="red3"))+facet_grid(site~indpop)+theme_bw()
field %>%
  group_by(site) %>%
  summarize(maxFT = max(FT.date,na.rm=T))

# e= filter(envsub, if(Site =='Tuebingen'){capture_time < as.Date('2016-04-15')})
# e=envsub[-(which(envsub$Site=='Tuebingen' & envsub$capture_time > as.Date('2016-04-15') )) , ]
# e=envsub[-(which(e$Site=='Madrid' & e$capture_time > as.Date('2016-04-7') )) , ]



set.seed(1)
size=10000
envsample<- ## need to sample for every treatment!
rbind(
sample_n(dplyr::filter(envsub, Site=='Tuebingen' ,Treatment=='out' ),size) ,
sample_n(dplyr::filter(envsub, Site=='Tuebingen' ,Treatment=='low' ),size) ,
sample_n(dplyr::filter(envsub, Site=='Tuebingen' ,Treatment=='high' ),size) ,
sample_n(dplyr::filter(envsub, Site=='Madrid' ,Treatment=='out' ),size) ,
sample_n(dplyr::filter(envsub, Site=='Madrid' ,Treatment=='low' ),size) ,
sample_n(dplyr::filter(envsub, Site=='Madrid' ,Treatment=='high' ),size)
) %>% ungroup


# lm(
#   data=filter(envsample, group %in% c("Tuebingen_low","Madrid_low","Madrid_out")),
#   vwc_percent ~group
#    ) %>% anova
#
# lm(
#   data=filter(envsample, group %in% c("Tuebingen_high","Madrid_high","Tuebingen_out")),
#   vwc_percent ~group
#    ) %>% anova


# highs should be zero
wilcox.test(
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='low' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample, Site=='Madrid' ,Treatment=='low' ) %>% select(vwc_percent) %>% fn() )
)

# lows should be zero

wilcox.test(
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='low' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample, Site=='Madrid' ,Treatment=='low' ) %>% select(vwc_percent) %>% fn() )
)


# Outside high should be zero
wilcox.test(
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='out' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample, Site=='Madrid' ,Treatment=='high' ) %>% select(vwc_percent) %>% fn() )
)

wilcox.test(
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='out' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='high' ) %>% select(vwc_percent) %>% fn() )
)

# Outside low should be zero

wilcox.test(
(dplyr::filter(envsample, Site=='Madrid' ,Treatment=='out' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='low' ) %>% select(vwc_percent) %>% fn() )
)

wilcox.test(
(dplyr::filter(envsample, Site=='Madrid' ,Treatment=='out' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='low' ) %>% select(vwc_percent) %>% fn() )
)

# high versus low should be significant

wilcox.test(
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='high' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='low' ) %>% select(vwc_percent) %>% fn() )
)

wilcox.test(
(dplyr::filter(envsample, Site=='Madrid' ,Treatment=='high' ) %>% select(vwc_percent) %>% fn() ),
(dplyr::filter(envsample, Site=='Madrid' ,Treatment=='low' ) %>% select(vwc_percent) %>% fn() )
)


### Temperatures

# between treatments should be 0
wilcox.test(
(dplyr::filter(envsample, Site=='Madrid' ,Treatment=='high' ) %>% select(air_temperature_celsius) %>% fn() ),
(dplyr::filter(envsample, Site=='Madrid' ,Treatment=='low' ) %>% select(air_temperature_celsius) %>% fn() )
)

wilcox.test(
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='high' ) %>% select(air_temperature_celsius) %>% fn() ),
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment=='low' ) %>% select(air_temperature_celsius) %>% fn() )
)

# between treatments should be 0

wilcox.test(
(dplyr::filter(envsample, Site=='Tuebingen' ,Treatment %in% c('high', 'low')) %>% select(air_temperature_celsius) %>% fn() ),
(dplyr::filter(envsub, Site=='Tuebingen' ,Treatment=='out' ) %>% select(air_temperature_celsius) %>% fn() )
)



################################################################################
#
#
# downy=-10
# upy=55
# if (counter==1){
# library(lubridate)
#
#
# par(mar = c(8, 4, 4, 4) + 0.1)
# plot(y = data[,columnames[2]] ,x=data$capture_ts, ylab=columnfinalnames[2], frame.plot=T,     cex=0,xlab="",axes=F,col="blue",xlim = as.POSIXct(c(range(data$capture_ts) )+ c(hours(0),hours(3))) ,ylim=c(downy, upy), xaxt="n")
#
#   # lines(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,lty=as.numeric(shapewater))
#   # lines(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,lty=as.numeric(shapetemp))
#   points(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,pch=as.numeric(shapewater))
#   points(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,pch=as.numeric(shapetemp))
# # add  axis and grid
#
#     axis(2, ylim=c(downy,upy),at=seq(downy,upy,5),col="black",las=1)
#   axis(4, ylim=c(downy,upy),at=seq(downy,upy,5),col="black",las=1)
#   mtext("Temp (C)",side=4,line=2.5)
#   labelspretty=pretty(range(data$capture_ts),15)
#
#   abline(h=seq(downy,upy,5), v=labelspretty, col="gray", lty=3)
#
# # add the sensor name in the water lines
# text(y = tail(data[,columnames[2]], n=1) ,x= as.POSIXct(tail(data$capture_ts, n=1)+hours(3))   ,paste(posnumber,name,sep="-"))
#
# # labels axis
#   labelspretty=paste( format(labelspretty,format="%Y-%m-%d %H"), "H",sep="" )
#   axis.POSIXct(1,at=pretty(range(data$capture_ts),15),labels=labelspretty,las=2)
#
# # legend
# legend("topleft",legend = c("LOW_water","LOW_temp","HIGH_water","HIGH_temp"),col=c(colorfile[1,2:3],colorfile[2,2:3]) ,bty="n",title="")
#
# }
# else{
#   # lines(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,lty=as.numeric(shapewater))
#   # lines(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,lty=as.numeric(shapetemp))
#   points(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,pch=as.numeric(shapewater))
#   points(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,pch=as.numeric(shapetemp))
#
#   # add the sensor name in the water lines
# text(y = tail(data[,columnames[2]], n=1) ,x= as.POSIXct(tail(data$capture_ts, n=1)+hours(3))   ,paste(posnumber,name,sep="-"))
#
#
# }
# } # end file
#
#
# ################################################################################
# ### Info on columns of spreadsheet
#
# columnpositions<-c(2,3,4)
# columnames<-c("par_umole_m2s","vwc_percent","air_temperature_celsius")
# columnfinalnames<-c("PAR (photons /sec m2 ) ", "Water content (%)","air Temp (C)")
#
# # collumnames<-c("par_umole_m2s","vwc_percent","ecp_ms_cm","air_temperature_celsius")
# # "Fertilizer (conductivity mS/cm)","
#
#
# ### Merge files and information of sensors ####
#
# sensor_files_merge<-merge(sensor_files,by.x="name",  sensor_info, by.y="Sensor_name" )
# # sensor_files$name
# # sensor_info$Sensor_name
#
# setdiff(sensor_files$name,sensor_info$Sensor_name)
# print(paste("total number of sensors:"  ,as.character(dim(sensor_files_merge)[1 ]) ) )
#
# #### Colors & shapes of sensors  ####
#
# # colorfile<-matrix(c('low','dodgerblue2','indianred1',
# #                     'high','navy','darkred',
# #                     "out","grey","grey") ,ncol=3,nrow=3,byrow=T)
# # colnames(colorfile)<-c("Treatment","water","temp")
# #
# # shapefile<-matrix(c('low',1,1,
# #                     'high',1,1,
# #                     "out",1,1) ,ncol=3,nrow=3,byrow=T)
# # colnames(shapefile)<-c("Treatment","water","temp")
#
# ### Parse files and plot ### Tuebingen #####
# site="Tuebingen"
# for (site in unique(sensor_files_merge$Site) ) {
#
# finalfilelist= subset( sensor_files_merge, sensor_files_merge$Site==site)
# rownames(finalfilelist)=finalfilelist$name
#
# numberfiles =dim(finalfilelist)[1]
# print (paste("Sensors in",site,sep=" ") )
# numberfiles
#
#
# # working_directory="~/GoogleDrive/PhD_docs/experiment_1001G/experiment_field_1001g_share/climate_sensors/"
# working_directory="../environment/"
#
# # pdf(paste(working_directory,"water_temp_series_TUEBINGEN.pdf",sep="/"),height =8 ,width = 15,useDingbats = F)
# pdf(paste(working_directory,"/water_temp_series_",site,".pdf",sep=""),height =8 ,width = 15,useDingbats = F)
#
# counter=0
# for (sensor in finalfilelist$name )   {
#   file=as.character(finalfilelist[sensor,"filename"])
#   name=as.character(finalfilelist[sensor,"name"])
#   posnumber= as.character(finalfilelist[sensor,"Position"])
#   datestart= as.character(finalfilelist[sensor,"Date"])
#
#   data<-read.csv(file)
#   head(data);tail(data)
#
#   if( 0 %in% dim(data) ){
#     # break
#     print (paste(name," Flower Power data EMPTY ! "))
#     next
#   }else{
#     print (paste(name," Plotting Flower Power time series"))
#     counter=counter+1
#   }
#
#   # make the date series type
#   data$capture_ts<-strptime(data$capture_ts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
#   class(data$capture_ts)
#   data<-subset(data,data$capture_ts >= as.POSIXct(datestart) )
#
#     ## summary data
# #   library(plyr)
# #   data$week <- format(data$capture_ts, format="%Y-%U")
# #   ddply(data, .(week), summarize, air_temperature_celsius=mean(air_temperature_celsius))
# #   ddply(data, .(week), summarize, vwc_percent=mean(vwc_percent))
# #   summary(data$vwc_percent)
#
# colortemp= as.character(finalfilelist[sensor,"Color_temp"])
# colorwater= as.character(finalfilelist[sensor,"Color_water"])
#
# shapetemp<-1
# shapewater<-1
#
# downy=-10
# upy=55
# if (counter==1){
# library(lubridate)
#
#
# par(mar = c(8, 4, 4, 4) + 0.1)
# plot(y = data[,columnames[2]] ,x=data$capture_ts, ylab=columnfinalnames[2], frame.plot=T,     cex=0,xlab="",axes=F,col="blue",xlim = as.POSIXct(c(range(data$capture_ts) )+ c(hours(0),hours(3))) ,ylim=c(downy, upy), xaxt="n")
#
#   # lines(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,lty=as.numeric(shapewater))
#   # lines(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,lty=as.numeric(shapetemp))
#   points(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,pch=as.numeric(shapewater))
#   points(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,pch=as.numeric(shapetemp))
# # add  axis and grid
#
#     axis(2, ylim=c(downy,upy),at=seq(downy,upy,5),col="black",las=1)
#   axis(4, ylim=c(downy,upy),at=seq(downy,upy,5),col="black",las=1)
#   mtext("Temp (C)",side=4,line=2.5)
#   labelspretty=pretty(range(data$capture_ts),15)
#
#   abline(h=seq(downy,upy,5), v=labelspretty, col="gray", lty=3)
#
# # add the sensor name in the water lines
# text(y = tail(data[,columnames[2]], n=1) ,x= as.POSIXct(tail(data$capture_ts, n=1)+hours(3))   ,paste(posnumber,name,sep="-"))
#
# # labels axis
#   labelspretty=paste( format(labelspretty,format="%Y-%m-%d %H"), "H",sep="" )
#   axis.POSIXct(1,at=pretty(range(data$capture_ts),15),labels=labelspretty,las=2)
#
# # legend
# legend("topleft",legend = c("LOW_water","LOW_temp","HIGH_water","HIGH_temp"),col=c(colorfile[1,2:3],colorfile[2,2:3]) ,bty="n",title="")
#
# }
# else{
#   # lines(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,lty=as.numeric(shapewater))
#   # lines(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,lty=as.numeric(shapetemp))
#   points(y = data[,columnames[2]] ,x=data$capture_ts,col=colorwater,pch=as.numeric(shapewater))
#   points(y = data[,columnames[3]] ,x=data$capture_ts,col=colortemp,pch=as.numeric(shapetemp))
#
#   # add the sensor name in the water lines
# text(y = tail(data[,columnames[2]], n=1) ,x= as.POSIXct(tail(data$capture_ts, n=1)+hours(3))   ,paste(posnumber,name,sep="-"))
#
#
# }
# } # end file loop
#
# dev.off()
#
# }
