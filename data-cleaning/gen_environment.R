### Script to read all environmental data from my FLower Power sensors ###

library(lubridate)

### Define information of sensors ####

working_directory="../environment"
working_directory="data-raw/climatesensors/"

#
# if ( "Google Drive" %in%  dir("~") ) {
#   print("Google Drive exists\n")
#   root="~/Google Drive"
# }else if( "GoogleDrive" %in% dir("~")){
#   print("GoogleDrive exists\n")
#   root="~/GoogleDrive"
#
# }else{
#   print("No Google Drive folder in home directory")
# }

# setwd(paste(root,working_directory,sep="") )

root=""

setwd(paste(root,working_directory,sep="") )
getwd()


treatments_index<-matrix(ncol=2,nrow=1)
sensor_info<-read.table("Sensor_list_position.txt",header=T)
print("sensor information loadeed")
sensor_info
sensor_info<-na.omit(sensor_info)
colnames(sensor_info)[1]<-c("Site")

### Define working directory of downloaded data #####

# working_directory="/PhD_docs/experiment_1001G/experiment_field_1001g_share/climate_sensors/FlowerPower-Tools-dev/CSV-Dump"
working_directory="."

setwd(paste(root,working_directory,sep="") )

### List of sensors files ####

command=paste("ls ", root,working_directory,"/A0143D**.csv","> sensor_list_FILES.txt",sep="")
system(command = command)

sensor_files<-(read.table("sensor_list_FILES.txt"))

parsefile<-function(filelist){
  parsedlist=c()
  for (z in 1:dim(filelist)[1] ){
library("stringr")
file=as.character(filelist[z,]  )
pars=str_sub(file,start=-8)
pars=str_sub(pars,end = -5)
parsedlist=append(parsedlist,pars)
  }
return(parsedlist)
}

sensor_files$names=parsefile(sensor_files)
colnames(sensor_files)=c("filename","name")

# print(sensor_files)

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
