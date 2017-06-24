environment_sensors_info<-function(overwrite=F){

  sensorinfo=read.delim('data-raw/environment_sensors_info.tsv',header = T,stringsAsFactors = F)

  devtools::use_data(sensorinfo,overwrite)

}

