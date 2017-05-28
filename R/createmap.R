
createmap<-function(mapcol="seashell3",backcol="white",countrycol=mapcol,xlim=c(-200,+200),ylim=c(-60,+80)){
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
