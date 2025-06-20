#--------- DATA ------------
User="Matias"

if(!exists('handl_OneDrive'))
{
  if(User=="Matias") source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')
}

if(User=="Matias") source(handl_OneDrive("Analyses/Population dynamics/Git_Stock.assessments/NextGeneration.R"))

#1. Sharks data base
if(User=="Matias") source(handl_OneDrive('Analyses/SOURCE_SCRIPTS/Git_other/Source_Shark_bio.R'))

#2. Species list
All.species.names=read.csv(handl_OneDrive("Data/Species.code.csv"),stringsAsFactors=FALSE, fileEncoding="latin1")


HNDL=handl_OneDrive('Analyses/Parks Australia/outputs/2022 Project/Longlining/')
le.paste=function(x) paste(HNDL,x,sep='')
source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_Population.dynamics/fn.fig.R"))
Do.tiff="YES"
Do.jpeg="NO"
source(handl_OneDrive('Analyses/SOURCE_SCRIPTS/Git_other/ggplot.themes.R'))

library(tidyverse)
library(Hmisc)

#---------Basic manipulation of PA observer data------------
DATA=DATA[grep("PA", DATA$SHEET_NO), ]%>%
  filter(year>=2022 & year<=2025)
names(DATA)=tolower(names(DATA))

DATA=DATA%>%
  mutate(type=ifelse(type=='Elasmo','Shark',type),
         length=ifelse(!is.na(fl),round(fl),ifelse(is.na(fl) & !is.na(tl),round(tl),NA)),
         common_name=capitalize(tolower(common_name)))

#---------Map------------
do.map=FALSE
if(do.map)
{
  library(grDevices)
  library(PBSmapping)
  Bathymetry_120=read.table(handl_OneDrive("Data/Mapping/get_data112_120.cgi"))
  Bathymetry_138=read.table(handl_OneDrive("Data/Mapping/get_data120.05_138.cgi"))
  Bathymetry=rbind(Bathymetry_120,Bathymetry_138)
  Bathymetry=Bathymetry%>%filter(V2<=(-26))
  Bathymetry=Bathymetry[order(Bathymetry$V1,Bathymetry$V2),]
  xbat=sort(unique(Bathymetry$V1))
  ybat=sort(unique(Bathymetry$V2)) 
  reshaped=as.matrix(reshape(Bathymetry,idvar="V1",timevar="V2",v.names="V3", direction="wide"))
  
  library(rgdal)
  SDGDLL_zone1=readOGR(handl_OneDrive("Data/Mapping/Shark_shape_files/SDGDLL_zone1.shp"), layer="SDGDLL_zone1") 
  SDGDLL_zone2=readOGR(handl_OneDrive("Data/Mapping/Shark_shape_files/SDGDLL_zone2.shp"), layer="SDGDLL_zone2") 
  WCDGDLL=readOGR(handl_OneDrive("Data/Mapping/Shark_shape_files/WCDGDLL.shp"), layer="WCDGDLL") 
  
  #Spatial range
  #Lat.range=round(c(min(DATA$mid.lat)-1,max(DATA$mid.lat)+1))
  #Long.range=round(c(min(DATA$mid.long)-1,max(DATA$mid.long)+1))  
  Lat.range=c(-36,-29)
  Long.range=c(113,119)  
  
  seq.Lat=seq(Lat.range[1],Lat.range[2])
  seq.Long=seq(Long.range[1],Long.range[2])
  
  Sites=DATA%>%distinct(sheet_no,mid.lat,mid.long,method)%>%
    mutate(Col=ifelse(method=='GN', "#00BFC4","#F8766D"))
  #bring in shape file
  data(worldLLhigh)
  
  fn.fig(le.paste("Figure 1_map site area"),1600,2400)
  par(mar = c(0, 0, 0, 0),oma=c(0,0,0,0),mgp=c(.1, 0.15, 0))
  
  #plot shots' Sampling site locations 
  plotMap(worldLLhigh, xlim=Long.range,ylim=Lat.range,axes=F,
          col="dark grey",tck = 0.025, tckMinor = 0.0125, xlab="",ylab="")
  
  #add zones
  plot(WCDGDLL,add=T,col="aquamarine3")
  text(114,-31,"West coast",srt=90,cex=1.5)
  
  plot(SDGDLL_zone1,add=T,col="deepskyblue3")
  text(114.5,-35,"(Zone 1)",cex=1.5,srt=-45)
  
  plot(SDGDLL_zone2,add=T,col="chartreuse3")
  text(118,-35.75,"(Zone 2)",cex=1.5)
  
  
  points(Sites$mid.long,Sites$mid.lat,col='black',pch=21,bg=Sites$Col,cex=1.25)
  
  #add bathymetry
  contour(xbat, ybat, reshaped[,2:ncol(reshaped)],ylim=plotlat[[i]],xlim=plotlong[[i]], zlim=c(-1,-300),
          nlevels = 3,labcex=1,lty = c(1,2,3),col=c(rep("black",3)),add=T)
  axis(side = 1, at =seq.Long, labels = seq.Long, tcl = .5,las=1,cex.axis=0.9)
  axis(side = 2, at = seq.Lat, labels = -seq.Lat,tcl = .5,las=2,cex.axis=0.9)
  
  mtext(expression(paste("Latitude (",degree,"S)",sep="")),side=2,line=1.25,las=3,cex=1.5)
  mtext(expression(paste("Longitude (",degree,"E)",sep="")),side=1,line=1.5,cex=1.5)
  legend('bottomleft',c("Longline"),pch=21,
         pt.bg=c("#F8766D"),bty='n',cex=1.25)
  box()
  
  #inset Australia
  par(fig=c(.35,.95,.35,.95), new = T,mgp=c(.1,.4,0),mai=c(.01,01,.01,.01))
  plotMap(worldLLhigh, xlim=c(113,155), ylim=c(-44,-11),col="grey80", axes=F, xlab="", ylab="",
          border="black",bg="white",plt = NULL)
  
  text(122,-24,"Western",col="black",cex=1,font=2.5)
  text(122,-27,"Australia",col="black",cex=1,font=2.5)
  polygon(x=c(Long.range,rev(Long.range)),
          y=c(rep(Lat.range[1],2),rep(Lat.range[2],2)),
          col=rgb(.1, .6, .1, alpha = .4),border = "black")
  dummy.ln=c(152.8,154.6)
  dummy.la=c(-12.03,-10.94)
  polygon(x=c(dummy.ln,rev(dummy.ln)),
          y=c(rep(dummy.la[1],2),rep(dummy.la[2],2)),
          col='white',border = 'white')
  lines(x=c(129,129),y=c(-31.64,-15),lty=2)
  dev.off()
  
}  

#---------Table of shots specs------------
write.csv(DATA%>%
            distinct(sheet_no,year,month,botdepth,soak.time,n.hooks)%>%
            rename(Shot=sheet_no,
                   'Soak time (hours)'=soak.time,
                   'Number of hooks'=n.hooks,
                   'Depth (m)'=botdepth,
                   Month=month,
                   Year=year)%>%
            arrange(Year,Month)%>%
            mutate(Shot=row_number()),
          le.paste("Table 1_Shot specs.csv"),row.names = F)


#---------Table of species and catch rates------------
Numbers=DATA%>%
              group_by(common_name,scientific_name,type)%>%
              tally()%>%
              arrange(type,-n)%>%
              dplyr::select(-type)
Catch.rates=DATA%>%
              group_by(sheet_no,common_name)%>%
              tally()%>%
              ungroup()%>%
              left_join(DATA%>%
                          distinct(sheet_no,soak.time,n.hooks),
                        by='sheet_no')%>%
              mutate(effort=(soak.time*n.hooks)/1000,
                     cpue=n/effort)%>%
              group_by(common_name)%>%
              summarise(Min.cpue=round(min(cpue),2),
                        Max.cpue=round(max(cpue),2))
TAB1=left_join(Numbers,Catch.rates,by='common_name')%>%
            rename('Common name'=common_name,
                   'Scientific name'=scientific_name,
                   'Min cpue (n/1000 hook hour)'=Min.cpue,
                   'Max cpue (n/1000 hook hour)'=Max.cpue)
write.csv(TAB1,le.paste("Table 2_All species.csv"),row.names = F)

#---------Length compositions------------
DATA%>%
  filter(!is.na(length))%>%
  mutate(botdepth=as.character(100*(round(botdepth/100))),
         length=factor(10*(round(length/10)),levels=seq(20,250,10)))%>%
  group_by(botdepth,common_name,length)%>%
  summarise(number=sum(number))%>%
  ggplot(aes(x=length,y=number,fill=botdepth)) +
  geom_bar(stat='identity',position="dodge")+
  facet_wrap(~common_name,scales='free')+
  xlab('Length (cm)')+ylab('Frequency')+
  theme_PA(axs.t.siz=16,axs.T.siz=18)+
  theme(legend.position="top",
        legend.title=element_blank(),
        plot.margin=margin(.1,.5,.1,.1, "cm"))+
  labs(caption = "Length: TL for scalefish, FL for sharks")
#  scale_y_continuous(labels = scales::number_format(accuracy = 1))
ggsave(le.paste("Figure 2_size frequency.tiff"),width = 11,height = 8,compression = "lzw")

