# SCRIPT FOR ANALYSING AND REPORTING PARKS AUSTRALIA PROJECT 2019

#notes: For all cameras, C. obscurus, C. brachyurus and C. spp all combined under C. obscurus
#         as they cannot be distinguished
# to do: 
#   Once all data entered and validated, copy data set to C drive from M drive and move M drive to U/Shark databases
#   Review calculations with Jack '#4. Rates of depredation, bait loss and drop outs'
#   EM vs OM, which shots are comparable? some of the EM videos run out of battery..exclude those
#   Fish depredation. Calculate also per km gn hours and per caught fish. Same for other behaviours
#   MISSING: Depredation codes from Underwater video: Codes 10 Predated on & 12 Caught while predating
#            There is also depredation data from Cameras 1 & 2 in the comments section (videos reviewed by Sarah and anyone else??)
#            REVIEW & UPDATE issues    
#    consider manyglm instead of adonis

rm(list=ls(all=TRUE))

  library(devtools)
  library(tidyverse)
  library(dplyr)
  library(readxl)
  library(ggrepel)
  library(ggpubr)
  library(rlang)
  library(MASS)
  library(ggmosaic)
  library(Hmisc)
  library(gridExtra)
  library(stringr)
  library(extdplyr)
  library(ggpubr)
  library(ggridges)
  library(vegan)
  library(pairwiseAdonis)
  library(overlapping)
  library(tweedie)
  library(mgcv)
  library(emmeans)  #for model predictions
  library(doParallel)
  library(flextable)
  library(webshot)
  library(visNetwork)
  library(ozmaps)
  library(sf)
  library(ggpmisc)
  library(forecast)
  library(performance)
  library(ggforce)
  library(scales)
  library(igraph)
  library(janitor)
  library(lubridate)

options(stringsAsFactors = FALSE,dplyr.summarise.inform = FALSE) 

#--------- DATA ------------
User="Matias"
#User <- "Sarah"
# User="Abbey"

if(!exists('handl_OneDrive'))
{
  if(User=="Matias") source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')
  if(User=="Sarah")    handl_OneDrive=function(x)paste("C:/Users/S.Jesso/OneDrive - Department of Primary Industries And Regional Development/Documents/GitHub",x,sep='/')
  if(User=="Abbey")    handl_OneDrive=function(x)paste('C:/Users/A.Shutt/OneDrive - Department of Primary Industries And Regional Development/Masters_Rscript_2023/GitHub',x,sep='/')
}




#1. Sharks data base
if(User=="Matias") source(handl_OneDrive('Analyses/SOURCE_SCRIPTS/Git_other/Source_Shark_bio.R'))
if(User=="Sarah") source(handl_OneDrive("Git_other/Source_Shark_bio.R"))
if(User=="Abbey") source(handl_OneDrive('Git_other/Source_Shark_bio.R'))
 
 #2. PA - Video
if(User=="Matias") setwd(handl_OneDrive('Analyses/Parks Australia/Git_parks.australia'))
if(User=="Abbey") setwd("C:/Users/A.Shutt/OneDrive - Department of Primary Industries And Regional Development/Masters_Rscript_2023/GitHub")
if(User=="Sarah") setwd("~/GitHub/shark_analysis")
 # Clean data
source("underwater_clean_loop.R")


#3. Species list
if(User=="Matias") All.species.names=read.csv(handl_OneDrive("Data/Species.code.csv"))
if(User=="Sarah") All.species.names=read.csv(handl_OneDrive("Data/Species.code.csv"))
if(User=="Abbey") All.species.names=read.csv(handl_OneDrive("Git_other/Species.code.csv"))

setwd(hndl.Sarah)
if(User=="Abbey") setwd("C:/Users/A.Shutt/OneDrive - Department of Primary Industries And Regional Development/Masters_Rscript_2023")
CAAB.gillnet.txt=read.delim('CAAB gillnet.txt', header = TRUE, sep = "\t", dec = ".")

#4. PA - TEPS interactions recorded by Observers
setwd(paste0(hndl.Sarah,'/Data'))
if(User=="Abbey") setwd("C:/Users/A.Shutt/OneDrive - Department of Primary Industries And Regional Development/Masters_Rscript_2023")
TEPS <- read_excel("TEPS interactions.xlsx", sheet = "Sheet1",skip = 1)


#5. PA - number of hook combinations used and lost in PA project
Hook.combos <- read_excel("Hook count.xlsx", sheet = "Sheet1",skip = 0)
Lost.snoods<- read_excel("Broken hook specs.xlsx",sheet = "Sheet1")

#6. PA - battery life
setwd(hndl.Sarah)
D1.cam.battery <- read.csv("Deck 1 final metadata check.csv") %>% 
  dplyr::select(x, Full.LL, Full.GN, More.comments) %>% 
  rename(`DIPRD code` = x)
D2.cam.battery <- read.csv("Deck 2 final metadata check.csv")%>% 
  dplyr::select(x, Full.LL, Full.GN, More.comments) %>% 
  rename(DPIRD.code = x)



Do.tiff="YES"
Do.jpeg="NO"

if(User=="Matias")
{
  source(handl_OneDrive('Analyses/SOURCE_SCRIPTS/Git_other/ggplot.themes.R'))
  source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_other/Smart_par.R"))
  source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_Population.dynamics/fn.fig.R"))
  
}

if(User=="Abbey") 
{
  source(handl_OneDrive("Git_other/Analyses/fn.fig.R"))
  source(handl_OneDrive('Git_other/ggplot.themes.R'))
  source(handl_OneDrive("Git_other/Smart_par.R"))
}

if(User=="Sarah")
{
  source(handl_OneDrive('Git_other/ggplot.themes.R'))
  source(handl_OneDrive("Git_other/Smart_par.R"))
  source(handl_OneDrive("Git_Population.dynamics/fn.fig.R"))
}

if(User=="Matias")  HNDL=handl_OneDrive('Analyses/Parks Australia/outputs/')
if(User=="Abbey")  HNDL=handl_OneDrive('Git_other/Analyses/Parks_Australia/outputs/')
if(User=="Sarah")  HNDL=handl_OneDrive('Outputs/')
le.paste=function(x) paste(HNDL,x,sep='')

#---------CONTROL SECTION------------
explr.dat.entry=FALSE
do.len_len=FALSE
do.Historic=FALSE
do.soc.econ.papr=FALSE

distance.roller.spreader.Anthony=4.3  #in metres
distance.roller.spreader.Tim=NA  #in metres
distance.roller.spreader.Nils=4  #in metres
distance.roller.spreader=mean(c(distance.roller.spreader.Anthony,distance.roller.spreader.Tim,distance.roller.spreader.Nils),na.rm=T)

#mesh.deep.Anthony=3.3                   #in metres  (20 meshes of 6.5 inch, source: Jeff Cooke)

metres.observed=5 # average metres observed by underwater cameras.        REVIEW THIS
hooks.observed=2   #average number of hooks observed by underwater cameras.        REVIEW THIS

hours.underwater.ll='1329'          #total number of hours of longline underwater footage analysed   #UPDATE ALL THESE
hours.underwater.gn='2245'         #total number of hours of gillnet underwater footage analysed  
hours.subsurface.ll='27'           #total number of hours of subsurface footage longline analysed  
hours.subsurface.gn='54'           #total number of hours of subsurface footage gillnet analysed  
hours.deck1.ll='42'               #total number of hours of deck camera 1 longline footage analysed  
hours.deck1.gn='84'               #total number of hours of deck camera 1 gillnet footage analysed  
hours.deck2.ll='41'               #total number of hours of deck camera 2 longline footage analysed  
hours.deck2.gn='82'               #total number of hours of deck camera 2 gillnet footage analysed  

No.good.water.column=c('PA0059','PA0061') # data sheets no good for looking at Composition around weight or float   #UPDATE IF APPROPRIATE

Main.species=c("Dusky shark","Whiskery shark","Gummy shark","Sandbar shark",
               "Queen snapper","Blue groper","West Australian dhufish","Pink snapper")
names(Main.species)=c(18003,17003,17001,18007,
                      377004,384002,320000,353001)
Length.weight.main=data.frame(common_name=Main.species,
                              a_weight=c(1.23e-5,1.63e-5,1.36e-6,6e-6,
                                         1.627539e-05,1.858271e-05,2.451526e-05,1.637477e-05),
                              b_weight=c(2.855,2.733,3.224263,2.9698,
                                         2.924,3.034411,2.91928,2.9251),
                              a_FL.to.TL=c(1.2062,1.0700731,1.0952846,1.1256996,
                                           rep(NA,4)),
                              b_FL.to.TL=c(1.9133,7.818082,3.8499,6.182718,
                                           rep(NA,4))) #Norris et al 2016 & Smallwood et al 2018 for scalefish

Length.weight.discards=data.frame(common_name=c("Buffalo bream","Dusky morwong",
                                                "Port Jackson","Wobbegongs",
                                                "Fiddler ray","Southern eagle ray","Stingrays"),
                                  a_weight=c(2.261e-5,2e-5,
                                             1.630e-7,3.645e-3,
                                             1.3e-6,3.6e-06,2.03e-05),
                                  b_weight=c(3.055,2.902,
                                             3.9,1.767,
                                             3.2097,3.37,3),
                                  a_FL.to.TL=rep(NA,7),
                                  b_FL.to.TL=rep(NA,7))  #Simpfendorfer & McAuley 2003 & Coulson pers com


#from Fishbase and Smallwood et al 2018
Additional.length.weight.economics=
  data.frame(
    common_name=c("Baldchin groper", "Breaksea cod","Boxfishes","Boarfishes","Banded sweep","Coral trout",
                  "Foxfish","Gurnards","Harlequin fish","John Dory", "Knifejaw", "Lings",
                  "Moonlighter","Mulloway", "Parrotfishes","Nannygai","Samson fish","Sea sweep",
                  "Tunas","Skipjack trevally","Leatherjackets",
                  "Common blacktip shark", "Blacktip sharks", "Bronze whaler","Hammerheads","Scalloped hammerhead","Smooth hammerhead",
                  "Spinner shark","Shortfin mako","Pencil shark","Common sawshark","Spotted wobbegong","Western wobbegong","Other sharks"),
    a_weight=c(3.504558e-05,1.242766e-05,6.46e-06,1.85e-05,7.63e-06,2.292184e-05,
               9.75e-06,9.75e-06,1.83e-05,2.29e-05,2.66e-05,1.1e-06,
               5.255e-05,6.3e-06,2.09e-05,1.274212e-05,4.07e-05,7.63e-06,
               2.14e-05,2.51e-05,2.63e-05,
               1.07e-05,1.07e-05,1.04E-05,1.62e-05,7.75E-06,1.62E-05,
               1.13e-06,5.24E-06,2.29E-05,2.00E-07,3.65E-03,2.19E-06,1e-5),
    b_weight=c(2.86,3.099,3.168,2.931,3.136,2.88,
               3,3,2.891,2.89,3,3.37,
               2.82, 3.05,2.96,3,2.67,3.136,
               2.93,2.86,2.85,
               2.9,2.9,2.9,2.721,3.0669,2.7208,
               3.3,3.1407,2.6125,3.4617,1.757,3.23,3),
    a_FL.to.TL=c(rep(NA,21),rep(mean(Length.weight.main$a_FL.to.TL[1:4]),13)), 
    b_FL.to.TL=c(rep(NA,21),rep(mean(Length.weight.main$b_FL.to.TL[1:4]),13))) 


Length.weight=rbind(Length.weight.main,Length.weight.discards,Additional.length.weight.economics)

Shark.palette=c("firebrick4","red",'yellow')
Teleost.palette=c('steelblue','deepskyblue','royalblue4')


#cpue.scaler=1000  #display cpue as km gn hour / 1000 hook hour
cpue.scaler=1

Min.N.drop.out=25         #minimum sample size for drop out analyses
Min.N.gaff=10              #minimum sample size for gaffing
Min.obs.comp.wei.flot=5   #minimum sample size for comp around weight and floats
Minobs.per=5              #minimum smaple size for percentage diff OM vs EM



# -----------------Define TEPS---------------------------------------------
#note: UPDATE IF NEW TEP SPECIES OBSERVED
TEPS_Shark.rays=c(37008001,37010003,37035001,37035002)
TEPS_marine.mam=4.1e+07:4.115e+07
TEPS_seabirds=4.0e+07:4.09e+07
TEPS_reptiles=3.9e7:3.99e+07          #3.747e7:3.99e+07
TEPS.codes=c(TEPS_Shark.rays,TEPS_marine.mam,TEPS_seabirds,TEPS_reptiles)
TEPS.names=data.frame(Name=c('Grey nurse shark','White shark','Smooth stingray','Black stingray','Oversized dusky shark',
                             'Shearwaters','Cormorants','Little black cormorant',
                             'Pied cormorant','Seabird','Pacific gull','Yellow-nosed albatross','Brown skua',
                             'Gulls',
                             'Australian sea-lion','Humpback whale',
                             'Green turtle'),
                      Code=c(37008001,37010003,37035001,37035002,37018003,
                             40041050,40048000,40048005,
                             40048006,40000000,40128014,40040003,40128908,
                             40128000,
                             41131005,41112006,
                             39020002),
                      Colr=c("brown1","firebrick4","darkorange","chocolate3",'darksalmon',
                             "darkolivegreen","greenyellow","olivedrab",
                             "chartreuse3","forestgreen","darkolivegreen3",'darkseagreen4','darkgreen',
                             "lightgreen",
                             "steelblue","deepskyblue2",
                             "darkorchid1"))
TEPS.cols=TEPS.names$Colr
names(TEPS.cols)=TEPS.names$Name


#---------Manipulate Species names------------
suppressWarnings({
  All.species.names=All.species.names%>%
    mutate(Code=ifelse(Taxa=='Elasmobranch'&!is.na(CAAB_code) & nchar(CAAB_code)==4,
                       as.numeric(paste('3700',CAAB_code,sep='')),
                ifelse(Taxa=='Elasmobranch'&!is.na(CAAB_code) & nchar(CAAB_code)==5,
                              as.numeric(paste('370',CAAB_code,sep='')),
                ifelse(Taxa=='Teleost'&!is.na(CAAB_code)& nchar(CAAB_code)==5,
                       as.numeric(paste('370',CAAB_code,sep='')),
                ifelse(Taxa=='Teleost'&!is.na(CAAB_code)& nchar(CAAB_code)==6,
                      as.numeric(paste('37',CAAB_code,sep='')),
                NA))))) 
})
# All.species.names=All.species.names%>%
#   mutate(Code=ifelse(COMMON_NAME=='Scaly mackerel',37085018,Code))


#Add Codes used by Jack   NOTE: add here any species not in All.species.names
Addis=All.species.names[1:2,]%>%
  mutate(Species=rep('',),
         COMMON_NAME=c('Whaler sharks','Stingrays'),
         SCIENTIFIC_NAME=c('Carcharhinus spp','Dasyatis spp'),
         Taxa=rep('Elasmobranch',2),
         CAES_Code=rep(NA,2),
         CAAB_code=rep(NA,2),
         Code=c(37018904,37035901))
All.species.names=rbind(All.species.names,Addis)

#Video.net.interaction <- Video.net.interaction %>% filter(!Interaction =="Missing data")
#video.longline.interactions <- Video.longline.interaction%>%filter(!Interaction =="Missing data")


CAAB.gillnet.txt=CAAB.gillnet.txt%>%
  rename(Code=CAAB.CODE)%>%
  mutate(Species='',COMMON_NAME='',CAES_Code='',CAAB_code='',
         SCIENTIFIC_NAME=paste(GENUS,SPECIES))%>%
  filter(!SPECIES=='')%>%
  filter(!Code%in%All.species.names$Code)%>%
  mutate(Taxa=ifelse(Code%in% 37005000:37041902,"Elasmobranch",
              ifelse(Code%in% 37067000:37470903,"Teleost",
              NA)))%>%
  dplyr::select(colnames(All.species.names))
  
All.species.names=rbind(All.species.names,CAAB.gillnet.txt)



#---------Manipulate PA hook size, type and snood combinations  ------------
if('no of baited hooks'%in%names(Hook.combos))  Hook.combos<-Hook.combos%>%dplyr::select(-'no of baited hooks')
if('Total deployed'%in%names(Hook.combos))  Hook.combos<-Hook.combos%>%dplyr::select(-'Total deployed')
Hook.combos<-Hook.combos%>%
  dplyr::rename(sheet_no="Sheet no",
                baiting.time='Baiting time (min)',
                baiting.crew='Baiting staff no')%>%
  filter(!is.na(sheet_no))%>%
  mutate_at(c("C10/W","C12/W","C14/W",
              "Eb10/W","Eb12/W","Eb14/W",
              "C10/M","C12/M","C14/M",
              "Eb10/M","Eb12/M","Eb14/M"), as.numeric)%>%
  data.frame

Hook.combos<-Hook.combos%>%
  mutate(hooks.baited=C10.W+C12.W+C14.W+Eb10.W+Eb12.W+Eb14.W+
           C10.M+C12.M+C14.M+Eb10.M+Eb12.M+Eb14.M)

#---------Basic manipulation of PA observer data------------
DATA=DATA[grep("PA", DATA$SHEET_NO), ]%>%
  filter(year>=2020)
DATA=DATA%>%
  dplyr::rename(IDL=TrunkL,
                Rel.cond="RELEASE CONDITION")%>%
  mutate(COMMENTS=ifelse(is.na(COMMENTS),'',COMMENTS),
         COMMENTS=paste(COMMENTS,NewComments))%>%
  dplyr::select(SHEET_NO,LINE_NO,RECORDER,Mid.Lat,Mid.Long,Lat.round,Long.round,zone,date,Day,Month,year,BOAT,BLOCK,SKIPPER,BOTDEPTH,
                Set.time,Haul.time,Set.time.end,Haul.time.end,Set.time.avg,Haul.time.avg,SOAK.TIME,
                BaitSpeciesId,Method,MESH_SIZE,MESH_DROP,NET_LENGTH,
                N.hooks,HookLocation,HookRemoved,HookType,HookSize,WireTrace,Rel.cond,
                SPECIES,COMMON_NAME,SCIENTIFIC_NAME,Taxa,RetainedFlag,SEX,Number,TL,FL,PL,IDL,
                BloodFlag,FinClipFlag,MuscleFlag,Lactate,BleedingFlag,BAG_NO,COMMENTS,COMMENTS.hdr)
names(DATA)=tolower(names(DATA))
DATA=DATA%>%
  mutate(hooktype=case_when(hooktype%in%c('Circular','Offset circular')~'circular',
                            hooktype=='EZ-baiter kerbed'~'Ezb'),
         hooksize=as.numeric(substr(hooksize,1,2)),
         skipper=ifelse(skipper%in%c("Tim","TIM"),"Tim Goodall",skipper))


#---------Add number of hook combos to PA observer data------------
#note: only adding number of hooks recovered, either lost hooks are removed from calculations of cpue
DATA=DATA%>% 
  left_join(Hook.combos%>%   #remove lost hooks
              left_join(Lost.snoods%>%
                          filter(!is.na(Sheet.no))%>%
                          `colnames<-`(paste(colnames(Lost.snoods),'.lost',sep=''))%>%
                          dplyr::select(-c(Date.lost,Unknown.lost,'Total broken.lost',Comments.lost)),
                        by=c('sheet_no'='Sheet.no.lost'))%>%
              map_if(is.numeric,~ifelse(is.na(.x),0,.x))%>%
              data.frame%>%
              mutate(C10.W=C10.W- C10.W.lost,
                     C12.W=C12.W- C12.W.lost,
                     C14.W=C14.W- C14.W.lost,
                     Eb10.W=Eb10.W- Eb10.W.lost,
                     Eb12.W=Eb12.W- Eb12.W.lost,
                     Eb14.W=Eb14.W- Eb14.W.lost,
                     C10.M=C10.M- C10.M.lost,
                     C12.M=C12.M- C12.M.lost,
                     C14.M=C14.M- C14.M.lost,
                     Eb10.M=Eb10.M- Eb10.M.lost,
                     Eb12.M=Eb12.M- Eb12.M.lost,
                     Eb14.M=Eb14.M- Eb14.M.lost)%>%
              dplyr::select(-c(Date,baiting.time,baiting.crew,Comments,
                               C10.W.lost,C12.W.lost,C14.W.lost,
                               Eb10.W.lost,Eb12.W.lost,Eb14.W.lost,
                               C10.M.lost,C12.M.lost,C14.M.lost,
                               Eb10.M.lost,Eb12.M.lost,Eb14.M.lost)),
            by='sheet_no')%>%
  mutate(Effort=case_when(method=="GN"~soak.time*net_length,
                          method=="LL"~soak.time*n.hooks),
         Effort.hook.combo=case_when(
           method=="LL" & hooktype=='circular' & hooksize==10 & wiretrace=='Yes' ~soak.time * C10.W,
           method=="LL" & hooktype=='circular' & hooksize==12 & wiretrace=='Yes' ~soak.time * C12.W,
           method=="LL" & hooktype=='circular' & hooksize==14 & wiretrace=='Yes' ~soak.time * C14.W,
           method=="LL" & hooktype=='circular' & hooksize==10 & wiretrace=='No' ~soak.time * C10.M,
           method=="LL" & hooktype=='circular' & hooksize==12 & wiretrace=='No' ~soak.time * C12.M,
           method=="LL" & hooktype=='circular' & hooksize==14 & wiretrace=='No' ~soak.time * C14.M,
           method=="LL" & hooktype=='Ezb' & hooksize==10 & wiretrace=='Yes' ~soak.time * Eb10.W,
           method=="LL" & hooktype=='Ezb' & hooksize==12 & wiretrace=='Yes' ~soak.time * Eb12.W,
           method=="LL" & hooktype=='Ezb' & hooksize==14 & wiretrace=='Yes' ~soak.time * Eb14.W,
           method=="LL" & hooktype=='Ezb' & hooksize==10 & wiretrace=='No' ~soak.time * Eb10.M,
           method=="LL" & hooktype=='Ezb' & hooksize==12 & wiretrace=='No' ~soak.time * Eb12.M,
           method=="LL" & hooktype=='Ezb' & hooksize==14 & wiretrace=='No' ~soak.time * Eb14.M,
           TRUE~NA_real_))

#replace NA number of hook combos with mean by trip by hook combo
DATA=DATA%>%
  arrange(date)%>%
  mutate(diff_days=difftime(date,lag(date,n=1),units="days"),
         diff_days=ifelse(is.na(diff_days),0,diff_days),
         delta_days=ifelse(diff_days>1,1,0),
         trip=1+cumsum(delta_days))%>%
  dplyr::select(-c(diff_days,delta_days))%>%
  group_by(trip)%>%
  mutate(C10.W.infered=ifelse(is.na(C10.W),round(mean(C10.W,na.rm=T)),C10.W),
         C12.W.infered= ifelse(is.na(C12.W),round(mean(C12.W,na.rm=T)),C12.W), 
         C14.W.infered= ifelse(is.na(C14.W),round(mean(C14.W,na.rm=T)),C14.W),
         C10.M.infered= ifelse(is.na(C10.M),round(mean(C10.M,na.rm=T)),C10.M),
         C12.M.infered= ifelse(is.na(C12.M),round(mean(C12.M,na.rm=T)),C12.M),
         C14.M.infered= ifelse(is.na(C14.M),round(mean(C14.M,na.rm=T)),C14.M),
         Eb10.W.infered= ifelse(is.na(Eb10.W),round(mean(Eb10.W,na.rm=T)),Eb10.W),
         Eb12.W.infered= ifelse(is.na(Eb12.W),round(mean(Eb12.W,na.rm=T)),Eb12.W), 
         Eb14.W.infered= ifelse(is.na(Eb14.W),round(mean(Eb14.W,na.rm=T)),Eb14.W),
         Eb10.M.infered= ifelse(is.na(Eb10.M),round(mean(Eb10.M,na.rm=T)),Eb10.M),
         Eb12.M.infered= ifelse(is.na(Eb12.M),round(mean(Eb12.M,na.rm=T)),Eb12.M), 
         Eb14.M.infered= ifelse(is.na(Eb14.M),round(mean(Eb14.M,na.rm=T)),Eb14.M))%>%
  ungroup()%>%
  mutate(Sum=ifelse(method=='LL',C10.W.infered+C12.W.infered+C14.W.infered+Eb10.W.infered+Eb12.W.infered+Eb14.W.infered+
                      C10.M.infered+C12.M.infered+C14.M.infered+Eb10.M.infered+Eb12.M.infered+Eb14.M.infered,
                    NA),
         C10.W.infered=ifelse(!n.hooks==Sum,floor(n.hooks*C10.W.infered/Sum), C10.W.infered),
         C12.W.infered=ifelse(!n.hooks==Sum,floor(n.hooks*C12.W.infered/Sum), C12.W.infered),
         C14.W.infered=ifelse(!n.hooks==Sum,floor(n.hooks*C14.W.infered/Sum), C14.W.infered),
         Eb10.W.infered=ifelse(!n.hooks==Sum,floor(n.hooks*Eb10.W.infered/Sum), Eb10.W.infered),
         Eb12.W.infered=ifelse(!n.hooks==Sum,floor(n.hooks*Eb12.W.infered/Sum), Eb12.W.infered),
         Eb14.W.infered=ifelse(!n.hooks==Sum,floor(n.hooks*Eb14.W.infered/Sum), Eb14.W.infered),
         C10.M.infered=ifelse(!n.hooks==Sum,floor(n.hooks*C10.M.infered/Sum), C10.M.infered),
         C12.M.infered=ifelse(!n.hooks==Sum,floor(n.hooks*C12.M.infered/Sum), C12.M.infered),
         C14.M.infered=ifelse(!n.hooks==Sum,floor(n.hooks*C14.M.infered/Sum), C14.M.infered),
         Eb10.M.infered=ifelse(!n.hooks==Sum,floor(n.hooks*Eb10.M.infered/Sum), Eb10.M.infered),
         Eb12.M.infered=ifelse(!n.hooks==Sum,floor(n.hooks*Eb12.M.infered/Sum), Eb12.M.infered),
         Eb14.M.infered=ifelse(!n.hooks==Sum,floor(n.hooks*Eb14.M.infered/Sum), Eb14.M.infered))%>%
  mutate(Effort.hook.combo.infered=case_when(
    method=="LL" & hooktype=='circular' & hooksize==10 & wiretrace=='Yes' ~soak.time * C10.W.infered,
    method=="LL" & hooktype=='circular' & hooksize==12 & wiretrace=='Yes' ~soak.time * C12.W.infered,
    method=="LL" & hooktype=='circular' & hooksize==14 & wiretrace=='Yes' ~soak.time * C14.W.infered,
    method=="LL" & hooktype=='circular' & hooksize==10 & wiretrace=='No' ~soak.time * C10.M.infered,
    method=="LL" & hooktype=='circular' & hooksize==12 & wiretrace=='No' ~soak.time * C12.M.infered,
    method=="LL" & hooktype=='circular' & hooksize==14 & wiretrace=='No' ~soak.time * C14.M.infered,
    method=="LL" & hooktype=='Ezb' & hooksize==10 & wiretrace=='Yes' ~soak.time * Eb10.W.infered,
    method=="LL" & hooktype=='Ezb' & hooksize==12 & wiretrace=='Yes' ~soak.time * Eb12.W.infered,
    method=="LL" & hooktype=='Ezb' & hooksize==14 & wiretrace=='Yes' ~soak.time * Eb14.W.infered,
    method=="LL" & hooktype=='Ezb' & hooksize==10 & wiretrace=='No' ~soak.time * Eb10.M.infered,
    method=="LL" & hooktype=='Ezb' & hooksize==12 & wiretrace=='No' ~soak.time * Eb12.M.infered,
    method=="LL" & hooktype=='Ezb' & hooksize==14 & wiretrace=='No' ~soak.time * Eb14.M.infered,
    TRUE~NA_real_))%>%
  mutate(
    Effort.wire.infered=case_when(
      method=="LL" & wiretrace=='Yes'~ soak.time * (C10.W.infered+C12.W.infered+C14.W.infered+Eb10.W.infered+Eb12.W.infered+Eb14.W.infered),
      method=="LL" & wiretrace=='No'~ soak.time * (C10.M.infered+C12.M.infered+C14.M.infered+Eb10.M.infered+Eb12.M.infered+Eb14.M.infered),
      TRUE~NA_real_),
    Effort.hooksize.infered=case_when(
      method=="LL" & hooksize==10~ soak.time * (C10.W.infered+C10.M.infered+Eb10.W.infered+Eb10.M.infered),
      method=="LL" & hooksize==12~ soak.time * (C12.W.infered+C12.M.infered+Eb12.W.infered+Eb12.M.infered),
      method=="LL" & hooksize==14~ soak.time * (C14.W.infered+C14.M.infered+Eb14.W.infered+Eb14.M.infered),
      TRUE~NA_real_),
    Effort.hooktype.infered=case_when(
      method=="LL" & hooktype=='circular'~ soak.time * (C10.W.infered+C12.W.infered+C14.W.infered+C10.M.infered+C12.M.infered+C14.M.infered),
      method=="LL" & hooktype=='Ezb'~soak.time * (Eb10.W.infered+Eb12.W.infered+Eb14.W.infered+Eb10.M.infered+Eb12.M.infered+Eb14.M.infered),
      TRUE~NA_real_)
  )


DATA=DATA%>%
  mutate(n.hooks=ifelse(method=='LL' & is.na(n.hooks) & !is.na(hooks.baited),
                        hooks.baited,n.hooks),
         hooks.deployed=n.hooks)

DATA=DATA%>%
  group_by(trip)%>%
  mutate(soak.time.infRD=ifelse(is.na(soak.time),round(mean(soak.time,na.rm=T)),soak.time),
         net_length.infRD=ifelse(method=='GN' & is.na(net_length),round(mean(net_length,na.rm=T)),net_length),
         n.hooks.infRD=ifelse(method=='LL' & (is.na(n.hooks)|n.hooks==0),round(mean(n.hooks,na.rm=T)),n.hooks),
         Effort.infRD=ifelse(is.na(Effort) & method=='LL',n.hooks.infRD*soak.time.infRD,
                             ifelse(is.na(Effort) & method=='GN',net_length.infRD*soak.time.infRD,
                                    Effort)))%>%
  ungroup()



#---------Explore PA observer data for data issues------------
#note: not needed once all data have been entered
if(explr.dat.entry)
{
  setwd(handl_OneDrive("Git_other/Analyses/Parks_Australia/outputs/fix this"))
  file.remove(list.files())
  
  No.times=DATA%>%
    filter(is.na(soak.time))%>%
    distinct(sheet_no,.keep_all = T)
  if(nrow(No.times)>0)write.csv(No.times,'No.times.csv',row.names = F)
  
  No.position=DATA%>%
    filter(is.na(mid.lat) | is.na(mid.long))%>%
    distinct(sheet_no,.keep_all = T)
  if(nrow(No.position)>0)write.csv(No.position,'No.position.csv',row.names = F)
  
  No.species=DATA%>%
    filter(is.na(common_name) | is.na(scientific_name))
  if(nrow(No.species)>0) write.csv(No.species,'No.species.csv',row.names = F)
  
  
  LL.issues=DATA%>%
    filter(method=="LL")%>%
    filter(is.na(hooktype) | is.na(hooksize) | is.na(wiretrace) | n.hooks<1)%>%
    dplyr::select(sheet_no,line_no,species,method,n.hooks,hooktype,hooksize,wiretrace)
  if(nrow(LL.issues)>0) write.csv(LL.issues,'LL.issues.csv',row.names = F)
  
  a=DATA%>%
    filter(method=='GN' & (n.hooks>=0 | !is.na(hooksize) | !is.na(hooktype) | !is.na(wiretrace)))%>%
    dplyr::select(sheet_no,line_no,method,n.hooks,hooksize,hooktype,wiretrace,common_name)
  if(nrow(a)>0) write.csv(a,'GN.with.longline.data_n.hooks_not_NA.csv',row.names = F)
  
  
  GN.issues=DATA%>%filter(method=="GN")%>%
    filter(is.na(net_length) | net_length<3 | net_length>8)%>%
    distinct(sheet_no,.keep_all = T)
  if(nrow(GN.issues)>0) write.csv(GN.issues,'GN.issues.csv',row.names = F)
  
  a=DATA%>%
    filter(method=='LL' & (net_length>=0 | !is.na(mesh_size)))%>%
    dplyr::select(sheet_no,line_no,net_length,method,mesh_size,common_name)
  if(nrow(a)>0) write.csv(a,'LL.with.gillnet.data.csv',row.names = F)
  
  
  a=subset(DATA,method=="LL")%>%
    dplyr::select(sheet_no,line_no,n.hooks,hooks.deployed)%>%
    mutate(dummy=paste(sheet_no,line_no))%>%
    distinct(dummy,.keep_all = T)%>%
    dplyr::select(-dummy)
  a$keep=a$n.hooks==a$hooks.deployed
  a=subset(a,keep=="FALSE")
  if(nrow(a)>0) write.csv(a,'N_hooks.different_to_hooks.deployed.csv',row.names = F)
  
  #retained issues
  dodgy.retained=c("DM.T","SK.T","WC","PJ","SH","WW","SR","BK","FR")
  a=DATA%>%filter(species%in%dodgy.retained & retainedflag=='Yes')%>%
    dplyr::select(sheet_no,line_no,species,retainedflag)%>%
    arrange(sheet_no,line_no)
  if(nrow(a)>0) write.csv(a,'discards_entered_as_retained.csv',row.names = F)
  
  check=c("BG.T","QS.T","PS.T",'JE.T',
          "TK","WH","CP","BW","GM")
  a=DATA%>%filter(species%in%check & retainedflag=="No")%>%distinct(sheet_no,line_no,species,retainedflag,fl,tl,comments,comments.hdr)
  if(nrow(a)>0) write.csv(a,'Main.commercial.sp_not.retained.csv',row.names = F)
  
  
  Lat.range=c(-36,-29)
  Long.range=c(113,119)  
  
  seq.Lat=seq(Lat.range[1],Lat.range[2])
  seq.Long=seq(Long.range[1],Long.range[2])
  
  library(PBSmapping)  
  data(worldLLhigh)
  Sites=DATA%>%distinct(sheet_no,mid.lat,mid.long,method)%>%
    mutate(Col=ifelse(method=='GN',"#F8766D", "#00BFC4"))
  Lat.range=c(-37.6,-29)
  Long.range=c(114,120)
  tiff(file="Map issues.tiff",width = 1600, height = 2400,units = "px", res = 300,compression = "lzw")
  par(mar = c(0, 0, 0, 0),oma=c(0,0,0,0),mgp=c(.1, 0.15, 0))
  
  #plot shots' Sampling site locations 
  plotMap(worldLLhigh, xlim=Long.range,ylim=Lat.range,axes=F,
          col="dark grey",tck = 0.025, tckMinor = 0.0125, xlab="",ylab="")
  points(Sites$mid.long,Sites$mid.lat,col='black',pch=21,bg=Sites$Col,cex=1.25)
  
  # #add bathymetry
  # contour(xbat, ybat, reshaped[,2:ncol(reshaped)],ylim=plotlat[[i]],xlim=plotlong[[i]], zlim=c(-1,-300),
  #         nlevels = 3,labcex=1,lty = c(1,2,3),col=c(rep("grey60",3)),add=T)
  axis(side = 1, at =seq.Long, labels = seq.Long, tcl = .5,las=1,cex.axis=0.9)
  axis(side = 2, at = seq.Lat, labels = -seq.Lat,tcl = .5,las=2,cex.axis=0.9)
  
  mtext(expression(paste("Latitude (",degree,"S)",sep="")),side=2,line=1.25,las=3,cex=1.5)
  mtext(expression(paste("Longitude (",degree,"E)",sep="")),side=1,line=1.25,cex=1.5)
  legend('topright',c("Gillnet","Longline"),pch=21,
         pt.bg=c("#F8766D", "#00BFC4"),bty='n',cex=1.25)
  box()
  
  aa=subset(Sites,sheet_no%in%c("PA0133","PA0149"))
  text(aa$mid.long,aa$mid.lat,aa$sheet_no,pos=4,col="black",font=2,srt=-35)
  dev.off()
  
  #no soak time or net length
  no.soak=DATA%>%filter(is.na(soak.time))%>%distinct(sheet_no)
  if(nrow(no.soak)>0) write.csv(no.soak,'No_soak_time.csv',row.names = F)
  
  no.netlen=DATA%>%filter(method=="GN")%>%filter(is.na(net_length))%>%distinct(sheet_no)
  if(nrow(no.netlen)>0) write.csv(no.netlen,'No_net_length.csv',row.names = F)
  
  #no number of hook combos
  no.n.huk.com=Hook.combos%>%dplyr::select(sheet_no,C10.W,C12.W,C14.W,
                                           Eb10.W,Eb12.W,Eb14.W,
                                           C10.M,C12.M,C14.M,
                                           Eb10.M,Eb12.M,Eb14.M)
  no.n.huk.com$Total.hooks=rowSums(no.n.huk.com[,-1])
  no.n.huk.com=no.n.huk.com%>%filter(is.na(Total.hooks))%>%pull(sheet_no)
  
  a=unique(DATA%>%filter(method=="LL")%>%pull(sheet_no))
  not.in.huk.combo=a[which(!a%in%Hook.combos$sheet_no)]
  if(length(not.in.huk.combo)>0) no.n.huk.com=sort(c(no.n.huk.com,not.in.huk.combo)) 
  write.csv(no.n.huk.com,"no.number.of.hook.combinations_in_Hook_count_xlsx.csv",row.names = F)
}



#---------Fishing days by zone and method  -------------------------------
Fishing.days.by.zone.method=DATA%>%
  distinct(date,zone,method)%>%
  group_by(zone,method)%>%
  tally()

#---------Functions: Multivariate stats  -------------------------------
multivariate.fn=function(d,Terms,Def.sp.term,Transf,Show.term,Group,hndl,
                         MDS.title,Simper.title,Permanova.title,
                         simper.cumsum=0.85,
                         LGSIZE=12)
{
  names(d)=tolower(names(d))    
  
  what.species.GN=d%>%
    filter(method=='GN')%>%
    group_by_at(Def.sp.term)%>%
    tally()%>%
    arrange(-n)%>%
    ungroup()%>%
    mutate(Row=1:n(),
           Cumktch=cumsum(n),
           Cumktch=Cumktch/sum(n))
  
  if(Group=='95') what.species.GN=what.species.GN%>%mutate(species2=ifelse(Cumktch<=0.95,species,'Other'))
  if(Group=='Top20') what.species.GN=what.species.GN%>%mutate(species2=ifelse(Row<=20,species,'Other'))
  what.species.GN=what.species.GN%>%
    distinct(species,method,species2)%>%
    filter(!species2=='Other')
  
  what.species.LL=d%>%
    filter(method=='LL')%>%
    group_by_at(Def.sp.term)%>%
    tally()%>%
    arrange(-n)%>%
    ungroup()%>%
    mutate(Row=1:n(),
           Cumktch=cumsum(n),
           Cumktch=Cumktch/sum(n))
  if(Group=='95') what.species.LL=what.species.LL%>%mutate(species2=ifelse(Cumktch<=0.95,species,'Other'))
  if(Group=='Top20') what.species.LL=what.species.LL%>%mutate(species2=ifelse(Row<=20,species,'Other'))
  
  what.species.LL=what.species.LL%>%
    distinct(species,method,species2)%>%
    filter(!species2=='Other')
  
  what.species=rbind(what.species.GN,what.species.LL)
  
  d=d%>%
    left_join(what.species,by=c('species','method'))%>%
    mutate(method.zone=factor(paste(method,zone)))%>%
    mutate(method=factor(method))%>%
    filter(!is.na(species2))%>%
    group_by_at(c(Terms,'species2'))%>%
    summarise(livewt.c=sum(livewt.c,na.rm=T))%>%
    spread(species2,livewt.c,fill=0)%>%
    ungroup()
  
  d$ColSum=rowSums(d[-match(Terms,names(d))])
  d=d%>%filter(ColSum>10)
  if(Transf=='proportion') d[-match(c(Terms,'ColSum'),names(d))]=d[-match(c(Terms,'ColSum'),names(d))]/d$ColSum
  d=d%>%
    dplyr::select(-ColSum)%>%
    data.frame
  
  Community <<- d[-match(Terms,names(d))]
  if(Transf=='sqrt')Community <- sqrt(Community)
  
  this.nm=colnames(Community) %>% str_replace("^[A-Z]*", "")%>%as.numeric()
  names(Community)=All.species.names%>%
    filter(CAES_Code%in%this.nm)%>%
    distinct(CAES_Code,.keep_all=T)%>%
    arrange(CAES_Code)%>%
    pull(COMMON_NAME)
  
  
  #MDS
  MDS <- metaMDS(comm = Community, distance = "bray",k=2,trymax=100, trace = FALSE, autotransform = FALSE)
  MDS_xy <- data.frame(MDS$points)
  MDS_xy$dummy <- d[,match(Show.term,names(d))]
  
  if(Show.term=="method.zone")
  {
    MDS_xy=MDS_xy%>%
      mutate(zone=case_when(grepl("West",dummy)~'West',
                            grepl("Zone1",dummy)~'Zone1',
                            grepl("Zone2",dummy)~'Zone2'),
             method=case_when(grepl("GN",dummy)~"Gillnet",
                              grepl("LL",dummy)~"Longline"))
  }
  p=ggplot(MDS_xy, aes(MDS1, MDS2, color = dummy)) +
    geom_point(size=3,aes(color=zone,shape=method)) +
    annotate(geom="text", x=0.85*max(MDS_xy$MDS1), y=min(MDS_xy$MDS2), 
             label=paste("Stress=",round(MDS$stress,3)))+
    theme_PA(leg.siz=14,axs.t.siz=12,axs.T.siz=14)+
    theme(legend.position = "top",
          legend.title = element_blank())+
    guides(colour = guide_legend(nrow = 1))+
    xlab('')+ylab('')
  if(Show.term=="method.zone")
  {
    p=p+scale_color_manual(values=c("West"="#F8766D","Zone1"="#00BFC4","Zone2"="#7CAE00"))+
      scale_shape_manual(values=c("Gillnet"=8,"Longline"=19))
  }
  
  fn.fig(paste(hndl,MDS.title,sep=""),2400,2400)
  print(p)
  dev.off()
  
  
  #Permanova
  # 1. overall significance test
  adon.results<-adonis2(formula(paste('Community',Show.term,sep='~')),data=d, method="bray",perm=5e3)
  write.csv(as.data.frame(adon.results),paste(hndl,Permanova.title,'.csv',sep=""))
  #adon.results<-adonis(formula(paste('Community',Show.term,sep='~')),data=d, method="bray",perm=5e3)
  #write.csv(as.data.frame(adon.results$aov.tab),paste(hndl,Permanova.title,'.csv',sep=""))
  
  # 2. multilevel pairwise comparison with adjusted p-values
  #adonis.pairwise=pairwise.adonis(Community,d[,match(Show.term,names(d))])
  dummy=pairwise.adonis2(Community~method.zone,data=d)
  adonis.pairwise=vector('list',(length(dummy)-1))
  for(qq in 2:length(dummy))
  {
    adonis.pairwise[[qq]]=data.frame(Pairs=names(dummy)[[qq]],
                                     P=dummy[[qq]]$`Pr(>F)`[1])
  }
  adonis.pairwise=do.call(rbind,adonis.pairwise)
  write.csv(adonis.pairwise,paste(hndl,Permanova.title,'_pairwise.csv',sep=""),row.names = F)
  
  
  #Simper analysis to identify species that discriminate among groups
  SIMPER <- summary(simper(Community, d%>%pull(Show.term),parallel=7))
  
  #1. display species accounting for group differences
  Get=as.data.frame(str_split(names(SIMPER), "_", simplify = TRUE))%>%
    mutate(V1.method=sub("\\ .*", "", V1),
           V2.method=sub("\\ .*", "", V2),
           V1.zone=sub(".* ", "", V1),
           V2.zone=sub(".* ", "", V2),
           id=1:n())%>%
    filter(!V1.method==V2.method & V1.zone==V2.zone)
  SIMPER=SIMPER[Get$id]
  
  disp.simp=vector('list',length(SIMPER))
  for(n in 1:length(disp.simp))
  {
    disp.simp[[n]]=cbind(d[Show.term],
                         Community[row.names(SIMPER[[n]]%>%filter(cumsum<=simper.cumsum))])%>%
      filter(!!sym(Show.term)%in%str_split(names(SIMPER)[n], "_", simplify = TRUE))%>%
      gather(species,prop,-method.zone,)%>%
      mutate(groups=names(SIMPER)[n])
  }
  disp.simp=do.call(rbind,disp.simp)
  
  dis.cls=unique(disp.simp$species)
  dis.cls=All.species.names%>%
    filter(COMMON_NAME%in%unique(disp.simp$species))
  
  colfunc <- colorRampPalette(Shark.palette)
  n.col.elasmos=colfunc(length(dis.cls$CAES_Code[dis.cls$CAES_Code<50000]))
  names(n.col.elasmos)=dis.cls%>%filter(CAES_Code<50000)%>%pull(COMMON_NAME)
  
  colfunc <- colorRampPalette(Teleost.palette)
  n.col.teleos=colfunc(length(dis.cls$CAES_Code[dis.cls$CAES_Code>=50000]))
  names(n.col.teleos)=dis.cls%>%filter(CAES_Code>=50000)%>%pull(COMMON_NAME)
  
  p=disp.simp%>%
    group_by(method.zone,groups,species)%>%
    summarise(prop=mean(prop))%>%
    mutate(method=sub("\\ .*", "", method.zone),
           zone=sub(".* ", "", method.zone))%>%
    ggplot(aes(x=method,y=prop, fill=species))+
    geom_bar(stat="identity", width = 0.5)+
    facet_wrap(~zone,scales='free_y')+
    ylab("Average proportion")+xlab("Method")+
    scale_fill_manual(values=c(n.col.elasmos,n.col.teleos))+
    theme_PA(str.siz=14,leg.siz=LGSIZE,axs.t.siz=12,axs.T.siz=16)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
    guides(fill = guide_legend(nrow = 4))
  
  fn.fig(paste(hndl,Simper.title,sep=""),2400,2400)
  print(p)
  dev.off()
  
  SIMPER.out=SIMPER
  for(s in 1:length(SIMPER.out))
  {
    x=SIMPER[[s]]%>%
      mutate(group=names(SIMPER)[s],
             species=row.names(SIMPER[[s]]))%>%
      relocate(group,species, .before=average)
    x=x%>%
      filter(cumsum<=simper.cumsum)
    SIMPER.out[[s]]=x 
  }
  write.csv(do.call(rbind,SIMPER.out),paste(hndl,Simper.title,'.csv',sep=""),row.names = F)
}
#---------Functions: CPUE standardisation ------------              
cpue.stand.fun=function(d,Formula)
{
  mod=gam(Formula,dat=d,family=tw,method="REML")
  return(list(mod=mod,dat=d))
}
pred.fun=function(mod,biascor,PRED)             
{
  if(biascor=="YES")  #apply bias correction for log transf
  {
    lsm=summary(emmeans(mod, PRED, type="link"))%>%
      mutate(response=exp(emmean)*exp(SE^2/2),
             lower.CL=exp(lower.CL)*exp(SE^2/2),
             upper.CL=exp(upper.CL)*exp(SE^2/2))
  }
  if(biascor=="NO") lsm=summary(emmeans(mod, PRED, type="response"))
  
  lsm$SD=lsm$SE
  
  return(lsm)
}
pred.fun.continuous=function(d,mod,PRED,Formula)
{
  NewD=NULL
  VARS=all.vars(Formula)[-1]
  if(PRED[1]%in%VARS)
  {
    #create new data
    VARS=VARS[!VARS%in%PRED]
    NewD=d%>%pull(PRED)
    if(length(PRED)==1)
    {
      NewD=seq(min(NewD,na.rm=T),max(NewD,na.rm=T))
      if(length(NewD)<50) NewD=seq(min(NewD),max(NewD),length.out = 100)
      NewD=as.data.frame(NewD)
      names(NewD)=PRED
    }else
    {
      NewD$dummy=paste(NewD[,1],NewD[,2])
      NewD=NewD%>%
        distinct(dummy,.keep_all = TRUE)%>%
        dplyr::select(-dummy)
    }
    
    FixedVar=as.data.frame(matrix(NA,nrow=1,ncol=length(VARS)))
    names(FixedVar)=VARS
    for(l in 1:length(VARS))
    {
      vv=d%>%pull(VARS[l])
      
      if(is.factor(vv))
      {
        FF=names(sort(-table(vv)))[1]
        FixedVar[,l]=factor(FF,levels(vv))
      }else
        FixedVar[,l]=mean(vv,na.rm=T)
      
      rm(vv)
    }
    NewD=cbind(NewD,FixedVar)
    
    #predict new data
    a=predict(mod,newdata=NewD,type="response",se.fit=T)
    NewD$Pred=a$fit
    NewD$Pred.SE=a$se.fit
    
    NewD=NewD%>%
      dplyr::rename(response=Pred,
                    SE=Pred.SE)%>%
      mutate(lower.CL=response-1.96*SE,
             upper.CL=response+1.96*SE)%>%
      dplyr::select(all_of(PRED),response,SE,lower.CL,upper.CL)
  }
  return(NewD)  
}
fn.barplot.cpue=function(d,YLAB,XLAB,cex,Rotate=NULL,Relative,NROW=2,Y_sqrt=NULL,RETURN=TRUE)
{
  if(Relative=="Yes")d=d%>%
      group_by(facet)%>%
      mutate(lower.CL=lower.CL/mean(y),
             upper.CL=upper.CL/mean(y),
             y=y/mean(y))
  p=d%>%
    ggplot(aes(x,y,fill=fill))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(x, ymin=lower.CL, ymax=upper.CL),colour='black',width=0.25)+
    facet_wrap(~facet,scales='free',nrow=NROW)+
    theme_PA(strx.siz=cex-2,leg.siz=cex,axs.t.siz=cex-2,axs.T.siz=cex+1)+
    theme(legend.position='none')+
    ylab(YLAB)+xlab(XLAB)
  if(!is.null(Rotate)) p=p+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  if(!is.null(Y_sqrt)) p=p+scale_y_sqrt()
  if(isFALSE(RETURN))return(p)
  if(RETURN)
  {
    print(p)
    return(d%>%dplyr::select(species,x,y))
  }
  
}
fn.continuous.cpue=function(d,YLAB,XLAB,cex,Relative)
{
  if(Relative=="Yes")d=d%>%
      group_by(facet)%>%
      mutate(lower.CL=lower.CL/mean(y),
             upper.CL=upper.CL/mean(y),
             y=y/mean(y))
  p=d%>%
    ggplot(aes(x,y,fill=fill))+
    geom_line()+
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), alpha=0.4)+
    facet_wrap(~facet,scales='free',nrow=2)+
    theme_PA(strx.siz=cex-2,leg.siz=cex-2,axs.t.siz=cex-2,axs.T.siz=cex+1)+
    theme(legend.position='top',
          legend.title=element_blank())+
    ylab(YLAB)+xlab(XLAB)
  print(p)
}
MeanSE <- function(x)
{
  v <- c(mean(x) - sd(x)/sqrt(length(x)), mean(x), mean(x) + sd(x)/sqrt(length(x)))
  names(v) <- c("ymin", "middle", "ymax")
  v
}
#---------Functions: mapping ------------  
plt.map=function(D,Title,Scaler)
{
  p=ggplot(oz_states) + 
    geom_sf(fill="brown4") + 
    coord_sf(xlim = c(112, 129), ylim = c(-25, -36))+
    geom_point(data = D, mapping = aes(x = lon.port, y = lat.port),
               size=D$Prop*Scaler, colour = grDevices::adjustcolor("steelblue", alpha=0.6))+
    geom_sf_text(data = sites,aes(label = Cityname),color="White",fontface="bold",
                 nudge_x=Nudge$nudge_x,nudge_y=Nudge$nudge_y)+
    ylab("Latitude")+xlab("Longitude")+
    theme(plot.margin=unit(c(.05,.05,.05,.05),"cm"),
          plot.title = element_text(size = 16,vjust = 0,face = 'bold'),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14))+
    ggtitle(Title)
  print(p)
}


#---------Manipulate PA observer TEPS ------------
TEPS.code_contact=data.frame(contact.code=c('WWC','BFC','WER','WEN','WDNN','WDNF','WDDN','WDDF'),
                             contact.code.meaning=c('Wildlife on/in water, contact with vessel',
                                                    'Bird flying, contact with vessel or gear',
                                                    'Wildlife entangled in ropes',
                                                    'Wildlife entangled/hooked',
                                                    'Wildlife diving for but not feeding from net/longline',
                                                    'Wildlife diving for and feeding from net/longline',
                                                    'Wildlife diving for but not feeding on discards',
                                                    'Wildlife diving for and feeding on discards'),
                             contact.code.to.complete=c('on/in water, contact with vessel',
                                                        'flying, contact with vessel or gear',
                                                        'entangled in ropes',
                                                        'entangled/hooked',
                                                        'diving for but not feeding from net/longline',
                                                        'feeding from net/longline',
                                                        'diving for but not feeding on discards',
                                                        'feeding on discards'))
TEPS.code_behav=data.frame(sighting.behav=c('INT','IRR','DES','ROW'),
                           sighting.behav.meaning=c('Intensively searching',
                                                    'Irregularly searching',
                                                    'desinterested',
                                                    'Roaming wild'))

TEPS<-TEPS%>%
  dplyr::rename(sheet_no="Sheet #",
                ves.act="Ves act",
                obs.sector="Obs sector",
                common.name="Common name",
                sighting.period="Period",
                sighting.dist="Dist",
                sighting.count.method="Count method",
                sighting.count="Count",
                sighting.behav="Behav",
                gear.type="Gear type",
                hook.type="Hook type",
                hook.size="Hook size",
                snood.type="Snood type",
                contact.code="Contact code",
                contact.count="Contact count",
                disc.prey="Disc prey",
                steam.away="Steam away")%>%
  mutate(contact.code=ifelse(contact.code=="WEN/ WWC" & sheet_no=='PA0036',NA,contact.code))%>%
  data.frame%>%
  filter(!is.na(contact.code))%>%
  left_join(TEPS.code_contact,by='contact.code')%>%
  left_join(TEPS.code_behav,by='sighting.behav')

TEPS=TEPS%>%
  mutate(common.name=tolower(common.name),
         SP.group=case_when(common.name%in%c("grey nurse shark")~"protected sharks",
                            common.name%in%c("australian sealion")~"marine mammals",
                            TRUE~"other stuff"),
         common.name=case_when(common.name=="australian sealion" ~ "Australian sea-lion",
                               TRUE~common.name),
         common.name=capitalize(common.name))

#function for integer axis labels
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


#---------Manipulate PA Observer and underwater cameras data ------------ 

#Observer data  
DATA_PA=DATA%>%
  distinct(sheet_no,.keep_all=TRUE)%>%
  dplyr::select(sheet_no,mid.lat,mid.long,date,day,month,year,
                boat,block,skipper,botdepth,set.time,set.time.end,haul.time,haul.time.end,
                set.time.avg,haul.time.avg,soak.time,method,mesh_size,net_length,n.hooks)

#Tabulate retained or discarded
Retained.tabl=DATA%>%
  mutate(species=toupper(species))%>%
  group_by(species,retainedflag)%>%
  filter(!is.na(retainedflag))%>%
  tally()%>%
  ungroup%>% 
  group_by(species) %>% 
  arrange(desc(n)) %>% 
  slice(1) %>% 
  ungroup()%>%
  data.frame%>%
  left_join(All.species.names,by=c('species'='Species'))%>%
  dplyr::select(Code,retainedflag)%>%
  dplyr::rename(retained=retainedflag)%>%
  filter(!is.na(Code))

#Combine Interaction with Observation data sheets
Video.net.obs=Video.net.obs%>%
  mutate(Interaction=case_when(!code%in%c(54079901,11183901,11183902)~'Swim Past'),
         Escape=NA,
         Method="Gillnet", 
         Position="Gillnet",
         Species=tolower(observation))
if(!is.na(match('code',colnames(Video.net.obs)))) #Video.net.obs=Video.net.obs%>%rename(Code=code)
cols.vid.inter=colnames(Video.net.interaction)
add=cols.vid.inter[which(!cols.vid.inter%in%colnames(Video.net.obs))]
empty_df = Video.net.obs[,1:length(add)]
colnames(empty_df)=add
empty_df[,]=NA
Video.net.obs=cbind(Video.net.obs,empty_df)
Video.net.obs=Video.net.obs[,colnames(Video.net.interaction)]
Video.net.interaction=rbind(Video.net.interaction,Video.net.obs)


Video.longline.obs=Video.longline.obs%>%
  mutate(Method="longline", 
         Position="longline",
         Interaction='Swim Past',
         Interaction=ifelse(grepl('birds feeding',Observation),paste("feeding from",Method),
                            NA),
         Escape=NA,
         Species=tolower(Observation))
if(!is.na(match('code',colnames(Video.longline.obs))))
{
  if(!is.na(match('Code',colnames(Video.longline.obs))))
  {
    if(is.na(sum(Video.longline.obs$Code))) Video.longline.obs=Video.longline.obs%>%dplyr::select(-Code)
    Video.longline.obs=Video.longline.obs%>%dplyr::rename(Code=code)
  }
  
}
cols.vid.inter=colnames(Video.longline.interaction)
add=cols.vid.inter[which(!cols.vid.inter%in%colnames(Video.longline.obs))]
empty_df = Video.longline.obs[,1:length(add)]
colnames(empty_df)=add
empty_df[,]=NA
Video.longline.obs=cbind(Video.longline.obs,empty_df)
Video.longline.obs=Video.longline.obs[,colnames(Video.longline.interaction)]
Video.longline.interaction=rbind(Video.longline.interaction,Video.longline.obs)

#some manipulations                       
Video.net.interaction=Video.net.interaction%>%
  mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                            Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                            Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                            Code >=3.7046e7 & Code<=3.747e7 ~"Scalefish",
                            Code >=3.747e7 & Code<=4.0e+07 ~"Turtles",
                            Code >=4.1e+07 & Code<=4.115e+07 ~"Marine mammals",
                            Code >=4.0e+07 & Code<4.1e+07 ~"Seabirds",
                            Code >=1.2e7 & Code<3.7e7 ~"Invertebrates",
                            Code >=1.1e7 & Code<1.2e7 ~"Rock/reef structure",
                            Code >=5.4e7 & Code<5.49e7 ~"Macroalgae",
                            Code == 10000910 ~"Sponges"),
         Period=tolower(Period),
         Depth=as.numeric(gsub("[^0-9.-]", "", Depth)),
         OpCode=case_when(OpCode=='Augusta_Gillnet_PA00013_Camera_#S1'~'Augusta_Gillnet_PA0013_Camera_#S1',
                          TRUE~ OpCode),   
         sheet_no=sapply( strsplit( OpCode, "_" ), "[", 3),
         Camera=paste("Camera",sapply( strsplit(OpCode, "_" ), "[", 5)),
         Escape2=ifelse(Escape=='landed',NA,Escape),
         Interaction=ifelse(Interaction=="Bounce off","Bounced off",
                            ifelse(Interaction=="Swimpast","Swim past",
                                   Interaction)),
         Interaction=capitalize(tolower(Interaction)),
         Interaction2=ifelse(Interaction%in%grep(paste(c("Caught","caught"),collapse="|"),Interaction, value=TRUE) &
                               !is.na(Escape2),"Escape",Interaction))%>%
  left_join(DATA_PA,by='sheet_no')%>%
  data.frame

Video.net.interaction=Video.net.interaction%>%
  left_join(Retained.tabl,by='Code')%>%
  mutate(Method='Gillnet',
         Retain.group=
           ifelse(retained=='Yes' & SP.group%in% c('Sharks','Rays'),"Retained elasmobranch",
                  ifelse(retained=='No' & SP.group%in% c('Sharks','Rays'),"Discarded elasmobranch",
                         ifelse(retained=='Yes' & SP.group%in% c('Scalefish'),"Retained scalefish",   
                                ifelse(retained=='No' & SP.group%in% c('Scalefish'),"Discarded scalefish",
                                       NA)))))


Video.longline.interaction=Video.longline.interaction%>%
  mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                            Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                            Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                            Code >=3.7046e7 & Code<=3.747e7 ~"Scalefish",
                            Code >=3.747e7 & Code<=4.0e+07 ~"Turtles",
                            Code >=4.1e+07 & Code<=4.115e+07 ~"Marine mammals",
                            Code >=4.0e+07 & Code<4.1e+07 ~"Seabirds",
                            Code >=1.2e7 & Code<3.7e7 ~"Invertebrates",
                            Code >=1.1e7 & Code<1.2e7 ~"Rock/reef structure",
                            Code >=5.4e7 & Code<5.49e7 ~"Macroalgae",
                            Code == 10000910 ~"Sponges"),
         Period=tolower(Period),
         Depth=as.numeric(gsub("[^0-9.-]", "", Depth)),
         OpCode=case_when(OpCode=='Augusta_longline_PA20024_Camera_#2'~'Augusta_longline_PA0024_Camera_#2',
                          TRUE~ OpCode),
         sheet_no=sapply( strsplit( OpCode, "_" ), "[", 3),
         Camera=paste("Camera",sapply( strsplit(OpCode, "_" ), "[", 5)),
         Interaction=ifelse(Interaction=="Swimpast","Swim past",Interaction),
         Interaction=capitalize(tolower(Interaction)),
         Escape2=ifelse(Escape=='landed',NA,Escape),
         Interaction2=ifelse(Interaction%in%grep(paste(c("Caught","caught"),collapse="|"),Interaction, value=TRUE) &
                               !is.na(Escape2),"Escape",Interaction))%>%
  left_join(DATA_PA,by='sheet_no')%>%
  data.frame

Video.longline.interaction=Video.longline.interaction%>%
  left_join(Retained.tabl,by='Code')%>%
  mutate(Retain.group=
           ifelse(retained=='Yes' & SP.group%in% c('Sharks','Rays'),"Retained elasmobranch",
                  ifelse(retained=='No' & SP.group%in% c('Sharks','Rays'),"Discarded elasmobranch",
                         ifelse(retained=='Yes' & SP.group%in% c('Scalefish'),"Retained scalefish",   
                                ifelse(retained=='No' & SP.group%in% c('Scalefish'),"Discarded scalefish",
                                       NA)))))

Video.net.maxN=Video.net.maxN%>%
  mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                            Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                            Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                            Code >=3.7046e7 & Code<=3.747e7 ~"Scalefish",
                            Code >=4.1e+07 & Code<=4.115e+07 ~"Marine mammals",
                            Code >=4.0e+07 & Code<4.1e+07 ~"Seabirds",
                            Code >=1.2e7 & Code<3.7e7 ~"Invertebrates",
                            Code >=1.1e7 & Code<1.2e7 ~"Rock/reef structure",
                            Code >=5.4e7 & Code<5.49e7 ~"Macroalgae",
                            Code == 10000910 ~"Sponges"),
         Period=tolower(Period),
         Depth=as.numeric(gsub("[^0-9.-]", "", Depth)),
         sheet_no=sapply( strsplit( OpCode, "_" ), "[", 3),
         Camera=paste("Camera",sapply( strsplit(OpCode, "_" ), "[", 5)))%>%
  left_join(DATA_PA,by='sheet_no')%>%
  data.frame

Video.longline.maxN=Video.longline.maxN%>%
  mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                            Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                            Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                            Code >=3.7046e7 & Code<=3.747e7 ~"Scalefish",
                            Code >=4.1e+07 & Code<=4.115e+07 ~"Marine mammals",
                            Code >=4.0e+07 & Code<4.1e+07 ~"Seabirds",
                            Code >=1.2e7 & Code<3.7e7 ~"Invertebrates",
                            Code >=1.1e7 & Code<1.2e7 ~"Rock/reef structure",
                            Code >=5.4e7 & Code<5.49e7 ~"Macroalgae",
                            Code == 10000910 ~"Sponges"),
         Period=tolower(Period),
         Depth=as.numeric(gsub("[^0-9.-]", "", Depth)),
         sheet_no=sapply( strsplit( OpCode, "_" ), "[", 3),
         Camera=paste("Camera",sapply( strsplit(OpCode, "_" ), "[", 5)))%>%
  left_join(DATA_PA,by='sheet_no')%>%
  data.frame

Video.net.interaction=Video.net.interaction%>%
  mutate(Number=as.numeric(Number))

Video.longline.interaction=Video.longline.interaction%>%
  mutate(Number=as.numeric(Number))

#Add species names and code to DATA
DATA=DATA%>%
  mutate(species=toupper(species))%>%
  left_join(All.species.names,by=c('species'='Species'))

#Add LL configuration combos
DATA=DATA%>%
  mutate(hooktype=capitalize(hooktype),
         wiretrace=tolower(wiretrace),
         wiretrace=ifelse(wiretrace=='no',"Mono",ifelse(wiretrace=="yes","Wire",NA)),
         LL.config=ifelse(method=="LL",
                          paste(hooktype, hooksize, wiretrace),
                          NA))

#PL for stingrays and eagle rays are DW and create Size variable
DATA=DATA%>%mutate(dw=ifelse(CAES_Code %in% 35000:39100,pl,NA),
                   Size=case_when(taxa=='Elasmobranch'~fl,
                                  taxa=='Teleost'~tl,
                                  CAES_Code %in% 35000:39100 ~ dw))


#---------Add total weight for main species to PA observer data------------
these.sharks=c('Dusky shark','Whiskery shark','Gummy shark','Sandbar shark',
               "Common blacktip shark", "Blacktip sharks", "Bronze whaler", "Hammerheads",          
               "Scalloped hammerhead", "Smooth hammerhead", "Spinner shark", "Shortfin mako",        
               "Pencil shark", "Common sawshark", "Spotted wobbegong", "Western wobbegong" ,   
               "Other sharks")
DATA=DATA%>%
  left_join(Length.weight,by='common_name')%>%
  mutate(tl=case_when(is.na(tl) & 
                        common_name%in%these.sharks~a_FL.to.TL*fl+b_FL.to.TL,
                      TRUE~tl),
         tl=ifelse(CAES_Code%in%35000:39100 & is.na(tl),dw,tl),
         tw=ifelse(!is.na(a_weight) & !is.na(tl),a_weight*tl^b_weight,NA),
         tw=ifelse(tw>2500,NA,tw))

#set to average weight if no size information
DATA=DATA%>%
  group_by(common_name,method)%>%
  mutate(tw.average=mean(tw,na.rm=T))%>%
  ungroup()%>%
  mutate(tw=ifelse(is.na(tw),tw.average,tw))

#---------Number of fishing days and shots by zone ------------
N.fshn=DATA%>%distinct(sheet_no,.keep_all=T)
Tab.n.fshn.days=table(N.fshn$date,N.fshn$zone,useNA = 'ifany')
N.fshn.days=Tab.n.fshn.days
N.fshn.days[N.fshn.days>0]=1
colSums(N.fshn.days)

table(N.fshn$method,useNA = 'ifany')
table(N.fshn$method,N.fshn$zone,useNA = 'ifany')

N.fshn%>%
  filter(skipper=='Tim Goodall' & method=="GN")%>%
  dplyr::select(date,sheet_no,net_length)%>%
  data.frame()%>%arrange(date,sheet_no)  #Tim split the net in 2 and 1 km

with(subset(N.fshn,zone=="Zone2"),table(method,skipper,useNA = 'ifany'))
#---------Observer catch composition ------------ 
# Map of sampling sites
# do.map=TRUE
# if(do.map)
# {
#   library(grDevices)
#   library(PBSmapping)
#   if(User=='Abbey')
#   {
#     Bathymetry_120=read.table(handl_OneDrive("Data/Mapping/get_data112_120.cgi"))
#     Bathymetry_138=read.table(handl_OneDrive("Data/Mapping/get_data120.05_138.cgi"))
#   }
# 
#   if(User=='Sarah')
#   {
#     Bathymetry_120=read.table(handl_OneDrive("Mapping/get_data112_120.cgi"))
#     Bathymetry_138=read.table(handl_OneDrive("Mapping/get_data120.05_138.cgi"))
#   }
#   
#   Bathymetry=rbind(Bathymetry_120,Bathymetry_138)
#   Bathymetry=Bathymetry%>%filter(V2<=(-26))
#   Bathymetry=Bathymetry[order(Bathymetry$V1,Bathymetry$V2),]
#   xbat=sort(unique(Bathymetry$V1))
#   ybat=sort(unique(Bathymetry$V2)) 
#   reshaped=as.matrix(reshape(Bathymetry,idvar="V1",timevar="V2",v.names="V3", direction="wide"))
#   
#   library(rgdal)
#   if(User=='Abbey')
#   {
#     SDGDLL_zone1=readOGR(handl_OneDrive("Data/Mapping/SDGDLL_zone1.shp"), layer="SDGDLL_zone1") 
#     SDGDLL_zone2=readOGR(handl_OneDrive("Data/Mapping/SDGDLL_zone2.shp"), layer="SDGDLL_zone2") 
#     WCDGDLL=readOGR(handl_OneDrive("Data/Mapping/WCDGDLL.shp"), layer="WCDGDLL") 
#     
#   }
#   
#   if(User=='Sarah')
#   {
#     SDGDLL_zone1=readOGR(handl_OneDrive("Mapping/SDGDLL_zone1.shp"), layer="SDGDLL_zone1") 
#     SDGDLL_zone2=readOGR(handl_OneDrive("Mapping/SDGDLL_zone2.shp"), layer="SDGDLL_zone2") 
#     WCDGDLL=readOGR(handl_OneDrive("Mapping/WCDGDLL.shp"), layer="WCDGDLL") 
#     
#   }
#   
#   #Spatial range
#   #Lat.range=round(c(min(DATA$mid.lat)-1,max(DATA$mid.lat)+1))
#   #Long.range=round(c(min(DATA$mid.long)-1,max(DATA$mid.long)+1))  
#   Lat.range=c(-36,-29)
#   Long.range=c(113,119)  
#   
#   seq.Lat=seq(Lat.range[1],Lat.range[2])
#   seq.Long=seq(Long.range[1],Long.range[2])
#   
#   Sites=DATA%>%distinct(sheet_no,mid.lat,mid.long,method)%>%
#     mutate(Col=ifelse(method=='GN',"#F8766D", "#00BFC4"))
#   #bring in shape file
#   data(worldLLhigh)
#   
#   fn.fig(le.paste("Map site area"),1600,2400)
#   par(mar = c(0, 0, 0, 0),oma=c(0,0,0,0),mgp=c(.1, 0.15, 0))
#   
#   #plot shots' Sampling site locations 
#   plotMap(worldLLhigh, xlim=Long.range,ylim=Lat.range,axes=F,
#           col="dark grey",tck = 0.025, tckMinor = 0.0125, xlab="",ylab="")
#   
#   #add zones
#   plot(WCDGDLL,add=T,col="aquamarine3")
#   text(114,-31,"West coast",srt=90,cex=1.5)
#   
#   plot(SDGDLL_zone1,add=T,col="deepskyblue3")
#   text(114.5,-35,"(Zone 1)",cex=1.5,srt=-45)
#   
#   plot(SDGDLL_zone2,add=T,col="chartreuse3")
#   text(118,-35.75,"(Zone 2)",cex=1.5)
#   
#   
#   points(Sites$mid.long,Sites$mid.lat,col='black',pch=21,bg=Sites$Col,cex=1.25)
#   
#   #add bathymetry
#   contour(xbat, ybat, reshaped[,2:ncol(reshaped)],ylim=plotlat[[i]],xlim=plotlong[[i]], zlim=c(-1,-300),
#           nlevels = 3,labcex=1,lty = c(1,2,3),col=c(rep("black",3)),add=T)
#   axis(side = 1, at =seq.Long, labels = seq.Long, tcl = .5,las=1,cex.axis=0.9)
#   axis(side = 2, at = seq.Lat, labels = -seq.Lat,tcl = .5,las=2,cex.axis=0.9)
#   
#   mtext(expression(paste("Latitude (",degree,"S)",sep="")),side=2,line=1.25,las=3,cex=1.5)
#   mtext(expression(paste("Longitude (",degree,"E)",sep="")),side=1,line=1.5,cex=1.5)
#   legend('bottomleft',c("Gillnet","Longline"),pch=21,
#          pt.bg=c("#F8766D", "#00BFC4"),bty='n',cex=1.25)
#   box()
#   
#   #inset Australia
#   par(fig=c(.35,.95,.35,.95), new = T,mgp=c(.1,.4,0),mai=c(.01,01,.01,.01))
#   plotMap(worldLLhigh, xlim=c(113,155), ylim=c(-44,-11),col="grey80", axes=F, xlab="", ylab="",
#           border="black",bg="white",plt = NULL)
#   
#   text(122,-24,"Western",col="black",cex=1,font=2.5)
#   text(122,-27,"Australia",col="black",cex=1,font=2.5)
#   polygon(x=c(Long.range,rev(Long.range)),
#           y=c(rep(Lat.range[1],2),rep(Lat.range[2],2)),
#           col=rgb(.1, .6, .1, alpha = .4),border = "black")
#   dummy.ln=c(152.8,154.6)
#   dummy.la=c(-12.03,-10.94)
#   polygon(x=c(dummy.ln,rev(dummy.ln)),
#           y=c(rep(dummy.la[1],2),rep(dummy.la[2],2)),
#           col='white',border = 'white')
#   lines(x=c(129,129),y=c(-31.64,-15),lty=2)
#   dev.off()
#   
# }  

# 1. Table of number of individuals caught by species and gear

Tab.n.sp.gear=DATA%>%
  group_by(method,common_name,scientific_name,taxa)%>%
  tally()%>%
  ungroup()%>%
  spread(method,n,fill=0)%>%
  mutate(Total=GN+LL)%>%
  arrange(taxa,-Total)%>%
  dplyr::select(-taxa)%>%
  filter(!common_name=='')
write.csv(Tab.n.sp.gear,le.paste("Observer/Observed.number.species.by.gear.csv"),row.names=F) 

Tab.n.sp.longline=DATA%>%
  filter(method=='LL')%>%
  group_by(LL.config,common_name,taxa)%>%
  tally()%>%
  ungroup()%>%
  spread(LL.config,n,fill=0)%>%
  mutate(Total = rowSums(across(where(is.numeric))))%>%
  arrange(taxa,-Total)%>%
  dplyr::select(-taxa,-Total)%>%
  filter(!common_name=='')
write.csv(Tab.n.sp.longline,le.paste("Observer/Observed.number.species.by.longline.config.csv"),row.names=F)

Tab.n.sp.gillnet=DATA%>%
  filter(method=='GN')%>%
  group_by(LL.config,common_name,taxa)%>%
  tally()%>%
  ungroup()%>%
  spread(LL.config,n,fill=0)%>%
  mutate(Total = rowSums(across(where(is.numeric))))%>%
  arrange(taxa,-Total)%>%
  dplyr::select(-taxa,-Total)%>%
filter(!common_name=='')
write.csv(Tab.n.sp.longline,le.paste("Observer/Observed.number.species.by.gillnet.config.csv"),row.names=F)

#library(extrafont)

# 2. Number of individuals caught by snood configuration 
fn.fig(le.paste("Observer/Barplot Longline configurations"),2400,2000)
DATA%>%
  filter(method=="LL")%>%
  filter(!is.na(hooktype) & !is.na(hooksize) & !is.na(wiretrace))%>%
  group_by(LL.config,hooktype, hooksize, wiretrace)%>%
  tally()%>%
  ggplot(aes(x= LL.config, y=n,fill=wiretrace)) +
  geom_bar(stat="identity", position = "dodge")+
  theme_PA(lgT.siz=14,leg.siz=12,axs.t.siz=12,axs.T.siz=16)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Number of individuals caught")+xlab('Snood configuration')
dev.off()

# 3. Catch composition        
Grouped.species.fn=function(METH)
{
  x=DATA%>%
    filter(method==METH)%>%
    group_by(COMMON_NAME)%>%
    tally()%>%
    ungroup()%>%
    mutate(Prop=n/sum(n))%>%
    arrange(-Prop)%>%
    mutate(CumSum=cumsum(Prop))%>%
    filter(CumSum>.9)%>%
    pull(COMMON_NAME)
  x=subset(x,!x%in%Main.species)
  return(x)
}
Grouped.species.GN=Grouped.species.fn("GN")
Grouped.species.LL=Grouped.species.fn("LL")

fn.brplt=function(d,Grouped.species,CX.nm)
{
  dd=d%>%
    filter(Taxa%in%c('Elasmobranch','Teleost'))%>%
    mutate(new.COMMON_NAME=ifelse(COMMON_NAME%in%Grouped.species & Taxa=='Elasmobranch',
                                  "Other elasmobranchs",
                                  ifelse(COMMON_NAME%in%Grouped.species & Taxa=='Teleost',
                                         "Other scalefish",COMMON_NAME)))%>%
    group_by(new.COMMON_NAME,Taxa)%>%
    tally()%>%
    ungroup()%>%
    mutate(Prop=n/sum(n))%>%
    arrange(-Prop)
  
  n.other.elasmo=d%>%filter(COMMON_NAME%in%Grouped.species & Taxa=='Elasmobranch')%>%pull(COMMON_NAME)
  n.other.elasmo=length(unique(n.other.elasmo))
  n.other.scalies=d%>%filter(COMMON_NAME%in%Grouped.species & Taxa=='Teleost')%>%pull(COMMON_NAME)
  n.other.scalies=length(unique(n.other.scalies))
  
  dd=dd%>%
    mutate(colr=ifelse(Taxa=='Elasmobranch',"steelblue","firebrick"),
           colr=ifelse(new.COMMON_NAME%in%c("Other elasmobranchs","Other scalefish"),"forestgreen",colr),
           new.COMMON_NAME=ifelse(new.COMMON_NAME=="Other elasmobranchs",
                                  paste("Other elasmos (n=",n.other.elasmo," species)",sep=""),
                                  ifelse(new.COMMON_NAME=="Other scalefish",
                                         paste("Other scalefish (n=",n.other.scalies," species)",sep=""),
                                         new.COMMON_NAME)))%>%
    group_by(new.COMMON_NAME,colr)%>%
    summarise(Prop=sum(Prop))%>%
    data.frame%>%
    arrange(Prop)
  with(dd,barplot(Prop,horiz = T,col=colr,names.arg=new.COMMON_NAME,cex.names=CX.nm,las=2))
  box()
}

#Overall GN vs LL
fn.fig(le.paste("Observer/catch_comp/Catch.comp_GN_LL_overall"),2600,1600)
par(mfcol=c(1,2),mar=c(2,10.5,1,.1),oma=c(1,.1,.5,.65),mgp=c(1.5,.5,0),cex.axis=.9)
fn.brplt(d=DATA%>%
           filter(method=="GN"),
         Grouped.species=Grouped.species.GN,
         CX.nm=.8)
mtext(paste("Gillnet (",length(unique(subset(DATA,method=='GN')$sheet_no))," shots, ",
            sum(subset(DATA,method=='GN')$number)," individuals)",sep=""),3,cex=.9)
fn.brplt(d=DATA%>%
           filter(method=="LL"),
         Grouped.species=Grouped.species.LL,
         CX.nm=.8)
mtext(paste("Longline (",length(unique(subset(DATA,method=='LL')$sheet_no))," shots, ",
            sum(subset(DATA,method=='LL')$number)," individuals)",sep=""),3,cex=.9)
mtext("Proportion of total catch",1,outer=T,line=0.1,cex=1.2) 
dev.off()

#by zone
dis.zoun=c("West","Zone1","Zone2")
fn.fig(le.paste("Observer/catch_comp/Catch.comp_GN_LL_by.zone"),2600,2000)
smart.par(length(dis.zoun)*2,MAR=c(2,10.5,.15,.1),OMA=c(1,.1,1,.3),MGP=c(1.5,.5,0))
par(cex.axis=.9)
for(z in 1:length(dis.zoun))
{
  fn.brplt(d=DATA%>%
             filter(method=="GN" & zone==dis.zoun[z]),
           Grouped.species=Grouped.species.GN,
           CX.nm=.8)
  if(z==1)mtext("Gillnet",3,cex=.9)
  LGN=paste(dis.zoun[z]," (",length(unique(subset(DATA,method=='GN'& zone==dis.zoun[z])$sheet_no))," shots, ",
            sum(subset(DATA,method=='GN'& zone==dis.zoun[z])$number)," individuals)",sep="")
  legend('bottomright',LGN,bty='n')
  
  fn.brplt(d=DATA%>%
             filter(method=="LL" & zone==dis.zoun[z]),
           Grouped.species=Grouped.species.LL,
           CX.nm=.8)
  if(z==1)mtext("Longline",3,cex=.9)
  LGN=paste(dis.zoun[z]," (",length(unique(subset(DATA,method=='LL'& zone==dis.zoun[z])$sheet_no))," shots, ",
            sum(subset(DATA,method=='LL'& zone==dis.zoun[z])$number)," individuals)",sep="")
  legend('bottomright',LGN,bty='n')
}
mtext("Proportion of total catch",1,outer=T,line=0.1,cex=1.2) 
dev.off()

#by LL configuration 
LL.configs=DATA%>%
  filter(method=="LL")%>%
  filter(!(is.na(hooktype)| is.na(hooksize) | is.na(wiretrace)))%>%
  group_by(LL.config)%>%
  tally()%>%pull(LL.config)
fn.fig(le.paste("Observer/catch_comp/Catch.comp_by_LL.configuration"),2400,1800)
smart.par(length(LL.configs),MAR=c(2,10.5,1,.1),OMA=c(1,.1,.5,.8),MGP=c(1.5,.5,0))
par(cex.axis=.9)
for(i in 1:length(LL.configs))
{
  d=DATA%>%
    filter(method=="LL" & LL.config==LL.configs[i])
  fn.brplt(d=d,Grouped.species=Grouped.species.LL,CX.nm=.7)
  mtext(paste(LL.configs[i]," (",nrow(d)," individuals)",sep=""),3,cex=.6)
  rm(d)
}
mtext("Proportion of total catch",1,outer=T,line=0.1,cex=1.2) 
dev.off()

#by snood
nn=DATA%>%
  filter(method=="LL")%>%
  filter(!(is.na(hooktype)| is.na(hooksize) | is.na(wiretrace)))%>%
  group_by(wiretrace)%>%
  tally()%>%pull(wiretrace)
fn.fig(le.paste("Observer/catch_comp/Catch.comp_by_snood"),2400,1800)
smart.par(length(nn),MAR=c(2,10.5,1,.1),OMA=c(1,.1,.5,.8),MGP=c(1.5,.5,0))
par(cex.axis=.9)
for(i in 1:length(nn))
{
  d=DATA%>%
    filter(method=="LL" & wiretrace==nn[i])
  fn.brplt(d=d,Grouped.species=Grouped.species.LL,CX.nm=.7)
  mtext(paste(nn[i]," (",nrow(d)," individuals)",sep=""),3,cex=.6)
  rm(d)
}
mtext("Proportion of total catch",1,outer=T,line=0.1,cex=1.2) 
dev.off()


#Multivariate stats
UNIK.gear.zone=unique(paste(DATA$method,DATA$zone))
colfunc <- colorRampPalette(c("orange","firebrick1"))
n.col.GN=colfunc(length(grep("GN",UNIK.gear.zone)))
colfunc <- colorRampPalette(c("lightblue3", "dodgerblue4"))
n.col.LL=colfunc(length(grep("LL",UNIK.gear.zone)))
dis.cols=c(n.col.GN,n.col.LL)
names(dis.cols)=UNIK.gear.zone
multivariate.fn(d=DATA%>%  
                  filter(!is.na(zone))%>%
                  filter( !is.na(method))%>%
                  mutate(species=CAES_Code,
                         method.zone=paste(method,zone),
                         livewt.c=number)%>%
                  dplyr::select(method,zone,block,month,common_name,species,method.zone,livewt.c),
                Terms=c('method.zone','block','month'),
                Def.sp.term=c('method','species'),
                Transf='proportion',
                Show.term='method.zone',
                Group='Top20',
                hndl=le.paste("Observer"),
                MDS.title="/catch_comp/Catch.comp_GN_LL_MDS",
                Simper.title="/catch_comp/Catch.comp_GN_LL_Simper",
                Permanova.title="/catch_comp/Catch.comp_GN_LL_Permanova",
                simper.cumsum=0.8,
                LGSIZE=10.5)  
rm(Community)  
#---------Observer size distribution ------------ 

#GN vs LL for main species
dumi=DATA%>%
  filter(common_name%in%Main.species)%>%
  mutate(size=Size)
Sumri=dumi %>%
  group_by(common_name,Taxa,method) %>% 
  summarise(n=sum(number),
            Min=min(size,na.rm=T),
            Max=max(size,na.rm=T),
            size=median(size,na.rm=T))%>%
  filter(n>2)%>% 
  ungroup()%>%
  mutate_if(is.numeric, round)

fn.fig(le.paste("Observer/size/Size.density.dist.main.sp_GN_LL"),2400,1600)
dumi%>%
  ggplot(aes(x = size, y = common_name,fill=Taxa)) + 
  geom_density_ridges(alpha = .8, color = "white")+
  facet_wrap(~method)+
  scale_fill_manual(values=c("steelblue","firebrick"))+
  ylab('')+
  xlab('Size (cm, FL for elasmobranchs, TL for teleosts)')+
  theme_PA(leg.siz=13,axs.t.siz=12,axs.T.siz=14)+
  theme(legend.position = "top",
        legend.title = element_blank())+
  geom_text(data=Sumri,aes(label=paste('n= ',n,', median= ',size,' (',Min,'-',Max,')',sep='')),
            position=position_nudge(y=-0.1), colour="black", size=3)
dev.off()


#by LL configuration for main species
dumi=DATA%>%
  filter(common_name%in%Main.species)%>%
  filter(LL.config%in%LL.configs)%>%
  mutate(size=ifelse(Taxa=="Elasmobranch",fl,
                     ifelse(Taxa=="Teleost",tl,
                            NA)))
Sumri=dumi %>% group_by(common_name,Taxa,LL.config) %>% 
  summarise(n=sum(number),size=median(size,na.rm=T)) %>%
  filter(n>2)

fn.fig(le.paste("Observer/size/Size.density.dist.main.sp_by_LL.configuration"),2400,1600)
dumi%>%
  ggplot(aes(x = size, y = common_name,fill=Taxa)) + 
  geom_density_ridges(alpha = .8, color = "white")+
  facet_wrap(~LL.config)+
  scale_fill_manual(values=c("steelblue","firebrick"))+
  ylab('')+
  xlab('Size (cm, FL for elasmobranchs, TL for teleosts)')+
  theme_PA(leg.siz=12,str.siz=10,axs.t.siz=11,axs.T.siz=13)+
  theme(legend.position = "top",
        legend.title = element_blank())+
  geom_text(data=Sumri,aes(label=paste('n= ',n,', median= ',round(size)," cm",sep='')),
            position=position_nudge(y=-0.1), colour="black", size=2.5)
dev.off()


#Compare overlap between GN and LL for most commonly caught species
Tabl.sizes=DATA%>%
  filter(!is.na(Size) & !is.na(common_name))%>%
  group_by(common_name,method)%>%
  tally()%>%
  spread(method,n,fill=0)%>%
  filter(GN>10 & LL>10)%>%
  pull(common_name)

Overlap.res=vector('list',length(Tabl.sizes))
for(o in 1:length(Overlap.res))
{
  x <- list(GN=DATA%>%
              filter(common_name==Tabl.sizes[o] & !is.na(Size) & method=='GN')%>%
              pull(Size),
            LL=DATA%>%
              filter(common_name==Tabl.sizes[o] & !is.na(Size) & method=='LL')%>%
              pull(Size))
  out <- overlap(x)
  #final.plot(x,out$OV)
  Overlap.res[[o]]=data.frame(common_name=Tabl.sizes[o],
                              overlap=out$OV,
                              N.GN=length(x$GN),
                              N.LL=length(x$LL))
}
Overlap.res=do.call(rbind,Overlap.res)
fn.fig(le.paste("Observer/size/Size.density.dist.overlap_GN_LL"),2700,1600)
DATA%>%
  filter(common_name%in%Tabl.sizes & !is.na(Size))%>%
  left_join(Overlap.res,by='common_name')%>%
  mutate(method=ifelse(method=="GN","Gillnet",
                       ifelse(method=="LL","Longline",
                              NA)),
         common_name=ifelse(common_name=="West Australian dhufish","WA dhufish",
                            common_name),
         LBL=paste(common_name," (N=",N.GN," GN, ",N.LL," LL; ","overlap= ",100*round(overlap,2),"%)",sep=''))%>%
  ggplot(aes(x = Size, fill=method)) +
  facet_wrap( ~LBL,scales='free') + 
  geom_density(alpha=.5) +
  ylab('Density')+
  xlab('Size (cm, FL for elasmobranchs, TL for teleosts)')+
  theme_PA(leg.siz=10,strx.siz=8,axs.t.siz=9,axs.T.siz=12)+
  theme(legend.position = "top",
        legend.title = element_blank())
dev.off()

#Kolmogorov-Smirnov test
KS.result=vector('list',length(Tabl.sizes))
fn.kolmo=function(GN,LL)  Kolmo=ks.test(GN,LL)
for(s in 1:length(KS.result))
{
  KS.result[[s]]=fn.kolmo(GN=DATA%>%
                            filter(common_name==Tabl.sizes[s] & !is.na(Size) & method=='GN')%>%
                            pull(Size),
                          LL=DATA%>%
                            filter(common_name==Tabl.sizes[s] & !is.na(Size) & method=='LL')%>%
                            pull(Size))
  
}
Tab.Kolmo=as.data.frame(do.call(rbind,KS.result))
Tab.Kolmo=cbind(Species=Tabl.sizes,Tab.Kolmo)
rownames(Tab.Kolmo)=NULL
Tab.Kolmo=data.frame(Species=unlist(Tab.Kolmo$Species),
                     Statistic=round(unlist(Tab.Kolmo$statistic),3),
                     P.value=round(unlist(Tab.Kolmo$p.value),3))
Tab.Kolmo=Tab.Kolmo%>%
  mutate(P.value=ifelse(P.value<0.001,'<0.001',P.value))
write.csv(Tab.Kolmo,le.paste("Observer/size/Size.Kolmo_Smirnov.csv"),row.names = F)

#Stats
 this.sp.sizes=DATA%>%
   filter(!is.na(Size))%>%
   group_by(common_name)%>%
   tally()%>%
   filter(n>50)%>%
   pull(common_name)
this.sp.sizes=Tabl.sizes
fn.glm.size=function(d,Formula,Pred,Anova.title)
{
  d=d%>%
    mutate(method=as.factor(method),
           zone=as.factor(zone))
  
  #rum model
  mod=gam(Formula,dat=d,family=gaussian(link = "identity"))
  
  
  #export anova table
  ft <- as_flextable(mod)
  save_as_image(ft, path = le.paste(paste("ANOVA_",Anova.title,".png",sep='')))
  rm(ft)
  
  if(any(Pred=="method"))
  {
    p=pred.fun(mod=mod,biascor="NO",PRED=Pred)%>%
      mutate(species=NM)
    return(p)
  }else
  {
    a=summary(mod)$p.pv
    a=a[grep(paste(Pred, collapse="|"),names(a))]
    a=subset(a,a<0.05)
    if(length(a)>0)
    {
      #predict term
      aa=Pred[str_detect(paste(names(a),collapse='|'), Pred)]
      p=vector('list',length(aa))
      names(p)=aa
      for(ss in 1:length(p))
      {
        p[[ss]]=pred.fun(mod=mod,biascor="NO",PRED=Pred[str_detect(names(a)[ss], Pred)])%>%
          mutate(species=NM)
      }
      return(p)
    }
    rm(a)
  }
  
}

Store.zone.method=vector('list',length(this.sp.sizes))
names(Store.zone.method)=this.sp.sizes

#Gear effect
for(m in 1:length(this.sp.sizes))
{
  NM=this.sp.sizes[m]
  print(paste(NM,'size GLM-------------'))
  
  D=DATA%>%
    filter(!is.na(Size) & common_name ==NM)
  Tab=D%>%
    group_by(method)%>%
    tally()%>%
    mutate(n=ifelse(n>=15,1,0))
  if(sum(Tab$n)>1)
  {
    Store.zone.method[[m]]=fn.glm.size(d=D,
                                       Formula=formula(Size~method+zone),
                                       Pred='method',
                                       Anova.title=paste("Method",NM,sep='_'))
  }
  
}
fn.fig(le.paste("Observer/size/Standardized_Size_Method"),2400,2400)
out=fn.barplot.cpue(d=do.call(rbind,Store.zone.method)%>%
                      mutate(species=factor(species,levels=this.sp.sizes),
                             x=method,
                             y=emmean,
                             fill=method,
                             facet=species),
                    YLAB="Relative standardized size",
                    XLAB='Method',
                    cex=14,
                    Relative="Yes",
                    NROW=3)
dev.off()

#Longline configuration effect
Store=vector('list',length(this.sp.sizes))
names(Store)=this.sp.sizes
for(m in 1:length(this.sp.sizes))
{
  NM=this.sp.sizes[m]
  print(paste(NM,'size GLM-------------'))
  
  D=DATA%>%
    filter(!is.na(Size) & common_name ==NM & method=='LL')%>%
    filter(!is.na(hooktype) & !is.na(wiretrace) & !is.na(hooksize))%>%
    mutate(hooktype=as.factor(hooktype),
           wiretrace=as.factor(wiretrace),
           hooksize=factor(hooksize))
  
  Tab=D%>%
    group_by(hooksize)%>%
    tally()%>%
    mutate(n=ifelse(n>=10,1,0))
  if(sum(Tab$n)>1)
  {
    a=fn.glm.size(d=D,  
                  Formula=formula(Size~hooktype+hooksize+wiretrace+zone),
                  Pred=c('hooktype','hooksize','wiretrace'),
                  Anova.title=paste("longine.config",NM,sep='_'))
    if(!is.null(a)) Store[[m]]=a
  }
}

fun.list.out=function(LIS,what)
{
  dummy=vector('list',length(LIS))
  for(s in 1:length(LIS))
  {
    X=match(what,names(LIS[[s]]))
    if(!is.na(X)) dummy[[s]]=LIS[[s]][[X]]
  }
  dummy=dummy[!sapply(dummy,is.null)]
  return(dummy <- do.call(rbind,dummy))
}

#Hook size
fn.fig(le.paste("Observer/size/Standardized_Size_Hook.size"),2000,1400)
out=fn.barplot.cpue(d=fun.list.out(LIS=Store,what='hooksize')%>%
                      mutate(species=factor(species,levels=this.sp.sizes),
                             x=hooksize,
                             y=emmean,
                             fill=hooksize,
                             facet=species),
                    YLAB="Relative standardized size",
                    XLAB='Hook size',
                    cex=14,
                    Relative="Yes")
dev.off()

#Hook type
fn.fig(le.paste("Observer/size/Standardized_Size_Hook.type"),1400,2400)
out=fn.barplot.cpue(d=fun.list.out(LIS=Store,what='hooktype')%>%
                      mutate(species=factor(species,levels=this.sp.sizes),
                             x=hooktype,
                             y=emmean,
                             fill=hooktype,
                             facet=species),
                    YLAB="Relative standardized size",
                    XLAB='Hook type',
                    cex=14,
                    Relative="Yes")
dev.off()


#Wire trace
if(!is.null(fun.list.out(LIS=Store,what='wiretrace')))
{
  fn.fig(le.paste("Observer/size/Standardized_Size_Snood.type"),2000,2000)
  out=fn.barplot.cpue(d=fun.list.out(LIS=Store,what='wiretrace')%>%
                        mutate(species=factor(species,levels=this.sp.sizes),
                               x=wiretrace,
                               y=emmean,
                               fill=wiretrace,
                               facet=species),
                      YLAB="Relative standardized size",
                      XLAB='Hook type',
                      cex=14,
                      Relative="Yes")
  dev.off()
}

#---------General tables and plots of PA underwater Video ------------ 
SP.group.levels=c("Invertebrates","Scalefish","Sharks","Rays","Marine mammals","Seabirds","Turtles")
TEP.groups=c("Marine mammals","Seabirds","Reptiles","Turtles")

#Mean soak time by gear
DATA%>%
  distinct(sheet_no,.keep_all=T)%>%
  group_by(method)%>%
  summarise(Mean=mean(soak.time,na.rm=T),
            MIN=min(soak.time,na.rm=T),
            MAX=max(soak.time,na.rm=T))%>%
  arrange(method)

#Total soak time 
# soak.time1 <- (DATA_PA$soak.time)
  
# sum(soak.time1, na.rm=T)  


#1. number of events 
#added line ( filter(!Interaction=='Missing data')%>% ) to remove Missing data from interactions
#remove NA from SP group: (  filter(!SP.group%in%NA)%>% )
do.events=TRUE
if(do.events)
{
  #1.1. by gear and species group
rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species),
        Video.net.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species))%>%
    filter(!Species=='birds feeding at surface')%>%
    filter(!SP.group%in%TEP.groups)%>%
    filter(!Interaction=='Missing data')%>%
    filter(!SP.group%in%NA)%>%
    mutate(Number=1,
           Method=capitalize(Method))%>%
    group_by(Method,Interaction,SP.group)%>%
    tally(Number)%>%
    filter(!is.na(Interaction))%>%
    mutate(Interaction=capitalize(tolower(Interaction)),
           SP.group=factor(SP.group,levels=SP.group.levels))%>%
    ggplot(aes(fill=Method, y=n, x=Interaction)) + 
    geom_bar(position="dodge", stat="identity")+
    coord_flip() + scale_y_sqrt()+
    facet_wrap(~SP.group,dir='h',scales='free_x')+ 
    theme(legend.position = "top",
          strip.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          axis.text=element_text(size=14),
          axis.title=element_text(size=16))+
    xlab('')+ylab('Number of events')+ guides(color = guide_legend(nrow = 1))
ggsave(le.paste("Video/underwater/Interactions_number.events_sqrt.transf_by_group.tiff"),
        width = 12,height = 10,compression = "lzw")

  #1.2. by gear
rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species),
        Video.net.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species))%>%
    filter(!Species=='birds feeding at surface')%>%
    filter(!SP.group%in%TEP.groups)%>%
    filter(!Interaction=='Missing data')%>%
    mutate(Number=1,
           Method=capitalize(Method))%>%
    group_by(Method,Interaction)%>%
    tally(Number)%>%
    filter(!is.na(Interaction))%>%
    mutate(Interaction=capitalize(tolower(Interaction)))%>%
    ggplot(aes(fill=Method, y=n, x=Interaction)) + 
    geom_bar(position="dodge", stat="identity")+
    coord_flip() + scale_y_sqrt()+
    facet_wrap(~Method,dir='h',scales='free_x')+ 
    theme(legend.position = "top",
          strip.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          axis.text=element_text(size=14),
          axis.title=element_text(size=16))+
    xlab('')+ylab('Number of events')+ guides(color = guide_legend(nrow = 1))
ggsave(le.paste("Video/underwater/Interactions_number.events_sqrt.transf.tiff"),width = 16,height = 8,compression = "lzw")
  
}

#2. Number of individuals  
#2.1. by gear and species group
rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species),
      Video.net.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species))%>%
  filter(!Species=='birds feeding at surface')%>%
  filter(!SP.group%in%TEP.groups)%>%
  filter(!SP.group%in%NA)%>%
  mutate(Method=capitalize(Method))%>%
  group_by(Method,Interaction,SP.group)%>%
  # tally(Number, name="n")%>%  #workaround for no "swim past" data-check
  summarise(n=sum(Number))%>%
  filter(!is.na(Interaction))%>%
  mutate(Interaction=capitalize(tolower(Interaction)),
         SP.group=factor(SP.group,levels=SP.group.levels))%>%
  ggplot(aes(fill=Method, y=n, x=Interaction)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip() + scale_y_sqrt()+
  facet_wrap(~SP.group,dir='h',scales='free_x')+ 
  theme_PA(strx.siz=17,leg.siz=18,axs.t.siz=14,axs.T.siz=16)+
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.margin = margin(.3, 1, .3, .3, "cm"))+
  xlab('')+ylab('Number of individuals')+
  guides(color = guide_legend(nrow = 1))
ggsave(le.paste("Video/underwater/Interactions_number.individuals_sqrt.transf_by.group.tiff"),
       width = 15,height = 10,compression = "lzw")

#2.2. by gear
rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species),
      Video.net.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species))%>%
  filter(!Species=='birds feeding at surface')%>%
  filter(!SP.group%in%TEP.groups)%>%
  #filter(!Interaction=='Missing data')%>%
  mutate(Method=capitalize(Method))%>%
  group_by(Method,Interaction)%>%
  #tally(Number, name="n")%>%      #workaround for no "swim past" data-check
  summarise(n=sum(Number))%>%
  filter(!is.na(Interaction))%>%
  mutate(Interaction=capitalize(tolower(Interaction)))%>%
  ggplot(aes(fill=Method, y=n, x=Interaction)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip() + scale_y_sqrt()+
  facet_wrap(~Method,dir='h',scales='free_x')+ 
  theme_PA(strx.siz=17,leg.siz=18,axs.t.siz=14,axs.T.siz=16)+
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.margin = margin(.3, 1, .3, .3, "cm"))+
  xlab('')+ylab('Number of individuals')+
  guides(color = guide_legend(nrow = 1))
ggsave(le.paste("Video/underwater/Interactions_number.individuals_sqrt.transf.tiff"),
       width = 13,height = 8,compression = "lzw")

#2.3. Main target species only
these.codes=c(37018003,37017003,37017001,37018007,
              37377004,37384002,37320004,37353001)
dummy=data.frame(Code=these.codes,
                 Names=All.species.names[match(these.codes,All.species.names$Code),"COMMON_NAME"])
colfunc <- colorRampPalette(Shark.palette)
n.col.elasmos=colfunc(length(these.codes[these.codes<37300000]))
colfunc <- colorRampPalette(Teleost.palette)  
n.col.teleos=colfunc(length(these.codes[these.codes>37300000]))
d=rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number,Code),
        Video.net.interaction%>%dplyr::select(Method,Interaction,Number,Code))%>%
  mutate(Method=capitalize(Method))%>%
  filter(Code%in%these.codes)%>%
  left_join(dummy,by='Code')%>%
  group_by(Method,Interaction,Names)%>%
  tally(Number)%>%
  filter(!is.na(Interaction))%>%
  mutate(Interaction=capitalize(tolower(Interaction)),
         Names=factor(Names,levels=dummy%>%arrange(Code)%>%pull(Names)))
dis.cols=c(n.col.elasmos,n.col.teleos)
names(dis.cols)=levels(d$Names)
dis.cols=dis.cols[match(levels(droplevels(d$Names)),names(dis.cols))]
d%>%
  ggplot(aes(fill=Names, y=n, x=Interaction)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip() + 
  facet_wrap(~Method,dir='h',scales='free_x')+ 
  theme_PA(strx.siz=17,leg.siz=18,axs.t.siz=14,axs.T.siz=16)+
  theme(legend.position = "top",
        legend.title = element_blank())+
  xlab('')+ylab('Number of individuals')+ guides(color = guide_legend(nrow = 1))+
  scale_fill_manual(values=dis.cols)
ggsave(le.paste("Video/underwater/Interactions_number.individuals_main.target.tiff"),
        width = 12,height = 10,compression = "lzw")

#3. Export data for Abbey
export.abbeys=FALSE
if(export.abbeys)
{
  add.effort=rbind(Video.longline.interaction%>%dplyr::select(sheet_no,Method,Camera),
                   Video.net.interaction%>%dplyr::select(sheet_no,Method,Camera))%>%
    mutate(Method=capitalize(Method))%>%
    group_by(sheet_no,Method,Camera)%>%
    summarise(n=n())%>%
    group_by(sheet_no,Method)%>%
    summarise(n.cameras=n())
  Abbey.data.chapter.1_species=rbind(Video.longline.interaction%>%dplyr::select(sheet_no,Method,Interaction,Number,Code,SP.group),
                                     Video.net.interaction%>%dplyr::select(sheet_no,Method,Interaction,Number,Code,SP.group))%>%
    mutate(Method=capitalize(Method),
           Interaction=capitalize(tolower(Interaction)))%>%
    filter(!SP.group%in%c('Macroalgae','Rock/reef structure'))%>%
    group_by(sheet_no,Method,Interaction,Code)%>%
    summarise(Number=sum(Number))%>%
    spread(Code,Number,fill = 0)%>%
    data.frame%>%
    left_join(add.effort,by=c('sheet_no','Method'))
  Abbey.data.chapter.1_SP.group=rbind(Video.longline.interaction%>%dplyr::select(sheet_no,Method,Interaction,Number,SP.group),
                                      Video.net.interaction%>%dplyr::select(sheet_no,Method,Interaction,Number,SP.group))%>%
    mutate(Method=capitalize(Method),
           Interaction=capitalize(tolower(Interaction)))%>%
    filter(!SP.group%in%c('Macroalgae','Rock/reef structure'))%>%
    group_by(sheet_no,Method,Interaction,SP.group)%>%
    summarise(Number=sum(Number))%>%
    spread(SP.group,Number,fill = 0)%>%
    data.frame%>%
    left_join(add.effort,by=c('sheet_no','Method'))
  Abbey.data.chapter.1_Retain.group=rbind(Video.longline.interaction%>%dplyr::select(sheet_no,Method,Interaction,Number,Retain.group),
                                          Video.net.interaction%>%dplyr::select(sheet_no,Method,Interaction,Number,Retain.group))%>%
    mutate(Method=capitalize(Method),
           Interaction=capitalize(tolower(Interaction)))%>%
    group_by(sheet_no,Method,Interaction,Retain.group)%>%
    summarise(Number=sum(Number))%>%
    filter(!is.na(Retain.group))%>%
    spread(Retain.group,Number,fill = 0)%>%
    data.frame%>%
    left_join(add.effort,by=c('sheet_no','Method'))
  write.csv(Abbey.data.chapter.1_species,handl_OneDrive('Git_other/Analyses/Parks_Australia/outputs/Abbey.data.chapter.1_species.csv'),row.names = F)
  write.csv(Abbey.data.chapter.1_SP.group,handl_OneDrive('Git_other/Analyses/Parks_Australia/outputs/Abbey.data.chapter.1_SP.group.csv'),row.names = F)
  write.csv(Abbey.data.chapter.1_Retain.group,handl_OneDrive('Git_other/Analyses/Parks_Australia/outputs/Abbey.data.chapter.1_Retain.group.csv'),row.names = F)
  
}
  


#4. Lolipop approach
do.loli=TRUE
if(do.loli)
{
  #Lollipop graph of % of interaction types by gear 
  df=rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number),
           Video.net.interaction%>%dplyr::select(Method,Interaction,Number))%>%
  filter(!Interaction=='NA')
  df=pct_routine(df, Method, Interaction)
  ggplot(df)+
    geom_linerange(aes(x = Interaction, ymin = 0, ymax = pct, colour = Method), 
                   position = position_dodge(width = 1))+
    geom_point(aes(x = Interaction, y = pct, colour = Method),
               position = position_dodge(width = 1))+
    coord_flip()
  ggsave(le.paste("Video/underwater/Interactions_lollipop.tiff"), 
          width = 12,height = 10,compression = "lzw")
  
  
  #Mosaic plot
  #gillnet
  TAB=table(Video.net.interaction$Interaction)
  lab=c(TAB)
  lab=paste(names(TAB)," (n= ",lab,")",sep='')
  names(lab)=names(TAB)
  
  ggplot(data = Video.net.interaction) +
    geom_mosaic(aes(x = product(SP.group), fill=SP.group), na.rm=TRUE) + 
    labs(x = "",y='', title='Gillnet')+
    facet_wrap(~Interaction,labeller = labeller(Interaction=lab)) + labs(fill = "")
   ggsave(le.paste("Video/underwater/Interactions_mosaic_gillnet.tiff"), 
          width = 12,height = 10,compression = "lzw")
   
  #longline
  TAB=table(Video.longline.interaction$Interaction)
  lab=c(TAB)
  lab=paste(names(TAB)," (n= ",lab,")",sep='')
  names(lab)=names(TAB)
  
  ggplot(data = Video.longline.interaction %>% filter(!is.na(Interaction))) +
    geom_mosaic(aes(x = product(SP.group), fill=SP.group), na.rm=TRUE) + 
    labs(x = "",y='', title='Longline')+
    facet_wrap(~Interaction,labeller = labeller(Interaction=lab)) + labs(fill = "")
   ggsave(le.paste("Video/underwater/Interactions_mosaic_longline.tiff"), 
          width = 12,height = 10,compression = "lzw")
  
}


#5. Test differences in interactions by method and species group from underwater camera
fn.mds=function(tav,COM,SUBT)
{
  iid=match(c('Method','SP.group','SPECIES'),names(tav))
  drop.col=colSums(tav[,-iid])
  drop.col=names(drop.col[drop.col==0])
  COM=COM[,-match(drop.col,names(COM))]
  MDS <- metaMDS(comm = COM, distance = "bray",k=2,trymax=100, trace = FALSE, autotransform = FALSE)
  MDS_xy <- data.frame(MDS$points)%>%
    mutate(SP.group= tav$SP.group)
  p=ggplot(MDS_xy, aes(MDS1, MDS2)) +
    geom_point(aes(shape=SP.group, color = SP.group),size=3) +
    theme_bw() +
    annotate(geom="text", x=0.85*max(MDS_xy$MDS1), y=min(MDS_xy$MDS2), 
             size=3.5,label=paste("Stress=",round(MDS$stress,3)))+
    theme_PA(Sbt.siz=16)+
    theme(legend.position = "top",
          legend.title = element_blank())+
    guides(colour = guide_legend(nrow = 1))+
    labs(subtitle =SUBT, y='', x='') 
  return(p)
}
simper.fn=function(COM,tav,SUBT,Sig.pair)
{
  SIMPER <- summary(simper(COM, tav%>%pull(SP.group),parallel=7))
  Get=as.data.frame(str_split(names(SIMPER), "_", simplify = TRUE))%>%
    mutate(id=1:n())
  SIMPER=SIMPER[Get$id]
  
  disp.simp=vector('list',length(SIMPER))
  for(n in 1:length(disp.simp))
  {
    disp.simp[[n]]=cbind(tav['SP.group'],
                         COM[row.names(SIMPER[[n]]%>%filter(cumsum<=simper.cumsum))])%>%
      filter(SP.group%in%str_split(names(SIMPER)[n], "_", simplify = TRUE))%>%
      gather(species,prop,-SP.group,)%>%
      mutate(groups=names(SIMPER)[n])
  }
  disp.simp=do.call(rbind,disp.simp)
  p=disp.simp%>%
    filter(groups%in%str_remove(Sig.pair,"vs_"))%>%
    group_by(SP.group,groups,species)%>%
    summarise(prop=mean(prop))%>%
    ggplot(aes(x=SP.group,y=prop, fill=species))+
    geom_bar(stat="identity", width = 0.5)+
    ylab("Average proportion")+xlab("Group")+
    theme_PA(Ttl.siz=18,Sbt.siz=18,strx.siz=14,
             leg.siz=13,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
    guides(fill = guide_legend(nrow = 2))+
    labs(subtitle =SUBT, y='', x='') 
  return(p)
}
fn.test.underwtr=function(what,trans)
{
  d=rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Genus,Species,Code),
          Video.net.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Genus,Species,Code))%>%
    filter(!Species=='birds feeding at surface')%>%
    filter(!SP.group%in%TEP.groups)%>%
    filter(!is.na(Interaction))%>%  
    mutate(Method=capitalize(Method),
           Interaction=capitalize(tolower(Interaction)),
           SP.group=factor(SP.group,levels=SP.group.levels))%>%
    left_join(All.species.names%>%
                dplyr::select(COMMON_NAME,Code)%>%
                distinct(Code,.keep_all=T),
              by="Code")%>%
    mutate(SPECIES=paste(Genus,Species))
  if(what=='Events') d=d%>%mutate(Number=1)
  
  #Table of all behaviours by species
  TAB=d%>%
    group_by(Method,SP.group,Interaction,COMMON_NAME,SPECIES)%>%
    summarise(N=sum(Number))%>%
    spread(Interaction,N,fill='')%>%
    arrange(Method,SP.group,COMMON_NAME)
  write.csv(TAB,le.paste("Video/underwater/Table_number.interactions.by.species.csv"),row.names=F)
  
  
  #Multivariate - differences in behaviours between method and species group
  TAB=d%>%
    group_by(Method,SP.group,Interaction,SPECIES)%>%
    summarise(N=sum(Number))%>%
    spread(Interaction,N,fill=0)%>%
    arrange(Method,SP.group)
  id=match(c('Method','SP.group','SPECIES'),names(TAB))
  
  TAB.GN=TAB%>%filter(Method=="Gillnet")%>%filter(SP.group%in%c('Scalefish','Sharks','Rays'))
  TAB.LL=TAB%>%filter(Method=="Longline")%>%filter(SP.group%in%c('Scalefish','Sharks','Rays'))
  
  Community.GN <<- TAB.GN[-id]
  if(trans=='proportion') Community.GN=Community.GN/rowSums(Community.GN)
  if(trans=='squareroot') Community.GN=Community.GN^(1/2)
  if(trans=='4throot')    Community.GN=Community.GN^(1/4)
  
  Community.LL <<- TAB.LL[-id]
  if(trans=='proportion') Community.LL=Community.LL/rowSums(Community.LL)
  if(trans=='squareroot') Community.LL=Community.LL^(1/2)
  if(trans=='4throot')    Community.LL=Community.LL^(1/4)
  
  #MDS
  p.mdsGN=fn.mds(tav=TAB.GN,COM=Community.GN,SUBT="Gillnet")
  p.mdsLL=fn.mds(tav=TAB.LL,COM=Community.LL,SUBT="Longline")
  
  #Permanova
  # 1. overall significance test
  adon.results.GN<-adonis2(formula('Community.GN~SP.group'),data=TAB.GN%>%
                                                              mutate(SP.group=as.character(SP.group)),
                            method="bray",perm=5e3)
  adon.results.LL<-adonis2(formula('Community.LL~SP.group'),data=TAB.LL%>%
                                                                  mutate(SP.group=as.character(SP.group)),
                           method="bray",perm=5e3)
  
  # 2. multilevel pairwise comparison with adjusted p-values
  dummy=pairwise.adonis2(Community.GN~SP.group,data=TAB.GN)
  adonis.pairwise=vector('list',(length(dummy)-1))
  for(qq in 2:length(dummy))
  {
    adonis.pairwise[[qq]]=data.frame(Pairs=names(dummy)[[qq]],
                                     P=dummy[[qq]]$`Pr(>F)`[1])
  }
  adonis.pairwise.GN=do.call(rbind,adonis.pairwise)
  
  dummy=pairwise.adonis2(Community.LL~SP.group,data=TAB.LL)
  adonis.pairwise=vector('list',(length(dummy)-1))
  for(qq in 2:length(dummy))
  {
    adonis.pairwise[[qq]]=data.frame(Pairs=names(dummy)[[qq]],
                                     P=dummy[[qq]]$`Pr(>F)`[1])
  }
  adonis.pairwise.LL=do.call(rbind,adonis.pairwise)
  
  
  #Simper analysis to identify interactions that discriminate among groups
  simper.GN=simper.fn(COM=Community.GN,
                      tav=TAB.GN,
                      SUBT="Gillnet",
                      Sig.pair=adonis.pairwise.GN%>%filter(P<0.05)%>%pull(Pairs))
  simper.LL=NULL
  if(nrow(adonis.pairwise.LL%>%filter(P<0.05))>0)
  {
    simper.LL=simper.fn(COM=Community.LL,
                        tav=TAB.LL,
                        SUBT="Longline",
                        Sig.pair=adonis.pairwise.LL%>%filter(P<0.05)%>%pull(Pairs))
  }
  
  return(list(p.mdsGN=p.mdsGN,
              p.mdsLL=p.mdsLL,
              Permanova.GN=as.data.frame(adon.results.GN),
              Permanova.LL=as.data.frame(adon.results.LL),
              adonis.pairwise.GN=adonis.pairwise.GN,
              adonis.pairwise.LL=adonis.pairwise.LL,
              simper.GN=simper.GN,
              simper.LL=simper.LL))
}
simper.cumsum=.95
out.under.n=fn.test.underwtr(what='Numbers',trans='squareroot')
rm(Community.GN,Community.LL)
#export stuff
{
ggarrange(plotlist=list(out.under.n$p.mdsGN+rremove("xlab")+rremove("ylab"),
                        out.under.n$p.mdsLL+rremove("xlab")+rremove("ylab")),
          ncol = 1,
          common.legend=TRUE)
ggsave(le.paste("Video/underwater/interactions_MDS.tiff"),
      width = 6,height = 9,compression = "lzw")


write.csv(out.under.n$Permanova.GN,le.paste("Video/underwater/interactions_Permanova.GN.csv"))
write.csv(out.under.n$Permanova.LL,le.paste("Video/underwater/interactions_Permanova.LL.csv"))
write.csv(out.under.n$adonis.pairwise.GN,le.paste("Video/underwater/interactions_Permanova_pairwise.GN.csv"),row.names = F)
write.csv(out.under.n$adonis.pairwise.LL,le.paste("Video/underwater/interactions_Permanova_pairwise.LL.csv"),row.names = F)

plist=list(out.under.n$simper.GN)
if(!is.null(out.under.n$simper.LL))plist=list(out.under.n$simper.GN,out.under.n$simper.LL) #%>%discard(is.null)
fig=ggarrange(plotlist=plist,ncol = 1,common.legend=FALSE)
annotate_figure(fig,
                bottom = text_grob("Group",size = 20),
                left = text_grob("Average proportion",size = 20,rot = 90))

ggsave(le.paste("Video/underwater/interactions_simper.tiff"),
        width = 6,height = 8,compression = "lzw")
}




#fn.test.underwtr(what='Events',trans='squareroot')



#---------Analyse PA Habitat ------------
# NOTE: combine Underwater habitat classification with habitat damage (deck and underwater)

# 1. Underwater habitat classification. MISSING: Data still to be updated
do.habitat <- FALSE
if(do.habitat){
#gillnets
Video.habitat=Video.habitat%>%
  data.frame%>%
  filter(!BROAD=="Open water")%>%  #remove cameras facing up
  mutate(SHEET_NO=toupper(opcode),
         Depth_m=Depth..M.,
         BROAD=capitalize(tolower(BROAD)),
         MORPHOLOGY=capitalize(MORPHOLOGY),
         TYPE=capitalize(TYPE),
         BROAD=case_when(BROAD%in%c('Consolidated','Unconsolidated') ~'Substrate',
                         TRUE ~ BROAD))%>%
  filter(FieldOfView=='Facing Down')%>%
  filter(!BROAD=='Open Water')%>%
  dplyr::select(SHEET_NO,Depth_m,Collector,BROAD,MORPHOLOGY,TYPE,CODE)%>%
  mutate(Algae.morph=case_when(MORPHOLOGY=='Erect course branching'~'ECB',
                               MORPHOLOGY=='Erect fine branching'~'EFB',
                               MORPHOLOGY=='Filamentous/turf'~'FT',
                               MORPHOLOGY=='Large Canopy forming'~'LCF',
                               MORPHOLOGY=='Sheet-like/membraneous'~'SLM'))
#longline
Video.habitat.LL=Video.habitat.LL%>%
  data.frame%>%
  filter(!BROAD=="Open water")%>%  #remove cameras facing up
  mutate(SHEET_NO=toupper(opcode),
         Depth_m=Depth..M.,
         BROAD=capitalize(tolower(BROAD)),
         MORPHOLOGY=capitalize(MORPHOLOGY),
         TYPE=capitalize(TYPE),
         BROAD=case_when(BROAD%in%c('Consolidated','Unconsolidated') ~'Substrate',
                         TRUE ~ BROAD))%>%
  filter(FieldOfView=='Facing Down')%>%
  filter(!BROAD=='Open Water')%>%
  dplyr::select(SHEET_NO,Depth_m,Collector,BROAD,MORPHOLOGY,TYPE,CODE)%>%
  mutate(Algae.morph=case_when(MORPHOLOGY=='Erect course branching'~'ECB',
                               MORPHOLOGY=='Erect fine branching'~'EFB',
                               MORPHOLOGY=='Filamentous/turf'~'FT',
                               MORPHOLOGY=='Large Canopy forming'~'LCF',
                               MORPHOLOGY=='Sheet-like/membraneous'~'SLM'))
#Coarse data
do.barplt=T

#Overall Broad categories
Hab.cols=c('lightskyblue','red4','seagreen',
           'orange3','tomato4','khaki')
names(Hab.cols)=c('Hydroids','Macroalgae','Seagrasses',
                  'Sponges','Stony corals','Substrate')
coul=c('brown','green3',"firebrick2")
fn.habitat.damg.sub=function(d,TITL)
{
  p1=d%>%
    group_by(BROAD)%>%
    tally()%>%
    mutate(n=100*n/sum(n))%>%
    ggplot(aes(x=n,y=reorder(BROAD,n),fill=BROAD))+
    geom_bar(stat = "identity",colour="black")+
    xlim(0,100)+
    theme_PA(Ttl.siz=18,str.siz=20,strx.siz=20,
             leg.siz=16,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "none",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    scale_fill_manual(values=Hab.cols)+
    labs(x="Percentage",y="",title=TITL,subtitle="Habitat type")
  
  
  #Substrate categories
  colfunc <- colorRampPalette(c('khaki4', "khaki"))
  p2=d%>%
    filter(BROAD=='Substrate')%>%
    group_by(MORPHOLOGY)%>%
    tally()%>%
    mutate(n=100*n/sum(n))
  p2=p2%>%
    ggplot(aes(x=n,y=reorder(MORPHOLOGY,n),fill=MORPHOLOGY))+
    geom_bar(stat = "identity",colour="black")+
    xlim(0,100)+
    theme_PA(Ttl.siz=18,str.siz=20,strx.siz=20,
             leg.siz=16,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "none",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    scale_fill_manual(values=colfunc(nrow(p2)))+
    labs(x="Percentage",y="",subtitle="Substrate type")
  
  
  #Algae categories
  n=d%>%
    filter(BROAD=='Macroalgae' & !is.na(TYPE))%>%
    group_by(MORPHOLOGY)%>%
    tally()%>%
    rename(N=n)
  p3=d%>%
    filter(BROAD=='Macroalgae' & !is.na(TYPE))%>%
    group_by(MORPHOLOGY,TYPE)%>%
    tally()%>%
    ungroup()%>%
    mutate(n=100*n/sum(n))%>%
    left_join(n,by="MORPHOLOGY")%>%
    ggplot(aes(x=n,y=reorder(MORPHOLOGY,N),fill=TYPE))+
    geom_bar(stat = "identity",colour="black")+
    xlim(0,100)+
    theme_PA(Ttl.siz=18,str.siz=20,strx.siz=20,
             leg.siz=16,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "none",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    scale_fill_manual(values=coul)+
    labs(x="Percentage",y="",subtitle="Macroalgae type")
  
  return(list(p1=p1,p2=p2,p3=p3))
}
Out.damg_under_GN=fn.habitat.damg.sub(d=Video.habitat%>%
                                        filter(!BROAD=='Open water'),  #remove 'Open water' which means camera was pointing upwards
                                      TITL="Gillnet")
Out.damg_under_LL=fn.habitat.damg.sub(d=Video.habitat.LL%>%
                                        filter(!BROAD=='Open water'),
                                      TITL="Longline")
ggarrange(Out.damg_under_GN$p1, Out.damg_under_LL$p1,
          Out.damg_under_GN$p2, Out.damg_under_LL$p2,
          Out.damg_under_GN$p3, Out.damg_under_LL$p3,
          ncol = 2, nrow = 3)
ggsave(le.paste("Video/underwater/Habitats_coarse.records.tiff"),
       width = 10,height = 10,compression = "lzw")


# 2. Underwater observations of habitat damage
#gillnets  
Net.damage.underwater=Video.net.interaction%>%
  mutate(Habitat.damage=ifelse(
    SP.group%in%c("Macro algae","Rock/reef structure","Sponges"),"Damage",
    "No damage"))%>%
  dplyr::select(sheet_no,Camera,Habitat.damage)
Damage.events=Net.damage.underwater%>%
  group_by(Habitat.damage)%>%tally()
Total.observed.metres=length(unique(Video.net.interaction$OpCode))*metres.observed
text = paste(Damage.events%>%filter(Habitat.damage=='Damage')%>%pull(n),
             "habitat damage events in\n",
             Total.observed.metres,
             "metres of nets observed with \n",
             "  underwater cameras")
ggplot() + 
  annotate("text", x = 4, y = 25, size=7, label = text) + 
  ggtitle(label ="Underwater cameras")+
  theme_void()+
  theme(plot.title = element_text(size = 20, face = "bold"))
ggsave(le.paste("Video/underwater/Habitat.damage_GN.tiff"),
       width = 6,height = 10,compression = "lzw")

#longlines
Longline.damage.underwater=Video.longline.interaction%>%
  mutate(Habitat.damage=ifelse(
    SP.group%in%c("Macro algae","Rock/reef structure","Sponges"),"Damage",
    "No damage"))%>%
  dplyr::select(sheet_no,Camera,Habitat.damage)
Damage.events=Longline.damage.underwater%>%
  group_by(Habitat.damage)%>%tally()
if(!"Damage"%in%Damage.events$Habitat.damage)
{
  Damage.events=rbind(Damage.events,data.frame(Habitat.damage="Damage",n=0))
}
Total.observed.metres=length(unique(Video.longline.interaction$OpCode))*metres.observed
text = paste(Damage.events%>%filter(Habitat.damage=='Damage')%>%pull(n),
             "habitat damage events in\n",
             Total.observed.metres,
             "metres of longline observed with \n",
             "  underwater cameras")
ggplot() + 
  annotate("text", x = 4, y = 25, size=7, label = text) + 
  ggtitle(label ="Underwater cameras")+
  theme_void()+
  theme(plot.title = element_text(size = 20, face = "bold"))
ggsave(le.paste("Video/underwater/Habitat.damage_LL.tiff"),width = 6,height = 10,compression = "lzw")


# 3. Deck camera observations of habitat damage (gillnets only) 
Video.habitat.deck=Video.habitat.deck%>%
  data.frame%>%
  mutate(Period=tolower(Period),
         SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DIPRD.code,1,sep = "\\/"),'GN'),
                            Period=='longline'~str_remove(word(DIPRD.code,2,sep = "\\/"),'LL')),
         Percentage.cover=ifelse(original.meshed=='<1',1,
                          ifelse(original.meshed=='<10',10,original.meshed)),
         Percentage.cover=as.numeric(Percentage.cover),
         Species=capitalize(tolower(Species)),
         Frame_sheet=paste(SHEET_NO,Frame))%>%
  left_join(DATA%>%
              distinct(sheet_no,.keep_all=T)%>%
              dplyr::select(sheet_no,net_length),
            by=c("SHEET_NO"="sheet_no"))%>%
  mutate(net_length=net_length*1000,      #add net length in m
         Species=ifelse(Species=="Radiata","Macroalgae",Species),      #group Eklonia with macroalgae
         Period=ifelse(SHEET_NO%in%c("PA0129","PA0094","PA0084"),'longline',
                       Period),
         net_length=ifelse(is.na(net_length) & SHEET_NO=='PA0142',1800,net_length))   #wrongfully allocated method

fn.habitat.damg.deck=function(Gear)  
{
  d=Video.habitat.deck%>%
    filter(Period==Gear)%>%
    group_by(Frame,SHEET_NO,net_length,Species)%>%
    summarise(Percentage.cover=sum(Percentage.cover))%>%
    group_by(Frame,SHEET_NO,net_length)%>%
    mutate(Total.damage=sum(Percentage.cover))%>%
    spread(Species,Percentage.cover,fill = 0)%>%
    mutate(Dist.roller.spreader=distance.roller.spreader)%>%
    data.frame
  
  #add 0 habitat damage frames
  habitat.sheet=unique(d$SHEET_NO)
  dummy=vector('list',length(habitat.sheet))
  for(l in 1:length(habitat.sheet))
  {
    a=subset(d,SHEET_NO==habitat.sheet[l])
    n.exp=floor(a$net_length[1]/a$Dist.roller.spreader[1])
    Plus.row=n.exp-nrow(a)
    PLUS=a[1:Plus.row,]
    PLUS[,]=0
    PLUS$SHEET_NO=a$SHEET_NO[1]
    PLUS$Frame='dummy'
    PLUS$net_length=a$net_length[1]
    PLUS$Dist.roller.spreader=a$Dist.roller.spreader[1]
    a$Frame=as.character(a$Frame)
    dummy[[l]]=rbind(a,PLUS)
  }
  d.no.zeros=d
  d=do.call(rbind,dummy)            
  
  #exploratory stuff
  fn.hist=function(x,Main) hist(x,col=2,main=Main,xlab="",ylab="")
  fn.hab.explr=function(d.no_zeros,d)
  {
    par(mfrow=c(5,2),mar=c(2,2,1,.1),oma=c(2,2,3,.1),mgp=c(1.5,.6,0))
    fn.hist(d$Macroalgae,"Macroalgae")
    mtext("With 0s",3,line=1.5,col="steelblue",cex=1.5)
    fn.hist(d.no_zeros$Macroalgae,"Macroalgae")
    mtext("Without 0s",3,line=1.5,col="steelblue",cex=1.5)
    fn.hist(d$Coral,"Coral"); fn.hist(d.no_zeros$Coral,"Coral")
    fn.hist(d$Rock,"Rock"); fn.hist(d.no_zeros$Rock,"Rock")
    fn.hist(d$Seagrass,"Seagrass"); fn.hist(d.no_zeros$Seagrass,"Seagrass")
    fn.hist(d$Sponge,"Sponge"); fn.hist(d.no_zeros$Sponge,"Sponge")
    mtext('Percentage damage',1,outer=T,cex=1.25)
    mtext('Frequency',2,outer=T,cex=1.25,las=3)
    
    par(mfrow=c(1,2),mar=c(2,2,1,.1),oma=c(2,2,3,.1))
    fn.hist(d$Total.damage,"Total.damage")
    mtext("With 0s",3,line=1.5,col="steelblue",cex=1.5)
    fn.hist(d.no_zeros$Total.damage,"Total.damage")
    mtext("Without 0s",3,line=1.5,col="steelblue",cex=1.5)
    mtext('Percentage damage',1,outer=T,cex=1.25)
    mtext('Frequency',2,outer=T,cex=1.25,las=3)
  }
  pdf(le.paste(paste("Video/deck.cameras/Habitat.damage_explore_",Gear,".pdf",sep='')))
  fn.hab.explr(d.no_zeros=d.no.zeros,d=d)
  dev.off()
  
  
  #Pie chart of frame with damage / no damage
  p2=d%>%
    mutate(Damage=ifelse(Total.damage>0,"Damage","No damage"))%>%
    group_by(Damage)%>%
    tally()%>%
    filter(!is.na(Damage))%>%
    mutate(Percent=100*round(n/sum(n),2),
           ymax=cumsum(Percent),
           ymin = c(0, head(ymax, n=-1)),
           labelPosition =(ymax + ymin) / 2)%>%
    ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Damage)) +
    geom_rect() +
    geom_text(x=3.5,aes(y=labelPosition, label=paste(Percent,"%")),size = 8,check_overlap = TRUE)+
    scale_fill_manual(values=c("brown2","steelblue"))+ 
    coord_polar(theta="y") + xlim(c(2, 4)) + 
    ggtitle(label ="Deck cameras",
            subtitle = paste('Percentage of frames (~',distance.roller.spreader,' m of net) with damage',sep=''))+
    theme_void() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          plot.title = element_text(size = 20, face = "bold",hjust=-920),
          plot.subtitle = element_text(size = 17,face = "italic",hjust=0.3))
  
  #Pie chart of different habitat damage categories
  p3=d.no.zeros%>%
    dplyr::select(-Frame,-Total.damage,-SHEET_NO,-net_length,-Dist.roller.spreader)%>%
    gather(Species,Percent)%>%
    group_by(Species)%>%
    summarise(Percent=sum(Percent))%>%
    mutate(label=paste(round(100*Percent/sum(Percent)),'%',sep=''),
           ymax=cumsum(Percent),
           ymin = c(0, head(ymax, n=-1)),
           labelPosition =(ymax + ymin) / 2)%>%
    ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Species)) +
    geom_rect() +
    geom_text(x=3.5,aes(y=labelPosition, label=label,angle = 0),size = 5,check_overlap = TRUE)+
    scale_fill_manual(values=c("deepskyblue2","darkorange1","tomato4","forestgreen",'khaki3'))+ 
    coord_polar(theta="y") + xlim(c(2, 4)) +
    ggtitle(label ="",
            subtitle = "Habitat damage categories")+
    theme_void() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 17,face = "italic",hjust=1))
  
  #Histogram of proportion of net damage
  p4=d.no.zeros%>%
    dplyr::select(-Frame,-Total.damage,-SHEET_NO,-net_length,-Dist.roller.spreader)%>%
    gather(Species,Percent)%>%
    filter(Percent>0)%>%
    mutate(Percent.bin=5*round(Percent/5))%>%
    ggplot( aes(x=Percent.bin)) +
    geom_histogram( fill="red",col="black", alpha=0.9,binwidth = 5)+
    ggtitle(label ="",
            subtitle = "Percentage damage per frame")+
    theme_classic()+
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 17,face = "italic",hjust=-.15))+
    xlab("Percentage")+
    ylab("Number of frames")
  
  return(list(p2=p2, p3=p3, p4=p4))
}
Out.damg_GN=fn.habitat.damg.deck(Gear='gillnet')
ggarrange(Out.damg_GN$p2, Out.damg_GN$p3, Out.damg_GN$p4, ncol = 1, nrow = 3)
ggsave(le.paste("Video/deck.cameras/Habitat.interactions.tiff"),
       width = 6,height = 10,compression = "lzw")
}

#---------Dropouts, Gaffing & Position in water column (catch around weight or float) ---------  
Video.camera2.deck=Video.camera2.deck%>%
  data.frame%>%
  mutate(Period=tolower(Period),
         Period=ifelse(Period=='gilnet','gillnet',Period),
         DPIRD.code=ifelse(DPIRD.code=='GNPA003/LLPA0004','GNPA0003/LLPA0004',
                           ifelse(DPIRD.code=='GNPA006/LLPA0005','GNPA0006/LLPA0005',
                                  DPIRD.code)),
         SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DPIRD.code,1,sep = "\\/"),'GN'),
                            Period=='longline'~str_remove(word(DPIRD.code,2,sep = "\\/"),'LL')),
         dropout=ifelse(is.na(dropout),'No',dropout),
         dropout=capitalize(tolower(dropout)),
         gaffed=capitalize(tolower(gaffed)))

Video.camera2.deck_observations=Video.camera2.deck_observations%>%  
  data.frame%>%
  mutate(Period=tolower(Period),
         Method=ifelse(Period=='gillnet','GN',ifelse(Period=='longline','LL',NA)),
         Code=ifelse(grepl('sea bird',comment) & is.na(Code),40000000,
                     ifelse(grepl('shear water',comment) & Code==40048000,40041050,
                            Code)),
         SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DPIRD.code,1,sep = "\\/"),'GN'),
                            Period=='longline'~str_remove(word(DPIRD.code,2,sep = "\\/"),'LL')),
         Activity=case_when(grepl("feeding",comment)~paste("feeding from",Period),
                            is.na(Activity) & grepl("shear water",tolower(comment))~'resting',
                            grepl("dead",comment)~'caught',
                            grepl("takes bate",comment)~paste('feeding from',Period),
                            TRUE~Activity))



Video.subsurface=Video.subsurface%>%  
  data.frame%>%
  mutate(Period=tolower(Period),
         Drop.out=ifelse(is.na(Drop.out),'No',Drop.out),    
         Drop.out=tolower(Drop.out),
         Dropout.condition=tolower(Dropout.condition),
         dropout=capitalize(Drop.out),
         SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DPIRD.code,1,sep = "\\/"),'GN'),
                            Period=='longline'~str_remove(word(DPIRD.code,2,sep = "\\/"),'LL')))
Video.subsurface.comments=Video.subsurface.comments%>%  
  data.frame%>%
  mutate(Period=tolower(Period),
         Code=as.numeric(str_trim(Code)),
         Code=ifelse(grepl('shearwater',comment) & Code==40128000,40041050,Code),
         SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DPIRD.code,1,sep = "\\/"),'GN'),
                            Period=='longline'~str_remove(word(DPIRD.code,2,sep = "\\/"),'LL')))


#1. Dropouts

#1.1 Barplot for deck camera 2 and subsurface camera
fn.plt.dropouts=function(d,TITLE,LegPos,var,MinN)
{
  these.drop.out.sp=table(d$Code)
  these.drop.out.sp=as.numeric(names(these.drop.out.sp[these.drop.out.sp>MinN]))
  names(these.drop.out.sp)=All.species.names[match(these.drop.out.sp,All.species.names$Code),"COMMON_NAME"]
  
  Col.vec=these.drop.out.sp
  Col.vec=ifelse(Col.vec<37049001,"firebrick","steelblue")
  
  d=d%>%
    filter(Code%in%these.drop.out.sp)%>%
    group_by(Code,!!sym(var),Period)%>%
    tally()%>%
    mutate(dummy=factor(!!sym(var)),
           Period=capitalize(Period))%>%
    left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by="Code")%>%  
    mutate(COMMON_NAME=factor(COMMON_NAME,levels=names(sort(these.drop.out.sp))))
  
  vals=c("#F8766D","#00BFC4","#7CAE00")
  
  p=d%>%
    ggplot(aes(fill=dummy, y=n, x=COMMON_NAME)) + 
    geom_bar(position="stack", stat="identity")+
    coord_flip() +
    facet_wrap(~Period,dir='h',scales='free_x')+ 
    theme_PA(Ttl.siz=18,Sbt.siz=16,str.siz=18,strx.siz=18,
             cap.siz=10,lgT.siz=14,leg.siz=16,axs.t.siz=14,axs.T.siz=16)+
    theme(legend.position = LegPos,
          legend.title = element_blank(),
          axis.text.y = element_text(colour = Col.vec),
          plot.title = element_text(hjust = -.4))+
    xlab('')+ylab('Number of events')+
    scale_fill_manual(values=vals[1:length(unique(d$dummy))])
  
  if(!is.null(TITLE))p=p+ggtitle(TITLE)
  
  print(p)
}

p1=fn.plt.dropouts(d=Video.camera2.deck,
                   TITLE="Deck camera 2",
                   LegPos="top",
                   var='dropout',
                   MinN=Min.N.drop.out)
p2=fn.plt.dropouts(d=Video.subsurface%>%
                     mutate(dropout=ifelse(dropout=='Gaffed','Yes',dropout)),
                   TITLE="Subsurface camera",
                   LegPos="none",
                   var='dropout',
                   MinN=Min.N.drop.out)
ggarrange(p1, p2, ncol = 1, nrow = 2,heights=c(1.5,1))
ggsave(le.paste("Video/deck.cameras/Drop.out.events.tiff"),
       width = 10,height = 12,compression = "lzw")


#1.2 glm for drop out probability by method  
fn.glm.dropouts=function(d,var,MinN)
{
  these.drop.out.sp=table(d$Code)
  these.drop.out.sp=as.numeric(names(these.drop.out.sp[these.drop.out.sp>MinN]))
  names(these.drop.out.sp)=All.species.names[match(these.drop.out.sp,All.species.names$Code),"COMMON_NAME"]
  
  Col.vec=these.drop.out.sp
  Col.vec=ifelse(Col.vec<37049001,"firebrick","steelblue")
  
  d=d%>%
    filter(Code%in%these.drop.out.sp)%>%
    group_by(Code,!!sym(var),Period)%>%
    tally()%>%
    mutate(dummy=!!sym(var),
           Period=capitalize(Period))%>%
    left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by="Code")%>%
    ungroup()%>%
    dplyr::select(Period,n,dummy,COMMON_NAME)%>%
    spread(dummy,n,fill=0)
  
  d.GN=d%>%filter(Period=="Gillnet")%>%
    mutate(COMMON_NAME=factor(COMMON_NAME))
  GN.glm <- glm( cbind(No, Yes) ~ COMMON_NAME, data = d.GN, family = "binomial")
  
  d.LL=d%>%filter(Period=="Longline")%>%
    mutate(COMMON_NAME=factor(COMMON_NAME))
  LL.glm <- glm( cbind(No, Yes) ~ COMMON_NAME, data = d.LL, family = "binomial")
  
  
  #Predict species
  GN.pred=summary(emmeans(GN.glm, 'COMMON_NAME', type="response"))
  LL.pred=summary(emmeans(LL.glm, 'COMMON_NAME', type="response"))
  
  return(list(GN.glm=GN.glm,LL.glm=LL.glm,GN.pred=GN.pred,LL.pred=LL.pred,
              Col.vec=data.frame(COMMON_NAME=names(Col.vec),Col=Col.vec)))
}
Glm.dropouts=fn.glm.dropouts(d=Video.camera2.deck,
                             var='dropout',
                             MinN=Min.N.drop.out)

ft <- as_flextable(Glm.dropouts$GN.glm)
save_as_image(ft, path = le.paste("Video/deck.cameras/anovas/drop_outs_GN.png"))
#save_as_docx(ft,path = le.paste("Video/deck.cameras/anovas/drop_outs_GN.docx"))

ft <- as_flextable(Glm.dropouts$LL.glm)
save_as_image(ft, path = le.paste("Video/deck.cameras/anovas/drop_outs_LL.png"))
#save_as_docx(ft,path = le.paste("Video/deck.cameras/anovas/drop_outs_LL.docx"))

rm(ft)

p=fn.barplot.cpue(d=rbind(Glm.dropouts$GN.pred%>%
                            mutate(method="Gillnet"),
                          Glm.dropouts$LL.pred%>%
                            mutate(method="Longline"))%>%
                    left_join(Glm.dropouts$Col.vec,by='COMMON_NAME')%>%
                    mutate(Group=Col,
                           species=COMMON_NAME,
                           x=COMMON_NAME,
                           y=1-prob,
                           fill=Group,
                           facet=method,
                           lower.CL=1-asymp.LCL,
                           upper.CL=1-asymp.UCL),
                  YLAB="Drop out probability",
                  XLAB='',
                  cex=12,
                  Rotate='Yes',
                  Relative="No",
                  RETURN=FALSE)
fn.fig(le.paste("Video/deck.cameras/Drop.out.events_predictions_deckcam2"),2000,2000)
p+scale_fill_manual(values=c("firebrick","steelblue"))+
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1))
dev.off()


#1.3.Table of drop out percent (drop out numbers per total number of individuals interacting with gear)
fun.table.ret.disc=function(d)
{
  TAB=d%>%
    left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by="Code")%>%
    mutate(dropout=ifelse(dropout=='No','Retained','Drop.out'))%>%
    group_by(COMMON_NAME,dropout,Period)%>%
    tally()%>%
    ungroup()%>%
    mutate(Period=capitalize(Period),
           drop.period=paste(dropout,Period,sep='.'))%>%
    dplyr::select(-c(dropout,Period))%>%
    spread(drop.period,n,fill=0)%>%
    mutate(Percent.drop.out.gillnet=round(100*Drop.out.Gillnet/(Retained.Gillnet+Drop.out.Gillnet),1),
           Percent.drop.out.longline=round(100*Drop.out.Longline/(Retained.Longline+Drop.out.Longline),1))%>%
    arrange(COMMON_NAME)%>%
    relocate(COMMON_NAME,Retained.Gillnet,Drop.out.Gillnet,Percent.drop.out.gillnet,
             Retained.Longline,Drop.out.Longline,Percent.drop.out.longline)%>%
    mutate_at(vars(Percent.drop.out.longline), ~replace(., is.nan(.), ""))%>%
    mutate_at(vars(Percent.drop.out.gillnet), ~replace(., is.nan(.), ""))%>%
    mutate(Drop.out.Gillnet=ifelse(Percent.drop.out.gillnet=='','',Drop.out.Gillnet),
           Retained.Gillnet=ifelse(Percent.drop.out.gillnet=='','',Retained.Gillnet),
           Drop.out.Longline=ifelse(Percent.drop.out.longline=='','',Drop.out.Longline),
           Retained.Longline=ifelse(Percent.drop.out.longline=='','',Retained.Longline))%>%
    left_join(DATA%>%
                distinct(common_name,Taxa),by=c('COMMON_NAME'='common_name'))%>%
    mutate(n=as.numeric(Retained.Gillnet)+as.numeric(Drop.out.Gillnet))%>%
    ungroup()%>%
    arrange(Taxa,-n)%>%
    dplyr::select(-Taxa,-n)
  return(TAB)
}
write.csv(fun.table.ret.disc(d=Video.camera2.deck),
          le.paste("Video/deck.cameras/Drop.out.events_table_camera2.csv"),row.names = F)

write.csv(fun.table.ret.disc(d=Video.subsurface),
          le.paste("Video/deck.cameras/Drop.out.events_table_subsurface.csv"),row.names = F)

#1.4 Drop outs fate
p=fn.plt.dropouts(d=Video.subsurface%>%filter(!dropout=='No'),
                  TITLE="Subsurface camera",
                  LegPos="top",
                  var='Dropout.condition',
                  MinN=1)
p+theme(plot.title=element_text(hjust = -.25))
ggsave(le.paste("Video/deck.cameras/Drop.out.events_fate_subsurface.tiff"),width = 10,height = 8,compression = "lzw")


#2. Gaffing of drop outs
#subsurface camera
p1=fn.plt.dropouts(d=Video.subsurface%>%filter(!dropout=='No')%>%
                     mutate(Gaffed=ifelse(dropout=="Yes","Lost",dropout),
                            Gaffed=factor(Gaffed,levels=c("Gaffed","Lost"))),
                   TITLE="Subsurface camera",
                   LegPos="top",
                   var='Gaffed',
                   MinN=1)
p1=p1+theme(plot.title=element_text(hjust = -.25))
ggsave(le.paste("Video/deck.cameras/Drop.out.events_gaffing_subsurface.tiff"),width = 10,height = 8,compression = "lzw")

#deck camera 2  
p2=fn.plt.dropouts(d=Video.camera2.deck%>%
                     mutate(Gaffed=case_when(dropout=='Yes' & gaffed%in%c('No') ~  'Lost',
                                             dropout=='Yes' & gaffed%in%c('Yes') ~  'Gaffed',
                                             TRUE ~ gaffed))%>%
                     filter(!is.na(gaffed) & dropout=='Yes'),
                   TITLE="Deck camera 2",
                   LegPos="top",
                   var='Gaffed',
                   MinN=1)
p2=p2+theme(plot.title=element_text(hjust = -.25))

fig=ggarrange(p1+rremove("xlab"), p2+rremove("xlab"),
              ncol = 1, nrow = 2,
              common.legend=TRUE)
annotate_figure(fig,bottom = text_grob("Number of events",size = 18))
ggsave(le.paste("Video/deck.cameras/Drop.out.events_gaffing.tiff"),width = 10,height = 8,compression = "lzw")


#3. Composition around weight or float
# Display data
Data_water.column=Video.camera2.deck%>%
  filter(!SHEET_NO%in%No.good.water.column)%>%
  mutate(Code=ifelse(Code==37018001,37018003,Code),
         water.column=tolower(hook.distance.to.float.weight))%>%
  filter(!is.na(water.column))%>%
  filter(grepl('longlin',tolower(Period)))%>%
  mutate(Period='Longline')
these.compo.sp=Data_water.column%>%
  group_by(Code)%>%
  tally()%>%
  filter(n>=Min.obs.comp.wei.flot)%>%
  pull(Code)
names(these.compo.sp)=All.species.names[match(these.compo.sp,All.species.names$Code),"COMMON_NAME"]
Col.vec=these.compo.sp
Col.vec=ifelse(Col.vec<37049001,"firebrick","steelblue")
Data_water.column=Data_water.column%>%
  filter(Code%in%these.compo.sp)%>%
  left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by="Code")%>%
  mutate(COMMON_NAME=factor(COMMON_NAME,levels=names(these.compo.sp)))
Data_water.column%>%
  group_by(Code,water.column,Period,COMMON_NAME)%>%
  tally()%>%
  mutate(water.column=factor(water.column,levels=c("1w","2w","3w","1f","2f","3f")),
         Period=capitalize(Period))%>%
  ggplot(aes(fill=water.column, y=n, x=COMMON_NAME)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip() +
  facet_wrap(~Period,dir='h',scales='free_x')+ 
  theme_PA(str.siz=20,strx.siz=20,
           leg.siz=18,axs.t.siz=16,axs.T.siz=18)+
  theme(legend.position = "top",
        legend.title = element_blank())+
  scale_fill_manual(values=c("orange","firebrick2","firebrick4","lightblue2","deepskyblue2","dodgerblue4"))+
  xlab('')+ylab('Number of events')+ 
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))
ggsave(le.paste("Video/deck.cameras/Weight_float_species.events.tiff"),width = 10,height = 8,compression = "lzw")

#fit binomial glm
Data_water.column=Data_water.column%>%
  left_join(DATA%>%
              filter(sheet_no%in%Data_water.column$SHEET_NO)%>%
              distinct(sheet_no,botdepth),
            by=c("SHEET_NO"="sheet_no"))%>%
  mutate(Position=ifelse(grepl('w',water.column),"weight","float"))

glm.dat=Data_water.column%>% 
  mutate(N=1)%>%
  dplyr::select(COMMON_NAME,N,Position,botdepth)%>%
  group_by(COMMON_NAME,Position)%>%
  summarise(N=sum(N))%>%
  spread(Position,N,fill=0)%>%
  mutate(COMMON_NAME=factor(as.character(COMMON_NAME)))
Mod <- glm( cbind(float, weight) ~ COMMON_NAME, data = glm.dat, family = "binomial")  

#Predict species
Pred=summary(emmeans(Mod, 'COMMON_NAME', type="response"))
p=fn.barplot.cpue(d=Pred%>%
                    left_join(data.frame(COMMON_NAME=names(Col.vec),Col=Col.vec),
                              by='COMMON_NAME')%>%
                    mutate(Group=Col,
                           x=COMMON_NAME,
                           species=COMMON_NAME,
                           y=1-prob,
                           fill=Group,
                           facet="Longline",
                           lower.CL=1-asymp.LCL,
                           upper.CL=1-asymp.UCL),
                  YLAB="Probability of capture near a weight",
                  XLAB='',
                  cex=18,
                  Rotate='Yes',
                  Relative="No",
                  RETURN=FALSE)
fn.fig(le.paste("Video/deck.cameras/Weight_float_species_predictions"),2400,2400)
p+scale_fill_manual(values=c("firebrick","steelblue"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
dev.off()

#Export anova table
ft <- as_flextable(Mod)
save_as_image(ft, path = le.paste("Video/deck.cameras/anovas/Weight_float_species.png"))
#save_as_docx(ft,path = le.paste("Video/deck.cameras/anovas/Weight_float_species.docx"))



#4. Rates of depredation, bait loss and drop outs
#Underwater camera
#1. All shots (to add 0 observations)
All.underwater.shts=rbind(Video.longline.interaction%>%
                            distinct(Camera,sheet_no,Method,soak.time),
                          Video.net.interaction%>%
                            distinct(Camera,sheet_no,Method,soak.time))%>%
  mutate(Method=capitalize(Method))%>%
  filter(!is.na(sheet_no))%>%
  mutate(n=1)%>%
  group_by(Method,sheet_no)%>%
  summarise(N.cameras=sum(n),
            soak.time=max(soak.time,na.rm=T))%>%
  ungroup()%>%
  mutate(Effort=case_when(Method=='Longline'~N.cameras*soak.time*hooks.observed,
                          Method=='Gillnet'~N.cameras*soak.time*metres.observed))


#2. Bait loss (in # of baits lost per hook hour)
Rate.under_bait.loss=rbind(Video.longline.interaction%>%
                             distinct(Camera,sheet_no,Method,soak.time,Interaction),
                           Video.net.interaction%>%
                             distinct(Camera,sheet_no,Method,soak.time,Interaction))%>%
  filter(Interaction=="Bait feeding")%>%
  mutate(Number=1,
         Method=capitalize(Method))
Rate.under_bait.loss=Rate.under_bait.loss%>%
  left_join(All.underwater.shts%>%
              filter(sheet_no%in%unique(Rate.under_bait.loss$sheet_no))%>%
              dplyr::select(sheet_no,N.cameras),by='sheet_no')%>%
  group_by(sheet_no,Method,Interaction)%>%
  summarise(n=sum(Number),
            soak.time=max(soak.time,na.rm=T),
            N.cameras=max(N.cameras))%>%
  mutate(Effort=case_when(Method=='Longline'~soak.time*hooks.observed*N.cameras,
                          Method=='Gillnet'~soak.time*metres.observed*N.cameras))

Bait.loss.rate_under=unique(rbind(Rate.under_bait.loss%>%
                                    dplyr::select(Method,sheet_no,n,Effort),
                                  All.underwater.shts%>%     #add observed 0 records
                                    filter(Method=='Longline' & !sheet_no%in%Rate.under_bait.loss$sheet_no)%>%
                                    mutate(n=0)%>%
                                    dplyr::select(Method,sheet_no,n,Effort))%>%
                              ungroup()%>%
                              mutate(Rate=sum(n)/sum(Effort,na.rm=T))%>%
                              pull(Rate))


#3. Drop outs and depredation (in # of individuals per m hour or hook hour)   
Rate.under=rbind(Video.longline.interaction%>%
                   distinct(Camera,sheet_no,Method,soak.time,Interaction),
                 Video.net.interaction%>%
                   distinct(Camera,sheet_no,Method,soak.time,Interaction))%>%
  filter(Interaction%in%c("Predated on","Caught while predating","Feeding","Escape"))%>%
  mutate(Number=1,
         Method=capitalize(Method),
         Interaction=ifelse(Interaction%in%c("Caught while predating","Predated on","Feeding"),
                            "Depredation",Interaction))
Rate.under=Rate.under%>%
  left_join(All.underwater.shts%>%
              filter(sheet_no%in%unique(Rate.under$sheet_no))%>%
              dplyr::select(sheet_no,N.cameras),by='sheet_no')%>%  
  group_by(sheet_no,Method,Interaction)%>%
  summarise(n=sum(Number),
            soak.time=max(soak.time),
            N.cameras=max(N.cameras))%>%
  mutate(Effort=case_when(Method=='Longline'~soak.time*hooks.observed*N.cameras,
                          Method=='Gillnet'~soak.time*metres.observed*N.cameras))

Drop.out.rate_under=rbind(Rate.under%>%
                            filter(Interaction=='Escape')%>%
                            dplyr::select(Method,sheet_no,n,Effort),
                          All.underwater.shts%>%     #add observed 0 records
                            filter(!sheet_no%in%Rate.under$sheet_no)%>%
                            mutate(n=0)%>%
                            dplyr::select(Method,sheet_no,n,Effort))%>%
  group_by(Method)%>%
  summarise(Rate=sum(n)/sum(Effort))


Drepedation.rate_under=rbind(Rate.under%>%
                               filter(Interaction=='Depredation')%>%
                               dplyr::select(Method,sheet_no,n,Effort),
                             All.underwater.shts%>%     #add observed 0 records
                               filter(!sheet_no%in%Rate.under$sheet_no)%>%
                               mutate(n=0)%>%
                               dplyr::select(Method,sheet_no,n,Effort))%>%
  group_by(Method)%>%
  summarise(Rate=sum(n)/sum(Effort))

#4. Tabulate all events
Tab.out=data.frame(Event=c("Bait loss",
                           rep("Drepedation",2),
                           rep("Drop outs",2)),
                   Method=c("Longline",
                            "Gillnet","Longline",
                            "Gillnet","Longline"),
                   Rate=c(Bait.loss.rate_under,
                          Drepedation.rate_under%>%filter(Method=="Gillnet")%>%pull(Rate),
                          Drepedation.rate_under%>%filter(Method=="Longline")%>%pull(Rate),
                          Drop.out.rate_under%>%filter(Method=="Gillnet")%>%pull(Rate),
                          Drop.out.rate_under%>%filter(Method=="Longline")%>%pull(Rate)),
                   Units=c("Number of lost baits per 1 hook hour",
                           "Number of depredated individuals per 1 m hour","Number of depredated individuals per 1 hook hour",
                           "Number of dropouts per 1 m hour","Number of dropouts per 1 hook hour"))%>%
  mutate(Rate=round(Rate,6))
write.csv(Tab.out%>%
            filter(Event=='Drop outs')%>%
            dplyr::select(-Event),
          le.paste("Video/underwater/Rate_Drop.out.csv"),row.names=F)
write.csv(Tab.out%>%
            filter(Event=='Drepedation')%>%
            dplyr::select(-Event),
          le.paste("Video/underwater/Rate_Drepedation.csv"),row.names=F)
write.csv(Tab.out%>%
            filter(Event=='Bait loss')%>%
            dplyr::select(-Event),
          le.paste("Video/underwater/Rate_Bait.loss.csv"),row.names=F)
rm(Tab.out)

#Subsurface and deck 2 cameras (effort is 1 m or 1 hook per hauling hour)
All.shts=DATA%>%
  distinct(sheet_no,method,haul.time,haul.time.end,net_length,n.hooks)%>%
  mutate(method=ifelse(method=="GN","Gillnet",ifelse(method=="LL","Longline",NA)),
         haul.time.end=as.POSIXct(haul.time.end, format = "%H:%M"),
         haul.time=as.POSIXct(haul.time, format = "%H:%M"),
         haul=difftime(haul.time.end,haul.time,units='hours'))%>%
  group_by(method)%>%
  mutate(Mean.haul=mean(haul,na.rm=T))%>%
  ungroup()%>%
  mutate(haul=ifelse(is.na(haul),Mean.haul,haul))%>%
  filter(!is.na(sheet_no))%>%
  group_by(method,sheet_no)%>%
  summarise(net_length=max(net_length,na.rm=T),
            n.hooks=max(n.hooks,na.rm=T),
            haul=max(haul,na.rm=T))%>%
  ungroup()%>%
  mutate(Effort=case_when(method=='Longline'~haul*n.hooks,
                          method=='Gillnet'~haul*net_length*1000))%>%
  dplyr::select(sheet_no,method,Effort)


fn.rates_sub_deck2=function(d,MinN)
{
  these.drop.out.sp=table(d$Code)
  these.drop.out.sp=as.numeric(names(these.drop.out.sp[these.drop.out.sp>MinN]))
  names(these.drop.out.sp)=All.species.names[match(these.drop.out.sp,All.species.names$Code),"COMMON_NAME"]
  
  d1=d%>%
    filter(Code%in%these.drop.out.sp & dropout=="Yes")%>%
    mutate(Method=capitalize(Period))%>%
    group_by(Code,Method)%>%
    tally()%>%
    left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by="Code")%>%
    ungroup()%>%
    left_join(All.shts%>%
                filter(sheet_no%in%unique(d$sheet_no))%>%
                group_by(method)%>%
                summarise(Effort=sum(Effort))%>%
                ungroup(),
              by=c('Method'='method'))%>%
    mutate(Rate=n/Effort)%>%
    dplyr::select(-Code,-n,-Effort)%>%
    spread(Method,Rate,fill=0)%>%
    left_join(DATA%>%
                distinct(common_name,Taxa),by=c('COMMON_NAME'='common_name'))%>%
    ungroup()%>%
    arrange(Taxa)%>%
    dplyr::select(-Taxa)
  return(d1)
}

Drop.out.rate_deck2=fn.rates_sub_deck2(d=Video.camera2.deck%>%rename(sheet_no=SHEET_NO),
                                       MinN=Min.N.drop.out)
Drop.out.rate_deck2=rbind(Drop.out.rate_deck2%>%mutate_if(is.numeric, round,5),
                          Drop.out.rate_deck2[1,]%>%
                            mutate(COMMON_NAME="Units",
                                   Gillnet="Number of dropouts per 1 m haul-hour",
                                   Longline="Number of dropouts per 1 hook haul-hour"))
write.csv(Drop.out.rate_deck2,
          le.paste("Video/deck.cameras/Rate_Drop.out_deck2.csv"),row.names=F)


Drop.out.rate_subsurface=fn.rates_sub_deck2(d=Video.subsurface%>%
                                              rename(sheet_no=SHEET_NO)%>%
                                              mutate(dropout=ifelse(dropout=='Gaffed','Yes',dropout)),
                                            MinN=Min.N.drop.out)
Drop.out.rate_subsurface=rbind(Drop.out.rate_subsurface%>%mutate_if(is.numeric, round,5),
                               Drop.out.rate_subsurface[1,]%>%
                                 mutate(COMMON_NAME="Units",
                                        Gillnet="Number of dropouts per 1 m haul-hour",
                                        Longline="Number of dropouts per 1 hook haul-hour"))
write.csv(Drop.out.rate_subsurface,
          le.paste("Video/deck.cameras/Rate_Drop.out_subsurface.csv"),row.names=F)



#---------Analysis of Deck 1 camera VS observers --------------------------------------------------------------------
#   For paper:
#     Try model: EM = OM + species group (either species, or genus or family) + day/night + boat + EM analysist
#           Consider EM analysis to discuss potential bias (Abbey and Jack could remember species from when onboard).
#     For duskies-coppers, group as 'whalers' for gummy-whiskery, also group.
#     
#AKA   STILL NEED TO CHECK FOR CODES THAT ARE IN METADATA CHECK BUT NOT DATAFRAME
# Get video shots with bull battery life 
D1.good.ones <- D1.cam.battery %>% filter(str_detect(Full.LL, "(?i)y")|str_detect(Full.GN, "(?i)y")) %>% 
  mutate(
    LL = ifelse(str_detect(`DIPRD code`, "(?i)ll"), as.character(`DIPRD code`), as.character(NA)),
    GN = ifelse(str_detect(`DIPRD code`, "(?i)gn"), as.character(`DIPRD code`), as.character(NA)),
    sheet_no = ifelse(is.na(LL), as.character(GN), as.character(LL))
  )
D2.good.ones <- D2.cam.battery %>% filter(str_detect(Full.LL, "(?i)y")|str_detect(Full.GN, "(?i)y")) %>% 
  mutate(
    LL = ifelse(str_detect(DPIRD.code, "(?i)ll"), as.character(DPIRD.code), as.character(NA)),
    GN = ifelse(str_detect(DPIRD.code, "(?i)gn"), as.character(DPIRD.code), as.character(NA)),
    sheet_no = ifelse(is.na(LL), as.character(GN), as.character(LL))
  )

#### Write csv for in meta but not df and vice versa
do.check = FALSE
if(do.check) {
d1meta <- unique(D1.cam.battery$`DIPRD code`)
d1df <- as.data.frame(unique(Video.camera1.deck$`DIPRD code`)) %>% separate("unique(Video.camera1.deck$`DIPRD code`)", c("code1", "code2"), sep = "/") %>% unlist()
write.csv(setdiff(d1meta, d1df), file = "U:/Shark/ParksAustralia_2019/d1.in.meta.but.not.df.csv")
write.csv(setdiff(d1df, d1meta), file = "U:/Shark/ParksAustralia_2019/d1.in.df.but.not.meta.csv")
d2meta <- unique(D2.cam.battery$DPIRD.code)
d2df <- as.data.frame(unique(Video.camera2.deck$DPIRD.code)) %>% separate( "unique(Video.camera2.deck$DPIRD.code)", c("code1", "code2"), sep = "/") %>% unlist()
write.csv(setdiff(d2meta, d2df), file = "U:/Shark/ParksAustralia_2019/d2.in.meta.but.not.df.csv")
write.csv(setdiff(d2df, d2meta), file = "U:/Shark/ParksAustralia_2019/d2.in.df.but.not.meta.csv")
}

# Deck 1  (pointing to deck)                      
Video.camera1.deck=Video.camera1.deck%>%
  mutate(Code=gsub('\\s+', '',Code),
         Code=as.numeric(Code),
         method = case_when(Period=='Gillnet'~ 'GN',
                            Period=='Longline' ~ 'LL'))%>%
  left_join(All.species.names%>%
              dplyr::select(COMMON_NAME,Code)%>%
              distinct(Code,.keep_all=T),
            by="Code") %>% 
  rename(DIPRD.code='DIPRD code')

Video.camera1 <- Video.camera1.deck %>%
                      dplyr::select(DIPRD.code,Code,Period,number,condition) %>% 
  data.frame%>%
  separate(DIPRD.code, into = c("GN", "LL"), sep = "/", remove = FALSE) %>% 
  mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                            Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                            Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                            Code >=3.7046e7 & Code<=3.747e7 ~"Scalefish",
                            Code >=4.1e+07 & Code<=4.115e+07 ~"Marine mammals",
                            Code >=4.0e+07 & Code<4.1e+07 ~"Seabirds",
                            Code >=1.2e7 & Code<3.7e7 ~"Invertebrates",
                            Code >=1.1e7 & Code<1.2e7 ~"Rock/reef structure",
                            Code >=5.4e7 & Code<5.49e7 ~"Macroalgae",
                            Code == 10000910 ~"Sponges"),
         Period=tolower(Period),
         sheet_no=case_when(Period == 'gillnet'~ substr(GN, 3, 8),
                            Period == 'longline'~ substr(LL, 3, 8))) %>% 
  left_join(DATA_PA,by = 'sheet_no')%>%
  mutate(Data.set = "camera",
         Period = ifelse(Period == 'gillnet' & method == 'LL','longline',
                       ifelse(Period == 'longline' & method == 'GN','gillnet', Period))) %>% 
  mutate(filter.sheet_no = case_when(str_detect(Period, "(?i)long") ~ as.character(LL),
                                    str_detect(Period, "(?i)gill") ~ as.character(GN))) %>% 
  filter(filter.sheet_no %in% D1.good.ones$sheet_no) 

#1. Export data for Abbey\
#### NOTE: made changes to code here because there was a "q" in number, will fix in EMob and remove changes 
Abbey.data.chapter.2=rbind(Video.camera1%>%
                             dplyr::rename(Number=number)%>%
                             mutate(Method=capitalize(Period))%>%
                             filter(Code>=3.7e7 & Code<=3.747e7)%>%  #compare only fish and sharks
                             dplyr::select(sheet_no,Method,Data.set,Code,SP.group,Number),
                           DATA%>%
                             filter(sheet_no%in%unique(Video.camera1$sheet_no))%>%
                             filter(Code>=3.7e7 & Code<=3.747e7)%>%     #compare only fish and sharks
                             mutate(Number=1,
                                    Data.set='Onboard.observer',
                                    Method=ifelse(method=="GN","Gillnet",
                                                  ifelse(method=="LL","Longline",
                                                         NA)),
                                    SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                                                       Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                                                       Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                                                       Code >=3.7046e7 & Code<=3.747e7 ~"Scalefish"))%>%
                             dplyr::select(sheet_no,Method,Data.set,Code,SP.group,Number))%>%
  mutate(Code=as.numeric(Code))

add.effort=DATA%>%
  filter(sheet_no%in%unique(Abbey.data.chapter.2$sheet_no))%>%
  mutate(Effort=ifelse(method=="GN",soak.time*net_length,
                       ifelse(method=="LL",soak.time*n.hooks,
                              NA)))%>%
  group_by(sheet_no,method)%>%
  summarise(Effort=max(Effort))%>%
  mutate(Method=ifelse(method=="GN","Gillnet",
                       ifelse(method=="LL","Longline",
                              NA)))%>%
  dplyr::rename(Fishing.effort=Effort)%>%
  dplyr::select(-method)
Abbey.data.chapter.2_species=mutate(Abbey.data.chapter.2, Number = as.numeric(1))%>%
  group_by(sheet_no,Method,Data.set,Code)%>%
  summarise(Number=sum(Number))%>%
  spread(Code,Number,fill = 0)%>%
  data.frame%>%
  left_join(add.effort,by=c('sheet_no','Method'))
Abbey.data.chapter.2_SP.group=mutate(Abbey.data.chapter.2, Number = as.numeric(1))%>%
  group_by(sheet_no,Method,Data.set,SP.group)%>%
  summarise(Number=sum(Number))%>%
  spread(SP.group,Number,fill = 0)%>%
  data.frame%>%
  left_join(add.effort,by=c('sheet_no','Method'))
Abbey.data.chapter.2_Retain.group=mutate(Abbey.data.chapter.2, Number = as.numeric(1))%>%
  left_join(Retained.tabl,by='Code')%>%
  mutate(Retain.group=
           ifelse(retained=='Yes' & SP.group%in% c('Sharks','Rays'),"Retained elasmobranch",
                  ifelse(retained=='No' & SP.group%in% c('Sharks','Rays'),"Discarded elasmobranch",
                         ifelse(retained=='Yes' & SP.group%in% c('Scalefish'),"Retained scalefish",   
                                ifelse(retained=='No' & SP.group%in% c('Scalefish'),"Discarded scalefish",
                                       NA)))))%>%
  group_by(sheet_no,Method,Data.set,Retain.group)%>%
  summarise(Number=sum(Number))%>%
  filter(!is.na(Retain.group))%>%
  spread(Retain.group,Number,fill = 0)%>%
  data.frame%>%
  left_join(add.effort,by=c('sheet_no','Method'))
# write.csv(Abbey.data.chapter.2_species,le.paste('Data for Abbey/Abbey.data.chapter.2_species.csv'),row.names = F)
# write.csv(Abbey.data.chapter.2_SP.group,le.paste('Data for Abbey/Abbey.data.chapter.2_SP.group.csv'),row.names = F)
# write.csv(Abbey.data.chapter.2_Retain.group,le.paste('Data for Abbey/Abbey.data.chapter.2_Retain.group.csv'),row.names = F)

#2 Barplot of observer VS camera
EM.vs.OM.DATA=DATA%>%
  mutate(COMMON_NAME=case_when(grepl(paste(c("whaler","dusky shark"),collapse = '|'),tolower(COMMON_NAME))~'Whalers',
                               grepl(paste(c("blacktip sharks","Common blacktip"),collapse = '|'),tolower(COMMON_NAME))~'Blacktip sharks',
                               grepl('wobbegong',tolower(COMMON_NAME))~'Wobbegongs',
                               grepl('hammerhead',tolower(COMMON_NAME))~'Hammerheads',
                               grepl('stingray',tolower(COMMON_NAME))~'Stingrays',
                               grepl('angel',tolower(COMMON_NAME))~'Angel sharks',
                               grepl('catshark',tolower(COMMON_NAME))~'Catsharks',
                               grepl('gurnard',tolower(COMMON_NAME))~'Gurnards',
                               grepl('jacket',tolower(COMMON_NAME))~'Leatherjackets',
                               grepl('guitarfish',tolower(COMMON_NAME))~'Guitarfish & shovelnose rays',
                               grepl(paste(c("gummy shark","whiskery shark"),collapse = '|'),tolower(COMMON_NAME))~"Gummy/Whiskery sharks",
                               TRUE~COMMON_NAME))
Reset.disc.ret=EM.vs.OM.DATA%>% 
  filter(!is.na(retainedflag))%>%
  filter(!is.na(COMMON_NAME) & !COMMON_NAME=='')%>%
  group_by(retainedflag,COMMON_NAME)%>%
  tally()%>%
  ungroup()%>%
  group_by(COMMON_NAME) %>%
  slice(which.max(n)) %>%
  arrange(COMMON_NAME,n)
Reset.disc.ret_RET=Reset.disc.ret%>%
  filter(retainedflag=="Yes")%>%
  pull(COMMON_NAME)
EM.vs.OM.DATA=EM.vs.OM.DATA%>%
  mutate(retainedflag=ifelse(COMMON_NAME%in%Reset.disc.ret_RET,'Yes','No'))

EM.vs.OM.Video.camera1=Video.camera1%>%
  mutate(Code=as.numeric(Code))%>%
  left_join(All.species.names%>%
              dplyr::select(COMMON_NAME,Code)%>%
              distinct(Code,.keep_all=T),
            by="Code")%>%
  mutate(method=ifelse(method=="GN","Gillnet",
                       ifelse(method=="LL","Longline",
                              NA)))%>%
  mutate(COMMON_NAME=case_when(grepl(paste(c("whaler","dusky shark"),collapse = '|'),tolower(COMMON_NAME))~'Whalers',
                               grepl(paste(c("blacktip sharks","Common blacktip"),collapse = '|'),tolower(COMMON_NAME))~'Blacktip sharks',
                               grepl('wobbegong',tolower(COMMON_NAME))~'Wobbegongs',
                               grepl('hammerhead',tolower(COMMON_NAME))~'Hammerheads',
                               grepl('stingray',tolower(COMMON_NAME))~'Stingrays',
                               grepl('angel',tolower(COMMON_NAME))~'Angel sharks',
                               grepl('catshark',tolower(COMMON_NAME))~'Catsharks',
                               grepl('gurnard',tolower(COMMON_NAME))~'Gurnards',
                               grepl('jacket',tolower(COMMON_NAME))~'Leatherjackets',
                               grepl('guitarfish',tolower(COMMON_NAME))~'Guitarfish & shovelnose rays',
                               grepl(paste(c("gummy shark","whiskery shark"),collapse = '|'),tolower(COMMON_NAME))~"Gummy/Whiskery sharks",
                               TRUE~COMMON_NAME))

TAB=EM.vs.OM.DATA%>%
  filter(!is.na(COMMON_NAME) & !COMMON_NAME=='')%>%
  group_by(retainedflag,COMMON_NAME)%>%
  tally()%>%
  ungroup()
TAB.comm=TAB%>%
  filter(retainedflag=='Yes')%>%
  arrange(-n)%>%
  mutate(Cum=cumsum(n),
         Percent=100*Cum/sum(n))
TAB.disc=TAB%>%
  filter(retainedflag=='No' & !COMMON_NAME%in%TAB.comm$COMMON_NAME)%>%
  arrange(-n)%>%
  mutate(Cum=cumsum(n),
         Percent=100*Cum/sum(n))

fn.obs_cam.barplot=function(n.shots,Cam,Obs,LGN,TITL)
{
  LGN.LL=paste(' (',n.shots$LL,' shots)',sep='')
  LGN.GN=paste(' (',n.shots$GN,' shots)',sep='')
  
  d=rbind(Cam,Obs)%>%
    mutate(Method.hour=ifelse(method=='Longline',paste(method,LGN.LL,sep=''),
                              ifelse(method=='Gillnet',paste(method,LGN.GN,sep=''),
                                     NA)))%>%
    filter(!is.na(method))%>%
    mutate(COMMON_NAME=ifelse(COMMON_NAME%in%This.sp.other,COMMON_NAME,"Other"))
  p=d%>%
    ggplot(aes(x=COMMON_NAME, y=n, fill=Platform)) + 
    geom_bar(position="dodge", stat="identity")+
    coord_flip() +
    facet_wrap(~Method.hour,dir='h',scales='free_x')+ 
    theme_PA(Ttl.siz=20,str.siz=16,strx.siz=16,
             leg.siz=18,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = LGN,
          legend.justification='center',
          legend.direction='horizontal',
          legend.title = element_blank())+
    xlab('')+ylab('Number of individuals')+
    scale_y_continuous(breaks = integer_breaks())+
    guides(fill = guide_legend(nrow = 1))+
    scale_x_discrete(position = "top")
  
  if(!is.null(TITL)) p=p+ggtitle(TITL)
  
  print(p)
}
#Commercial species
This.sp=TAB.comm%>%pull(COMMON_NAME)
This.sp.other=TAB.comm%>%filter(Percent<=95)%>%pull(COMMON_NAME)
p1=fn.obs_cam.barplot(n.shots=EM.vs.OM.DATA%>%
                        filter(sheet_no%in%unique(Video.camera1$sheet_no))%>%
                        distinct(sheet_no,method)%>%
                        group_by(method)%>%
                        tally()%>%
                        spread(method,n),
                      Cam=EM.vs.OM.Video.camera1%>% 
                        filter(COMMON_NAME%in%This.sp)%>%
                        group_by(COMMON_NAME,method)%>%
                        summarise(n=sum(number))%>%
                        mutate(Platform="Camera"),
                      Obs=EM.vs.OM.DATA%>%
                        filter(sheet_no%in%unique(Video.camera1$sheet_no) & 
                                 COMMON_NAME%in%This.sp)%>%
                        mutate(method=ifelse(method=="GN","Gillnet",
                                             ifelse(method=="LL","Longline",
                                                    NA)))%>%
                        group_by(COMMON_NAME,method)%>%
                        summarise(n=sum(number))%>%
                        mutate(Platform="Observer"),
                      LGN="top",
                      TITL="Commercial species")

#Bycatch species    
This.sp=TAB.disc%>%pull(COMMON_NAME)
This.sp.other=TAB.disc%>%filter(Percent<=95)%>%pull(COMMON_NAME)
p2=fn.obs_cam.barplot(n.shots=EM.vs.OM.DATA%>%
                        filter(sheet_no%in%unique(Video.camera1$sheet_no))%>%
                        distinct(sheet_no,method)%>%
                        group_by(method)%>%
                        tally()%>%
                        spread(method,n),
                      Cam=EM.vs.OM.Video.camera1%>%
                        filter(COMMON_NAME%in%This.sp)%>%
                        group_by(COMMON_NAME,method)%>%
                        summarise(n=sum(number))%>%
                        mutate(Platform="Camera"),
                      Obs=EM.vs.OM.DATA%>%
                        filter(sheet_no%in%unique(Video.camera1$sheet_no) & 
                                 COMMON_NAME%in%This.sp)%>%
                        mutate(method=ifelse(method=="GN","Gillnet",
                                             ifelse(method=="LL","Longline",
                                                    NA)))%>%
                        group_by(COMMON_NAME,method)%>%
                        summarise(n=sum(number))%>%
                        mutate(Platform="Observer"),
                      LGN="top",
                      TITL="Discarded species")

fig=ggarrange(p1+rremove("xlab"), p2+rremove("xlab"),
              ncol = 1, nrow = 2,
              common.legend=TRUE)
annotate_figure(fig,bottom = text_grob("Number of individuals",size = 18))
ggsave(le.paste("Camera_v_Observer/Barplot.tiff"),width = 12,height = 12,compression = "lzw")


#3. Statistical comparison
little.fn=function(d,Method,group,Not)
{
  n=d%>%
    filter(method==Method)%>%
    group_by(COMMON_NAME)%>%
    summarise(Tot=sum(n))%>%
    arrange(-Tot)%>%
    mutate(Cumktch=cumsum(Tot)/sum(Tot))
  if(group=='95')
  {
    what.species=n%>%mutate(species2=ifelse(Cumktch<=0.95,COMMON_NAME,'Other'))%>%pull(species2)
  }
  if(group=='Top20')
  {
    what.species=n%>%mutate(Row=1:n(),
                            species2=ifelse(Row<=20,COMMON_NAME,'Other'))%>%pull(species2)
  }
  what.species=unique(what.species)
  dd=d%>%
    filter(method==Method)%>%
    mutate(COMMON_NAME=ifelse(COMMON_NAME%in%what.species,COMMON_NAME,"Other"))%>%
    group_by(COMMON_NAME,sheet_no,Platform)%>%
    summarise(N=sum(n))%>%
    spread(COMMON_NAME,N,fill=0)%>%
    ungroup()
  
  dd$ColSum=rowSums(dd[-match(Not,names(dd))])
  dd[-match(c(Not,'ColSum'),names(dd))]=dd[-match(c(Not,'ColSum'),names(dd))]/dd$ColSum
  dd=dd%>%
    dplyr::select(-ColSum)%>%
    data.frame
  return(dd)
}
D2=function(null,dev) (null-dev)/null
fn.mod=function(dd,SBTL)
{
  #check_overdispersion(Pois.mod)
  #Pois.mod <- glm(Camera ~ Observer * method, family=poisson(link = "identity"), data=d1) #identity link to assume linearity
  #QuasiPois.mod <- glm(Camera ~ Observer * method, family=quasipoisson(link = "identity"), data=d1)
  #NB.mod <- glm.nb(Camera ~ Observer * method, data=d1, link='identity')
  
  mod <- glm(Camera ~ Observer + method, family="gaussian", data=dd)
  D2.mod=D2(mod$null.deviance,mod$deviance)
  
  pred=predict(mod,type='response',se.fit=T)
  Preds=data.frame(Observer=dd$Observer,
                   method=dd$method)%>%
    mutate(Camera=pred$fit,
           Pred.SE=pred$se.fit,
           low=Camera-1.96*Pred.SE,
           high=Camera+1.96*Pred.SE)
  p=dd%>%
    ggplot(aes(Observer,Camera,color=method))+
    geom_jitter(size=2.5)+
    geom_line(aes(Observer,Observer),linetype=2,color='black')+
    geom_line(data=Preds,aes(Observer,Camera))+
    geom_ribbon(data=Preds,aes(ymin = low, ymax = high,fill=method),alpha = 0.2)+
    theme_PA(Ttl.siz=18,Sbt.siz=17,str.siz=16,strx.siz=16,
             leg.siz=18,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "top",
          legend.title = element_blank())+
    xlab("Number of individuals reported by observer")+
    ylab("Number of individuals recorded from camera")+
    labs(subtitle=bquote(.(SBTL)~.("(")*D^2*.('=')*.(round(D2.mod,3))*.(")")))
  
  COEF=summary(mod)
  COEF=as.data.frame(COEF$coefficients)[1:2,1:2]%>%
    rename(SE="Std. Error")%>%
    mutate(low=Estimate-1.96*SE,
           high=Estimate+1.96*SE,
           Coef=c("Intercept","Slope"))
  
  return(list(mod=mod,p=p))
}
lolipop=function(x)
{
  p=x%>%
    ungroup()%>%
    mutate(COMMON_NAME = fct_reorder(COMMON_NAME, N))%>%
    ggplot()+
    geom_linerange(aes(x = COMMON_NAME, ymin = 0, ymax =  Mean, colour = method), 
                   position = position_dodge(width = 0),show.legend=F)+
    geom_hline(yintercept = 0,linetype=2)+
    geom_errorbar(aes(ymin = xmin,ymax = xmax,x=COMMON_NAME),width = .125,show.legend=F)+
    geom_point(aes(x = COMMON_NAME, y = Mean, fill = method),pch=21,size=4,
               position = position_dodge(width = 0))+
    geom_text(aes(x = COMMON_NAME, y = Mean,label = paste("n= ",N,sep='')),
              size=6,alpha=.8,hjust=0.5, vjust=-0.5)+
    coord_flip()+
    facet_wrap(~method)+
    theme_PA(Ttl.siz=18,Sbt.siz=17,str.siz=16,strx.siz=20,
             leg.siz=14,axs.t.siz=14,axs.T.siz=18)+
    theme(legend.position = "none")+
    ylab("Percentage diference")+xlab('')+ylim(-100,100)
  return(p)
}
fn.compare.obs.cam=function(CAM, OBS, GROUP, Terms, Minobs.per)  
{
  Not=c('sheet_no',Terms)
  d=rbind(CAM%>%
            mutate(Code=as.numeric(Code))%>%
            filter(!is.na(Code))%>%
            filter(!is.na(COMMON_NAME))%>%
            filter(sheet_no%in%unique(OBS$sheet_no))%>%
            group_by(COMMON_NAME,method,sheet_no)%>%
            summarise(n=sum(number))%>%
            mutate(Platform="Camera"),
          OBS%>%
            filter(!is.na(COMMON_NAME))%>%
            filter(!COMMON_NAME=="Unidentified")%>%
            filter(!COMMON_NAME=="")%>%
            group_by(COMMON_NAME,method,sheet_no)%>%
            summarise(n=sum(number))%>%
            mutate(Platform="Observer"))%>%
    ungroup()
  
  d=d%>%
    mutate(method=ifelse(method=="GN","Gillnet",
                         ifelse(method=="LL","Longline",
                                method)))
  # Efrt=OBS%>%
  #   filter(sheet_no%in%d$sheet_no)%>%
  #   distinct(sheet_no,method,Effort.infRD)
  
  # d=d%>%
  #   left_join(Efrt,by=c("sheet_no","method"))%>%
  #   filter(!COMMON_NAME=="Unidentified")
  
  #extract non-match data for review
  out.for.Jack=d%>%
    dplyr::select(-method)%>%
    spread(Platform,n,fill=0)%>%
    mutate(Discrepancy=abs(Camera-Observer))%>%
    filter(Discrepancy>0)
  
  #display discrepancies
  Shts=d%>%
    distinct(sheet_no)%>%
    arrange(sheet_no)%>%
    mutate(Shot=1:n(),
           Shot=factor(Shot,levels=Shot))
  
  my.formula <- y ~ x
  p.ind=d%>%
    spread(Platform,n,fill=0)%>%
    mutate(Discrepancy=abs(Observer-Camera))%>%
    left_join(Shts,by="sheet_no")%>%
    filter(!is.na(method))%>%
    ungroup()%>%
    ggplot(aes(x=Observer,y=Camera))+
    geom_line(aes(Observer,Observer),size=1.5)+
    geom_jitter(aes(color=COMMON_NAME), size=3)+
    # geom_smooth(method = "lm",se=T,formula = my.formula)+
    #  stat_poly_eq(formula = my.formula, 
    #              aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    #             label.y = "top",label.x="left",parse = TRUE, size = 5) +
    facet_wrap(~ method,ncol=2,scales = "free")+
    xlab("Number of individuals reported by observer")+
    ylab("Number of individuals recorded by camera")+
    theme_PA(Ttl.siz=18,Sbt.siz=17,str.siz=16,strx.siz=16,
             leg.siz=11,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "top",
          legend.title = element_blank())+
    xlim(0,NA) 
  
  p.sp=d%>%
    mutate(n=1)%>%
    spread(Platform,n,fill=0)%>%
    mutate(Discrepancy=abs(Observer-Camera))%>%
    left_join(Shts,by="sheet_no")%>%
    filter(!is.na(method))%>%
    group_by(Shot,method)%>%
    summarise(Camera=sum(Camera),
              Observer=sum(Observer))%>%
    ggplot(aes(x=Observer,y=Camera))+
    geom_line(aes(Observer,Observer),size=1.5)+
    geom_jitter(aes(color=method), size=3,show.legend = F)+
    facet_wrap(~ method,ncol=2,scales = "free_x")+
    xlab("Number of species reported by observer")+
    ylab("Number of species recorded by camera")+
    theme_PA(Ttl.siz=18,Sbt.siz=17,str.siz=16,strx.siz=16,
             lgT.siz=18,leg.siz=18,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "top")+
    xlim(0,NA)
  
  
  #univariate stats (follow Emery et al 2019) 
  do.cpue=FALSE
  if(do.cpue)
  {
    d=d%>%
      mutate(cpue=n/Effort.infRD,
             log.cpue=log(cpue))
    d%>%
      ggplot(aes(n, fill = Platform)) +
      geom_histogram(binwidth=.5, position="dodge")
    
    d%>%
      filter(method=="GN")%>%
      ggplot(aes(log.cpue, color=Platform)) +geom_density()
    
    d%>%
      filter(method=="LL")%>%
      ggplot(aes(log.cpue, color=Platform)) +geom_density()
    
    mod.GN <- glm(log.cpue ~ Platform * COMMON_NAME, family="gaussian", data=d%>%filter(method=="GN"))
    mod.LL <- glm(log.cpue ~ Platform * COMMON_NAME, family="gaussian", data=d%>%filter(method=="LL"))
  }
  zero.ktch.sht=d%>%
    group_by(sheet_no)%>%
    summarise(N=sum(n))%>%
    filter(N==0)%>%
    pull(sheet_no)
  d1=d
  if(length(zero.ktch.sht)>0) d1=d1%>%filter(!sheet_no%in%zero.ktch.sht)
  d1=d1%>%
    spread(Platform,n,fill=0)%>%
    filter(!is.na(method))%>%
    mutate(method=factor(method))
  
  Discarded.species=TAB.disc%>%pull(COMMON_NAME)
  mod_all.species=fn.mod(dd=d1,SBTL="All species")
  mod_commercial.species=fn.mod(dd=d1%>%
                                  filter(!COMMON_NAME%in%Discarded.species),
                                SBTL="Commercial species")  
  mod_discarded.species=fn.mod(dd=d1%>%
                                 filter(COMMON_NAME%in%Discarded.species),
                               SBTL="Discarded species")
  
  #Percentage difference by species
  Per.diff=d1%>%
    rowwise()%>%
    mutate(Max=max(c_across(Camera:Observer)))%>%
    mutate(Per.diff=100*(Camera-Observer)/Max)%>%
    group_by(COMMON_NAME,method)%>%
    summarise(Mean=mean(Per.diff),
              SD=sd(Per.diff),
              N=n())%>%
    mutate(SE=SD/sqrt(SD),
           xmin=ifelse(!is.na(SE),Mean-SE,0),
           xmax=ifelse(!is.na(SE),Mean+SE,0))%>%
    ungroup()%>%
    filter(N>=Minobs.per)
  Per.dif=lolipop(x=Per.diff)
  Per.dif.com=lolipop(x=Per.diff%>%filter(!COMMON_NAME%in%Discarded.species))
  Per.dif.dis=lolipop(x=Per.diff%>%filter(COMMON_NAME%in%Discarded.species))
  
  
  
  #multivariate
  #1. GN
  dd=little.fn(d=d,Method="Gillnet",group=GROUP,Not=Not)   
  Community <- dd[-match(Not,names(dd))]  
  
  #MDS
  MDS <- metaMDS(comm = Community, distance = "bray",k=2,trymax=100, trace = FALSE, autotransform = FALSE)
  MDS_xy <-data.frame(MDS$points)%>%
    mutate(dummy =dd[,match(Terms,names(dd))],
           name=dd[,match('sheet_no',names(dd))])
  p.GN=MDS_xy%>%
    ggplot(aes(MDS1, MDS2, color = dummy,label =name)) +
    geom_point(size=3) + geom_text_repel(show.legend = F)+
    theme_bw() +
    annotate(geom="text", x=0.85*max(MDS_xy$MDS1), y=min(MDS_xy$MDS2), 
             label=paste("Stress=",round(MDS$stress,3)))+
    theme_PA(Ttl.siz=18,leg.siz=14,axs.t.siz=10,axs.T.siz=14)+
    theme(legend.position = "top",
          legend.justification='right',
          legend.title = element_blank())+
    guides(colour = guide_legend(nrow = 1))+
    ggtitle("Gillnet")+xlab('')+ylab('')
  
  #Permanova
  adon.results<-adonis(formula(paste('Community',Terms,sep='~')),data=dd, method="bray",perm=5e3)
  adon.GN=as.data.frame(adon.results$aov.tab)
  
  
  #2. LL
  dd=little.fn(d=d,Method="Longline",group=GROUP,Not=Not)   
  Community <- dd[-match(Not,names(dd))]
  
  #MDS
  MDS <- metaMDS(comm = Community, distance = "bray",k=2,trymax=100, trace = FALSE, autotransform = FALSE)
  MDS_xy <-data.frame(MDS$points)%>%
    mutate(dummy = dd[,match(Terms,names(dd))],
           name=dd[,match('sheet_no',names(dd))])
  p.LL=MDS_xy%>%
    ggplot(aes(MDS1, MDS2, color = dummy,label =name)) +
    geom_point(size=3) + geom_text_repel(show.legend = F)+
    theme_bw() +
    annotate(geom="text", x=0.85*max(MDS_xy$MDS1), y=min(MDS_xy$MDS2), 
             label=paste("Stress=",round(MDS$stress,3)))+
    theme_PA(Ttl.siz=18,leg.siz=12,axs.t.siz=10,axs.T.siz=14)+
    theme(legend.position = "none",
          legend.title = element_blank())+
    guides(colour = guide_legend(nrow = 1))+
    ggtitle("Longline")+xlab('')+ylab('')
  
  #Permanova
  adon.results<-adonis(formula(paste('Community',Terms,sep='~')),data=dd, method="bray",perm=5e3)
  adon.LL=as.data.frame(adon.results$aov.tab)
  
  
  return(list(out.for.Jack=out.for.Jack,
              p.ind=p.ind,p.sp=p.sp,
              mod_all.species=mod_all.species,
              mod_commercial.species=mod_commercial.species,
              mod_discarded.species=mod_discarded.species,
              Per.dif=Per.dif,
              Per.dif.com=Per.dif.com,
              Per.dif.dis=Per.dif.dis,
              p.GN=p.GN,p.LL=p.LL,
              adon.GN=adon.GN,adon.LL=adon.LL))
}
Out=fn.compare.obs.cam(CAM=EM.vs.OM.Video.camera1,
                       OBS=EM.vs.OM.DATA%>%
                         filter(sheet_no%in%unique(EM.vs.OM.Video.camera1$sheet_no)),
                       GROUP='Top20',
                       Terms='Platform',
                       Minobs.per=Minobs.per)
#export data issues
# write.csv(Out$out.for.Jack,le.paste("Camera_v_Observer/out.for.Jack.csv"),row.names=F)

#export Raw data comparison
Out$p.ind
# ggsave(le.paste("Camera_v_Observer/raw.tiff"),width = 12,height = 10,compression = "lzw")

Out$p.sp
# ggsave(le.paste("Camera_v_Observer/raw_species.tiff"),width = 12,height = 10,compression = "lzw")


#export GLMS
ft <- as_flextable(Out$mod_all.species$mod)
# save_as_image(ft, path = le.paste("Camera_v_Observer/GLM_mod_all.species.png"))
ft <- as_flextable(Out$mod_commercial.species$mod)
# save_as_image(ft, path = le.paste("Camera_v_Observer/GLM_mod_commercial.species.png"))
ft <- as_flextable(Out$mod_discarded.species$mod)
# save_as_image(ft, path = le.paste("Camera_v_Observer/GLM_mod_discarded.species.png"))

fig=ggarrange(Out$mod_all.species$p+rremove("xlab")+rremove("ylab"),
              Out$mod_commercial.species$p+rremove("xlab")+rremove("ylab"),
              Out$mod_discarded.species$p+rremove("xlab")+rremove("ylab"),
              ncol = 1, nrow = 3,
              common.legend=TRUE)
annotate_figure(fig,
                bottom = text_grob("Number of individuals reported by observer",size = 18),
                left = text_grob("Number of individuals recorded by camera",size = 18,rot = 90))
ggsave(le.paste("Camera_v_Observer/GLM_preds.tiff"),width = 8,height = 12,compression = "lzw")


#export Percentage difference
Out$Per.dif
ggsave(le.paste("Camera_v_Observer/Percentage.difference.tiff"),width = 10,height = 12,compression = "lzw")

#Out$Per.dif.com
ggsave(le.paste("Camera_v_Observer/Percentage.difference_com.tiff"),width = 10,height = 12,compression = "lzw")

#Out$Per.dif.dis
ggsave(le.paste("Camera_v_Observer/Percentage.difference_disc.tiff"),width = 10,height = 12,compression = "lzw")


#export MDS
fig=ggarrange(Out$p.GN, Out$p.LL,
              ncol = 1, nrow = 2,
              common.legend=TRUE)
ggsave(le.paste("Camera_v_Observer/MDS.tiff"),width = 8,height = 12,compression = "lzw")


#export Permanovas
write.csv(Out$adon.GN,le.paste("Camera_v_Observer/Permanova_GN.csv"),row.names = T)
write.csv(Out$adon.LL,le.paste("Camera_v_Observer/Permanova_LL.csv"),row.names = T)


#---------Analysis of Deck 2 camera VS observers --------------------------------------------------------------------
#AKA
# Deck 2  (over roller)                      
Video.camera2=Video.camera2.deck%>%
  mutate(Code=ifelse(Genus=="Kyphosus" & Species=="spp",'37361903',Code))%>%
  dplyr::select(DPIRD.code,Code,Period)%>%
  mutate(number=1)%>%
  data.frame%>%
  separate(DPIRD.code, into = c("GN", "LL"), sep = "/", remove = FALSE) %>% 
  mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                            Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                            Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                            Code >=3.7046e7 & Code<=3.747e7 ~"Scalefish",
                            Code >=4.1e+07 & Code<=4.115e+07 ~"Marine mammals",
                            Code >=4.0e+07 & Code<4.1e+07 ~"Seabirds",
                            Code >=1.2e7 & Code<3.7e7 ~"Invertebrates",
                            Code >=1.1e7 & Code<1.2e7 ~"Rock/reef structure",
                            Code >=5.4e7 & Code<5.49e7 ~"Macroalgae",
                            Code == 10000910 ~"Sponges"),
         Period=tolower(Period),
         Period=ifelse(Period=='gn','gillnet',
                       ifelse(Period=='ll','longline',Period)),
         sheet_no=ifelse(Period=='gillnet',  as.character(GN),as.character(LL)))%>%
  left_join(DATA_PA,by='sheet_no')%>%
  mutate(Data.set="camera") %>%
  filter(sheet_no %in% D2.good.ones$sheet_no) %>% 
  mutate(
    sheet_no = substr(sheet_no, 3,8)
  )

EM.vs.OM.Video.camera2=Video.camera2%>%
  mutate(Code=as.numeric(Code))%>%
  left_join(All.species.names%>%
              dplyr::select(COMMON_NAME,Code)%>%
              distinct(Code,.keep_all=T),
            by="Code")%>%
  mutate(method=ifelse(method=="GN","Gillnet",
                       ifelse(method=="LL","Longline",
                              NA)))%>%
  mutate(COMMON_NAME=case_when(grepl(paste(c("whaler","dusky shark"),collapse = '|'),tolower(COMMON_NAME))~'Whalers',
                               grepl(paste(c("blacktip sharks","Common blacktip"),collapse = '|'),tolower(COMMON_NAME))~'Blacktip sharks',
                               grepl('wobbegong',tolower(COMMON_NAME))~'Wobbegongs',
                               grepl('hammerhead',tolower(COMMON_NAME))~'Hammerheads',
                               grepl('stingray',tolower(COMMON_NAME))~'Stingrays',
                               grepl('angel',tolower(COMMON_NAME))~'Angel sharks',
                               grepl('catshark',tolower(COMMON_NAME))~'Catsharks',
                               grepl('gurnard',tolower(COMMON_NAME))~'Gurnards',
                               grepl('jacket',tolower(COMMON_NAME))~'Leatherjackets',
                               grepl('guitarfish',tolower(COMMON_NAME))~'Guitarfish & shovelnose rays',
                               grepl(paste(c("gummy shark","whiskery shark"),collapse = '|'),tolower(COMMON_NAME))~"Gummy/Whiskery sharks",
                               TRUE~COMMON_NAME))


#1 Barplot of observer VS camera
TAB=EM.vs.OM.DATA%>%
  filter(!is.na(COMMON_NAME) & !COMMON_NAME=='')%>%
  group_by(retainedflag,COMMON_NAME)%>%
  tally()%>%
  ungroup()
TAB.comm=TAB%>%
  filter(retainedflag=='Yes')%>%
  arrange(-n)%>%
  mutate(Cum=cumsum(n),
         Percent=100*Cum/sum(n))
TAB.disc=TAB%>%
  filter(retainedflag=='No' & !COMMON_NAME%in%TAB.comm$COMMON_NAME)%>%
  arrange(-n)%>%
  mutate(Cum=cumsum(n),
         Percent=100*Cum/sum(n))


#Commercial species
This.sp=TAB.comm%>%pull(COMMON_NAME)
This.sp.other=TAB.comm%>%filter(Percent<=95)%>%pull(COMMON_NAME)
p1=fn.obs_cam.barplot(n.shots=EM.vs.OM.DATA%>%
                        filter(sheet_no%in%unique(Video.camera2$sheet_no))%>%
                        distinct(sheet_no,method)%>%
                        group_by(method)%>%
                        tally()%>%
                        spread(method,n),
                      Cam=EM.vs.OM.Video.camera2%>%
                        filter(COMMON_NAME%in%This.sp)%>%
                        group_by(COMMON_NAME,method)%>%
                        summarise(n=sum(number))%>%
                        mutate(Platform="Camera"),
                      Obs=EM.vs.OM.DATA%>%
                        filter(sheet_no%in%unique(Video.camera2$sheet_no) & 
                                 COMMON_NAME%in%This.sp)%>%
                        mutate(method=ifelse(method=="GN","Gillnet",
                                             ifelse(method=="LL","Longline",
                                                    NA)))%>%
                        group_by(COMMON_NAME,method)%>%
                        summarise(n=sum(number))%>%
                        mutate(Platform="Observer"),
                      LGN="top",
                      TITL="Commercial species")

#Bycatch species
This.sp=TAB.disc%>%pull(COMMON_NAME)
This.sp.other=TAB.disc%>%filter(Percent<=95)%>%pull(COMMON_NAME)
p2=fn.obs_cam.barplot(n.shots=EM.vs.OM.DATA%>%
                        filter(sheet_no%in%unique(Video.camera2$sheet_no))%>%
                        distinct(sheet_no,method)%>%
                        group_by(method)%>%
                        tally()%>%
                        spread(method,n),
                      Cam=EM.vs.OM.Video.camera2%>%
                        filter(COMMON_NAME%in%This.sp)%>%
                        group_by(COMMON_NAME,method)%>%
                        summarise(n=sum(number))%>%
                        mutate(Platform="Camera"),
                      Obs=EM.vs.OM.DATA%>%
                        filter(sheet_no%in%unique(Video.camera2$sheet_no) & 
                                 COMMON_NAME%in%This.sp)%>%
                        mutate(method=ifelse(method=="GN","Gillnet",
                                             ifelse(method=="LL","Longline",
                                                    NA)))%>%
                        group_by(COMMON_NAME,method)%>%
                        summarise(n=sum(number))%>%
                        mutate(Platform="Observer"),
                      LGN="top",
                      TITL="Discarded species")

fig=ggarrange(p1+rremove("xlab"), p2+rremove("xlab"),
              ncol = 1, nrow = 2,
              common.legend=TRUE)
annotate_figure(fig,bottom = text_grob("Number of individuals",size = 18))
ggsave(le.paste("Camera2_v_Observer/Barplot.tiff"),width = 12,height = 12,compression = "lzw")


#2. Statistical comparison
Out=fn.compare.obs.cam(CAM=EM.vs.OM.Video.camera2,
                       OBS=EM.vs.OM.DATA%>%
                         filter(sheet_no%in%unique(EM.vs.OM.Video.camera2$sheet_no)),
                       GROUP='Top20',
                       Terms='Platform',
                       Minobs.per=Minobs.per)
#export data issues
# write.csv(Out$out.for.Jack,le.paste("Camera2_v_Observer/out.for.Jack.csv"),row.names=F)

#export Raw data comparison
Out$p.ind
ggsave(le.paste("Camera2_v_Observer/raw.tiff"),width = 12,height = 10,compression = "lzw")

Out$p.sp
ggsave(le.paste("Camera2_v_Observer/raw_species.tiff"),width = 12,height = 10,compression = "lzw")


#export GLMS
ft <- as_flextable(Out$mod_all.species$mod)
save_as_image(ft, path = le.paste("Camera2_v_Observer/GLM_mod_all.species.png"))
ft <- as_flextable(Out$mod_commercial.species$mod)
save_as_image(ft, path = le.paste("Camera2_v_Observer/GLM_mod_commercial.species.png"))
ft <- as_flextable(Out$mod_discarded.species$mod)
save_as_image(ft, path = le.paste("Camera2_v_Observer/GLM_mod_discarded.species.png"))

fig=ggarrange(Out$mod_all.species$p+rremove("xlab")+rremove("ylab"),
              Out$mod_commercial.species$p+rremove("xlab")+rremove("ylab"),
              Out$mod_discarded.species$p+rremove("xlab")+rremove("ylab"),
              ncol = 1, nrow = 3,
              common.legend=TRUE)
annotate_figure(fig,
                bottom = text_grob("Number of individuals reported by observer",size = 18),
                left = text_grob("Number of individuals recorded by camera",size = 18,rot = 90))
ggsave(le.paste("Camera2_v_Observer/GLM_preds.tiff"),width = 8,height = 12,compression = "lzw")


#export Percentage difference
Out$Per.dif
ggsave(le.paste("Camera2_v_Observer/Percentage.difference.tiff"),width = 10,height = 12,compression = "lzw")

Out$Per.dif.com
ggsave(le.paste("Camera2_v_Observer/Percentage.difference_com.tiff"),width = 10,height = 12,compression = "lzw")

# Out$Per.dif.dis
ggsave(le.paste("Camera2_v_Observer/Percentage.difference_disc.tiff"),width = 10,height = 12,compression = "lzw")


#export MDS
fig=ggarrange(Out$p.GN, Out$p.LL,
              ncol = 1, nrow = 2,
              common.legend=TRUE)
ggsave(le.paste("Camera2_v_Observer/MDS.tiff"),width = 8,height = 12,compression = "lzw")


#export Permanovas
write.csv(Out$adon.GN,le.paste("Camera2_v_Observer/Permanova_GN.csv"),row.names = T)
write.csv(Out$adon.LL,le.paste("Camera2_v_Observer/Permanova_LL.csv"),row.names = T)


#---------Gilled or bagged ------------ 
Tab.gilled.bagged=Video.camera1.deck%>%
  filter(Period=="gillnet")%>%
  filter(!is.na(meshed))%>%
  group_by(COMMON_NAME,meshed)%>%
  tally()%>%
  ungroup()%>%
  spread(meshed,n,fill=0)%>%
  left_join(DATA%>%
              distinct(common_name,Taxa),by=c('COMMON_NAME'='common_name'))%>%
  mutate(n=bagged+gilled)%>%
  ungroup()%>%
  arrange(Taxa,-n)%>%
  dplyr::select(-Taxa,-n)%>%
  filter(!is.na(COMMON_NAME))
# write.csv(Tab.gilled.bagged,le.paste("Video/deck.cameras/Tab.gilled.bagged.csv"),row.names=F)



#---------Analyses for Abbey ------------
if(export.abbeys)
{
  fn.get.top.1=function(d,N)
  {
    d=colSums(d%>%dplyr::select(-sheet_no,-Method,-Interaction,-n.cameras))
    out=names(d[d>N])
  }
  fn.explr.Abbey.1=function(d,vars)
  {
    d1=d[,vars]/d$Eff
    d1%>%
      mutate(sheet_no=d$sheet_no,
             Method=d$Method,
             Interaction=d$Interaction)%>%
      gather("Species","cpue",-sheet_no,-Method,-Interaction)%>%
      filter(cpue>0)%>%
      mutate(log.cpue=log(cpue))%>%
      ggplot(aes(x=Method, y=log.cpue, fill=Interaction)) + 
      geom_boxplot()+
      facet_wrap(~Species, scale="free")
  }
  fn.get.top.2=function(d,N)
  {
    d=colSums(d%>%dplyr::select(-sheet_no,-Method,-Data.set,-Fishing.effort))
    out=names(d[d>N])
  }
  fn.explr.Abbey.2=function(d,vars)
  {
    d1=d[,vars]/d$Eff
    d1%>%
      mutate(sheet_no=d$sheet_no,
             Method=d$Method,
             Data.set=d$Data.set)%>%
      gather("Species","cpue",-sheet_no,-Method,-Data.set)%>%
      filter(cpue>0)%>%
      mutate(log.cpue=log(cpue))%>%
      ggplot(aes(x=Method, y=log.cpue, fill=Data.set)) + 
      geom_boxplot()+
      facet_wrap(~Species, scale="free")
  }
  pdf(file=handl_OneDrive('Analyses/Parks_Australia/outputs/Data_for_Abbey/Exploraty.pdf'))
  fn.explr.Abbey.1(d=Abbey.data.chapter.1_species%>%dplyr::rename(Eff=n.cameras),
                   vars=fn.get.top.1(d=Abbey.data.chapter.1_species,N=250))
  fn.explr.Abbey.1(d=Abbey.data.chapter.1_SP.group%>%dplyr::rename(Eff=n.cameras),
                   vars=fn.get.top.1(d=Abbey.data.chapter.1_SP.group,N=30))
  fn.explr.Abbey.2(d=Abbey.data.chapter.2_species%>%dplyr::rename(Eff=Fishing.effort),
                   vars=fn.get.top.2(d=Abbey.data.chapter.2_species,N=100))
  fn.explr.Abbey.2(d=Abbey.data.chapter.2_SP.group%>%dplyr::rename(Eff=Fishing.effort),
                   vars=fn.get.top.2(d=Abbey.data.chapter.2_SP.group,N=30))
  dev.off()
  
}


#---------Analyse PA TEPS --------------  

#1. Cameras
#note: number of events by interaction type
fn.TEPS.barplot=function(n.shots,d,hours.LL,hours.GN,show.all.levels=TRUE,LGN,TITL)
{
  if(is.null(hours.LL))
  {
    LGN.LL=paste(' (',n.shots$longline,' shots)',sep='')
    LGN.GN=paste(' (',n.shots$gillnet,' shots)',sep='')
  }else
  {
    LGN.LL=paste(' (',hours.LL,' video hours; ',n.shots$longline,' shots)',sep='')
    LGN.GN=paste(' (',hours.GN,' video hours; ',n.shots$gillnet,' shots)',sep='')
  }
  
  d=d%>%
    mutate(Method.hour=ifelse(Method=='Longline',paste(Method,LGN.LL,sep=''),
                              ifelse(Method=='Gillnet',paste(Method,LGN.GN,sep=''),
                                     NA)))
  p=d%>%
    mutate(Name=factor(Name,levels=TEPS.names$Name))%>%
    ggplot(aes(x=Interaction, y=n, fill=Name)) + 
    geom_bar(position="stack", stat="identity")+
    coord_flip() +
    facet_wrap(~Method.hour,dir='h')+ 
    theme_PA(str.siz=16,strx.siz=16,leg.siz=17,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = LGN,
          legend.justification='top',
          legend.direction='horizontal',
          legend.title = element_blank())+
    xlab('')+ylab('Number of events')+
    scale_y_continuous(breaks = integer_breaks())+
    guides(fill = guide_legend(nrow = 4))+
    scale_x_discrete(position = "top")
  
  if(!is.null(TITL)) p=p+ggtitle(TITL)
  if(show.all.levels) p=p+scale_fill_manual(values=TEPS.cols)
  if(!show.all.levels)
  {
    THIS=match(unique(d$Name),names(TEPS.cols))
    p=p+scale_fill_manual(values=TEPS.cols[THIS])
  }
  print(p)
}


# 1.1. Underwater
p1=fn.TEPS.barplot(n.shots=rbind(Video.longline.interaction%>%
                                   dplyr::select(sheet_no,Method,Interaction,Number,SP.group,Species,Code)%>%
                                   mutate(Period='longline'),
                                 Video.net.interaction%>%
                                   dplyr::select(sheet_no,Method,Interaction,Number,SP.group,Species,Code)%>%
                                   mutate(Period='gillnet'))%>%
                     distinct(sheet_no,Period)%>%
                     group_by(Period)%>%
                     tally()%>%
                     spread(Period,n),
                   d=rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species,Code),
                           Video.net.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species,Code))%>%
                     mutate(Number=1,
                            Method=capitalize(Method))%>%
                     filter(SP.group%in%TEP.groups | Code%in%TEPS_Shark.rays)%>%
                     filter(!Species=='birds feeding at surface')%>%
                     left_join(TEPS.names,by="Code")%>%
                     group_by(Method,Interaction,Name,Colr,Code)%>%
                     tally(Number)%>%
                     filter(!is.na(Interaction))%>%
                     mutate(Interaction=capitalize(tolower(Interaction)))%>%
                     mutate(Interaction=case_when(Interaction=="Avoid" ~ "Avoidance",
                                                  Interaction=="Caught while predating" ~ "Caught while feeding",
                                                  Interaction=="Bounce off" ~ "Bounced off",
                                                  Interaction== "Swim past" ~ "Swam past",
                                                  Interaction== "Swim through" ~ "Swam through",
                                                  TRUE~Interaction)),
                   hours.LL=hours.underwater.ll,
                   hours.GN=hours.underwater.gn,
                   LGN="top",
                   TITL="Underwater cameras")
ggsave(le.paste("TEPS/Interactions_number.events_underwater.test.tiff"),width = 20,height = 8,compression = "lzw")


# 1.2. subsurface
p2=fn.TEPS.barplot(n.shots=Video.subsurface%>%
                     distinct(SHEET_NO,Period)%>%
                     group_by(Period)%>%
                     tally()%>%
                     spread(Period,n),
                   d=rbind(Video.subsurface.comments%>%
                             filter(Code%in%TEPS.codes)%>%
                             mutate(Method=capitalize(tolower(Period)),
                                    Number=1,
                                    Interaction=ifelse(grepl('diving',comment),'diving',NA))%>%
                             dplyr::select(Method,Interaction,Code,Number),
                           Video.subsurface%>%
                             filter(Code%in%TEPS.codes)%>%
                             mutate(Number=1,
                                    Method=capitalize(Period),
                                    Drop.out=capitalize(tolower(Drop.out)),
                                    Dropout.condition=tolower(Dropout.condition),
                                    Dropout.condition=case_when(is.na(Dropout.condition)~'',
                                                                TRUE~Dropout.condition),
                                    dummy=case_when(Drop.out=="Yes"~"Drop out",
                                                    Drop.out=="No"~"Caught"),
                                    Interaction=case_when(dummy=="Drop out"~paste(dummy," (",Dropout.condition,")",sep=''),
                                                          dummy=="Caught"~dummy))%>%
                             dplyr::select(Method,Interaction,Code,Number))%>%   
                     left_join(TEPS.names,by="Code")%>%
                     group_by(Method,Interaction,Name,Colr,Code)%>%
                     tally(Number)%>%
                     mutate(Name=factor(Name,levels=TEPS.names$Name)),
                   hours.LL=hours.subsurface.ll,
                   hours.GN=hours.subsurface.gn,
                   LGN="top",
                   TITL="Subsurface camera")
ggsave(le.paste("TEPS/Interactions_number.events_subsurface.tiff"),width = 20,height = 8,compression = "lzw")

# 1.3. Deck 1 
p3=fn.TEPS.barplot(n.shots=Video.camera1%>%
                     distinct(sheet_no,Period)%>%
                     group_by(Period)%>%
                     tally()%>%
                     spread(Period,n),
                   d=Video.camera1%>%
                     filter(Code%in%TEPS.codes)%>%
                     mutate(Interaction=paste("Caught (",condition,")",sep=''),
                            Code=as.numeric(Code))%>%
                     left_join(TEPS.names,by="Code")%>%
                     mutate(Number=1,
                            Method=capitalize(Period))%>%
                     group_by(Method,Interaction,Name,Colr,Code)%>%
                     tally(Number)%>%
                     mutate(Name=factor(Name,levels=TEPS.names$Name),
                            Method=factor(Method,levels=c('Gillnet','Longline'))),
                   hours.LL=hours.deck1.ll,
                   hours.GN=hours.deck1.gn,
                   LGN="none",
                   TITL="Deck camera 1")
ggsave(le.paste("TEPS/Interactions_number.events_deck1.tiff"),width = 12,height = 8,compression = "lzw")

# 1.4. Deck 2  (pointing to roller) 
#note: condition is not recorded by this camera
## NOTE: Activity strings changed because "passing" should be "caught"
p4=fn.TEPS.barplot(n.shots=rbind(Video.camera2.deck_observations%>%distinct(Period,SHEET_NO),
                                 Video.camera2.deck%>%distinct(Period,SHEET_NO))%>%
                     distinct(SHEET_NO,Period)%>%
                     group_by(Period)%>%
                     tally()%>%
                     spread(Period,n),
                   d=rbind(Video.camera2.deck_observations%>%    
                             filter(Code%in%TEPS.codes)%>%
                             mutate(Activity = case_when(original.hooklocation == "caught feeding on Sargent Baker" ~ "Feeding",
                                                         original.dropout == "sealion feeding on macki" ~ "Feeding",
                                                         Activity == "Passing" ~ "Caught"))%>%
                             dplyr::select(Period,Genus,Species,Code,Activity),
                           Video.camera2.deck%>%
                             filter(Code%in%TEPS.codes)%>%
                             # mutate(Activity=ifelse(dropout=='Yes' & gaffed=="No","Drop out",
                             #                        ifelse(dropout=='Yes' & gaffed=="Yes","Drop out and gaffed",
                             #                               as.character(Activity))),
                                mutate(Activity = case_when(original.hooklocation == "caught feeding on Sargent Baker" ~ "Feeding",
                                                           original.dropout == "sealion feeding on macki" ~ "Feeding",
                                                           dropout=='Yes' & gaffed=="No" ~ "Drop out",
                                                           dropout=='Yes' & gaffed=="Yes" ~ "Drop out and gaffed",
                                                           Activity == "Passing" ~ "Caught",
                                                           is.na(Activity) ~ "Caught"))%>%
                             dplyr::select(Period,Genus,Species,Code,Activity))%>%
                     filter(!is.na(Activity))%>%
                     left_join(TEPS.names,by="Code")%>%
                     mutate(Number=1,
                            Method=capitalize(Period),
                            Activity=capitalize(Activity))%>%
                     group_by(Method,Activity,Name,Colr,Code)%>%
                     tally(Number)%>%
                     mutate(Interaction= Activity)%>%
                     mutate(Name=factor(Name,levels=TEPS.names$Name)),
                   hours.LL=hours.deck2.ll,
                   hours.GN=hours.deck2.gn,
                   LGN="top",
                   TITL="Deck camera 2")
ggsave(le.paste("TEPS/Interactions_number.events_deck2_new.tiff"),width = 12,height = 8,compression = "lzw")

#2. Observer data 
#data from TEPS data sheets
d.teps=TEPS%>%  
  filter(!is.na(contact.code))%>%
  mutate(Method=ifelse(gear.type=="GN","Gillnet",
                       ifelse(gear.type=="LL","Longline",
                              NA)),
         condition=ifelse(is.na(Cond)|Cond==''|Cond=='?','unknown',
                          ifelse(Cond>0,"alive",
                                 ifelse(Cond==0,"dead",
                                        ''))),
         con.code.sp=paste(capitalize(common.name),contact.code.to.complete),
         con.code.sp=ifelse(con.code.sp=="NA NA",NA,con.code.sp),
         contact.code.to.complete=capitalize(contact.code.to.complete),
         contact.code.to.complete=case_when(contact.code.to.complete=='Diving for but not feeding from net/longline'~'Diving',
                                            contact.code.to.complete=='Entangled/hooked'~'Caught',
                                            contact.code.to.complete=='Feeding from net/longline'~'Feeding',
                                            contact.code=='WDNN; WDDN'~'Feeding',
                                            TRUE~contact.code.to.complete),
         contact.code.to.complete=ifelse(contact.code.to.complete=='Caught',
                                         paste(contact.code.to.complete," (",condition,")",sep=''),
                                         contact.code.to.complete))%>%
  group_by(sheet_no,Method,contact.code.to.complete,common.name)%>%
  summarise(n=sum(contact.count))%>%
  mutate(common.name=capitalize(common.name),
         Name=common.name,
         Interaction=contact.code.to.complete)

#data from biological data sheets
d.data=rbind(DATA%>%
               filter(grepl(paste(tolower(c('bird','sea lion','Smooth',TEPS.names$Name)),collapse='|'),tolower(comments))),
             DATA%>%
               filter(common_name%in%TEPS.names$Name),
             DATA%>%
               filter(common_name=="Dusky shark" & tl>190)%>%    #190 is tl corresponding to 70 cm idl
               mutate(common_name='Oversized dusky shark'))%>%    
  distinct(sheet_no,line_no,method,common_name,rel.cond)%>%
  filter(common_name%in%TEPS.names$Name)%>%   
  rename(common.name=common_name,
         Method=method)%>%
  mutate(condition=ifelse(is.na(rel.cond),'unknown',
                          ifelse(rel.cond>0,"alive",
                                 ifelse(rel.cond==0,"dead",
                                        ''))),
         contact.code.to.complete=paste('Caught (',condition,')',sep=''),
         n=1,
         Name=common.name,
         Interaction=contact.code.to.complete)%>%
  filter(!sheet_no%in%d.teps$sheet_no)
d.data=d.data[,match(names(d.teps),names(d.data))]%>%
  mutate(Method=ifelse(Method=="GN","Gillnet",
                       ifelse(Method=="LL","Longline",
                              NA)))  

p5=fn.TEPS.barplot(n.shots=DATA%>%
                     distinct(sheet_no,method)%>%
                     mutate(method=ifelse(method=="GN","gillnet",
                                          ifelse(method=="LL","longline",
                                                 NA)))%>%
                     group_by(method)%>%
                     tally()%>%
                     spread(method,n),
                   d=rbind(d.teps,d.data),
                   hours.LL=NULL,
                   hours.GN=NULL,
                   LGN="top",
                   TITL="Onboard observers")
ggsave(le.paste("TEPS/Numbers.interactions.by.gear_Observers.tiff"), width = 20,height = 8,compression = "lzw")

fig=ggarrange(p1+rremove("xlab"), p2+rremove("xlab"),
              p4+rremove("xlab"), p3+rremove("xlab"), 
              p5+rremove("xlab"),
              ncol = 1, nrow = 5,
              common.legend=TRUE,
              heights=c(1.5,rep(1,4)))
annotate_figure(fig,bottom = text_grob("Number of events",size = 18))   

ggsave(le.paste("TEPS/Interactions.tiff"),
       width = 15.2,height = 13,compression = "lzw")


do.inter.for.each.shot=FALSE
if(do.inter.for.each.shot)
{
  fn.tep.barplot=function(d,all.gn.shots,all.ll.shots) 
  {
    d.nets=d%>%filter(gear.type=="GN")
    d.ll=d%>%filter(gear.type=="LL")
    
    #add 0 record if no TEPS
    #gillnet
    no.teps=all.gn.shots[which(!all.gn.shots%in%unique(d.nets$sheet_no))]
    if(length(no.teps)>0)
    {
      add.d=d.nets[1:length(no.teps),]
      add.d[,]=NA
      add.d$sheet_no=no.teps
      d.nets=rbind(d.nets,add.d)
      Shots=data.frame(sheet_no=sort(unique(d.nets$sheet_no)))
      Shots$shot=1:nrow(Shots)
      d.nets=d.nets%>%
        left_join(Shots,by="sheet_no")
    }
    #lonline
    no.teps=all.ll.shots[which(!all.ll.shots%in%unique(d.ll$sheet_no))]
    if(length(no.teps)>0)
    {
      add.d=d.ll[1:length(no.teps),]
      add.d[,]=NA
      add.d$sheet_no=no.teps
      d.ll=rbind(d.ll,add.d)
      
      Shots=data.frame(sheet_no=sort(unique(d.ll$sheet_no)))
      Shots$shot=1:nrow(Shots)
      d.ll=d.ll%>%
        left_join(Shots,by="sheet_no")
    }
    
    #Add species contact group
    Drop= paste(c("/hooked","/longline"), collapse = "|")
    d.nets=d.nets%>%
      mutate(contact.code.to.complete= str_remove_all(contact.code.to.complete, Drop),
             con.code.sp=paste(capitalize(common.name),contact.code.to.complete),
             con.code.sp=ifelse(con.code.sp=="NA NA",NA,con.code.sp))
    
    Drop= paste(c("entangled/","net/"), collapse = "|")
    d.ll=d.ll%>%
      mutate(contact.code.to.complete= str_remove_all(contact.code.to.complete, Drop),
             con.code.sp=paste(capitalize(common.name),contact.code.to.complete),
             con.code.sp=ifelse(con.code.sp=="NA NA",NA,con.code.sp))
    
    
    #Plot gillnet
    p1=d.nets%>%
      group_by(shot,gear.type,con.code.sp)%>%
      tally()%>%
      mutate(n=ifelse(is.na(con.code.sp),0,n),)%>%
      ggplot(aes(fill=con.code.sp, y=n, x=shot)) + 
      geom_bar(position="stack", stat="identity")+
      scale_fill_discrete(na.translate=FALSE)+
      ylab('')+xlab('')+ labs(fill = "Gillnet")+
      theme(legend.position="top")+ 
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    
    
    
    #Plot longlines
    p2=d.ll%>%
      group_by(shot,gear.type,con.code.sp)%>%
      tally()%>%
      mutate(n=ifelse(is.na(con.code.sp),0,n),)%>%
      ggplot(aes(fill=con.code.sp, y=n, x=shot)) + 
      geom_bar(position="stack", stat="identity")+
      scale_fill_discrete(na.translate=FALSE)+
      ylab('')+xlab('')+ labs(fill = "Longline")+
      theme(legend.position="top")+ 
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    
    
    #export graphs
    infographic=grid.arrange(p1,p2, nrow = 2,ncol=1,heights=c(3,3))
    annotate_figure(infographic,
                    bottom = text_grob("Shot",size = 20),
                    left = text_grob("Number of interactions",rot = 90,size = 20))
    ggsave(le.paste("TEPS/Numbers.interactions.by.gear.tiff"), width = 12,
           height = 10,compression = "lzw")
    
    
    
  }
  fn.tep.barplot(d=TEPS,
                 all.gn.shots=DATA%>%
                   filter(method=="GN")%>%
                   distinct(sheet_no)%>%
                   pull(sheet_no),
                 all.ll.shots=DATA%>%
                   filter(method=="LL")%>%
                   distinct(sheet_no)%>%
                   pull(sheet_no))
}

#check ASLs
check.ASL=TRUE
if(check.ASL)
{
  #underwater
  ASL.uw=rbind(Video.longline.interaction%>%dplyr::select(OpCode,Method,Interaction,Number,SP.group,Species,Code),
               Video.net.interaction%>%dplyr::select(OpCode,Method,Interaction,Number,SP.group,Species,Code))%>%
    mutate(Number=1,
           Method=capitalize(Method))%>%
    filter(SP.group%in%TEP.groups | Code%in%TEPS_Shark.rays)%>%
    filter(!Species=='birds feeding at surface')%>%
    left_join(TEPS.names,by="Code")%>%
    filter(Code==41131005)%>%
    mutate(sheet_no=sapply( strsplit( OpCode, "_" ), "[", 3))%>%
    dplyr::select(-c(OpCode,Colr))%>%
    mutate(Obs.plat='Underwat')
  
  #subsurface
  ASL.subs=rbind(Video.subsurface.comments%>%
                   filter(Code%in%TEPS.codes)%>%
                   mutate(Method=capitalize(tolower(Period)),
                          Number=1,
                          Interaction=ifelse(grepl('diving',comment),'diving',NA))%>%
                   dplyr::select(Method,Interaction,Code,Number,SHEET_NO),
                 Video.subsurface%>%
                   filter(Code%in%TEPS.codes)%>%
                   mutate(Number=1,
                          Method=capitalize(Period),
                          Drop.out=capitalize(tolower(Drop.out)),
                          Dropout.condition=tolower(Dropout.condition),
                          Dropout.condition=case_when(is.na(Dropout.condition)~'',
                                                      TRUE~Dropout.condition),
                          dummy=case_when(Drop.out=="Yes"~"Drop out",
                                          Drop.out=="No"~"Caught"),
                          Interaction=case_when(dummy=="Drop out"~paste(dummy," (",Dropout.condition,")",sep=''),
                                                dummy=="Caught"~dummy))%>%
                   dplyr::select(Method,Interaction,Code,Number,SHEET_NO))%>%   
    left_join(TEPS.names,by="Code")%>%
    filter(Code==41131005)%>%
    mutate(Obs.plat='SubSurf')
  
  
  #Deck 2
  ASL.deck2=rbind(Video.camera2.deck_observations%>%    
                    filter(Code%in%TEPS.codes)%>%
                    dplyr::select(Period,Genus,Species,Code,Activity,SHEET_NO,Method),
                  Video.camera2.deck%>%
                    filter(Code%in%TEPS.codes)%>%
                    mutate(Activity=ifelse(dropout=='Yes' & gaffed=="No","Drop out",
                                           ifelse(dropout=='Yes' & gaffed=="Yes","Drop out and gaffed",
                                                  NA)))%>%
                    dplyr::select(Period,Genus,Species,Code,Activity,SHEET_NO,Method))%>%
    filter(!is.na(Activity))%>%
    left_join(TEPS.names,by="Code")%>%
    filter(Code==41131005)%>%
    mutate(Obs.plat='Deck2')
  
  #Observer
  ASL.obs=rbind(d.teps,d.data)%>%
    filter(common.name=='Australian sea-lion')%>%
    mutate(Obs.plat='Observer')
  
  #combine all
  ASL.tot=rbind(ASL.obs%>%
                  rename(Activity=contact.code.to.complete)%>%
                  dplyr::select(sheet_no,Method,Activity,Name,n,Obs.plat),
                ASL.deck2%>%
                  mutate(n=1)%>%
                  rename(sheet_no=SHEET_NO)%>%
                  dplyr::select(sheet_no,Method,Activity,Name,n,Obs.plat))
  
  ASL.tot=rbind(ASL.tot,
                ASL.uw%>%
                  rename(Activity=Interaction,
                         n=Number)%>%
                  dplyr::select(sheet_no,Method,Activity,Name,n,Obs.plat))
  
  ASL.tot=rbind(ASL.tot,
                ASL.subs%>%
                  rename(sheet_no=SHEET_NO,
                         Activity=Interaction,
                         n=Number)%>%
                  dplyr::select(sheet_no,Method,Activity,Name,n,Obs.plat))%>%
    mutate(Method=ifelse(Method=='GN','Gillnet',Method))
  
  ASL.tot%>%
    group_by(sheet_no,Method,Activity ,Obs.plat)%>%
    summarise(N=sum(n))%>%
    spread(sheet_no,N,fill=0)
  
  
  ASL.tot=ASL.tot%>%
    left_join(DATA%>%
                distinct(sheet_no, mid.lat, mid.long, date,botdepth,block,zone),
              by='sheet_no')%>%
    arrange(sheet_no)%>%
    mutate(Activity=ifelse(Activity=='feeding from gillnet','Feeding',Activity))
  
  ASL.tot%>%
    ggplot(aes(mid.long,mid.lat,color=Activity))+
    geom_jitter(size=2)+
    facet_wrap(~Method)+
    theme(legend.position = 'top',
          legend.title = element_blank())
  
  library(rgdal)
  if (User == "Sarah"){
    ASL_shape.file=readOGR(handl_OneDrive("Data/Mapping/Closures/ASL_closures/ASL_Closures.shp")) #south coast only
  }
  if (User == "Abbey"){
  ASL_shape.file=readOGR(handl_OneDrive("Data/Mapping/Closures/ASL_closures/ASL_Closures.shp")) #south coast only
  }
  if (User == "Matias"){
    ASL_shape.file=readOGR(handl_OneDrive("Data/Mapping/Closures/ASL_closures/ASL_Closures.shp")) #south coast only
  }
  plot(ASL_shape.file,col="chartreuse3")
  
}






########################SJ Testing Stuff For LL##########################
## Exploratory Analysis
# test for sig difference in number of fish caught on different hook type, size, position
## Dependent var = number of fish caught per unit effort
## Independent var = hook type, size, location
## Hypothesis - The mean number of fish caught on different hooks is not significantly different 

library(car)
library(doBy)

  # Use species with highest n
  IndicatorSpecies <- DATA %>% 
    count(scientific_name, sort = TRUE) %>%
    filter(n > 80)
    
  testDATA <- DATA %>%
    filter(scientific_name %in% IndicatorSpecies$scientific_name) %>%
    dplyr::select("hooktype", "scientific_name", "sheet_no", "hooksize", "soak.time", "zone", "hooklocation", "wiretrace", "CAAB_code", "n.hooks")
  
  # Hook type
  hooktypecatch <- testDATA %>% filter(!is.na(hooktype)) %>% 
    dplyr::select("hooktype", "scientific_name", "sheet_no") %>% 
    group_by(hooktype, scientific_name) %>% 
    summarise(n = n()) 
  hooktypeeffort <- testDATA %>% filter(!is.na(hooktype)) %>%
    dplyr::select("hooktype", "scientific_name", "sheet_no", "soak.time", "n.hooks") %>% 
    group_by(hooktype, scientific_name) %>% 
    summarise(effort.hours = max(soak.time), effort.hooks = max(n.hooks))
  hooktype.catchandeffort <- left_join(hooktypecatch, hooktypeeffort, join_by("scientific_name", "hooktype")) %>% 
    group_by(scientific_name, hooktype) %>% 
    mutate(cpue=CPUE(n, effort.hours, effort.hooks)) %>% 
    ungroup()
  
ggplot(hooktype.catchandeffort, aes(hooktype, cpue)) +
  geom_point() +
  facet_grid(. ~ scientific_name) +
  xlab("Hook Type") + ylab("CPUE") +
  ggtitle("Effect of hook type on mean catch of indicator species")
ggsave("hooktype.png")

# Hook size  
hooksizecatch <- testDATA %>% filter(!is.na(hooksize)) %>% 
  dplyr::select("hooksize", "scientific_name", "sheet_no") %>% 
  group_by(hooksize, scientific_name) %>% 
  summarise(n = n()) 
hooksizeeffort <- testDATA %>% filter(!is.na(hooksize)) %>%
  dplyr::select("hooksize", "scientific_name", "sheet_no", "soak.time", "n.hooks") %>% 
  group_by(hooksize, scientific_name) %>% 
  summarise(effort.hours = max(soak.time), effort.hooks = max(n.hooks))
hooksize.catchandeffort <- left_join(hooksizecatch, hooksizeeffort, join_by("scientific_name", "hooksize")) %>% 
  group_by(scientific_name, hooksize) %>% 
  mutate(cpue=CPUE(n, effort.hours, effort.hooks)) %>% 
  ungroup()

ggplot(hooksize.catchandeffort, aes(hooksize, cpue)) +
  geom_point() +
  facet_grid(. ~ scientific_name) +
  xlab("Hook Size") + ylab("CPUE") +
  ggtitle("Effect of hook size on mean catch of indicator species")
ggsave("hooksize.png")

# Snood type
wiretracecatch <- testDATA %>% filter(!is.na(wiretrace)) %>% 
  dplyr::select("wiretrace", "scientific_name", "sheet_no") %>% 
  group_by(wiretrace, scientific_name) %>% 
  summarise(n = n()) 
wiretraceeffort <- testDATA %>% filter(!is.na(wiretrace)) %>%
  dplyr::select("wiretrace", "scientific_name", "sheet_no", "soak.time", "n.hooks") %>% 
  group_by(wiretrace, scientific_name) %>% 
  summarise(effort.hours = max(soak.time), effort.hooks = max(n.hooks))
wiretrace.catchandeffort <- left_join(wiretracecatch, wiretraceeffort, join_by("scientific_name", "wiretrace")) %>% 
  group_by(scientific_name, wiretrace) %>% 
  mutate(cpue=CPUE(n, effort.hours, effort.hooks)) %>% 
  ungroup()

ggplot(wiretrace.catchandeffort, aes(wiretrace, cpue)) +
  geom_point() +
  facet_grid(. ~ scientific_name) +
  xlab("Hook Size") + ylab("CPUE") +
  ggtitle("Effect of snood type on mean catch of indicator species")
ggsave("snoodtype.png")

# Hook Distance to weight or float
hookdist <- Video.camera2.deck %>% filter(!is.na(`hook distance to float/weight`)) %>% 
  filter(taxa %in% IndicatorSpecies$scientific_name) %>%
  dplyr::select("hook distance to float/weight", "taxa") %>% 
  group_by(`hook distance to float/weight`, taxa) %>%
  mutate(n = n()) %>%
  ungroup()

ggplot(hookdist) +
  geom_col(aes(x = factor(`hook distance to float/weight`, levels = c("1w", "2w", "3w", "3f", "2f", "1f")),y = `n`)) +
  facet_grid(. ~ taxa) +
  xlab("Relative Distance to Weight or Float") + ylab("n") +
  ggtitle("Hook distance to weight or float")
ggsave("hookdist.png")


# Multivariate
multivar <- Video.camera2.deck %>% filter(!is.na(`hook distance to float/weight`)) %>% 
  filter(taxa %in% IndicatorSpecies$scientific_name)

# example
birds<-read.csv('https://raw.githubusercontent.com/collnell/lab-demo/master/bird_by_fg.csv')

#################################################George############################################
## Underwater Caught by species
### Run on data from underwater_clean_loop
# We want to filter data where the interaction is "Caught-Gilled/hooked" OR "Caught while predating" for both Video.longline.interaction AND Video.net.interaction
# And then we want to make a table of how many of each species we have caught

write.csv(Video.net.interaction %>% filter(Interaction == "Caught-Gilled/hooked"|Interaction == "Caught while predating") %>% 
  {table(.$taxa, .$Interaction)}, "Gillnet.n.for.George.csv")

write.csv(Video.longline.interaction %>% filter(Interaction == "Caught-Gilled/hooked"|Interaction == "Caught while predating") %>% 
  {table(.$taxa, .$Interaction)}, "Longline.n.for.George.csv")  
