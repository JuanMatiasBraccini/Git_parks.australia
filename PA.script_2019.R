                # SCRIPT FOR ANALYSING AND REPORTING PARKS AUSTRALIA PROJECT 2019

# Note: Upload of video data done by PA.script_shared.R see #5. PA - underwater video

# Missing: 
#   updated Habitat using Santiago's Video.habitat see #----Analyse PA Habitat --
#   Review calculations with Jack '#4. Rates of depredation, bait loss and drop outs'
#   Fish depredation. Calculate also per km gn hours and per caught fish. Same for other behaviours
#   Depredation codes from Underwater video: Codes 10 Predated on & 12 Caught while predating
#      There is also depredation data from Cameras 1 & 2 in the comments section (videos reviewed by Sarah and anyone else??)

#Consider this for depredation:
      # Indices used to quantify depredation and bycatch levels
      # We used the NOP dataset to quantify depredation based on three of the indices defined by
      # Rabearisoa et al. (2018): the interaction rate (IR), the gross depredation rate (GDR) and the depredation per unit effort (DPUE). 
      # The IR was used to assess how often depredation occurred. It was calculated as the proportion (%) of
      # sets on which catches were depredated (at least one depredated fish) out of all sets: 
      #   IR =100* nb of depredated sets /total nb of sets (where nb is number)
      # The GDR was used to assess the amplitude of fish damaged by depredation. It was calculated as the
      # proportion (%) of fish depredated out of the total number of fishes caught (whether depredated or not): 
      #   GDR =100* nb of depredated catch/ total nb of catch
      # 
      # When depredation occurred on a given set, the DPUE indicated the amount of fish depredated as a
      # function of the fishing effort expressed as the number of hooks deployed. For each depredated set i, it
      # was calculated as the number of fishes depredated for every 1,000 hooks deployed:
      #   DPUEi =1000 * depredated catchi / hooksi
      # 
      # Rabearisoa, Njaratiana, Philippe S. Sabarros, Evgeny V. Romanov, Vincent Lucas, and Pascal Bach.
      # 2018. “Toothed Whale and Shark Depredation Indicators: A Case Study from the Reunion Island
      # and Seychelles Pelagic Longline Fisheries” edited by H. M. Patterson. PLOS ONE
      # 13(8):e0202037

rm(list=ls(all=TRUE))

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
library(tictoc)
library(patchwork)

options(stringsAsFactors = FALSE,dplyr.summarise.inform = FALSE) 

#--------- DATA ------------
User="Matias"
# User="Sarah"
# User="Abbey"

if(!exists('handl_OneDrive'))
{
  if(User=="Matias") source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')
  if(User=="Sarah")  handl_OneDrive=function(x)paste('C:/Users',Usr,'OneDrive - Department of Primary Industries and Regional Development/Matias',x,sep='/')
  if(User=="Abbey")  handl_OneDrive=function(x)paste('C:/Users',Usr,'OneDrive - Department of Primary Industries and Regional Development/Matias',x,sep='/')
}

if(User=="Matias") source(handl_OneDrive("Analyses/Population dynamics/Git_Stock.assessments/NextGeneration.R"))

#1. Sharks data base
if(User=="Matias") source(handl_OneDrive('Analyses/SOURCE_SCRIPTS/Git_other/Source_Shark_bio.R'))
if(User=="Sarah") source(handl_OneDrive('Analyses/SOURCE_SCRIPTS/Git_other/Source_Shark_bio.R'))
if(User=="Abbey") source(handl_OneDrive('Analyses/SOURCE_SCRIPTS/Git_other/Source_Shark_bio.R'))

User="Matias"
# User="Sarah"
# User="Abbey"

#2. Species list
All.species.names=read.csv(handl_OneDrive("Data/Species.code.csv"),stringsAsFactors=FALSE, fileEncoding="latin1")
CAAB.gillnet.txt=read.delim('M:/Production Databases/Shark/ParksAustralia_2019/CAAB gillnet.txt', header = TRUE, sep = "\t", dec = ".")


#3. PA - TEPS interactions recorded by Observers
#setwd('M:/Agency Data/Draft Publications/Braccini/2019-20_Parks Australia Project/Fieldwork/Data')
#setwd("//fish.wa.gov.au/Data/Production Databases/Shark/ParksAustralia_2019") 
setwd("M:/Production Databases/Shark/ParksAustralia_2019/Data") 
TEPS <- read_excel("TEPS interactions.xlsx", sheet = "Sheet1",skip = 1)

#4. PA - number of hook combinations used and lost in PA project
Hook.combos <- read_excel("Hook count.xlsx", sheet = "Sheet1",skip = 0)
Lost.snoods<- read_excel("Broken hook specs.xlsx",sheet = "Sheet1")


#5. PA - underwater video
Event.Mes.data.dump='Abbey_Sarah'
#Event.Mes.data.dump='Jack'
if(Event.Mes.data.dump=='Jack')
{
  if(User=="Matias") setwd(handl_OneDrive("Parks Australia/2019_project/Data/cameras"))
  if(User=="Sarah") setwd(handl_OneDrive("Parks Australia/2019_project/Data/cameras"))
  if(User=="Abbey")  setwd(handl_OneDrive("Parks Australia/2019_project/Data/cameras"))
  
  #net
  file.name.GN="Gillnet_Data_2_12_2021_Clean.xlsx"
  Video.net.interaction <- read_excel(file.name.GN, sheet = "Interactions")
  Video.net.maxN <- read_excel(file.name.GN, sheet = "MaxN")
  Video.net.obs <- read_excel(file.name.GN, sheet = "Observation") 
  
  #longline
  file.name.LL="Longline_Data_1_12_2021_Clean.xlsx"
  Video.longline.interaction <- read_excel(file.name.LL, sheet = "Interactions")
  Video.longline.maxN <- read_excel(file.name.LL, sheet = "MaxN")
  Video.longline.obs <- read_excel(file.name.LL, sheet = "Observations")
  
  #habitat 
  #note: CATAMI code used to determine habitat types
  file.name.habitat="Gillnet_longline habitat.xlsx"
  Video.habitat<- read_excel(file.name.habitat, sheet = "gillnet habitat")
  Video.habitat.LL<- read_excel(file.name.habitat, sheet = "longline habitat")
  
  
  #6. PA - deck camera 1 (points to measuring board)
  file.name.habitat.deck="Deck 1 habitat and fish_6_12_2021.xlsx"
  Video.habitat.deck<- read_excel(file.name.habitat.deck, sheet = "Habitat")
  Video.camera1.deck<- read_excel(file.name.habitat.deck, sheet = "Deck 1 fish landed")
  Video.camera1.deck_extra.records<- read_excel(file.name.habitat.deck, sheet = "extra records")
  
  
  #7. PA - deck camera 2 (points to roller)
  file.name.camera2.deck="Deck 2_6_12_2021.xlsx"
  Video.camera2.deck<- read_excel(file.name.camera2.deck, sheet = "Deck 2")
  Video.camera2.deck_observations<- read_excel(file.name.camera2.deck, sheet = "Other observations")
  
  
  #8. PA - subsurface camera   
  file.name.subsurface="SubSurface_7_12_2021.xlsx"
  Video.subsurface<- read_excel(file.name.subsurface, sheet = "ALL Dot Point Measurements")
  Video.subsurface.comments<- read_excel(file.name.subsurface, sheet = "comment")

}
if(Event.Mes.data.dump=='Abbey') 
{
  #1. read in data
  setwd('M:/Agency Data/Draft Publications/Braccini/2019-20_Parks Australia Project/Fieldwork/Data/Gillnet EM output')
  filenames=list.files(pattern='*.csv')
  #dummy <- lapply(filenames, read.csv,skip=4)
  
  dummy=vector('list',length(filenames))
  for(i in 264:length(filenames)) dummy[[i]]=read.csv(filenames[i],skip=4)
  
  
  #2. put data in standard format
  for(i in i:length(dummy))
  {
    colnames(dummy[[i]])=tolower(colnames(dummy[[i]]))
  #  dummy[[i]]=
  #    maxn interaction escape 
  }
    
  #Video.net.interaction 
  #Video.net.maxN 
  #Video.net.obs
  
}
if(Event.Mes.data.dump=='Abbey_Sarah') #takes 1 min to load data 
{
  tic()
  source(handl_OneDrive("Analyses/Parks Australia/Git_parks.australia/PA.script_2019_shared.R"))
  toc()
}

#6. Ports
Port.loc=read.csv(handl_OneDrive('Analyses/Parks Australia/outputs/Historic_catch_effort/Ports.csv'))


#7. Fin proportion of livewt
Shark.Fin.Price.List=read.csv(handl_OneDrive("Data/Catch and Effort/Shark Fin Price List.csv")) #provided by Rory to Eva Lai


#8. Prices
PRICES_abares=read.csv(handl_OneDrive("Analyses/Data_outs/PRICES.csv")) 
Shark.price.list=read.csv(handl_OneDrive("Analyses/Labelling/Outputs/Shark.price.list.csv")) 
PRICES=read.csv(handl_OneDrive("Analyses/Data_outs/PRICES.csv"),stringsAsFactors = F)
Restaurant.price=read.csv(handl_OneDrive("Data/Labelling/Restaurants.csv"),stringsAsFactors = F)
Scale.factor=3.3  #multiplier for restaurant sold fish from wholesale price (Trenton Brennan, Ocean & Paddock). 

#9. PA - socio-economic survey
setwd(handl_OneDrive("Parks Australia/2019_project/Data/socio-economics"))
Survey.fishers<- read_excel("Questionnaire_fisher.xlsx", sheet = "questionnaire")
Survey.fishers.metadata<- read_excel("Questionnaire_fisher.xlsx", sheet = "metadata")
Survey.processor<- read_excel("Questionnaire_processor.xlsx", sheet = "questionnaire")
Survey.processor.metadata<- read_excel("Questionnaire_processor.xlsx", sheet = "metadata")
Additional.questions<- read_excel("Additional questions.xlsx", sheet = "data")
Additional.questions.metadata<- read_excel("Additional questions.xlsx", sheet = "metadata")

#10 Total number of vessel in TDGDLF
TDGDLF.vessels=read.csv(handl_OneDrive('Analyses/Catch and effort/State of fisheries/2018-19/1.4.Number.of.vessels.csv'))

#11. WA processors
WA.processors.landings=read_excel(handl_OneDrive("Data/Processors_Matias_2019_20.xlsx"), sheet = "landings")
WA.processors=read_excel(handl_OneDrive("Data/Processors_Matias_2019_20.xlsx"), sheet = "PROL")

source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_Population.dynamics/fn.fig.R"))
Do.tiff="YES"
Do.jpeg="NO"
source(handl_OneDrive('Analyses/SOURCE_SCRIPTS/Git_other/ggplot.themes.R'))
source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_other/Smart_par.R"))
source(handl_OneDrive("Analyses/Population dynamics/Git_Stock.assessments/NextGeneration.R"))
source(handl_OneDrive("Analyses/Population dynamics/Git_Stock.assessments/SelnCurveDefinitions.R")) #These can be extended by the user


#---------CONTROL SECTION------------
explr.dat.entry=FALSE
do.len_len=FALSE
do.Historic=FALSE
do.soc.econ.papr=FALSE
do.observer.catch.comp=FALSE
do.observer.size.dist=FALSE
do.observer.cpue=FALSE
do.observer.retained.vs.discarded=FALSE
do.Hook.baiting.loss=FALSE
do.Blood.lactate=FALSE
do.Longline.selectivity=FALSE
do.general.underwater=FALSE
do.habitat=FALSE
do.Abbeys=FALSE
do.dropouts.gaffing.water.column=FALSE
do.camera.VS.observer=FALSE

approach.camera.observer.comps='Sarah'
#approach.camera.observer.comps='Original'

distance.roller.spreader.Anthony=4.3  #in metres
distance.roller.spreader.Tim=NA  #in metres
distance.roller.spreader.Nils=4  #in metres
distance.roller.spreader=mean(c(distance.roller.spreader.Anthony,distance.roller.spreader.Tim,distance.roller.spreader.Nils),na.rm=T)

#mesh.deep.Anthony=3.3                   #in metres  (20 meshes of 6.5 inch, source: Jeff Cooke)

metres.observed=5 # average metres observed by underwater cameras
hooks.observed=2   #average number of hooks observed by underwater cameras

hours.underwater.ll='1329'          #total number of hours of longline underwater footage analysed   #UPDATE ALL THESE
hours.underwater.gn='2245'         #total number of hours of gillnet underwater footage analysed  
hours.subsurface.ll='27'           #total number of hours of subsurface footage longline analysed  
hours.subsurface.gn='54'           #total number of hours of subsurface footage gillnet analysed  
hours.deck1.ll='42'               #total number of hours of deck camera 1 longline footage analysed  
hours.deck1.gn='84'               #total number of hours of deck camera 1 gillnet footage analysed  
hours.deck2.ll='41'               #total number of hours of deck camera 2 longline footage analysed  
hours.deck2.gn='82'               #total number of hours of deck camera 2 gillnet footage analysed  

No.good.water.column=c('PA0059','PA0061') # data sheets no good for looking at Composition around weight or float

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


#Socio-economics
  #Source: Rogers 2017 (summary and page 51)
#note: Rogers 2017 calculated value chain multipliers (from beach to end use) to estimate 
#      Final Sale Return of Domestic GVP (FSRDGVP) at final point of sale and consumption. 
#      The multipliers integrate processing costs, packaging,
#      storage, distribution, profits and end retail costs. The difference between  
#      traditional GVP and FSRDGVP is the value enhancement along the supply chain
FSRDGVP.multipliers=data.frame(
  Species=c('Gummy shark','Dusky shark','Whiskery shark','Bronze whaler','Sandbar shark',
            'Blue groper','Pink snapper','Hapuku','Bight redfish'),
  Multiplier=c(6.5,6.3,7.9,6.3,6.3,
               3.5,3.5,3.8,3.3)) #Table 1 Appendix 2 for Rogers 2017
Proc.fillet.retail.ton=20000     #processor retail price for the original tonne of trunks
Prop.fillet.sold.fish.chip=0.865   #Processor's proportion sold to fish & chip and retail
Prop.fillet.sold.proc.retail=0.135
Trunk.wastage=data.frame(     #calculations based on 1000 kg of trunks
    Species='Gummy shark',
    Trunk.to.fillet=700/1000,
    Trunk.to.belly.flat=60/1000)
Scalefish.to.fillet=0.35      #scalefish recovery rate from whole to fillet (Adam Soumalidis: "27 to 45% 
                                    # depending on species, 35% on average")

fillet.weight=150     #fillet weight in grams (Rogers 2017)

Trunk.multiplier=data.frame(      #All columns are in $ per kg
        Species='Gummy shark',
        Trunk.sold.as.fillet_wholesale=11550/700,             # $ per kg
        Trunk.sold.as.belly.flat_wholesale=360/60,            # $ per kg
        Trunk.sold.as.fillet_retail=Proc.fillet.retail.ton/700,                # $ per kg
        Trunk.sold.as.belly.flat_retail=(Proc.fillet.retail.ton/11550)*360/60, # $ per kg
        Trunk.sold.as.fillet_fish_chip=(700*12*1000/150),     #$ per tonne caught
        Trunk.sold.as.belly.flat_fish_chip=(60*12*1000/150))%>%  #$ per tonne caught
  mutate(Price.per.ton.to.fisher=8.5*1000,            #$ for a tonne of trunks
         Multiplier.fish.chip=(Trunk.sold.as.fillet_fish_chip+Trunk.sold.as.belly.flat_fish_chip)/Price.per.ton.to.fisher,
         Multiplier.proc.retail=Proc.fillet.retail.ton/Price.per.ton.to.fisher,
         Multiplier.overall=weighted.mean(x=c(Multiplier.fish.chip,Multiplier.proc.retail),
                                          w=c(Prop.fillet.sold.fish.chip,Prop.fillet.sold.proc.retail)))   

Scalefish.price.list=data.frame(Species=c('Pink snapper',
                                          'Hapuku','Blue groper','West Australian dhufish','Nannygai'),
                                Price.per.fillet=c(mean(c(9,12,13.5,12,18.9,15,8.5,10.3)),
                                                   20,18,24.9,21))
Other.scalefish.price.multiplier=min(FSRDGVP.multipliers$Multiplier[6:9])

Fisher.and.processor=data.frame(BoatName=c('san margo','kabralee ii','ocean mistress',
                                           'southwestern','tracey lea','gwendolyn'),
                                Fisher=c('Nick Triantafilou','Steve Buckeridge','Nick Soulos',
                                         'Brian Scimone','Jeff Cooke','Emanue Karaterpos'))

HNDL=handl_OneDrive('Analyses/Parks Australia/outputs/')
le.paste=function(x) paste(HNDL,x,sep='')

#---------Define TEPS------------
TEPS_Shark.rays=c(37008001,37010003,37035001,37035002)
TEPS_marine.mam=4.1e+07:4.115e+07
TEPS_seabirds=4.0e+07:4.09e+07
TEPS_reptiles=3.747e7:3.99e+07  #3.747e7:3.99e+07
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
not.applicable=c("Shearwaters","Gulls","Little black cormorant","Pied cormorant","Pacific gull")
TEPS.names=TEPS.names%>%filter(!Name%in%not.applicable)
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
All.species.names=All.species.names%>%
                  filter(!(COMMON_NAME=='Trevally' & Code==37337924))%>%
  filter(!(COMMON_NAME=='Leopard shark' & Code==37013006))%>%
  filter(!(COMMON_NAME=='Swallow tail' & Code==37258005))%>%
  filter(!(COMMON_NAME=='Mackerels' & Code==37441000))%>%
  filter(!(COMMON_NAME=='North west blowfish' & Code==37467007))
# All.species.names=All.species.names%>%
#   mutate(Code=ifelse(COMMON_NAME=='Scaly mackerel',37085018,Code))

#Add extra Codes used by Jack
Addis=All.species.names[1:2,]%>%
  mutate(Species=rep('',),
         COMMON_NAME=c('Whaler sharks','Stingrays'),
         SCIENTIFIC_NAME=c('Carcharhinus spp','Dasyatis spp'),
         Taxa=rep('Elasmobranch',2),
         CAES_Code=rep(NA,2),
         CAAB_code=rep(NA,2),
         Code=c(37018904,37035901))

#Add extra Codes used by Sarah
Addis2=All.species.names[1:9,]%>%
  mutate(Species=rep('',),
         COMMON_NAME=c('Gastropods','Cormorants','Crustaceans','Western rock lobster',
                       'Tunas','Stingarees','Drummers',
                       "Australian sealion",'Flathead'),
         SCIENTIFIC_NAME=c('Aplysiidae','Phalacrocoracidae','Brachyura','Panulirus cygnus',
                           'Thunnus spp.','Urolophus spp.','Girella spp.',
                           'Neophoca cinerea','Platycephalus spp.'),
         Taxa=c('Mollusc','','Crustacean','Crustacean','Teleost','Elasmobranch','Teleost','','Teleost'),
         CAES_Code=rep(NA,9),
         CAAB_code=rep(NA,9),
         Code=c(24388000,40048000,28850901,28820005,37441925,37038903,37361902,41131005,37296913))

Addis3=All.species.names[1:6,]%>%
  mutate(Species=rep('',6),
         COMMON_NAME=c('Black reef leatherjacket','Brownspotted wrasse','Surf Parrotfish',
                       'Varied carpetshark','Dusky snake blenny','Banded seaperch'),
         SCIENTIFIC_NAME=c('Eubalichthys bucephalus','Notolabrus parilus','Scarus rivulatus',
                           'Parascyllium variolatum','Ophiclinus antarcticus','Hypoplectrodes nigroruber'),
         Taxa=c(rep('Teleost',3),'Elasmobranch',rep('Teleost',2)),
         CAES_Code=rep(NA,6),
         CAAB_code=rep(NA,6),
         Code=c(37465039,37384022,37386027,
                37013004,37414008,37311037))


All.species.names=rbind(All.species.names,Addis,Addis2,Addis3)

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
  dplyr::select(colnames(All.species.names))%>%
  filter(!Code%in%unique(All.species.names$Code))

All.species.names=rbind(All.species.names,CAAB.gillnet.txt)

#fix some final issues
All.species.names=All.species.names%>%
  mutate(COMMON_NAME=case_when(COMMON_NAME=="" &Code==99999999 ~"Seabird",
                               COMMON_NAME=="" &Code==37311035 ~"Western wirrah",
                               COMMON_NAME=="" &Code==37466010 ~"Whitebarred boxfish",
                               COMMON_NAME=="" &Code==37015026 ~"Western spotted catshark",
                               COMMON_NAME=="" &Code==37469001 ~"Globefish",
                               COMMON_NAME=="" &Code==37465003 ~"Mosaic leatherjacket",
                               COMMON_NAME=="" &Code==37060006 ~"Green moray",
                               COMMON_NAME=="" &Code==37224005 ~"Largetooth beardie",
                               COMMON_NAME=="" &Code==37465035 ~"Yellowstriped leatherjacket",
                               COMMON_NAME=="" &Code==37337040 ~"Pilotfish",
                               COMMON_NAME=="" &Code==37287003 ~"Bighead gurnard perch",
                               COMMON_NAME=="" &Code==37367002 ~"Giant boarfish",
                               COMMON_NAME=="" &Code==37296037 ~"Southern bluespotted flathead",
                               COMMON_NAME=="" &Code==37336005 ~"Spearfish remora",
                               COMMON_NAME=="" &Code==37367005 ~"Blackspot boarfish",
                               COMMON_NAME=="" &Code==37287002 ~"Blackspotted gurnard perch",        
                               COMMON_NAME=="" &Code==37361012 ~"Western buffalo bream",
                               TRUE~COMMON_NAME))

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
Lactate.for.Taylor=DATA%>%
  filter(!is.na(Lactate) | grepl(paste(c("lacta",'lactate:'),collapse="|"), COMMENTS))%>%
  filter(SPECIES%in%c("TG","BW","TK","PJ") & Method%in%c('LL','GN'))
write.csv(Lactate.for.Taylor,handl_OneDrive('Students/2020_Taylor Grosse/data.for.Taylor/Lactate.for.Taylor.csv'),row.names = F)

# Observer.LL.data.historic=DATA%>%
#               filter(Method=='LL' & Mid.Lat <(-26) & !BOAT%in%c('FLIN','HAM','HOU','NAT') & year<2019)
# names(Observer.LL.data.historic)=tolower(names(Observer.LL.data.historic))

DATA=DATA[grep("PA", DATA$SHEET_NO), ]%>%
      filter(year>=2020 & year<=2021)
Tab.vert.samp=DATA%>%
  filter(VERT_SAMPL=='Yes' & !is.na(COMMON_NAME))%>%
  group_by(COMMON_NAME)%>%
  tally()%>%
  data.frame%>%
  arrange(-n)
write.csv(Tab.vert.samp,handl_OneDrive('Analyses/Parks Australia/outputs/Vetebrae_samples.csv'),row.names=F)

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
  mutate(n.hooks=as.numeric(n.hooks))%>%
  group_by(trip)%>%
  mutate(soak.time.infRD=ifelse(is.na(soak.time),round(mean(soak.time,na.rm=T)),soak.time),
         net_length.infRD=ifelse(method=='GN' & is.na(net_length),round(mean(net_length,na.rm=T)),net_length),
         n.hooks.infRD=ifelse(method=='LL' & (is.na(n.hooks)|n.hooks==0),round(mean(n.hooks,na.rm=T)),n.hooks),
         Effort.infRD=ifelse(is.na(Effort) & method=='LL',n.hooks.infRD*soak.time.infRD,
                             ifelse(is.na(Effort) & method=='GN',net_length.infRD*soak.time.infRD,
                                    Effort)))%>%
  ungroup()

DATA=DATA%>%
  mutate(net_length=ifelse( method=='GN' & is.na(net_length) & skipper=='N.STOKKE',1.8,net_length))

#PortJackson tissue samples for Jo Day
Jo.Day.samples=c(paste("PA0119",c(38,44,55,54,30,15,29,34,52,31,21,53,16,22,28),sep='-'),
                 'PA0095-02','PA0062-04','PA0045-03',"PA0119-04")
For.Jo.Day=DATA%>%
  filter(bag_no%in%Jo.Day.samples)%>%
  dplyr::select(bag_no,date,mid.lat,mid.long,botdepth,set.time,haul.time,soak.time,method,common_name,sex,fl)%>%
  mutate(fl=round(fl))%>%
  rename(Fork.length_cm=fl,
         bottom.depth.metres=botdepth)
write.csv(For.Jo.Day,handl_OneDrive('Parks Australia/2019_project/For.Jo.Day.csv'),row.names = FALSE)  


#---------Explore PA observer data for data issues------------
#note: not needed once all data have been entered
if(explr.dat.entry)
{
  setwd(handl_OneDrive("Analyses/Parks Australia/fix this"))
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



# ---------Fishing days by zone and method  -------------------------------
Fishing.days.by.zone.method=DATA%>%
                              distinct(date,zone,method)%>%
                              group_by(zone,method)%>%
                              tally()

# ---------Functions: Multivariate stats  -------------------------------
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
  adon.results<-adonis(formula(paste('Community',Show.term,sep='~')),data=d, method="bray",perm=5e3)
  write.csv(as.data.frame(adon.results$aov.tab),paste(hndl,Permanova.title,'.csv',sep=""))
  
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
sites <- st_as_sf(Port.loc%>%filter(Cityname%in%c("Albany","Augusta","Bunbury","Perth","Esperance",
                                                  "Geraldton","Eucla")),
                  coords = c("lon.port", "lat.port"), 
                  crs = 4326, agr = "constant")
Nudge <- sites %>%
  mutate(nudge_x=ifelse(Cityname%in%c("Perth"),0.65,
                        ifelse(Cityname%in%c("Augusta"),0.9,
                               ifelse(Cityname%in%c("Bunbury"),1.1,
                                      ifelse(Cityname%in%c("Geraldton"),1.2,
                                             0)))) , 
         nudge_y=ifelse(Cityname%in%c("Albany","Esperance","Augusta","Eucla"),0.25,0))
oz_states <- ozmaps::ozmap_states


#---------Manipulate prices ------------ 
PRICES=PRICES%>%
  mutate(dolar.per.kg=Beach.Price..Adjusted.,
         dolar.per.kg=as.numeric(gsub("\\$", "", dolar.per.kg)),
         SPECIES=as.numeric(ASA.Species.Code))

#---------Historic catch analyses (commercial catch and effort) ------------    
if(do.Historic)  
{
  do.monthly=TRUE  
  library(fields)
  library(gridExtra)
  library(grid)
  library(data.table)
  library(plotrix)
  source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_other/Plot.Map.R"))
  source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_other/get_lat.long.R"))
  hndl=handl_OneDrive("Analyses/Parks Australia/outputs/Historic_catch_effort")
  
  # Data
  setwd(handl_OneDrive("Analyses/Data_outs"))
  Data.daily.original=fread("Data.daily.original.csv",data.table=FALSE)
  Data.daily=fread("Data.daily.csv",data.table=FALSE)
  Effort.daily=fread("Effort.daily.csv",data.table=FALSE)
  Data.monthly=fread("Data.monthly.csv",data.table=FALSE)
  Effort.monthly=fread("Effort.monthly.csv",data.table=FALSE)
  
  Licence.holders=read.csv(handl_OneDrive('Data/Boat.names.licence.holders_2021.csv'))

  List.of.species=Data.daily%>%distinct(SPECIES,SNAME)%>%arrange(SPECIES)
  TARGETS=list(whiskery=17003,gummy=17001,dusky=18003,sandbar=18007) 
  Scalefish.species=188000:599001
  Current.yr="2019-20" 
  
  # Power analysis
  do.power=FALSE
  if(do.power)
  {
    seq.lat=c(-26.83, -26.67, -26.50, -26.33, -26.17, -26.00,
              -27.83, -27.67, -27.50, -27.33, -27.17, -27.00,
              -28.83, -28.67, -28.50, -28.33, -28.17, -28.00,
              -29.83, -29.67, -29.50, -29.33, -29.17, -29.00,
              -30.83, -30.67, -30.50, -30.33, -30.17, -30.00,
              -31.83, -31.67, -31.50, -31.33, -31.17, -31.00,
              -32.83, -32.67, -32.50, -32.33, -32.17, -32.00,
              -33.83, -33.67, -33.50, -33.33, -33.17, -33.00,
              -34.83, -34.67, -34.50, -34.33, -34.17, -34.00,
              -35.83, -35.67, -35.50, -35.33, -35.17, -35.00)
    seq.lon=c(113.83, 113.67, 113.50, 113.33, 113.17, 113.00,
              114.83, 114.67, 114.50, 114.33, 114.17, 114.00,
              115.83, 115.67, 115.50, 115.33, 115.17, 115.00,
              116.83, 116.67, 116.50, 116.33, 116.17, 116.00,
              117.83, 117.67, 117.50, 117.33, 117.17, 117.00,
              118.83, 118.67, 118.50, 118.33, 118.17, 118.00,
              119.83, 119.67, 119.50, 119.33, 119.17, 119.00,
              120.83, 120.67, 120.50, 120.33, 120.17, 120.00,
              121.83, 121.67, 121.50, 121.33, 121.17, 121.00,
              122.83, 122.67, 122.50, 122.33, 122.17, 122.00,
              123.83, 123.67, 123.50, 123.33, 123.17, 123.00,
              124.83, 124.67, 124.50, 124.33, 124.17, 124.00,
              125.83, 125.67, 125.50, 125.33, 125.17, 125.00,
              126.83, 126.67, 126.50, 126.33, 126.17, 126.00,
              127.83, 127.67, 127.50, 127.33, 127.17, 127.00,
              128.83, 128.67, 128.50, 128.33, 128.17, 128.00,
              129.83, 129.67, 129.50, 129.33, 129.17, 129.00)
    
    fn.scale=function(x,MX,scaler) ((x/MX)^0.5)*scaler
    fn.plt=function(dd,Main,titl,MAX)
    {
      with(dd,
           {
             plot(LONG,LAT,pch=19,ylim=Ylim,xlim=Xlim,col="steelblue",cex=fn.scale(mean.cpue,MAX,2.5),main=Main)
             legend("topright",paste(round(MAX,2)),bty='n',cex=1.25,
                    pt.cex=fn.scale(MAX,MAX,2),pch=19,col="steelblue",title=titl)
           })
    }
    fn.img.plt=function(dd,TITL)
    {
      seq.lat1=subset(seq.lat,seq.lat<=Ylim[2] & seq.lat>=Ylim[1])
      seq.lon1=subset(seq.lon,seq.lon<=Xlim[2] & seq.lon>=Xlim[1])
      misn.lat=seq.lat1[which(!seq.lat1%in%unique(dd$lat10.corner))]
      
      misn.lon=seq.lon1[which(!seq.lon1%in%unique(dd$long10.corner))]
      if(length(misn.lat)>0 | length(misn.lon)>0)
      {
        combo=expand.grid(lat10.corner=seq.lat1, long10.corner=seq.lon1)
        dd=combo%>%left_join(dd,by=c("lat10.corner","long10.corner"))
      }
      dd=dd%>%arrange(long10.corner)
      dd=dd[,-match('block10',names(dd))]  
      dd=dd%>%
        spread(lat10.corner,mean.cpue)
      Lon=as.numeric(dd$long10.corner)
      dd=as.matrix(dd[,-1]) 
      LaT=as.numeric(colnames(dd))
      brk<- quantile( c(dd),probs=seq(0,1,.2),na.rm=T)
      YLim=Ylim
      XLim=Xlim
      YLim[1]=YLim[1]-0.5
      YLim[2]=YLim[2]+0.5
      XLim[1]=XLim[1]-0.5
      XLim[2]=XLim[2]+0.5
      image.plot(Lon,LaT,dd, breaks=brk, col=rev(heat.colors(length(brk)-1)), 
                 lab.breaks=names(brk),ylim=YLim,xlim=XLim,ylab="",xlab="")
      legend('topright',TITL,bty='n',cex=.9)
      
    }
    fn.parks.power=function(ktch,efF,do.what)
    {
      ktch=ktch%>%filter(METHOD%in%c('GN','LL') & Estuary=="NO")%>%
        group_by(Same.return.SNo,METHOD,day,FINYEAR,MONTH,BLOCKX,block10,LAT,LONG) %>%
        summarise(LIVEWT.c=sum(LIVEWT.c))%>%data.frame
      efF=efF%>%dplyr::select(-c(block10,LAT,LONG))%>%
        mutate(hook.days=hooks,
               hook.hours=hooks*hours.c)
      d=left_join(ktch,efF,by='Same.return.SNo')%>%
        dplyr::select(Same.return.SNo,METHOD,day,FINYEAR,MONTH,BLOCKX,block10,LAT,LONG,LIVEWT.c,
                      hours.c,shots.c,netlen.c,hooks,Km.Gillnet.Hours.c,hook.hours)%>%
        mutate(Hundred.m.Gillnet.Hours.c=Km.Gillnet.Hours.c*10,
               Hundred.hook.hours=hook.hours/100,
               cpue.hour=ifelse(METHOD=="GN",LIVEWT.c/Hundred.m.Gillnet.Hours.c,   
                                ifelse(METHOD=="LL",LIVEWT.c/Hundred.hook.hours,NA)))%>%
        filter(!cpue.hour=="Inf")
      Ylim=floor(range(d$LAT))
      Xlim=floor(range(d$LONG))
      
      #Overal boxplots and histograms
      d=d%>%group_by(METHOD)%>%mutate(n=n()/nrow(d))  #add data weight
      p=vector('list',5)
      p[[1]]=ggplot(d,aes(x=METHOD,y=cpue.hour))+
        geom_boxplot(varwidth = T,fill="grey60")+ facet_wrap(~ MONTH)+
        coord_cartesian(ylim=c(0, quantile(d$cpue.hour,.99)))+
        ylab("cpue (100 gillnet metres / number of hooks per hour)")
      p[[2]]=ggplot(d,aes(fill=METHOD,x=cpue.hour))+
        geom_density(,col=NA,alpha=.35)+ facet_wrap(~ MONTH)+
        coord_cartesian(ylim=c(0, 5),xlim=c(0,quantile(d$cpue.hour,.99)))+
        xlab("cpue (100 gillnet metres / number of hooks per hour)")
      p[[3]]=ggplot(d,aes(x=METHOD,y=cpue.hour))+
        geom_violin(fill="grey60")+ facet_wrap(~ MONTH)+
        coord_cartesian(ylim=c(0, quantile(d$cpue.hour,.99)))+
        ylab("cpue (100 gillnet metres / number of hooks per hour)")
      
      d.block=d%>%group_by(BLOCKX)%>%
        mutate(n=n()/nrow(d))%>%
        filter(n>0.01)%>%
        data.frame
      p[[4]]=ggplot(d.block,aes(x=METHOD,y=cpue.hour))+
        geom_boxplot(varwidth = T,fill="grey60")+ facet_wrap(~ BLOCKX)+
        coord_cartesian(ylim=c(0, quantile(d$cpue.hour,.999)))+
        ylab("cpue (100 gillnet metres / number of hooks per hour)")
      # p[[5]]=ggplot(d.block,aes(fill=METHOD,x=cpue.hour))+
      #   geom_density(,col=NA,alpha=.35)+ facet_wrap(~ BLOCKX)+
      #   coord_cartesian(ylim=c(0, 5),xlim=c(0,quantile(d$cpue.hour,.99)))+
      #   xlab("cpue (100 gillnet metres / number of hooks per hour)")
      p[[5]]=ggplot(d.block,aes(x=METHOD,y=cpue.hour))+
        geom_violin(fill="grey60")+ facet_wrap(~ BLOCKX)+
        coord_cartesian(ylim=c(0, quantile(d$cpue.hour,.999)))+
        ylab("cpue (100 gillnet metres / number of hooks per hour)")
      print(p)
      
      #Spatial
      #LL vs GN, all years combined
      par(mfcol=c(2,1),mar=c(1,1,2,1),oma=c(2,2,.1,.1),mgp=c(1,.45,0))
      if(do.what=="bubbles")
      {
        agg=d%>%group_by(METHOD,LAT,LONG)%>%summarise(mean.cpue=mean(cpue.hour))%>%data.frame
        fn.plt(dd=subset(agg,METHOD=="GN"),Main="All daily gillnet records",
               titl="kg per 100m gn hour",MAX=max(subset(agg,METHOD=="GN")$mean.cpue))
        fn.plt(dd=subset(agg,METHOD=="LL"),Main="All daily longline records",
               titl="kg per 100 hook hour",MAX=max(subset(agg,METHOD=="LL")$mean.cpue))
      }
      if(do.what=="image")
      {
        agg=d%>%group_by(METHOD,block10)%>%summarise(mean.cpue=mean(cpue.hour))%>%
          mutate(lat10.corner=round(-(abs(as.numeric(substr(block10,1,2))+10*(as.numeric(substr(block10,3,3)))/60)),2),
                 long10.corner=round(100+as.numeric(substr(block10,4,5))+10*(as.numeric(substr(block10,6,6)))/60,2))%>%
          data.frame
        fn.img.plt(dd=agg%>%filter(METHOD=="GN")%>%dplyr::select(-METHOD),TITL="All daily gillnet records")
        fn.img.plt(dd=agg%>%filter(METHOD=="LL")%>%dplyr::select(-METHOD),TITL="All daily longline records")
      }
      
      #LL vs GN, by month
      mnth=1:12
      par(mfcol=c(4,3),mar=c(1,2,2,2),oma=c(2,1,.1,1),mgp=c(1,.45,0))
      if(do.what=="bubbles")
      {
        agg=d%>%group_by(METHOD,MONTH,LAT,LONG)%>%summarise(mean.cpue=mean(cpue.hour))%>%data.frame
        for(m in mnth) 
        {
          if(nrow(subset(agg,METHOD=="GN" & MONTH==mnth[m]))==0) plot.new() else
            fn.plt(dd=subset(agg,METHOD=="GN" & MONTH==mnth[m]),Main=paste("Gillnet, Month=",mnth[m]),
                   titl="kg per 100m gn hour",MAX=quantile(subset(agg,METHOD=="GN")$mean.cpue,.99))
        }
        for(m in mnth) 
        {
          if(nrow(subset(agg,METHOD=="LL" & MONTH==mnth[m]))==0) plot.new() else
            fn.plt(dd=subset(agg,METHOD=="LL" & MONTH==mnth[m]),Main=paste("Longline, Month=",mnth[m]),
                   titl="kg per 100 hook hour",MAX=quantile(subset(agg,METHOD=="LL")$mean.cpue,.99))
        }
      }
      if(do.what=="image")
      {
        agg=d%>%group_by(METHOD,MONTH,block10)%>%summarise(mean.cpue=mean(cpue.hour))%>%
          mutate(lat10.corner=round(-(abs(as.numeric(substr(block10,1,2))+10*(as.numeric(substr(block10,3,3)))/60)),2),
                 long10.corner=round(100+as.numeric(substr(block10,4,5))+10*(as.numeric(substr(block10,6,6)))/60,2))%>%
          data.frame
        for(m in mnth)
        {
          if(nrow(subset(agg,METHOD=="GN" & MONTH==mnth[m]))<=1) plot.new() else
          {
            dd1=agg%>%filter(METHOD=="GN"& MONTH==mnth[m])%>%dplyr::select(-c(METHOD,MONTH))
            fn.img.plt(dd=dd1,TITL=paste("Gillnet, Month=",mnth[m]))
          }
        }
        for(m in mnth)
        {
          if(nrow(subset(agg,METHOD=="LL" & MONTH==mnth[m]))<=1) plot.new() else
          {
            dd1=agg%>%filter(METHOD=="LL"& MONTH==mnth[m])%>%dplyr::select(-c(METHOD,MONTH))
            fn.img.plt(dd=dd1,TITL=paste("Longline, Month=",mnth[m]))
          }
        }
      }
      
      #LL vs GN, by year
      yr=sort(unique(d$FINYEAR))
      par(mfcol=c(4,3),mar=c(1,2,2,2),oma=c(2,1,.1,1),mgp=c(1,.45,0))
      if(do.what=="bubbles")
      {
        agg=d%>%group_by(METHOD,FINYEAR,LAT,LONG)%>%summarise(mean.cpue=mean(cpue.hour))%>%data.frame   
        for(m in 1:length(yr)) 
        {
          if(nrow(subset(agg,METHOD=="GN" & FINYEAR==yr[m]))==0) plot.new() else
            fn.plt(dd=subset(agg,METHOD=="GN" & FINYEAR==yr[m]),Main=paste("Gillnet",yr[m]),
                   titl="kg per 100m gn hour",MAX=quantile(subset(agg,METHOD=="GN")$mean.cpue,.99))
        }
        for(m in 1:length(yr))
        {
          if(nrow(subset(agg,METHOD=="LL" & FINYEAR==yr[m]))==0) plot.new() else
            fn.plt(dd=subset(agg,METHOD=="LL" & FINYEAR==yr[m]),Main=paste("Longline",yr[m]),
                   titl="kg per 100 hook hour",MAX=quantile(subset(agg,METHOD=="LL")$mean.cpue,.99))
        }
      }
      if(do.what=="image")
      {
        agg=d%>%group_by(METHOD,FINYEAR,block10)%>%summarise(mean.cpue=mean(cpue.hour))%>%
          mutate(lat10.corner=round(-(abs(as.numeric(substr(block10,1,2))+10*(as.numeric(substr(block10,3,3)))/60)),2),
                 long10.corner=round(100+as.numeric(substr(block10,4,5))+10*(as.numeric(substr(block10,6,6)))/60,2))%>%
          data.frame
        for(m in 1:length(yr))
        {
          if(nrow(subset(agg,METHOD=="GN" & FINYEAR==yr[m]))<=1) plot.new() else
          {
            dd1=agg%>%filter(METHOD=="GN"& FINYEAR==yr[m])%>%dplyr::select(-c(METHOD,FINYEAR))
            fn.img.plt(dd=dd1,TITL=paste("Gillnet, Finyear=",yr[m]))
          }
        }
        for(m in 1:length(yr))
        {
          if(nrow(subset(agg,METHOD=="LL" & FINYEAR==yr[m]))<=1) plot.new() else
          {
            dd1=agg%>%filter(METHOD=="LL"& FINYEAR==yr[m])%>%dplyr::select(-c(METHOD,FINYEAR))
            fn.img.plt(dd=dd1,TITL=paste("Longline, Finyear=",yr[m]))
          }
        }
        
      }
      
      topmonth.GN=d%>%filter(METHOD=="GN")%>%
        group_by(MONTH)%>%
        summarise(mean.cpue=mean(cpue.hour))%>%
        top_n(2,mean.cpue)%>%
        dplyr::select(MONTH)%>%data.frame
      
      topmonth.LL=d%>%filter(METHOD=="LL")%>%
        group_by(MONTH)%>%
        summarise(mean.cpue=mean(cpue.hour))%>%
        top_n(2,mean.cpue)%>%
        dplyr::select(MONTH)%>%data.frame
      
      #GN
      a=d%>%filter(MONTH%in%topmonth.GN$MONTH & METHOD=="GN")
      a=sort(table(a$block10))
      a=names(a[a>=quantile(a,.975)])
      b=d%>%filter(MONTH%in%topmonth.GN$MONTH & METHOD=="GN" & block10%in%a)
      par(mfcol=c(1,1),mar=c(2,2,2,2),oma=c(2,1,.1,1),mgp=c(1,.45,0))
      boxplot(b$cpue.hour~as.factor(b$block10),col=2,main=paste("Gillnet, month=",paste(topmonth.GN$MONTH,collapse="&"),sep=""),
              las=2,ylab="cpue",xlab='')
      unik.bl=unique(b$block10)
      smart.par(length(unik.bl),c(1,2,2,2),c(2,1,.1,1),c(1,.45,0))
      for(u in 1:length(unik.bl))with(subset(b,block10==unik.bl[u]),hist(cpue.hour,col=2,xlim=c(0,max(b$cpue.hour)),
                                                                         ylab="",cex.main=0.9,main=paste("month=",paste(topmonth.GN$MONTH,collapse="&"),", block=",unik.bl[u],sep="")))
      
      #LL
      par(mfcol=c(1,1),mar=c(2,2,2,2),oma=c(2,1,.1,1),mgp=c(1,.45,0))
      a=d%>%filter(MONTH%in%topmonth.LL$MONTH & METHOD=="LL")
      a=sort(table(a$block10))
      a=names(a[a>=quantile(a,probs=c(.90))])
      b=d%>%filter(MONTH%in%topmonth.LL$MONTH & METHOD=="LL" & block10%in%a)
      boxplot(b$cpue.hour~as.factor(b$block10),col=2,main=paste("Longline, month=",paste(topmonth.LL$MONTH,collapse="&"),sep=""),
              las=2,ylab="cpue",xlab='')
      unik.bl=unique(b$block10)
      smart.par(length(unik.bl),c(1,2,2,2),c(2,1,.1,1),c(1,.45,0))
      for(u in 1:length(unik.bl))with(subset(b,block10==unik.bl[u]),hist(cpue.hour,col=2,xlim=c(0,max(b$cpue.hour)),
                                                                         ylab="",cex.main=0.9,main=paste("month=",paste(topmonth.LL$MONTH,collapse="&"),", block=",unik.bl[u],sep="")))
      
    }
    
    for(i in 1:length(TARGETS))
    {
      pdf(file=paste(hndl,"/Power.analysis/",names(TARGETS)[i],".pdf",sep=""))
      fn.parks.power(ktch=subset(Data.daily,SPECIES%in%TARGETS[[i]]),
                     efF=Effort.daily%>%distinct(Same.return.SNo,.keep_all=T),
                     do.what="image")
      dev.off()
    }
    
  }

  TDGDLF.lat.range=c(-26,-40)

  # Some manipulations
  Data.monthly=Data.monthly%>%
    filter(TYPE.DATA=='monthly')%>%
    filter(LAT<=TDGDLF.lat.range[1] & LAT >=TDGDLF.lat.range[2] & METHOD%in%c("GN","LL") &
             Estuary=="NO")%>%
    mutate(YEAR=as.numeric(substr(FINYEAR,1,4)),
           SNAME=tolower(SNAME),
           SNAME=case_when(SPECIES== 10001~ 'shortfin mako shark',
                           SPECIES== 13000~ 'wobbegong sharks',
                           SPECIES== 17001~ 'gummy shark',
                           SPECIES== 17003~ 'whiskery shark',
                           SPECIES== 17008~ 'school shark',
                           SPECIES== 18001~ 'copper shark',
                           SPECIES== 18003~ 'dusky shark',
                           SPECIES== 18007~ 'sandbar shark',
                           SPECIES== 18021~ 'bull shark',
                           SPECIES== 18023~ 'spinner shark',
                           SPECIES== 18022~ 'tiger shark',
                           SPECIES== 19000~ 'hammerheads',
                           SPECIES== 20000~ 'spurdogs',
                           SPECIES== 22999~ 'Other sharks',
                           SPECIES== 23002~ 'common saw shark',
                           SPECIES== 31000~ 'skates and rays',
                           SPECIES== 258004~ 'bight redfish',
                           SPECIES== 311100~ 'breaksea cod',
                           SPECIES== 311060~ 'grey banded cod',
                           SPECIES== 311152~ '8-bar cod',
                           SPECIES== 320000~ 'West Australian dhufish',
                           SPECIES== 337007~ 'samsonfish',
                           SPECIES== 335001~ 'cobia',
                           SPECIES== 344002~ 'western Australian salmon',
                           SPECIES== 346002~ 'goldband snapper',
                           SPECIES== 346914~ 'ruby snapper',
                           SPECIES== 350000~ 'sweetlip',
                           SPECIES== 351006~ 'blue-lined emperor',
                           SPECIES== 351009~ 'sweetlip emperor',
                           SPECIES== 353001~ 'pink snapper',
                           SPECIES== 353901~ 'mixed breams',
                           SPECIES== 353998~ 'silver bream',
                           SPECIES== 363001~ 'butterfish',
                           SPECIES== 367000~ 'boarfishes',
                           SPECIES== 367003~ 'long-snouted boarfish',
                           SPECIES== 377004~ 'blue morwong',
                           SPECIES== 384002~ 'western blue groper',
                           SPECIES== 384999~ 'baldchin groper',
                           SPECIES== 441018~ 'grey mackerel',
                           SPECIES== 441025~ 'shark mackerel',
                           SPECIES== 599000~ 'Other scalefish',
                           SPECIES== 702003~ 'blue manna',
                           SPECIES== 702009~ 'champagne crab',
                           TRUE~SNAME),
           SNAME=capitalize(SNAME)) 

  Data.daily=Data.daily%>%
        filter(LAT<=TDGDLF.lat.range[1] & LAT >=TDGDLF.lat.range[2] & METHOD%in%c("GN","LL") &
                 Estuary=="NO")%>%
        mutate(YEAR=as.numeric(substr(FINYEAR,1,4)),
               SNAME=tolower(SNAME),
               SNAME=case_when(SPECIES== 10001~ 'shortfin mako shark',
                               SPECIES== 13000~ 'wobbegong sharks',
                               SPECIES== 17001~ 'gummy shark',
                               SPECIES== 17003~ 'whiskery shark',
                               SPECIES== 17008~ 'school shark',
                               SPECIES== 18001~ 'copper shark',
                               SPECIES== 18003~ 'dusky shark',
                               SPECIES== 18007~ 'sandbar shark',
                               SPECIES== 18021~ 'bull shark',
                               SPECIES== 18023~ 'spinner shark',
                               SPECIES== 18022~ 'tiger shark',
                               SPECIES== 19000~ 'hammerheads',
                               SPECIES== 20000~ 'spurdogs',
                               SPECIES== 22999~ 'Other sharks',
                               SPECIES== 23002~ 'common saw shark',
                               SPECIES== 31000~ 'skates and rays',
                               SPECIES== 258004~ 'bight redfish',
                               SPECIES== 311100~ 'breaksea cod',
                               SPECIES== 311060~ 'grey banded cod',
                               SPECIES== 311152~ '8-bar cod',
                               SPECIES== 320000~ 'West Australian dhufish',
                               SPECIES== 337007~ 'samsonfish',
                               SPECIES== 335001~ 'cobia',
                               SPECIES== 344002~ 'western Australian salmon',
                               SPECIES== 346002~ 'goldband snapper',
                               SPECIES== 346914~ 'ruby snapper',
                               SPECIES== 350000~ 'sweetlip',
                               SPECIES== 351006~ 'blue-lined emperor',
                               SPECIES== 351009~ 'sweetlip emperor',
                               SPECIES== 353001~ 'pink snapper',
                               SPECIES== 353901~ 'mixed breams',
                               SPECIES== 353998~ 'silver bream',
                               SPECIES== 363001~ 'butterfish',
                               SPECIES== 367000~ 'boarfishes',
                               SPECIES== 367003~ 'long-snouted boarfish',
                               SPECIES== 377004~ 'blue morwong',
                               SPECIES== 384002~ 'western blue groper',
                               SPECIES== 384999~ 'baldchin groper',
                               SPECIES== 441018~ 'grey mackerel',
                               SPECIES== 441025~ 'shark mackerel',
                               SPECIES== 599000~ 'Other scalefish',
                               SPECIES== 702003~ 'blue manna',
                               SPECIES== 702009~ 'champagne crab',
                               TRUE~SNAME),
               SNAME=capitalize(SNAME))
  
  Data.daily.original=Data.daily.original%>%
                       filter(Same.return.SNo%in%unique(Data.daily$Same.return.SNo))
  
  Data.daily=Data.daily%>%
    mutate(Landing.Port=capitalize(tolower(Landing.Port)),
           Landing.Port=case_when(Landing.Port=="Albany town jetty"~"Albany",
                                  TRUE~Landing.Port))
  Data.monthly=Data.monthly%>%
    mutate(Landing.Port=capitalize(tolower(Landing.Port)))
  
  
  #Export annual average catch by species for last 5 years for economic valuation
  Lst.5yrs=2018
  #Lst.5yrs=as.numeric(substr(max(Data.daily$FINYEAR),1,4))  #not using 2019-20 as catch was much lower due to COVID
  Lst.5yrs=rev(seq(Lst.5yrs,(Lst.5yrs-4),-1))
  Lst.5yrs=paste(Lst.5yrs,substr(Lst.5yrs+1,3,4),sep='-')
  Avrg.ktch=Data.daily%>%
    filter(FINYEAR%in%Lst.5yrs)%>%
    group_by(SPECIES,FINYEAR)%>%
    summarise(Tot=sum(LANDWT,na.rm=T))%>%
    group_by(SPECIES)%>%
    summarise(Tot=mean(Tot,na.rm=T))%>%
    ungroup()
  Shark.Fin.prop=Shark.Fin.Price.List%>%    #Add fins weight
    rename(Percent="ï..Percent")%>%
    mutate(Prop.livewt=Percent/100)%>%
    dplyr::select(Prop.livewt,species)
  Avrg.prop.fin.Prop.livewt=mean(Shark.Fin.prop%>%
                                   filter(Prop.livewt>0)%>%
                                   filter(!species==22998)%>%
                                   pull(Prop.livewt))
  Avrg.ktch.fins=sum(Data.daily%>%
                       filter(FINYEAR%in%Lst.5yrs & SPECIES<=35000)%>%
                       group_by(SPECIES,FINYEAR)%>%
                       summarise(Tot=sum(LIVEWT))%>%
                       group_by(SPECIES)%>%
                       summarise(Tot=mean(Tot))%>%
                       ungroup()%>%
                       left_join(Shark.Fin.prop,by=c('SPECIES'='species'))%>%
                       mutate(Prop.livewt=ifelse(is.na(Prop.livewt),Avrg.prop.fin.Prop.livewt,Prop.livewt),
                              Fin.weight=Tot*Prop.livewt)%>%
                       pull(Fin.weight))
  Avrg.ktch=rbind(Avrg.ktch,data.frame(SPECIES=22998,Tot=Avrg.ktch.fins))
  write.csv(Avrg.ktch,handl_OneDrive('Analyses/Parks Australia/outputs/Socio-economics/Avrg.ktch.csv'),row.names=F)  

  #Export average annual catch by boat 
  Data.daily.original=Data.daily.original%>%
    mutate(BoatName=tolower(BoatName),
           BoatName=case_when(BoatName%in%c("barbarosa ll")~"barbarosa ii",
                              BoatName%in%c("catch fillet release")~"catch fillet & release",
                              BoatName%in%c("chivers regal 11")~"chivers regal 2",
                              BoatName%in%c("dorreen","dorren")~"doreen",
                              BoatName%in%c("elizabeht maria 11","elizabeth maria 11","elizabith maria 11")~"elizabeth maria ii",
                              BoatName%in%c("falcon 11","falcon ll")~"falcon ii",
                              BoatName%in%c("fish tales")~"fishtales",
                              BoatName%in%c("giuliano 2")~"giuliano ii",
                              BoatName%in%c("carado")~"corado",
                              BoatName%in%c("kabralee 11","kalbarri lass ii")~"kabralee ii",
                              BoatName%in%c("lone hand")~"lonehand",
                              BoatName%in%c("maniki 2")~"maniki ii",
                              BoatName%in%c("planjak 11")~"planjak ii",
                              BoatName%in%c("sea venture 11")~"sea venture ii",
                              BoatName%in%c("st. gerard m","st gerard")~"st gerard m",
                              BoatName%in%c("steve mayree d")~"steve-mayree d",
                              BoatName%in%c("sveti nikola")~"sveti-nikola",
                              BoatName%in%c("karina","karina 4")~"karina 5",
                              BoatName%in%c("tara-marie")~"tara marie",
                              BoatName%in%c("tracey-lea")~"tracey lea",
                              TRUE~BoatName))
  
  Total.ktch.per.boat=Data.daily.original%>%
    filter(FINYEAR%in%Lst.5yrs & !BoatName%in%c("n/a",""))%>%
    group_by(BoatName,FINYEAR)%>%
    summarise(Tot=sum(landwt,na.rm=T))%>%
    group_by(BoatName)%>%
    summarise(Tot=mean(Tot,na.rm=T))%>%
    ungroup()
  write.csv(Total.ktch.per.boat,handl_OneDrive('Analyses/Parks Australia/outputs/Socio-economics/Total.ktch.per.boat.csv'),row.names=F)  
  
  #Export boats reporting catch for last 2 years
  Licence.holders=Licence.holders%>%
                    mutate(Boat.name=tolower(Boat.name))
  Tot.dummy=Data.daily.original%>%
                        filter(FINYEAR%in%c('2018-19','2019-20'))%>%
                        filter(BoatName%in%Licence.holders$Boat.name)%>%
                        mutate(MastersName=tolower(MastersName))%>%
                        group_by(BoatName,VESSEL,MastersName)%>%
                        summarise(Tot=sum(livewt,na.rm=T)/1000)%>%
                        arrange(BoatName,-Tot)%>%
                        data.frame
  Tot.dummy=Tot.dummy%>%left_join(Licence.holders,by=c('BoatName'='Boat.name'))
  write.csv(Tot.dummy,handl_OneDrive('Analyses/Parks Australia/outputs/Socio-economics/Total.ktch.per.boat_skipper.csv'),row.names=F)  
  
  
  #Export number of returns lodged by operator by year
  Operators=Data.daily.original%>%
    distinct(TSNo,.keep_all=T)%>%
    mutate(MastersName=tolower(MastersName),
           MastersName=case_when(MastersName%in%c("brian & jason  scimone","brian / jason scimone",
                                                  "jason & brian scimone","jason / brian scimone")~"brian & jason scimone",
                                 MastersName%in%c("brian gregory scimone","scimone, brian gregory")~"brian scimone",
                                 MastersName%in%c('chris henderson','henderson, christian william')~"c. henderson",
                                 MastersName%in%c("mansted, storm",' storm mansted',"anthony david mansted")~"anthony mansted",
                                 MastersName=="charles brown"~"c.d.brown",
                                 MastersName=="glen brodie"~"g. brodie",
                                 MastersName=="peter hughes & william ronald reay"~"hughes, peter",
                                 MastersName=="anthony cooke"~"cooke, anthony",
                                 MastersName%in%c("jeffrey cooke","jeffrey frank cooke")~"cooke, jeffrey",
                                 MastersName%in%c("soulos, emanuel nicholas",
                                                  "emanuel nicholas soulos")~"emanuel soulos",
                                 MastersName=="glen brodi"~"glen brodie",
                                 MastersName=="robinson, mark"~"mark robinson",
                                 MastersName%in%c("jayson lyndsay  / scott farrant",
                                                  "jayson lindsay / scott farrant",
                                                  "jayson lindsay & scott farrant",
                                                  "lindsay, jayson / scott farrant",
                                                  "scott  farrant")~ "scott farrant",
                                 MastersName=="jeremy frances sanders"~"jf sanders",
                                 MastersName%in%c("n e soulos",'n.e soulos',"n.e.soulas","n.e.soulos","ne soulos")~"n.e. soulos",
                                 MastersName=="phil  toumazos"~"phil toumazos",
                                 grepl('triantafyllou',MastersName)~"n. triantafyllou",
                                 grepl('joy',MastersName)~"andrew joy",
                                 grepl('black',MastersName)~"c. black",
                                 grepl('sharp',MastersName)~"greg sharp",
                                 grepl('hawkins',MastersName)~"d. hawkins",
                                 grepl('campbell',MastersName)~"g. campbell",
                                 grepl('dunn',MastersName)~"s. dunn",
                                 grepl('dyer',MastersName)~" p. dyer",
                                 grepl('sell',MastersName)~"g.e. sell",
                                 grepl('thornton',MastersName)~"j. thornton",
                                 grepl('goodall',MastersName)~"t. goodall",
                                 grepl('madgen',MastersName)~"d. madgen",
                                 grepl('whetstone',MastersName)~"g. whetstone",
                                 grepl('brodie',MastersName)~"g. brodie",
                                 grepl('robb',MastersName)~"j. robb",
                                 grepl('richardson',MastersName)~"john richardson",
                                 grepl('rowbottom',MastersName)~"john rowbottom",
                                 grepl('tonkin',MastersName)~"m. tonkin",
                                 grepl('karaterpos',MastersName)~"m. karaterpos",
                                 grepl('branderhorst',MastersName)~"m. branderhorst",
                                 grepl('bubb',MastersName)~"m.r. bubb",
                                 grepl('matthews',MastersName)~"l. matthews",
                                 grepl('mcwhirter',MastersName)~"s. mcwhirter",
                                 grepl('murch',MastersName)~"p. murch",
                                 grepl('steele',MastersName)~"a. steele",
                                 grepl('parker',MastersName)~"r. parker",
                                 grepl('warrilow',MastersName)~"p. warrilow",
                                 grepl('bradley',MastersName)~"r. bradley",
                                 grepl('farrant',MastersName)~"s. farrant",
                                 grepl('bryant',MastersName)~"s. bryant",
                                 grepl('smythe',MastersName)~"j.l. smythe",
                                 grepl('buckeridge',MastersName)~"s. buckeridge",
                                 grepl('tindall',MastersName)~"j. tindall",
                                 grepl('farrant',MastersName)~"s. farrant",
                                 grepl('mason',MastersName)~"t. mason",
                                 grepl('goodall',MastersName)~"t. goodall",
                                 TRUE~MastersName),
           BoatName=tolower(BoatName),
           BoatName=case_when(BoatName%in%c("barbarosa ll")~"barbarosa ii",
                              BoatName%in%c("catch fillet release")~"catch fillet & release",
                              BoatName%in%c("chivers regal 11")~"chivers regal 2",
                              BoatName%in%c("doreen","dorren")~"dorreen",
                              BoatName%in%c("elizabeht maria 11","elizabeth maria 11","elizabith maria 11")~"elizabeth maria ii",
                              BoatName%in%c("falcon 11","falcon ll")~"falcon ii",
                              BoatName%in%c("fish tales")~"fishtales",
                              BoatName%in%c("giuliano 2")~"giuliano ii",
                              BoatName%in%c("carado")~"corado",
                              BoatName%in%c("kabralee 11","kalbarri lass ii")~"kabralee ii",
                              BoatName%in%c("lone hand")~"lonehand",
                              BoatName%in%c("maniki 2")~"maniki ii",
                              BoatName%in%c("planjak 11")~"planjak ii",
                              BoatName%in%c("sea venture 11")~"sea venture ii",
                              BoatName%in%c("st. gerard m","st gerard")~"st gerard m",
                              BoatName%in%c("steve mayree d")~"steve-mayree d",
                              BoatName%in%c("sveti-nikola")~"sveti nikola",
                              BoatName%in%c("karina","karina 4")~"karina 5",
                              BoatName%in%c("tara-marie")~"tara marie",
                              BoatName%in%c("tracey-lea")~"tracey lea",
                              TRUE~BoatName))
  Tab_Op_boats=Operators%>%
    group_by(FINYEAR,MastersName,BoatName)%>%
    tally()%>%
    spread(FINYEAR,n,fill=0)
  write.csv(Tab_Op_boats,handl_OneDrive('Analyses/Parks Australia/outputs/Socio-economics/annual.returns_by_operator.and.boat.csv'),row.names=F)  
  
  Tab_Op=Operators%>%
    group_by(FINYEAR,MastersName)%>%
    tally()%>%
    spread(FINYEAR,n,fill=0)
  Total.operators=Tab_Op%>%
    mutate_if(is.numeric, ~1 * (. > 0))
  XXX=Tab_Op[1,]%>%
    mutate(MastersName="Total.number.operators")
  XXX=cbind(MastersName="Total.number.operators",t(colSums(Total.operators[,-1])))
  write.csv(rbind(Tab_Op,XXX),handl_OneDrive('Analyses/Parks Australia/outputs/Socio-economics/annual.returns_by_operator.csv'),row.names=F)  
  
  
 
  #1. Vessel dynamics
    #1.1. Landing port dynamics at its peak and currently
  Data.monthly=Data.monthly%>%
    left_join(Port.loc,by=c("Landing.Port"="Cityname"))
  
  Data.daily=Data.daily%>%
    left_join(Port.loc,by=c("Landing.Port"="Cityname"))
  
  Max.ports.yr=Data.monthly%>%
    filter(YEAR.c<=2005)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    filter(!is.na(lat.port))%>%
    distinct(Same.return,.keep_all=T)%>%
    group_by(Landing.Port,FINYEAR)%>%
    tally()%>%
    ungroup()%>%
    mutate(n=1)%>%
    group_by(FINYEAR)%>%
    tally()%>%
    filter(n==max(n))
 
  p1=plt.map(D=Data.monthly%>%
               filter(FINYEAR==Max.ports.yr$FINYEAR)%>%
               filter(METHOD%in%c('GN',"LL"))%>%
               filter(!is.na(lat.port))%>%
               distinct(Same.return,.keep_all=T)%>%
               group_by(Landing.Port,lat.port,lon.port)%>%
               tally()%>%
               ungroup()%>%
               mutate(Prop=n/sum(n)),
             Title=paste("Monthly returns (",Max.ports.yr$n," ports in ",Max.ports.yr$FINYEAR,")",sep=''),
             Scaler=150)
  
  D=Data.daily%>%
    filter(FINYEAR==Current.yr)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    filter(!is.na(lat.port))%>%
    mutate(TSNo=word(Same.return.SNo, 3, 3))%>%
    distinct(TSNo,.keep_all=T)%>%
    group_by(Landing.Port,lat.port,lon.port)%>%
    tally()%>%
    ungroup()%>%
    mutate(Prop=n/sum(n))
  p2=plt.map(D=D,
             Title=paste("Daily logbooks (",nrow(D)," ports in ",Current.yr,")",sep=''),
             Scaler=150)
  Bubble.ref=data.frame(lon.port=c(128,128),
                        lat.port=c(-35.75,-33.75),
                        Prop=c(0.1,0.2))
  p2=p2+geom_point(data = Bubble.ref, mapping = aes(x = lon.port, y = lat.port),
             size=Bubble.ref$Prop*150, colour = grDevices::adjustcolor("steelblue", alpha=0.6))+
    geom_text(data=Bubble.ref,aes(x = lon.port, y = lat.port,label = paste(Prop*100,"%",sep='')),
              fontface="bold",color="white",size=5)
  
  ggarrange(p1, p2, ncol = 1, nrow = 2)
  ggsave(le.paste("Historic_catch_effort/Map_ports.tiff"),width = 7,height = 9,compression = "lzw")
  
  if(do.soc.econ.papr)
  {
    p1.map=p1
    p2.map=p2
  }
  
    #1.2. Number of departing ports and vessels by year
  a=Data.monthly%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    filter(!is.na(lat.port))%>%
    distinct(Same.return,.keep_all=T)%>%
    group_by(Landing.Port,FINYEAR,zone)%>%
    tally()%>%
    ungroup()%>%
    mutate(n=1)%>%
    group_by(FINYEAR,zone)%>%
    tally()%>%
    ungroup()%>%
    mutate(year=as.numeric(substr(FINYEAR,1,4)),
           dat="Monthly returns")%>%
    filter(year<=2005)
  b=Data.daily%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    filter(!is.na(lat.port))%>%
    mutate(TSNo=word(Same.return.SNo, 3, 3))%>%
    distinct(TSNo,.keep_all=T)%>%
    group_by(Landing.Port,FINYEAR,zone)%>%
    tally()%>%
    ungroup()%>%
    mutate(n=1)%>%
    group_by(FINYEAR,zone)%>%
    tally()%>%
    ungroup()%>%
    mutate(year=as.numeric(substr(FINYEAR,1,4)),
           dat="Daily logbooks")
  my.formula <- y ~ x  
  
  p1=rbind(a,b)%>%
    mutate(dat=factor(dat,levels=c("Monthly returns","Daily logbooks")))%>%
    ggplot(aes(x=year,y=n,color=zone))+
    geom_point(size=2.5)+
    geom_smooth(method = "lm",se=F,formula = my.formula)+
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 label.y = "top",label.x="right",parse = TRUE, size = 5)+
    #facet_wrap(~dat,scales='free')+
    ylab("Number of ports")+xlab("")+
    theme_PA(strx.siz=14,leg.siz=18,axs.t.siz=16,axs.T.siz=20)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))
    
  d=Data.monthly%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    mutate(This=paste(VESSEL,FINYEAR))%>%
    distinct(This,.keep_all=T)%>%
    group_by(YEAR,zone)%>%
    tally()%>%
    mutate(group=as.factor(paste(zone)),
           dat="Monthly returns")%>%
    filter(YEAR<=2005)
  
  d1=Data.daily%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    mutate(This=paste(VESSEL,FINYEAR))%>%
    distinct(This,.keep_all=T)%>%
    group_by(YEAR,zone)%>%
    tally()%>%
    mutate(group=as.factor(paste(zone)),
           dat="Daily logbooks")
  
  ddd=rbind(d,d1)%>%
    mutate(dat=factor(dat,levels=c("Monthly returns","Daily logbooks")))
  p2=ddd%>%
    ggplot(aes(x=YEAR,y=n,color=group))+
    geom_point(size=2.5)+
    geom_smooth(data=subset(ddd, YEAR >= 1980),method = "lm",se=F,formula = my.formula)+
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 label.y = "top",label.x="right", parse = TRUE, size = 5)+
    #facet_wrap(~dat,scales='free')+
    ylab("Number of vessels")+xlab("Financial year")+
    theme_PA(strx.siz=14,leg.siz=18,axs.t.siz=16,axs.T.siz=20)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))
  
  ggarrange(p1, p2, ncol = 1, nrow = 2,common.legend=TRUE)   
  ggsave(le.paste("Historic_catch_effort/Annual number of ports and vessel by zone.tiff"),width = 9,height = 10,compression = "lzw")
  
  if(do.soc.econ.papr)
  {
    p1.N.ports=p1
    p2.N.vessels=p2
    
    ggarrange(p1.N.ports,
              p1.map+ggtitle("1988-89 (53 ports)"),
              p2.N.vessels+ theme(legend.position = "none"),
              p2.map+ggtitle("2019-20 (14 ports)"),
              ncol = 2, nrow = 2)
    ggsave(le.paste("Papers/socio_economics/Figure1.tiff"),width = 9,height = 9,compression = "lzw")
  }
  
    #1.3. Number of vessels with gillnet or longline by year
  d=Data.monthly%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    mutate(YEAR=as.numeric(substr(FINYEAR,1,4)),
           This=paste(VESSEL,METHOD,FINYEAR))%>%
    distinct(This,.keep_all=T)%>%
    group_by(YEAR,METHOD,zone)%>%
    tally()%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Monthly")%>%
    filter(YEAR<=2005)
  
  d1=Data.daily%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    mutate(YEAR=as.numeric(substr(FINYEAR,1,4)),
           This=paste(VESSEL,METHOD,FINYEAR))%>%
    distinct(This,.keep_all=T)%>%
    group_by(YEAR,METHOD,zone)%>%
    tally()%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Daily")
  
  colfunc <- colorRampPalette(c("orange","firebrick1"))
  n.col.GN=colfunc(length(grep("GN",unique(d$group))))
  colfunc <- colorRampPalette(c("lightblue3", "dodgerblue4"))
  n.col.LL=colfunc(length(grep("LL",unique(d$group))))
  dis.cols=c(n.col.GN,n.col.LL)
  names(dis.cols)=levels(d$group)
  dis.cols=dis.cols[match(levels(droplevels(d$group)),names(dis.cols))]
  
  
  #add catch per vessel
  d2=Data.monthly%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    mutate(YEAR=as.numeric(substr(FINYEAR,1,4)))%>%
    group_by(YEAR,METHOD,zone)%>%
    summarise(Tons=sum(LIVEWT.c)/1000)%>%
    ungroup()%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Monthly")%>%
    filter(YEAR<=2005)
  
  d3=Data.daily%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    mutate(YEAR=as.numeric(substr(FINYEAR,1,4)))%>%
    group_by(YEAR,METHOD,zone)%>%
    summarise(Tons=sum(LIVEWT.c)/1000)%>%
    ungroup()%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Daily")
  
  d2=d2%>%
    left_join(d,by=c('YEAR','METHOD','zone','group','Data.set'))%>%
    mutate(ktch.per.ves=Tons/n)
  
  d3=d3%>%
    left_join(d1,by=c('YEAR','METHOD','zone','group','Data.set'))%>%
    mutate(ktch.per.ves=Tons/n)
  
  
  dd=rbind(d,d1)%>%
    mutate(Data.set=factor(Data.set,levels=c("Monthly","Daily")))
  dd1=rbind(d2,d3)%>%
    mutate(Data.set=factor(Data.set,levels=c("Monthly","Daily")))
  
  
  coeff=0.5
  X=dd%>%
    ggplot(aes(YEAR,n))+
    geom_col(aes(fill=group),alpha=0.65)+
    geom_line(data=dd1, aes(YEAR,ktch.per.ves/coeff,color=group),size=1.25)+
    scale_y_continuous(name = "Number of Vessels",
      sec.axis = sec_axis(~.*coeff, name="Annual catch (tonnes) per vessel"))+
    xlab("Financial year")+
    scale_color_manual(values=dis.cols)+
    scale_fill_manual(values=dis.cols)+
    theme_PA(Ttl.siz=18,Sbt.siz=16,str.siz=12,strx.siz=12,
             cap.siz=10,leg.siz=12,axs.t.siz=10,axs.T.siz=14)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
     guides(fill = guide_legend(nrow = 1),color = guide_legend(nrow = 1))
   
  fn.fig(le.paste("Historic_catch_effort/Annual number of vessels by method"),2400,1800)
  X
  dev.off()

  
  #2. Number of fishing days with gillnet or longline by year
    #Total
  d=Data.monthly%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    group_by(YEAR,METHOD,zone)%>%
    summarise(Bdays=sum(BDAYS))%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Monthly")%>%
    filter(YEAR<=2005)
  
  d1=Data.daily%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    group_by(YEAR,METHOD,zone)%>%
    summarise(Bdays=sum(BDAYS))%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Daily")
  
  P1=rbind(d,d1)%>%
    mutate(Data.set=factor(Data.set,levels=c("Monthly","Daily")))%>%
    ggplot(aes(YEAR,Bdays))+
    geom_col(aes(fill=group))+
    #facet_wrap(~Data.set,scales='free')+
    ylab("Number of fishing days")+xlab("")+
    scale_fill_manual(values=dis.cols)+
    theme_PA(Ttl.siz=18,Sbt.siz=16,str.siz=12,strx.siz=12,
             cap.siz=10,leg.siz=12,axs.t.siz=10,axs.T.siz=14)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
    guides(fill = guide_legend(nrow = 1))
  
  
    #Average 
  d=Data.monthly%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    group_by(YEAR,METHOD,zone,VESSEL)%>%
    summarise(BDAYS=sum(BDAYS,na.rm=T))%>%
    group_by(YEAR,METHOD,zone)%>%
    summarise(Bdays.mean=mean(BDAYS,na.rm=T),
              Bdays.sd=sd(BDAYS,na.rm=T))%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Monthly")%>%
    filter(YEAR<=2005)
  
  d1=Data.daily%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    group_by(YEAR,METHOD,zone,VESSEL)%>%
    summarise(BDAYS=sum(BDAYS,na.rm=T))%>%
    group_by(YEAR,METHOD,zone)%>%
    summarise(Bdays.mean=mean(BDAYS,na.rm=T),
              Bdays.sd=sd(BDAYS,na.rm=T))%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Daily")
  
  P2=rbind(d,d1)%>%
    mutate(Data.set=factor(Data.set,levels=c("Monthly","Daily")))%>%
    ggplot(aes(YEAR,Bdays.mean,color=group))+
    geom_errorbar(aes(x=YEAR, ymin=Bdays.mean-Bdays.sd, ymax=Bdays.mean+Bdays.sd,color=group),alpha=0.6,width=0.25)+
    geom_line(size=1.5,linetype='solid',alpha=0.6)+
    geom_point(size=2)+
    ylab("Average annual number of fishing days")+
    xlab("Financial year")+
    scale_color_manual(values=dis.cols)+
    theme_PA(Ttl.siz=18,Sbt.siz=16,str.siz=12,strx.siz=12,
             cap.siz=10,leg.siz=12,axs.t.siz=10,axs.T.siz=14)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
    guides(fill = guide_legend(nrow = 1))
  

  fn.fig(le.paste("Historic_catch_effort/Annual fishing days by method"),2400,2200)
  ggarrange(P1,P2,ncol=1,common.legend=TRUE)
  dev.off()
  
  if(do.soc.econ.papr)
  {
    X=dd%>%
      ggplot(aes(YEAR,n))+
      geom_line(data=dd1, aes(YEAR,ktch.per.ves,color=group),size=1.5)+
      ylab("Annual catch (tonnes) per vessel")+
      xlab("Financial year")+
      scale_color_manual(values=dis.cols)+
      scale_fill_manual(values=dis.cols)+
      theme_PA(Ttl.siz=18,Sbt.siz=16,str.siz=12,strx.siz=12,
               cap.siz=10,leg.siz=16,axs.t.siz=10,axs.T.siz=14)+
      theme(legend.position = "top",
            legend.title = element_blank(),
            plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
      guides(fill = guide_legend(nrow = 1),color = guide_legend(nrow = 1))
    
    P2=rbind(d,d1)%>%
      mutate(Data.set=factor(Data.set,levels=c("Monthly","Daily")))%>%
      ggplot(aes(YEAR,Bdays.mean,color=group))+
      geom_line(size=1.5,linetype='solid')+
      ylab("Annual fishing days per vessel")+
      xlab("")+
      scale_color_manual(values=dis.cols)+
      theme_PA(Ttl.siz=18,Sbt.siz=16,str.siz=12,strx.siz=12,
               cap.siz=10,leg.siz=12,axs.t.siz=10,axs.T.siz=14)+
      theme(legend.position = "top",
            legend.title = element_blank(),
            plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
      guides(fill = guide_legend(nrow = 1))
    
    X.catch.rate=dd1%>%
      left_join(rbind(d,d1)%>%
                  dplyr::select(-c(Bdays.sd,group,Data.set)),
                by=c('YEAR','METHOD','zone'))%>%
      mutate(CPUE=ktch.per.ves/Bdays.mean)%>%
      ggplot(aes(YEAR,CPUE,color=group))+
      geom_line(size=1.5)+
      ylab("Annual catch rate (tonnes per day) per vessel")+
      xlab("Financial year")+
      scale_color_manual(values=dis.cols)+
      scale_fill_manual(values=dis.cols)+
      theme_PA(Ttl.siz=18,Sbt.siz=16,str.siz=12,strx.siz=12,
               cap.siz=10,leg.siz=12,axs.t.siz=10,axs.T.siz=14)+
      theme(legend.position = 'none',
            plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
      guides(fill = guide_legend(nrow = 1),color = guide_legend(nrow = 1))
    ggarrange(P1+theme_PA(axs.T.siz=14)+theme(legend.title =element_blank()),
              P2+theme_PA(axs.T.siz=14),
              X+theme_PA(axs.T.siz=14),
              X.catch.rate+theme_PA(axs.T.siz=14),
              ncol=2,nrow=2,
              common.legend=TRUE)
    ggsave(le.paste("Papers/socio_economics/Figure2.tiff"),width = 10,height = 10,compression = "lzw")
    
  }

  #3. Catch composition GN and LL
    #3.1. Overall (show 20 top species, aggregate the rest)
  fn.br.plt=function(dd,TOP,yMx,CX.nm)
  {
    dd=dd%>%
      mutate(Prop=Total/sum(dd$Total))%>%
      arrange(-Prop)%>%mutate(new.sp=SNAME)
    if(nrow(dd)>TOP) dd$new.sp[(TOP+1):nrow(dd)]="Other"  
    n.other=length(unique(dd$SNAME))-(length(unique(dd$new.sp))-1)
    dd=dd%>%mutate(colr=ifelse(SPECIES<50000,"steelblue","firebrick"),
                   colr=ifelse(new.sp=="Other","forestgreen",colr),
                   new.sp=ifelse(new.sp=="Other",paste("Other (n=",n.other," species)",sep=""),new.sp))%>%
      group_by(new.sp,colr)%>%
      summarise(Prop=sum(Prop))%>%
      data.frame%>%
      arrange(Prop)
    Xmax=round(max(dd$Prop),2)+0.05
    with(dd,barplot(Prop,horiz = T,col=colr,names.arg=new.sp,cex.names=CX.nm,las=1,xlim=c(0,Xmax)))
    box()
  }
  Max.fn=function(d) round(max(d%>%mutate(Prop=Total/sum(d$Total))%>%pull(Prop)),2)+0.05
  fn.ktch.comp=function(ktch,what,Min.overlap) 
  {
     
    #Overall comparison GN vs LL
    dat=ktch%>%group_by(METHOD,SPECIES,SNAME)%>%
      summarise(Total = sum(LIVEWT.c))
    #Xmax<<-Max.fn(dat)
    fn.fig(paste(hndl,"/Catch_composition/Overall_GN_LL_overall",what,sep=""),2400,1600)
    par(mfcol=c(1,2),mar=c(2,10.5,1,.1),oma=c(1,.1,.5,.3),mgp=c(1.5,.5,0),cex.axis=.9)
    fn.br.plt(dd=dat%>%filter(METHOD=='GN'),TOP=20,yMx=1,CX.nm=1)
    mtext(paste("Gillnet (",round(sum(subset(dat,METHOD=='GN')$Total)/1000,0)," tonnes)",sep=""),3,cex=1)
    fn.br.plt(dd=dat%>%filter(METHOD=='LL'),TOP=20,yMx=1,CX.nm=1)
    mtext(paste("Longline (",round(sum(subset(dat,METHOD=='LL')$Total)/1000,0)," tonnes)",sep=""),3,cex=1)
    mtext("Proportion of total catch",1,outer=T,line=0.1,cex=1.2) 
    dev.off()
    
    #By zone comparison GN vs LL
    dat=ktch%>%group_by(METHOD,zone,SPECIES,SNAME)%>%
      summarise(Total = sum(LIVEWT.c))
    #Xmax<<-Max.fn(dat)
    CX.NM=.7
    Top=15
    fn.fig(paste(hndl,"/Catch_composition/Overall_GN_LL_by zone",what,sep=""),2400,1600)
    par(mfcol=c(3,2),mar=c(2,7,.1,.1),oma=c(1.1,.1,1.1,.35),mgp=c(1.5,.5,0))
      #GN
    fn.br.plt(dd=dat%>%filter(METHOD=='GN' & zone=="West"),TOP=Top,yMx=1,CX.nm=CX.NM)
    mtext("Gillnet",3,cex=1)
    legend('bottomright',paste("West (",round(sum(subset(dat,METHOD=='GN'& zone=="West")$Total)/1000,1),
                            " tonnes)",sep=""),bty='n')
    fn.br.plt(dd=dat%>%filter(METHOD=='GN' & zone=="Zone1"),TOP=Top,yMx=1,CX.nm=CX.NM)
    legend('bottomright',paste("Zone1 (",round(sum(subset(dat,METHOD=='GN'& zone=="Zone1")$Total)/1000,1),
                            " tonnes)",sep=""),bty='n')
    fn.br.plt(dd=dat%>%filter(METHOD=='GN' & zone=="Zone2"),TOP=Top,yMx=1,CX.nm=CX.NM)
    legend('bottomright',paste("Zone2 (",round(sum(subset(dat,METHOD=='GN'& zone=="Zone2")$Total)/1000,1),
                            " tonnes)",sep=""),bty='n')
    
      #LL
    fn.br.plt(dd=dat%>%filter(METHOD=='LL' & zone=="West"),TOP=Top,yMx=1,CX.nm=CX.NM)
    mtext("Longline",3,cex=1)
    legend('bottomright',paste("West (",round(sum(subset(dat,METHOD=='LL'& zone=="West")$Total)/1000,1),
                            " tonnes)",sep=""),bty='n')
    fn.br.plt(dd=dat%>%filter(METHOD=='LL' & zone=="Zone1"),TOP=Top,yMx=1,CX.nm=CX.NM)
    legend('bottomright',paste("Zone1 (",round(sum(subset(dat,METHOD=='LL'& zone=="Zone1")$Total)/1000,1),
                            " tonnes)",sep=""),bty='n')
    fn.br.plt(dd=dat%>%filter(METHOD=='LL' & zone=="Zone2"),TOP=Top,yMx=1,CX.nm=CX.NM)
    legend('bottomright',paste("Zone2 (",round(sum(subset(dat,METHOD=='LL'& zone=="Zone2")$Total)/1000,1),
                            " tonnes)",sep=""),bty='n')
    mtext("Proportion of total catch",1,outer=T,line=0.1,cex=1.2) 
    dev.off()
    
    
    #By zone comparison GN vs LL without targeting vessel
    Snapper.targeting.vessel=NULL
    if(what=="_Daily")
    {
      Snapper.targeting.vessel=ktch%>%
        filter(SPECIES%in%c(353001) & METHOD=="LL" & zone=="West")%>%
        group_by(VESSEL,SNAME)%>%
        summarise(Tot=sum(LIVEWT.c))%>%
        arrange(-Tot)%>%
        ungroup()%>%
        mutate(Cum.tot=cumsum(Tot),
               Cum.tot.per=100*Cum.tot/sum(Tot))%>%
        filter(Cum.tot.per<95)%>%
        pull(VESSEL)
      dat=ktch%>%
        filter(!VESSEL%in%Snapper.targeting.vessel)%>%
        group_by(METHOD,zone,SPECIES,SNAME)%>%
        summarise(Total = sum(LIVEWT.c))
      CX.NM=.7
      Top=15
      #Xmax<<-Max.fn(dat)
      fn.fig(paste(hndl,"/Catch_composition/Overall_GN_LL_by zone_without.3.vessels.accounting for 95 percent.catch",what,sep=""),2400,1600)
      par(mfcol=c(3,2),mar=c(2,7,.1,.1),oma=c(1.1,.1,1.1,.3),mgp=c(1.5,.5,0))
      #GN
      fn.br.plt(dd=dat%>%filter(METHOD=='GN' & zone=="West"),TOP=Top,yMx=1,CX.nm=CX.NM)
      mtext("Gillnet",3,cex=1)
      legend('bottomright',paste("West (",round(sum(subset(dat,METHOD=='GN'& zone=="West")$Total)/1000,1),
                                 " tonnes)",sep=""),bty='n')
      fn.br.plt(dd=dat%>%filter(METHOD=='GN' & zone=="Zone1"),TOP=Top,yMx=1,CX.nm=CX.NM)
      legend('bottomright',paste("Zone1 (",round(sum(subset(dat,METHOD=='GN'& zone=="Zone1")$Total)/1000,1),
                                 " tonnes)",sep=""),bty='n')
      fn.br.plt(dd=dat%>%filter(METHOD=='GN' & zone=="Zone2"),TOP=Top,yMx=1,CX.nm=CX.NM)
      legend('bottomright',paste("Zone2 (",round(sum(subset(dat,METHOD=='GN'& zone=="Zone2")$Total)/1000,1),
                                 " tonnes)",sep=""),bty='n')
      
      #LL
      fn.br.plt(dd=dat%>%filter(METHOD=='LL' & zone=="West"),TOP=Top,yMx=1,CX.nm=CX.NM)
      mtext("Longline",3,cex=1)
      legend('bottomright',paste("West (",round(sum(subset(dat,METHOD=='LL'& zone=="West")$Total)/1000,1),
                                 " tonnes)",sep=""),bty='n')
      fn.br.plt(dd=dat%>%filter(METHOD=='LL' & zone=="Zone1"),TOP=Top,yMx=1,CX.nm=CX.NM)
      legend('bottomright',paste("Zone1 (",round(sum(subset(dat,METHOD=='LL'& zone=="Zone1")$Total)/1000,1),
                                 " tonnes)",sep=""),bty='n')
      fn.br.plt(dd=dat%>%filter(METHOD=='LL' & zone=="Zone2"),TOP=Top,yMx=1,CX.nm=CX.NM)
      legend('bottomright',paste("Zone2 (",round(sum(subset(dat,METHOD=='LL'& zone=="Zone2")$Total)/1000,1),
                                 " tonnes)",sep=""),bty='n')
      mtext("Proportion of total catch",1,outer=T,line=0.1,cex=1.2) 
      dev.off()
    }
     
    
    #By similar block-month comparison GN vs LL
    ktch=ktch%>%mutate(BLK={if("block10" %in% names(.)) block10 else BLOCKX})
    
    dat=ktch%>%group_by(METHOD,BLK,MONTH,FINYEAR,SPECIES,SNAME)%>%
      summarise(Total = sum(LIVEWT.c))%>%mutate(dummy=paste(BLK,MONTH,FINYEAR))
    LL.recrds=dat%>%filter(METHOD=="LL")
    GN.recrds=dat%>%filter(METHOD=="GN")
    same.recs=intersect(LL.recrds$dummy, GN.recrds$dummy)
    dat=dat%>%filter(dummy%in%same.recs)
    TAB=as.data.frame.matrix(with(dat,table(dummy,METHOD)))
    same.recs=TAB%>%mutate(dummy=rownames(TAB))%>%filter(GN>=Min.overlap & LL>=Min.overlap)%>%.$dummy
    dat=dat%>%filter(dummy%in%same.recs)
    
    unIk=sort(unique(dat$dummy))
    #Xmax<<-Max.fn(dat)
    Top=10
    CX.NM=.7*4/length(unIk)
    fn.fig(paste(hndl,"/Catch_composition/Overall_GN_LL_same_blk_yr_mn",what,sep=""),2400,1600)
    par(mfcol=c(length(unIk),2),mar=c(2,7,.1,.1),oma=c(1,.1,1.1,.4),mgp=c(1.5,.5,0))
    #GN    
    for(u in 1:length(unIk))
    {
      fn.br.plt(dd=dat%>%filter(METHOD=='GN' & dummy==unIk[u]),TOP=Top,yMx=1,CX.nm=CX.NM)
      if(u==1)mtext("Gillnet",3,cex=1)
      legend('bottomright',paste(unIk[u]," (",round(sum(subset(dat,METHOD=='GN'& dummy==unIk[u])$Total)/1000,1),
                              " tonnes)",sep=""),bty='n',cex=.8)
    }
    #LL
    for(u in 1:length(unIk))
    {
      fn.br.plt(dd=dat%>%filter(METHOD=='LL' & dummy==unIk[u]),TOP=Top,yMx=1,CX.nm=CX.NM)
      if(u==1)mtext("Longline",3,cex=1)
      legend('bottomright',paste(unIk[u]," (",round(sum(subset(dat,METHOD=='LL'& dummy==unIk[u])$Total)/1000,1),
                              " tonnes)",sep=""),bty='n',cex=.8)
    }
    mtext("Proportion of total catch",1,outer=T,line=0,cex=1) 
    dev.off()
    
    return(list(Snapper.targeting.vessel=Snapper.targeting.vessel,
                same.BLK.MONTH.FINYEAR=unIk))
  }
      #daily
  out.daily=fn.ktch.comp(ktch=Data.daily%>%
                                filter(METHOD%in%c('GN',"LL"))%>%
                                dplyr::select(Same.return.SNo,VESSEL,FINYEAR,METHOD,zone,block10,BLOCKX,SPECIES,SNAME,LIVEWT.c,MONTH),
                          what="_Daily",
                          Min.overlap=10)
      #monthly
  Scalefish.years=unique(Data.monthly%>%filter(SPECIES>35000)%>%pull(FINYEAR)) #use only year since scalefish were reported to make it comparable
  if(do.monthly)
  {
    out.monthly=fn.ktch.comp(ktch=Data.monthly%>%
                               filter(METHOD%in%c('GN',"LL") & YEAR<=2005 & FINYEAR%in%Scalefish.years)%>%
                               dplyr::select(Same.return,VESSEL,FINYEAR,METHOD,zone,BLOCKX,SPECIES,SNAME,LIVEWT.c,MONTH),
                             what="_Monthly",
                             Min.overlap=18)
  }

              
    #3.2. Multivariate stats
      #Monthly
  if(do.monthly)
  {
    system.time({
      Out.multi_monthly=multivariate.fn(d=Data.monthly%>%filter(YEAR<=2005 & FINYEAR%in%Scalefish.years),
                                        Terms=c('method.zone','finyear'),
                                        Def.sp.term=c('method','species'),
                                        Transf='proportion',
                                        Show.term='method.zone',
                                        Group='Top20',
                                        hndl,
                                        MDS.title="/Catch_composition/MDS_monthly",
                                        Simper.title="/Catch_composition/Simper_monthly",
                                        Permanova.title="/Catch_composition/Permanova_monthly")
    })
    
    #comparable records
    system.time({
      Out.multi_daily_comparable.records=multivariate.fn(d=Data.monthly%>%
                                                           mutate(BLK={if("block10" %in% names(.)) block10 else BLOCKX},
                                                                  dummy=paste(BLK,MONTH,FINYEAR))%>%
                                                           filter(!zone=='Zone1' &
                                                                    dummy%in%out.monthly$same.BLK.MONTH.FINYEAR),
                                                         Terms=c('method.zone','finyear','month'),
                                                         Def.sp.term=c('method','species'),
                                                         Transf='proportion',
                                                         Show.term='method.zone',
                                                         Group='Top20',
                                                         hndl,
                                                         MDS.title="/Catch_composition/MDS_monthly_same.blk.mn.yr",
                                                         Simper.title="/Catch_composition/Simper_monthly_same.blk.mn.yr",
                                                         Permanova.title="/Catch_composition/Permanova_monthly_same.blk.mn.yr")
    }) 
  }

      #Daily
  system.time({
    Out.multi_daily=multivariate.fn(d=Data.daily%>%
                                      filter(!zone=='Zone1'),
                                   Terms=c('method.zone','finyear','month'),
                                   Def.sp.term=c('method','species'),
                                   Transf='proportion',
                                   Show.term='method.zone',
                                   Group='Top20',
                                   hndl,
                                   MDS.title="/Catch_composition/MDS_daily",
                                   Simper.title="/Catch_composition/Simper_daily",
                                   Permanova.title="/Catch_composition/Permanova_daily")
  }) 
  
      #Daily without vessels targeting snapper
  do.this=FALSE
  if(do.this)
  {
    system.time({
      Out.multi_daily_no.snapper.targeting.vessels=multivariate.fn(d=Data.daily%>%
                                                                     filter(!zone=='Zone1' &
                                                                              !VESSEL%in%out.daily$Snapper.targeting.vessel),
                                                                   Terms=c('method.zone','finyear','month'),
                                                                   Def.sp.term=c('method','species'),
                                                                   Transf='proportion',
                                                                   Show.term='method.zone',
                                                                   Group='Top20',
                                                                   hndl,
                                                                   MDS.title="/Catch_composition/MDS_daily_no.snapper.targeting.vessels",
                                                                   Simper.title="/Catch_composition/Simper_daily_no.snapper.targeting.vessels",
                                                                   Permanova.title="/Catch_composition/Permanova_daily_no.snapper.targeting.vessels")
    }) 
  }
  
      #Daily comparable records
  system.time({
    Out.multi_daily_comparable.records=multivariate.fn(d=Data.daily%>%
                                                         mutate(BLK={if("block10" %in% names(.)) block10 else BLOCKX},
                                                                dummy=paste(BLK,MONTH,FINYEAR))%>%
                                                         filter(!zone=='Zone1' &
                                                                  dummy%in%out.daily$same.BLK.MONTH.FINYEAR),
                                                       Terms=c('method.zone','finyear','month'),
                                                       Def.sp.term=c('method','species'),
                                                       Transf='proportion',
                                                       Show.term='method.zone',
                                                       Group='Top20',
                                                       hndl,
                                                       MDS.title="/Catch_composition/MDS_daily_same.blk.mn.yr",
                                                       Simper.title="/Catch_composition/Simper_daily_same.blk.mn.yr",
                                                       Permanova.title="/Catch_composition/Permanova_daily_same.blk.mn.yr")
  }) 

  
  
  #4. Hook characteristics (longline only)
  d=Effort.monthly%>%
    filter(METHOD=="LL")%>%
    dplyr::select(c(Same.return,HOURS.c,HOOKS,SHOTS.c))%>%
    mutate(dat="Monthly")%>%
    distinct(Same.return,.keep_all=T)
  d1=Data.daily.original%>%
    filter(METHOD=="LL")%>%
    dplyr::select(c(DSNo,TSNo,SNo,HOURS,HOOKS,SHOTS,
                    HookSize,HookType))%>%
    mutate(dat="Daily",Same.return.SNo=paste(DSNo,TSNo,SNo))%>%
    distinct(Same.return.SNo,.keep_all=T)
  colnames(d)=tolower(colnames(d))
  colnames(d1)=tolower(colnames(d1))
  d1=d1%>%
    mutate(hours.c=hours,
           shots.c=shots,
           hooksize=as.numeric(hooksize),
           hooktype=case_when(hooktype%in%c("Ezi-Baiter")~"Ezi-baiter",
                              hooktype%in%c("Tuna-Circle","tuna circle")~"Tuna-circle"))
  
  fn.fig(paste(hndl,"/longline only/longline.number of hooks_daily only",sep=""),2400,1600)
  rbind(d%>%dplyr::select(dat,hooks),d1%>%dplyr::select(dat,hooks)) %>% 
    gather(key=dat, value=hooks) %>% 
    filter(dat=="Daily")%>%
    ggplot(aes(x=hooks,fill=dat)) +
    geom_histogram(position="dodge",binwidth=100)+
    scale_fill_manual(values=c("darksalmon", "steelblue"))+
    xlab('Number of hooks per session')+ylab('Frequency')+ guides(fill=guide_legend(title="Data set"))+
    theme_PA(axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position="none",
          plot.margin=margin(.1,.5,.1,.1, "cm"))
  dev.off()
  
  d1.GN=Data.daily.original%>%
    filter(METHOD=="GN" & NETLEN>100)%>%
    dplyr::select(c(DSNo,TSNo,SNo,HOURS,SHOTS,NETLEN))%>%
    mutate(dat="Daily",Same.return.SNo=paste(DSNo,TSNo,SNo))%>%
    distinct(Same.return.SNo,.keep_all=T)%>%
    mutate(hours.c=HOURS)%>%
    filter(!is.na(SHOTS))
  colnames(d1.GN)=tolower(colnames(d1.GN))
    
  
  ddx=rbind(d1%>%dplyr::select(dat,hours.c,shots)%>%mutate(method='LL'),
        d1.GN%>%dplyr::select(dat,hours.c,shots)%>%mutate(method='GN'))%>%
        mutate(shots=ifelse(shots>2,">2",shots),
               shots=factor(shots,levels=c("1","2",">2")))
  fn.fig(paste(hndl,"/longline only/longline_gillnet.soak times_daily only",sep=""),2400,1600)
  ddx%>%
    ggplot(aes(x=hours.c,fill=method)) +
    facet_wrap(~method, scales="free")+
    geom_histogram(position="dodge",binwidth=1)+
    scale_fill_manual(values=c("#F8766D", "#00BFC4"))+
    xlab('Soaking hours per session')+ylab('Frequency')+ guides(fill=guide_legend(title="Data set"))+
    theme_PA(axs.t.siz=14,str.siz=14,axs.T.siz=16)+
    theme(legend.position="none")+
     xlim(0,quantile(ddx$hours.c,probs=0.999))
  dev.off()
  
  fn.fig(paste(hndl,"/longline only/longline.hook size_daily only",sep=""),2400,1600)
  subset(d1,hooksize>0)%>%
    ggplot(aes(x=hooksize)) + 
    geom_histogram(position="dodge",binwidth=1,fill="darksalmon")+
    xlab('Hook size')+ylab('Frequency')+
    scale_x_continuous(breaks=6:15,
                       labels=6:15)+
    theme_PA(axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position="none",
          legend.title=element_blank())
  dev.off()
  
  fn.fig(paste(hndl,"/longline only/longline.hook type_daily only",sep=""),2800,1600)
  ggplot(subset(d1,!is.na(hooktype)|hooktype==''), aes(hooktype)) + geom_bar(fill="darksalmon")+
    theme_PA(axs.t.siz=16,axs.T.siz=18)+ylab('Frequency')+xlab('Hook type')
  dev.off()
  
  
  
  #5. Catch rates 
    #5.1 Catch rates by  method for main species
  #note:this has positive and 0 catch records
  fn.catchrate=function(d1,d2)
  {
    main.sp=rbind(d1%>%dplyr::select(species,livewt),d2%>%dplyr::select(species,livewt))%>%
      group_by(species)%>%
      summarise(Tot=sum(livewt,na.rm=T))%>%
      arrange(-Tot)%>%
      mutate(Cum=cumsum(Tot),
             Per=100*Cum/sum(Tot))%>%
      filter(Per<90)%>%pull(species)
    
    main.sp=c(main.sp,c(353001,320000,377004))
    main.sp=unique(main.sp)
    main.sp=as.numeric(names(Main.species))   #just stick to these ones for consistency
    d3=rbind(d1%>%filter(species%in%main.sp),
             d2%>%filter(species%in%main.sp))%>%
      filter(!is.na(cpue)|!cpue==Inf)%>%
      mutate(cpue1=ifelse(method=="GN",cpue*cpue.scaler,
                   ifelse(method=="LL",cpue*cpue.scaler,
                   NA)))%>%
      dplyr::select(Same.return.SNo,SNAME,cpue1,zone,method,VESSEL,
                    depthMax,HookType,HookSize)%>%
      spread(SNAME,cpue1,fill=0)%>%
      gather(key = "SNAME", value = "cpue1", -c(zone,method,Same.return.SNo,VESSEL,
                                                depthMax,HookType,HookSize))
    
    d3=d3%>%left_join(d2%>%
                        distinct(SNAME,species)%>%
                        mutate(colr=ifelse(species<50000,"steelblue","firebrick")),
                      by='SNAME')
    d3=subset(d3,!is.infinite(cpue1))
    upper=quantile(d3$cpue1,probs=.995)  
    
    #By methodzone
    d3=d3%>%
      mutate(zone=ifelse(zone=='1','zone 1',ifelse(zone=='2','zone 2',zone)),
             method.zone=paste(method,zone))
    d3=d3%>%
      mutate(SNAME=ifelse(SNAME=="Blue morwong","Queen snapper",
                   ifelse(SNAME=="Western blue groper","Blue groper",
                   SNAME)),
             SNAME=factor(SNAME,levels=c("Dusky shark","Whiskery shark",
                                         "Gummy shark","Sandbar shark",
                                         "Queen snapper","Blue groper",
                                         "West Australian dhufish","Pink snapper")))
    
    p=d3%>%
      ggplot(aes(x=method.zone, y=cpue1)) + 
      geom_boxplot(outlier.size = 0.1,aes(fill=colr),alpha=.4)+ 
      coord_flip()+
      #coord_flip(ylim = c(0, upper))+
      facet_wrap(~SNAME,scales='free',nrow=4)+
      scale_fill_manual(values=c(firebrick="firebrick",steelblue="steelblue"))+
      ylab(paste("Catch rate per session (kg per ",cpue.scaler," m hour or ",cpue.scaler," hook hour)",sep=''))+
      xlab("")+ 
      theme_PA(strx.siz=12,axs.t.siz=10.75)+
      theme(legend.position="none",
            plot.margin = margin(.1, .5, .1, .1, "cm"))+
      stat_summary(fun=mean, geom="point", shape=21, size=2, color="darkgreen", fill="darkgreen")+
      scale_y_sqrt()
    print(p)
    
    return(d3)
  }
  
  #Daily
    #a. boxplot of observed cpues
  add.nms=Data.daily%>%distinct(SPECIES,SNAME)%>%distinct(SPECIES,.keep_all=T)
  Data.daily.original=Data.daily.original%>%
                      mutate(HookType=ifelse(HookType%in%c('Tuna-Circle','tuna circle','circle'),'Tuna-circle',
                                    ifelse(HookType%in%c('Ezi-Baiter'),'Ezi-baiter',
                                    ifelse(HookType%in%c(""),NA,
                                    HookType))),
                              HookSize=ifelse(HookSize%in%c("",".","0"),NA,HookSize),
                              HookSize=as.numeric(HookSize))
  d1=Data.daily.original%>%
    filter(METHOD=="LL" & !is.na(species) &!is.na(HOOKS) & !is.na(HOURS))%>%
    mutate(Same.return.SNo=paste(DSNo,TSNo,SNo),
           zone=case_when(zone=='*'~'west',
                          TRUE~zone))%>%
    dplyr::select(c(Same.return.SNo,HOURS,HOOKS,VESSEL,
                    livewt,species,zone,depthMax,HookType,HookSize))%>%
    group_by(Same.return.SNo,species,zone,VESSEL,depthMax,HookType,HookSize)%>%
    summarise(livewt=sum(livewt),
              HOURS=max(HOURS),
              HOOKS=max(HOOKS))%>%
    mutate(cpue=livewt/(HOURS*HOOKS))%>%
    left_join(add.nms,by=c("species"='SPECIES'))%>%
    mutate(colr=ifelse(species<50000,"steelblue","firebrick"),
           SNAME=case_when(species==18013~"Spot-tail shark",
                           species==18014~"Blacktip sharks",
                           species==18029~"Lemon shark",
                           species==18026~"Pigeye shark",
                           species==18030~"Grey reef shark",
                           TRUE~SNAME),
           method='LL')%>%
    dplyr::select(-c(HOURS,HOOKS))%>%
    ungroup()
  
  d2=Data.daily.original%>%
    filter(METHOD=="GN" & !is.na(species) &!is.na(NETLEN) & !is.na(HOURS))%>%
    mutate(Same.return.SNo=paste(DSNo,TSNo,SNo),
           zone=case_when(zone=='*'~'west',
                          TRUE~zone))%>%
    dplyr::select(Same.return.SNo,HOURS,VESSEL,
                  NETLEN,livewt,species,zone,depthMax,HookType,HookSize)%>%
    group_by(Same.return.SNo,species,zone,VESSEL,depthMax,HookType,HookSize)%>%
    summarise(livewt=sum(livewt),
              HOURS=max(HOURS),
              NETLEN=max(NETLEN))%>%
    mutate(cpue=livewt/(HOURS*NETLEN))%>%
    left_join(add.nms,by=c("species"='SPECIES'))%>%
    mutate(colr=ifelse(species<50000,"steelblue","firebrick"),
           method='GN')%>%
    dplyr::select(-c(HOURS,NETLEN))%>%
    ungroup()
  
  d1=d1[match(names(d2),names(d1))]
  
  fn.fig(paste(hndl,"/catch_rates/Catch rates by method and zone for main species_daily_y_sqrt",sep=""),2600,2000)
  daily.cpue=fn.catchrate(d1,d2) 
  dev.off()
  
    #b. standardization
  names(daily.cpue)=tolower(names(daily.cpue))
  daily.cpue=daily.cpue%>%
    filter(!is.na(depthmax) & depthmax<200)%>%
    filter(!zone=='zone 1')   #almost no longline records in zone 1

      #b.1 fit GAM
  main.sp=as.numeric(names(Main.species))
  cpue.stand.out.GN=vector('list',length(main.sp))
  names(cpue.stand.out.GN)=main.sp
  cpue.stand.out.LL=cpue.stand.out.GN
  GN.form=formula(cpue1~zone+s(vessel,bs='re'))
  LL.form=formula(cpue1~zone+hooksize+hooktype+s(vessel,bs='re'))
  #GN.form=formula(cpue1~zone+s(depthmax)+s(vessel,bs='re'))
  #LL.form=formula(cpue1~zone+s(depthmax)+hooksize+hooktype+s(vessel,bs='re'))

  system.time({  
    for(m in 1:length(main.sp))
    {
      #gillnets
      print(paste(main.sp[m],'GN GAM-------------'))
      D=daily.cpue%>%
        filter(method=='GN' & species==main.sp[m])%>%
        mutate(zone=as.factor(zone),
               method=as.factor(method),
               vessel=as.factor(vessel))
      cpue.stand.out.GN[[m]]=cpue.stand.fun(d=D,Formula=GN.form)  
      
      #longlines
      print(paste(main.sp[m],'LL GAM-------------'))
      D=daily.cpue%>%
        filter(method=='LL' & species==main.sp[m] & hooksize>11)%>%
        filter(!is.na(hooktype))%>%
        mutate(zone=as.factor(zone),
               method=as.factor(method),
               vessel=as.factor(vessel),
               hooksize=factor(hooksize),
               hooktype=as.factor(hooktype))
      form=LL.form
      if(main.sp[m]==17001) form=formula(cpue1~zone+hooktype+s(vessel,bs='re'))
      cpue.stand.out.LL[[m]]=cpue.stand.fun(d=D,Formula=form)  
      
      
      #Pink snapper without targeting vessels in West coast
      #note: It cannot be run because when removing the three 
      #       targeting vessels (out.daily$Snapper.targeting.vessel),
      #       there are no records left in West coast...
      do.snpr.targ=FALSE
      if(do.snpr.targ)
      {
        if(main.sp[m]==353001)
        {
          D=daily.cpue%>%
            filter(method=='GN' & species==main.sp[m])%>%
            filter(!vessel%in%out.daily$Snapper.targeting.vessel)%>%
            mutate(zone=as.factor(zone),
                   method=as.factor(method),
                   vessel=as.factor(vessel))
          Pink_snapper_not.targeting.GN=cpue.stand.fun(d=D,Formula=GN.form)  
          
          D=daily.cpue%>%
            filter(method=='LL' & species==main.sp[m] & hooksize>11)%>%
            filter(!vessel%in%out.daily$Snapper.targeting.vessel)%>%
            filter(!is.na(hooktype))%>%
            mutate(zone=as.factor(zone),
                   method=as.factor(method),
                   vessel=as.factor(vessel),
                   hooksize=factor(hooksize),
                   hooktype=as.factor(hooktype))
          form=LL.form
          Pink_snapper_not.targeting.LL=cpue.stand.fun(d=D,Formula=form)  
          
        }
      }
    }
  })   #takes 2 minutes
  #gam.check(cpue.stand.out[[m]])
  
      #b.2 export anova table
  for(m in 1:length(main.sp))
  {
    NM=Main.species[match(main.sp[m],names(Main.species))]
    
    #GN
    ft <- as_flextable(cpue.stand.out.GN[[m]]$mod)
    save_as_image(ft, path = paste(hndl,'catch_rates/GAM_Anova',paste(NM,'.GN.png',sep=''),sep='/'))
    save_as_docx(ft,path = paste(hndl,'catch_rates/GAM_Anova/',paste(NM,'.GN.docx',sep=''),sep='/'))
    rm(ft)
    
    #LL
    ft <- as_flextable(cpue.stand.out.LL[[m]]$mod)
    save_as_image(ft, path = paste(hndl,'catch_rates/GAM_Anova',paste(NM,'.LL.png',sep=''),sep='/'))
    save_as_docx(ft,path = paste(hndl,'catch_rates/GAM_Anova/',paste(NM,'.LL.docx',sep=''),sep='/'))
    
    rm(ft,NM)
  }
  
      #b.3 display term effect
  Store.zone.method=vector('list',length(main.sp))
  names(Store.zone.method)=main.sp
  Store.hook.type=Store.hook.size=Store.depth=Store.zone.method
  
  Get.hook.type.Pred=c(18003,353001,17003) #only significant for these species
  Get.hook.size.Pred=c(18007,320000)
  for(m in 1:length(main.sp))
  {
    SP=Main.species[match(main.sp[m],names(Main.species))]
      
    #zone-method effect                        
    GN=pred.fun(mod=cpue.stand.out.GN[[m]]$mod,biascor="NO",PRED=c('zone'))%>%
              mutate(method="GN",
                     species=SP)
    LL=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('zone'))%>%
              mutate(method="LL",
                     species=SP)
    Store.zone.method[[m]]=rbind(GN,LL)
    
    #depth effect
    # GN=pred.fun.continuous(d=cpue.stand.out.GN[[m]]$dat,
    #                        mod=cpue.stand.out.GN[[m]]$mod,
    #                        PRED='depthmax',
    #                        Formula=formula(cpue1~zone+s(depthmax)+s(vessel,bs='re')))%>%
    #           mutate(method="GN",
    #                  species=SP)
    # LL=pred.fun.continuous(d=cpue.stand.out.LL[[m]]$dat,
    #                        mod=cpue.stand.out.LL[[m]]$mod,
    #                        PRED='depthmax',
    #                        Formula=formula(cpue1~zone+s(depthmax)+hooksize+hooktype+s(vessel,bs='re')))%>%
    #           mutate(method="LL",
    #                  species=SP)
    # Store.depth[[m]]=rbind(GN,LL)
    
    #Hook type
    if(main.sp[m]%in%Get.hook.type.Pred)
    {
      Store.hook.type[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('hooktype'))%>%
                              mutate(species=SP)
    }
    
    #Hook size
    if(main.sp[m]%in%Get.hook.size.Pred)
    {
      Store.hook.size[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('hooksize'))%>%
                              mutate(species=SP)
    }
    
  }
  
  #Method-zone 
  fn.fig(paste(hndl,"/catch_rates/Standardized_Catch rates_Method.zone_daily_y_sqrt",sep=""),2600,1600)
  out=fn.barplot.cpue(d=do.call(rbind,Store.zone.method)%>%
                              mutate(method.zone=paste(method,zone,sep='-'),
                                     species=factor(species,levels=Main.species),
                                     x=method.zone,
                                     y=response,
                                     fill=method.zone,
                                     facet=species),
                            YLAB=fn.tit(cpue.scaler),
                            XLAB='Fishing method and zone',
                            cex=10.5,
                            Rotate='Yes',
                           Relative="No",
                           Y_sqrt="YES")
  dev.off()
  write.csv(out,paste(hndl,"/catch_rates/Standardized_Catch rates_Method.zone_daily.csv",sep=""),row.names = F)
  
  #Depth 
  # fn.fig(paste(hndl,"/catch_rates/Standardized_Catch rates_depth_daily",sep=""),2400,1400)
  # fn.continuous.cpue(d=do.call(rbind,Store.depth)%>%
  #                                  mutate(species=factor(species,levels=Main.species),
  #                                         x=depthmax,
  #                                         y=response,
  #                                         fill=method,
  #                                         facet=species),
  #                                YLAB="Relative standardized catch rate",
  #                                XLAB='Depth (m)',
  #                                cex=10,
  #                                Relative="Yes")
  # dev.off()
  
  
  #Hooksize
  fn.fig(paste(hndl,"/catch_rates/Standardized_Catch rates_hooksize_daily",sep=""),2000,2400)
  out=fn.barplot.cpue(d=do.call(rbind,Store.hook.size)%>%
                    mutate(species=factor(species,levels=Main.species),
                           x=hooksize,
                           y=response,
                           fill=hooksize,
                           facet=species),
                  YLAB="Relative standardized catch rate",
                  XLAB='Hook size',
                  cex=16,
                  Relative="Yes")
  dev.off()
  
  
  #Hooktype
  fn.fig(paste(hndl,"/catch_rates/Standardized_Catch rates_hooktype_daily",sep=""),2000,2400)
  out=fn.barplot.cpue(d=do.call(rbind,Store.hook.type)%>%
                    mutate(species=factor(species,levels=Main.species),
                           x=hooktype,
                           y=response,
                           fill=hooktype,
                           facet=species),
                  YLAB="Relative standardized catch rate",
                  XLAB='Hook type',
                  cex=16,
                  Relative="Yes")
  dev.off()
  

   
  #6. Average catch price per vessel-gear for last five years
  Lst.fiv.yrs=as.numeric(substr(Current.yr,1,4))
  Lst.fiv.yrs=seq(Lst.fiv.yrs-4,Lst.fiv.yrs)
  Lst.fiv.yrs=paste(Lst.fiv.yrs,substr(Lst.fiv.yrs+1,3,4),sep='-')
  a=Data.daily%>%
    filter(FINYEAR%in%Lst.fiv.yrs & METHOD%in%c("GN","LL"))%>%
    group_by(Same.return.SNo,VESSEL,SPECIES,METHOD)%>%
    summarise(Total.ktch = sum(LIVEWT.c))
  a=left_join(a,PRICES,by="SPECIES")%>%
    mutate(ktch.price=dolar.per.kg*Total.ktch)%>%
    group_by(Same.return.SNo,VESSEL,METHOD)%>%
    summarise(Catch.value=sum(ktch.price))
  
  # b=Effort.daily%>%filter(finyear%in%Lst.fiv.yrs & method%in%c("GN","LL"))%>%
  #   group_by(Same.return.SNo,vessel,method)%>%
  #   summarise(Effort = ifelse(method=="GN",max(Km.Gillnet.Hours.c),NA))
  
  # d=left_join(a,b,by=c("Same.return.SNo"))%>%
  #   group_by(vessel)%>%
  #   summarise(average.ktch.value=mean(Catch.value,na.rm=T),
  #             min.ktch.value=min(Catch.value,na.rm=T),
  #             max.ktch.value=max(Catch.value,na.rm=T),
  #             average.effort=mean(Effort,na.rm=T))%>%
  #   data.frame
  
  fn.fig(paste(hndl,"/Catch value per session per vessel for last 5 years",sep=""),2800,1600)
   ggplot(a, aes(x=VESSEL, y=Catch.value,colour=METHOD)) + 
    geom_boxplot()+ coord_flip()+
    scale_x_discrete(breaks=d$VESSEL,labels=1:nrow(d))+
    ylab("Catch value ($) per session")+ 
    theme(legend.position="top",
          legend.title=element_blank())
  dev.off()
  
  
  #7. Tim Nicholas request
  do.Tims=FALSE
  if(do.Tims)
  {
    South.WA.lat=c(-36,-25); South.WA.long=c(112,130)
    PLATE=c(.01,.9,.075,.9)
    Yrs=c("2017-18","2018-19")  
    aa= Data.daily.original%>%filter(FINYEAR%in%Yrs) %>%
      mutate(LatDeg=as.numeric(substr(block10,1,2)),
             LatMin=ifelse(is.na(LatMin),10*as.numeric(substr(block10,3,3)),LatMin),
             Lat=-abs(LatDeg+(LatMin/60)),
             LongDeg=ifelse(is.na(LongDeg),100+as.numeric(substr(block10,4,5)),LongDeg), 
             LongMin=ifelse(is.na(LongMin),10*as.numeric(substr(block10,6,6)),LongMin),
             Long=LongDeg+(LongMin/60))%>%
      filter(Lat<=(-26) & Lat>(-36.5)& Long<=(129) & Long >(111.9))
    
    numInt=20
    couleurs=rev(heat.colors(numInt)) 
    tcl.1=.5
    tcl.2=.5
    Long.seq=seq(South.WA.long[1]+1,South.WA.long[2]-1,by=3)
    Lat.seq=c(-26,-28,-30,-32,-34)
    numberLab=5
    colLeg=(rep(c("black",rep("transparent",numberLab-1)),(numInt+1)/numberLab))
    
    #lodged returns
    TAB1=aa %>% filter(FINYEAR%in%Yrs) %>%
      group_by(FINYEAR)%>%
      summarise(Unique_TSNo=n_distinct(TSNo))%>%
      data.frame
    
    TAB2=aa %>% filter(FINYEAR%in%Yrs) %>%
      group_by(FINYEAR,METHOD)%>%
      summarise(Unique_TSNo=n_distinct(TSNo))%>%
      data.frame
    
    #Who's been using hooks?
    TAB3_trips=aa%>%group_by(METHOD,FINYEAR,VESSEL)%>%
      summarise(Trips=n_distinct(TSNo))%>%
      spread(METHOD, Trips,fill=0)%>%
      data.frame
    
    
    bb=aa%>%filter(METHOD=="LL")%>%
      distinct(Same.return.SNo,.keep_all =T) %>%
      dplyr::select(VESSEL,BoatName,MastersName,port,block10,FINYEAR,MONTH,bioregion,Lat,Long,depthMax,
                    NilCatch,species,nfish,livewt,
                    HookSize,HookType,HOOKS,HOURS,nlines,SHOTS)
    
    TAB4=bb%>%group_by(VESSEL,BoatName,MastersName,port)%>%
      summarise(mean.hook.n=mean(HOOKS,na.rm=T),
                mean.hook.size=mean(HookSize,na.rm=T),
                mean.hook.hours=mean(HOURS,na.rm=T))%>%
      replace(is.na(.), "")%>%
      data.frame
    mytheme <- gridExtra::ttheme_default(
      base_size = 10,
      core = list(padding=unit(c(1, 1), "mm"),fg_params=list(cex = .65)),
      colhead = list(fg_params=list(cex = .75)),
      rowhead = list(fg_params=list(cex = .75)))
    
    pdf(file=paste(hndl,"/Parks_Australia_2018-19.effort_catch.pdf",sep=""))
    
    grid.draw(gridExtra::tableGrob(TAB3_trips, theme = mytheme,rows = NULL))
    grid.newpage()
    
    grid.draw(gridExtra::tableGrob(TAB4, theme = mytheme,rows = NULL))
    
    #effort
    b=aa %>% filter(METHOD=="GN") %>%
      mutate(Km.Gillnet.Hours=HOURS*NETLEN/1000)%>%
      filter(Km.Gillnet.Hours>0)%>%
      group_by(Same.return.SNo,FINYEAR, block10) %>%
      summarize(Km.Gillnet.Hours = max(Km.Gillnet.Hours, na.rm = TRUE))%>%
      group_by(FINYEAR, block10) %>%
      summarize(sum = sum(Km.Gillnet.Hours, na.rm = TRUE))%>%
      mutate(LatDeg=as.numeric(substr(block10,1,2)),
             LatMin=10*as.numeric(substr(block10,3,3)),
             Lat=-abs(LatDeg+(LatMin/60)),
             LongDeg=100+as.numeric(substr(block10,4,5)), 
             LongMin=10*as.numeric(substr(block10,6,6)),
             Long=LongDeg+(LongMin/60))%>%
      data.frame
    b=subset(b,select=c(FINYEAR,sum,Lat,Long))
    BREAKS=quantile(b$sum,probs=seq(0,1,1/numInt),na.rm=T)
    
    par(mfrow=c(2,1),mai = c(0.3, 0.4, 0.15, 0.2),oma = c(0.5, 0.4, 0.2, 0.1),mgp=c(.1, 0.15, 0))
    for(y in 1:length(Yrs))
    {
      bb=subset(b,FINYEAR==Yrs[y],select=-FINYEAR)%>%
        arrange(Lat,Long)
      long=sort(unique(bb$Long))
      lat=sort(unique(bb$Lat))      
      Reshaped=as.matrix(reshape(bb,idvar="Long",timevar="Lat",v.names="sum", direction="wide"))	
      Reshaped=Reshaped[order(Reshaped[,1]),]
      Reshaped=Reshaped[,-1]	
      
      a=South.WA.long[1]:South.WA.long[2]
      bx=seq(South.WA.lat[1],South.WA.lat[2],length.out=length(a))
      plotmap(a,bx,PLATE,"transparent",South.WA.long,South.WA.lat)
      image(long,lat,z=Reshaped,xlab="",ylab="",col =couleurs,breaks=BREAKS,axes = FALSE,add=T)			
      axis(side = 1, at =South.WA.long[1]:South.WA.long[2], labels = F, tcl = tcl.1)
      axis(side = 2, at = South.WA.lat[2]:South.WA.lat[1], labels = F,tcl =tcl.1)
      axis(side = 1, at =Long.seq, labels = Long.seq, tcl = .35,las=1,cex.axis=1,padj=-.15)
      axis(side = 2, at = Lat.seq, labels = -Lat.seq,tcl = .35,las=2,cex.axis=1,hadj=1.1)
      if(y==2)color.legend(129.5,South.WA.lat[2],South.WA.long[2],-33,round(BREAKS,0),
                           rect.col=couleurs,gradient="y",col=colLeg,cex=0.85)
      nnn=with(TAB2%>%filter(FINYEAR==Yrs[y]),paste(paste(Yrs[y]," (gillnet returns= ",Unique_TSNo[1],"; longline returns= ",Unique_TSNo[2],")",sep="")))
      mtext(nnn,3,-2)
    }
    mtext(expression(paste("Latitude (",degree,"S)",sep="")),side=2,line=-1,las=3,cex=1.1,outer=T)
    mtext(expression(paste("Longitude (",degree,"E)",sep="")),side=1,line=-.5,cex=1.1,outer=T)
    mtext("Effort (Km.gn.hours)",3,-0.75,outer=T)
    
    #catch
    for(s in 1:length(TARGETS))
    {
      b=aa %>% filter(METHOD=="GN" & species%in%TARGETS[[s]]) %>%
        group_by(FINYEAR, block10) %>%
        summarize(sum = sum(livewt, na.rm = TRUE))%>%
        mutate(LatDeg=as.numeric(substr(block10,1,2)),
               LatMin=10*as.numeric(substr(block10,3,3)),
               Lat=-abs(LatDeg+(LatMin/60)),
               LongDeg=100+as.numeric(substr(block10,4,5)), 
               LongMin=10*as.numeric(substr(block10,6,6)),
               Long=LongDeg+(LongMin/60))%>%
        data.frame
      b=subset(b,select=c(FINYEAR,sum,Lat,Long))
      BREAKS=quantile(b$sum,probs=seq(0,1,1/numInt),na.rm=T)
      
      par(mfrow=c(2,1),mai = c(0.3, 0.4, 0.15, 0.2),oma = c(0.5, 0.4, 0.2, 0.1),mgp=c(.1, 0.15, 0))
      for(y in 1:length(Yrs))
      {
        bb=subset(b,FINYEAR==Yrs[y],select=-FINYEAR)%>%
          arrange(Lat,Long)
        long=sort(unique(bb$Long))
        lat=sort(unique(bb$Lat))      
        Reshaped=as.matrix(reshape(bb,idvar="Long",timevar="Lat",v.names="sum", direction="wide"))	
        Reshaped=Reshaped[order(Reshaped[,1]),]
        Reshaped=Reshaped[,-1]	
        
        a=South.WA.long[1]:South.WA.long[2]
        bx=seq(South.WA.lat[1],South.WA.lat[2],length.out=length(a))
        plotmap(a,bx,PLATE,"transparent",South.WA.long,South.WA.lat)
        image(long,lat,z=Reshaped,xlab="",ylab="",col =couleurs,breaks=BREAKS,axes = FALSE,add=T)			
        axis(side = 1, at =South.WA.long[1]:South.WA.long[2], labels = F, tcl = tcl.1)
        axis(side = 2, at = South.WA.lat[2]:South.WA.lat[1], labels = F,tcl =tcl.1)
        axis(side = 1, at =Long.seq, labels = Long.seq, tcl = .35,las=1,cex.axis=1,padj=-.15)
        axis(side = 2, at = Lat.seq, labels = -Lat.seq,tcl = .35,las=2,cex.axis=1,hadj=1.1)
        if(y==2)color.legend(129.5,South.WA.lat[2],South.WA.long[2],-33,round(BREAKS,0),
                             rect.col=couleurs,gradient="y",col=colLeg,cex=0.85)
        mtext(Yrs[y],3,-2)
      }
      mtext(expression(paste("Latitude (",degree,"S)",sep="")),side=2,line=-1,las=3,cex=1.1,outer=T)
      mtext(expression(paste("Longitude (",degree,"E)",sep="")),side=1,line=-.5,cex=1.1,outer=T)
      mtext(paste(names(TARGETS)[s],"(catch in kg)"),3,-.75,outer=T)
      
      
    }
    
    #all scalefish
    {
      b=aa %>% filter(METHOD=="GN" & species%in%Scalefish.species) %>%
        group_by(FINYEAR, block10) %>%
        summarize(sum = sum(livewt, na.rm = TRUE))%>%
        mutate(LatDeg=as.numeric(substr(block10,1,2)),
               LatMin=10*as.numeric(substr(block10,3,3)),
               Lat=-abs(LatDeg+(LatMin/60)),
               LongDeg=100+as.numeric(substr(block10,4,5)), 
               LongMin=10*as.numeric(substr(block10,6,6)),
               Long=LongDeg+(LongMin/60))%>%
        data.frame
      b=subset(b,select=c(FINYEAR,sum,Lat,Long))
      BREAKS=quantile(b$sum,probs=seq(0,1,1/numInt),na.rm=T)
      
      par(mfrow=c(2,1),mai = c(0.3, 0.4, 0.15, 0.2),oma = c(0.5, 0.4, 0.2, 0.1),mgp=c(.1, 0.15, 0))
      for(y in 1:length(Yrs))
      {
        bb=subset(b,FINYEAR==Yrs[y],select=-FINYEAR)%>%
          arrange(Lat,Long)
        long=sort(unique(bb$Long))
        lat=sort(unique(bb$Lat))      
        Reshaped=as.matrix(reshape(bb,idvar="Long",timevar="Lat",v.names="sum", direction="wide"))	
        Reshaped=Reshaped[order(Reshaped[,1]),]
        Reshaped=Reshaped[,-1]	
        
        a=South.WA.long[1]:South.WA.long[2]
        bx=seq(South.WA.lat[1],South.WA.lat[2],length.out=length(a))
        plotmap(a,bx,PLATE,"transparent",South.WA.long,South.WA.lat)
        image(long,lat,z=Reshaped,xlab="",ylab="",col =couleurs,breaks=BREAKS,axes = FALSE,add=T)			
        axis(side = 1, at =South.WA.long[1]:South.WA.long[2], labels = F, tcl = tcl.1)
        axis(side = 2, at = South.WA.lat[2]:South.WA.lat[1], labels = F,tcl =tcl.1)
        axis(side = 1, at =Long.seq, labels = Long.seq, tcl = .35,las=1,cex.axis=1,padj=-.15)
        axis(side = 2, at = Lat.seq, labels = -Lat.seq,tcl = .35,las=2,cex.axis=1,hadj=1.1)
        if(y==2)color.legend(129.5,South.WA.lat[2],South.WA.long[2],-33,round(BREAKS,0),
                             rect.col=couleurs,gradient="y",col=colLeg,cex=0.85)
        mtext(Yrs[y],3,-2)
      }
      mtext(expression(paste("Latitude (",degree,"S)",sep="")),side=2,line=-1,las=3,cex=1.1,outer=T)
      mtext(expression(paste("Longitude (",degree,"E)",sep="")),side=1,line=-.5,cex=1.1,outer=T)
      mtext("Scalefish (catch in kg)",3,-.75,outer=T)
      
      
    }
    dev.off()
    
  }
  
  #8. Number of fishing days to catch one tonne of fish
  
    #8.1 Fit model to catch for shot=1
  hndl.catch.per.shot=handl_OneDrive('Analyses/Parks Australia/outputs/Socio-economics/catch.per.shot') 
    #Longlines
  dis.hook.size=Data.daily.original%>%
    filter(METHOD=="LL" & !is.na(species) &!is.na(HOOKS) & !is.na(HOURS) & SHOTS==1)%>%
    mutate(Same.return.SNo=paste(DSNo,TSNo,SNo))%>%
    distinct(Same.return.SNo,.keep_all=T)%>%
    group_by(HookSize)%>%
    tally()
  dis.hook.size=dis.hook.size%>%filter(n==max(n,na.rm=T))%>%pull(HookSize)
  
  dis.hook.type=Data.daily.original%>%
    filter(METHOD=="LL" & !is.na(species) &!is.na(HOOKS) & !is.na(HOURS) & SHOTS==1)%>%
    mutate(Same.return.SNo=paste(DSNo,TSNo,SNo))%>%
    distinct(Same.return.SNo,.keep_all=T)%>%
    group_by(HookType)%>%
    tally()
  dis.hook.type=dis.hook.type%>%filter(n==max(n,na.rm=T))%>%pull(HookType)
  
  Shot.per.ton_LL=Data.daily.original%>%
    filter(METHOD=="LL" & !is.na(species) &!is.na(HOOKS) & !is.na(HOURS) 
           & SHOTS==1 & HookSize==dis.hook.size & !species%in%c(22998,22999))%>%
    filter(HOOKS>=300)%>%
    mutate(Same.return.SNo=paste(DSNo,TSNo,SNo),
           zone=case_when(zone=='*'~'west',
                          TRUE~zone))%>%
    group_by(Same.return.SNo,VESSEL,depthMax,blockx,MONTH)%>%
    summarise(landwt=sum(landwt),
              HOURS=max(HOURS),
              HOOKS=max(HOOKS))%>%
    ungroup()%>%
    mutate(blockx=factor(blockx),
           VESSEL=factor(VESSEL))%>%
    data.frame
  
  Mod.LL=gam(log(landwt)~blockx+s(VESSEL,bs='re')+s(depthMax)+s(MONTH, bs = "cc", k = 12)+s(HOURS)+HOOKS,
             dat=Shot.per.ton_LL,family='gaussian',method="REML")
  save_as_docx(as_flextable(Mod.LL),path = paste(hndl.catch.per.shot,'Model.LL.docx',sep='/'))
  
  fn.fctr=function(d,x) return(factor(names(which.max(table(d[,x]))),levels=levels(d[,x])))
  fn.con=function(d,x) return(round(mean(d[,x])))
  
  Pred.LL=expand.grid(HOOKS=seq(min(100*round(Shot.per.ton_LL$HOOKS/100)),max(round(Shot.per.ton_LL$HOOKS)),by=10),
                      HOURS=seq(1,14,by=1),
                      VESSEL=fn.fctr(d=Shot.per.ton_LL,x='VESSEL'),
                      blockx=fn.fctr(d=Shot.per.ton_LL,x='blockx'),
                      depthMax=fn.con(d=Shot.per.ton_LL,x='depthMax'),
                      MONTH=fn.con(d=Shot.per.ton_LL,x='MONTH'))
  LL.preds=predict(Mod.LL,newdata=Pred.LL,type='response',se.fit=T)
  Pred.LL=Pred.LL%>%
    mutate(fit=LL.preds$fit,
           se=LL.preds$se.fit,
           Catch=exp(fit)*exp(se^2/2),
           lower.CL=exp(fit-1.96*se)*exp(se^2/2),
           upper.CL=exp(fit+1.96*se)*exp(se^2/2))
  
  Pred.LL%>%
    mutate(Wrap.title=factor(paste(HOURS,'hour',sep='-'),levels=paste(1:14,'hour',sep='-')))%>%
    ggplot(aes(HOOKS,Catch,color=Wrap.title))+
    geom_point()+
    geom_line(aes(HOOKS,lower.CL))+
    geom_line(aes(HOOKS,upper.CL))+
    facet_wrap(~Wrap.title)+ylab('Catch (kg)')+
    theme_PA()+
    theme(legend.position = 'none')
  ggsave(paste(hndl.catch.per.shot,'Preds_LL.tiff',sep='/'), width = 10,height = 10,dpi = 300, compression = "lzw")
  

    #Gillnets
  Shot.per.ton_GN=Data.daily.original%>%
    filter(METHOD=="GN" & !is.na(species) &!is.na(NETLEN) & !is.na(HOURS) & SHOTS==1)%>%
    filter(NETLEN>=600 & !species%in%c(22998,22999))%>%
    mutate(Same.return.SNo=paste(DSNo,TSNo,SNo),
           zone=case_when(zone=='*'~'west',
                          TRUE~zone))%>%
    group_by(Same.return.SNo,VESSEL,depthMax,blockx,MONTH)%>%
    summarise(landwt=sum(landwt,na.rm=T),
              HOURS=max(HOURS),
              NETLEN=max(NETLEN))%>%
    ungroup()%>%
    mutate(blockx=factor(blockx),
           VESSEL=factor(VESSEL))%>%
    data.frame
  
  Mod.GN=gam(log(landwt)~blockx+s(VESSEL,bs='re')+s(depthMax)+s(MONTH, bs = "cc", k = 12)+s(HOURS)+NETLEN,
             dat=Shot.per.ton_GN,family='gaussian',method="REML")  #takes 7 mins
  save_as_docx(as_flextable(Mod.GN),path = paste(hndl.catch.per.shot,'Model.GN.docx',sep='/'))
  
  Pred.GN=expand.grid(NETLEN=seq(600,max(round(Shot.per.ton_GN$NETLEN)),by=100),
                      HOURS=seq(1,24,by=1),
                      VESSEL=fn.fctr(d=Shot.per.ton_GN,x='VESSEL'),
                      blockx=fn.fctr(d=Shot.per.ton_GN,x='blockx'),
                      depthMax=fn.con(d=Shot.per.ton_GN,x='depthMax'),
                      MONTH=fn.con(d=Shot.per.ton_GN,x='MONTH'))
  GN.preds=predict(Mod.GN,newdata=Pred.GN,type='response',se.fit=T)
  Pred.GN=Pred.GN%>%
    mutate(fit=GN.preds$fit,
           se=GN.preds$se.fit,
           Catch=exp(fit)*exp(se^2/2),
           lower.CL=exp(fit-1.96*se)*exp(se^2/2),
           upper.CL=exp(fit+1.96*se)*exp(se^2/2))
  
  Pred.GN%>%
    mutate(Wrap.title=factor(paste(HOURS,'hour',sep='-'),levels=paste(1:24,'hour',sep='-')))%>%
    ggplot(aes(NETLEN,Catch,color=Wrap.title))+
    geom_point()+
    geom_line(aes(NETLEN,lower.CL))+
    geom_line(aes(NETLEN,upper.CL))+
    facet_wrap(~Wrap.title)+ylab('Catch (kg)')+
    theme_PA()+
    theme(legend.position = 'none')
  ggsave(paste(hndl.catch.per.shot,'Preds_GN.tiff',sep='/'), width = 10,height = 10,dpi = 300, compression = "lzw")
  

    #8.2 Scale average standardised catch to 1 tonne for most commonly used netlen and hooks and hours
  #note: also consider soak time of whole day
  mode <- function(codes) which.max(tabulate(codes))
  #mode(round(Shot.per.ton_LL$HOOKS))
  Pred.LL.one.tonne=Pred.LL%>%
                    dplyr::select(-c(VESSEL,blockx,depthMax,MONTH,fit,se))%>%
                    filter(HOURS%in%c(4) & 
                           HOOKS==1000)%>%
                    mutate(shot=c(1),
                           Catch.day=shot*Catch,
                           lower.CL.day=shot*lower.CL,
                           upper.CL.day=shot*upper.CL,
                           fishing.day.1.ton_Catch=1000/Catch.day,
                           fishing.day.1.ton_Catch.lower=1000/lower.CL.day,
                           fishing.day.1.ton_Catch.upper=1000/upper.CL.day)%>%
    dplyr::select(HOOKS,HOURS,shot,fishing.day.1.ton_Catch,
                  fishing.day.1.ton_Catch.lower,fishing.day.1.ton_Catch.upper)
  write.csv(Pred.LL.one.tonne,paste(hndl.catch.per.shot,'Ndays_one.tone_LL.csv',sep='/'),row.names=F)
  
  Pred.GN.one.tonne=Pred.GN%>%
                    dplyr::select(-c(VESSEL,blockx,depthMax,MONTH,fit,se))%>%
                    filter(HOURS%in%c(8) & 
                             NETLEN==mode(100*round(Shot.per.ton_GN$NETLEN/100)))%>%
                    mutate(shot=c(2),
                           Catch.day=shot*Catch,
                           lower.CL.day=shot*lower.CL,
                           upper.CL.day=shot*upper.CL,
                           fishing.day.1.ton_Catch=1000/Catch.day,
                           fishing.day.1.ton_Catch.lower=1000/lower.CL.day,
                           fishing.day.1.ton_Catch.upper=1000/upper.CL.day)%>%
    dplyr::select(NETLEN,HOURS,shot,fishing.day.1.ton_Catch,
                  fishing.day.1.ton_Catch.lower,fishing.day.1.ton_Catch.upper)
  write.csv(Pred.GN.one.tonne,paste(hndl.catch.per.shot,'Ndays_one.tone_GN.csv',sep='/'),row.names=F)
  

  #9. Catch composition by fishing method
 Comp_LL=Data.daily.original%>%
    filter(METHOD=="LL" & !is.na(species) & !species%in%c(22998,22999))%>%
   group_by(species)%>%
   summarise(landwt=sum(landwt,na.rm=T))%>%
   ungroup()%>%
   left_join(All.species.names%>%
               distinct(CAAB_code,.keep_all=T)%>%
               dplyr::select(CAAB_code,COMMON_NAME,Taxa),
             by=c('species'='CAAB_code'))%>%
   filter(!is.na(COMMON_NAME))%>%
   mutate(prop=landwt/sum(landwt))
 write.csv(Comp_LL,paste(hndl.catch.per.shot,'Composition_LL.csv',sep='/'),row.names=F)
 Comp_LL%>%
   ggplot(aes(COMMON_NAME,prop,fill=Taxa))+
   geom_col()+
   theme_PA()+
   theme(legend.position = 'top',
         legend.title = element_blank(),
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
   xlab('')+ylab('Proportion')
 ggsave(paste(hndl.catch.per.shot,'Composition_LL.tiff',sep='/'), 
        width = 12,height = 8,dpi = 300, compression = "lzw")
 
           
  Comp_GN=Data.daily.original%>%
    filter(METHOD=="GN" & !is.na(species) & !species%in%c(22998,22999))%>%
    group_by(species)%>%
    summarise(landwt=sum(landwt,na.rm=T))%>%
    ungroup()%>%
    left_join(All.species.names%>%
                distinct(CAAB_code,.keep_all=T)%>%
                dplyr::select(CAAB_code,COMMON_NAME,Taxa),
              by=c('species'='CAAB_code'))%>%
    filter(!is.na(COMMON_NAME) & Taxa%in%c('Elasmobranch','Teleost'))%>%
    mutate(prop=landwt/sum(landwt))
  write.csv(Comp_GN,paste(hndl.catch.per.shot,'Composition_GN.csv',sep='/'),row.names=F)
  Comp_GN%>%
    ggplot(aes(COMMON_NAME,prop,fill=Taxa))+
    geom_col()+
    theme_PA()+
    theme(legend.position = 'top',
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    xlab('')+ylab('Proportion')
  ggsave(paste(hndl.catch.per.shot,'Composition_GN.tiff',sep='/'), 
         width = 12,height = 8,dpi = 300, compression = "lzw")
  
  
  
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

#Export data for Abbey
if(do.Abbeys)
{
  Data.for.Abbey=list(DATA=DATA,All.species.names=All.species.names,
                      Video.net.obs=Video.net.obs,Video.net.interaction=Video.net.interaction,Video.net.maxN=Video.net.maxN,
                      Video.longline.obs=Video.longline.obs,Video.longline.interaction=Video.longline.interaction,Video.longline.maxN=Video.longline.maxN,
                      Video.camera1.deck=Video.camera1.deck, Video.camera1.deck_extra.records=Video.camera1.deck_extra.records,
                      Video.camera2.deck=Video.camera2.deck, Video.camera2.deck_observations=Video.camera2.deck_observations)
  for(i in 1:length(Data.for.Abbey))
  {
    write.csv(Data.for.Abbey[[i]],handl_OneDrive(paste('Students/2020_Abbey Shuttleworth/Data/',names(Data.for.Abbey)[i] ,'.csv',sep='')),
              row.names = F) 
  } 
}

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
#if('code'%in%colnames(Video.net.obs)) Video.net.obs=Video.net.obs%>%rename(Code=code)
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
  mutate(Code=case_when(is.na(Code) &Species=='seven legged starfish'~25154008,
                        is.na(Code) &Species=='commorant'~40048000,
                        is.na(Code) & Genus=='Neophoca' & Species=='cinerea'~41131005,
                        is.na(Code) & Genus=='Megaptera' & Species=='novaeangliae'~41112006,
                        is.na(Code) & Family=='Blenniidae'~37408000,
                        is.na(Code) & Family=='Pleuronectidae'~37461916,
                        is.na(Code) & Family=='Cheloniidae'~	39020000,
                        is.na(Code) & Genus=='Scorpis' & Species=='Aequipinnis'~37361004,
                        is.na(Code) & Genus=='Mola' & Species=='alexandrini'~37470001,
                        TRUE~Code),
       Genus=case_when(Code==23617909~'Sepioteuthis',
                       Code==23607901~'Sepia',
                       Species=='seven legged starfish'~'Astrostole',
                       TRUE~Genus),
       Species=case_when(Code%in%c(23607901,23617909)~'spp.',
                         Species=='seven legged starfish'~'scabra',
                         TRUE~Species),
       Family=case_when(Code%in%c(24388000)~'Aplysiidae',   
                        Code==40048000~'Phalacrocoracidae',
                        Code==28850901~'Brachyura',
                        Code==23659921~'Octopodidae',
                        Code%in%c(37099999,99999999)~'Osteichthyes',
                        TRUE~Family),
       taxa=case_when(taxa%in%c('squid','cuttlefish','seven legged starfish')~ paste(Genus,Species),
                      taxa%in%c('unknown fish','baitfish','crab','commorant','bird','sea hare')~Family,
                      Code%in%c(37408000,37461916,23659921,40048000,99999999)~Family,
                      TRUE~taxa),
       taxa.to.mutate=case_when(taxa.to.mutate%in%c('squid','cuttlefish','seven legged starfish')~ paste(Genus,Species),
                                taxa.to.mutate%in%c('unknown fish','baitfish','crab','commorant','bird','sea hare')~Family,
                                Code%in%c(37408000,37461916,23659921,40048000,99999999)~Family,  
                                TRUE~taxa.to.mutate))

Video.longline.interaction=Video.longline.interaction%>%
  mutate(Code=case_when(is.na(Code) &Species=='seven legged starfish'~25154008,
                        is.na(Code) &Species=='commorant'~40048000,
                        is.na(Code) & Genus=='Neophoca' & Species=='cinerea'~41131005,
                        is.na(Code) & Genus=='Megaptera' & Species=='novaeangliae'~41112006,
                        is.na(Code) & Family=='Blenniidae'~37408000,
                        is.na(Code) & Family=='Pleuronectidae'~37461916,
                        is.na(Code) & Family=='Cheloniidae'~	39020000,
                        is.na(Code) & Genus=='Scorpis' & Species=='Aequipinnis'~37361004,
                        is.na(Code) & Genus=='Mola' & Species=='alexandrini'~37470001,
                        TRUE~Code),
       Genus=case_when(Code==23617909~'Sepioteuthis',
                       Code==23607901~'Sepia',
                       Species=='seven legged starfish'~'Astrostole',
                       TRUE~Genus),
       Species=case_when(Code%in%c(23607901,23617909)~'spp.',
                         Species=='seven legged starfish'~'scabra',
                         TRUE~Species),
       Family=case_when(Code%in%c(24388000)~'Aplysiidae',   
                        Code==40048000~'Phalacrocoracidae',
                        Code==28850901~'Brachyura',
                        Code==23659921~'Octopodidae',
                        Code%in%c(37099999,99999999)~'Osteichthyes',
                        TRUE~Family),
       taxa=case_when(taxa%in%c('squid','cuttlefish','seven legged starfish')~ paste(Genus,Species),
                      taxa%in%c('unknown fish','baitfish','crab','commorant','bird','sea hare')~Family,
                      Code%in%c(37408000,37461916,23659921,40048000,99999999)~Family,
                      TRUE~taxa),
       taxa.to.mutate=case_when(taxa.to.mutate%in%c('squid','cuttlefish','seven legged starfish')~ paste(Genus,Species),
                                taxa.to.mutate%in%c('unknown fish','baitfish','crab','commorant','bird','sea hare')~Family,
                                Code%in%c(37408000,37461916,23659921,40048000,99999999)~Family,  
                                TRUE~taxa.to.mutate))

Video.net.interaction=Video.net.interaction%>%
              mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                                        Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                                        Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                                        Code >=3.7046e7 & Code<3.900e7 ~"Scalefish",
                                        Code ==99999999 ~"Scalefish",
                                        Code >=3.900e7 & Code<=4.0e+07 ~"Turtles",
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
                                        Code >=3.7046e7 & Code<3.900e7 ~"Scalefish",
                                        Code ==99999999 ~"Scalefish",
                                        Code >=3.900e7 & Code<=4.0e+07 ~"Turtles",
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
                                        Code >=3.7046e7 & Code<3.900e7 ~"Scalefish",
                                        Code ==99999999 ~"Scalefish",
                                        Code >=3.900e7 & Code<=4.0e+07 ~"Turtles",
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
                                          Code >=3.7046e7 & Code<3.900e7 ~"Scalefish",
                                          Code ==99999999 ~"Scalefish",
                                          Code >=3.900e7 & Code<=4.0e+07 ~"Turtles",
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
  mutate(Code=case_when(is.na(Code) & Genus=='Cheloniidae'~ 39020002,
                        is.na(Code) & Genus=='Neophoca'~ 41131005,
                        is.na(Code) & Genus=='Megaptera'~ 41112006,
                        is.na(Code) & Species=='commorant'~ 40048000,
                        TRUE~Code),
         SP.group=case_when(is.na(SP.group) & Genus=='Cheloniidae'~ "Turtles",
                            is.na(SP.group) & Genus=='Neophoca'~ "Marine mammals",
                            is.na(SP.group) & Genus=='Megaptera'~ "Marine mammals",
                            is.na(SP.group) & Species=='commorant'~ "Seabirds",
                            TRUE~SP.group),
         Interaction=case_when(is.na(Interaction) & 
                                 Code==40048000 &
                                 grepl(paste(c('DIVING FOR BAIT','feeding','bird',' sear water'),collapse = '|'),
                                       original.escape) ~'Attracted',
                               TRUE~Interaction))

Video.longline.interaction=Video.longline.interaction%>%
  mutate(Code=case_when(is.na(Code) & Genus=='Cheloniidae'~ 39020002,
                        is.na(Code) & Genus=='Neophoca'~ 41131005,
                        is.na(Code) & Genus=='Megaptera'~ 41112006,
                        is.na(Code) & Species=='commorant'~ 40048000,
                        TRUE~Code),
         SP.group=case_when(is.na(SP.group) & Genus=='Cheloniidae'~ "Turtles",
                            is.na(SP.group) & Genus=='Neophoca'~ "Marine mammals",
                            is.na(SP.group) & Genus=='Megaptera'~ "Marine mammals",
                            is.na(SP.group) & Species=='commorant'~ "Seabirds",
                            TRUE~SP.group),
         Interaction=case_when(is.na(Interaction) & 
                                 Code==40048000 &
                                 grepl(paste(c('DIVING FOR BAIT','feeding','bird',' sear water'),collapse = '|'),
                                       original.escape) ~'Attracted',
                               TRUE~Interaction))

Video.net.interaction=Video.net.interaction%>%
  mutate(soak.time=case_when(is.na(soak.time) & sheet_no=='Pa0142'~6.76,
                             is.na(soak.time) & sheet_no=='Pa0144'~7.48,
                             TRUE~soak.time)) 

#Add species names and code to DATA
DATA=DATA%>%
  mutate(species=toupper(species))%>%
  left_join(All.species.names,by=c('species'='Species'))%>%
  mutate(COMMON_NAME=ifelse(is.na(COMMON_NAME)& !is.na(common_name),common_name,COMMON_NAME),
         SCIENTIFIC_NAME=ifelse(is.na(SCIENTIFIC_NAME)& !is.na(scientific_name),scientific_name,SCIENTIFIC_NAME))

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
                                  CAES_Code %in% 35000:39100 ~ dw),
                   Size=ifelse(is.na(Size) & is.na(fl) & CAES_Code%in%(13000:13016),tl,Size),
                   Size=ifelse(is.na(Size) & CAES_Code %in% 35000:39100,dw,Size))


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
if(do.observer.catch.comp)
{
  # Map of sampling sites
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
      mutate(Col=ifelse(method=='GN',"#F8766D", "#00BFC4"))
    #bring in shape file
    data(worldLLhigh)
    
    fn.fig(le.paste("Map site area"),1600,2400)
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
    legend('bottomleft',c("Gillnet","Longline"),pch=21,
           pt.bg=c("#F8766D", "#00BFC4"),bty='n',cex=1.25)
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
  write.csv(Tab.n.sp.gillnet,le.paste("Observer/Observed.number.species.by.gillnet.config.csv"),row.names=F)
  
  
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
  
}

#---------Observer size distribution ------------ 
if(do.observer.size.dist)
{
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
  # this.sp.sizes=DATA%>%
  #   filter(!is.na(Size))%>%
  #   group_by(common_name)%>%
  #   tally()%>%
  #   filter(n>50)%>%
  #   pull(common_name)
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
    save_as_image(ft, path = le.paste(paste("Observer/size/ANOVA_",Anova.title,".png",sep='')))
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
}



#---------Observer cpue ------------ 
if(do.observer.cpue)
{
  #fix a few records where sum of hook combos > n.hooks due to gear loss
  fn.method.zone=function(what,GN.scale,LL.scale)
  {
    p=DATA%>%
      filter(!is.na(zone))%>%
      mutate(zone=tolower(zone),
             methodzone=paste(method,zone),
             Effort=ifelse(method=="GN",1000*net_length*soak.time,  #net length in m
                           ifelse(method=="LL",n.hooks*soak.time,
                                  NA)),
             common_name=case_when(!common_name%in%what~'Other',
                                   TRUE~common_name))%>%
      filter(common_name%in%c(what,'Other'))%>%  #add 'other' species to account for all shots
      group_by(sheet_no,method,methodzone,common_name)%>%
      summarise(Weight=sum(tw,na.rm=T),
                Effort=max(Effort))%>%
      ungroup()%>%
      spread(common_name,Weight,fill=0)%>%  #add 0 records
      gather(common_name,Weight,-c(sheet_no,method,methodzone,Effort))%>%
      filter(!common_name=='Other')%>%
      left_join(DATA%>%distinct(common_name,Taxa),by='common_name')%>%
      mutate(cpue=Weight/Effort,
             cpue1=ifelse(method=="GN",cpue*GN.scale,   # (comparable cpue 3000 m GN = 1000 m LL)
                          ifelse(method=="LL",cpue*LL.scale,
                                 NA)),
             colr=ifelse(Taxa=='Teleost',"firebrick",
                         ifelse(Taxa=='Elasmobranch',"steelblue",
                                NA)))%>%
      filter(!is.na(Effort))%>%
      filter(Effort>0)%>%
      group_by(common_name,methodzone)%>%
      mutate(Nominal=mean(cpue1),
             SD=sd(cpue1),
             N=n(),
             SE=SD/sqrt(N))%>%
      ungroup()%>%
      ggplot(aes(x=methodzone, y=cpue1)) + 
      geom_boxplot(outlier.size = 1,aes(fill=Taxa),alpha=.4)+ 
      coord_flip()+
      facet_wrap(~common_name,ncol=2,scales='free_x')+
      ylab(paste("Catch rate per session (kg per ",GN.scale," m hour or ",LL.scale," hook hour)",sep=''))+
      xlab("")+ 
      theme_PA(strx.siz=12,leg.siz=12,axs.t.siz=12,axs.T.siz=14)+
      theme(legend.position="none")+
      scale_fill_manual(values=c(Teleost="firebrick",Elasmobranch="steelblue"))+
      #stat_summary(fun.data=MeanSE, geom="errorbar", color="darkgreen",alpha=1,aes(width=0.5))+
      stat_summary(fun=mean, geom="point", shape=21, size=2, color="darkgreen", fill="darkgreen")+
      scale_y_sqrt()
    return(p)
  }
  
  #1. Main commercial 
  #1.a. By method-zone 
  fn.fig(le.paste("Observer/catch_rates/CPUE.main.sp_GN_LL_y_sqrt"),2400,2000)
  fn.method.zone(what=Main.species,
                 GN.scale=cpue.scaler,
                 LL.scale=cpue.scaler)
  dev.off()
  
  
  #1.b. by LL configuration  
  fn.LL.config=function(what,LL.scale)
  {
    p=DATA%>%
      filter(LL.config%in%LL.configs)%>%
      mutate(Effort=ifelse(method=="GN",1000*net_length*soak.time,  #net length in m
                           ifelse(method=="LL",Effort.hook.combo,
                                  NA)),
             common_name=case_when(!common_name%in%what~'Other',
                                   TRUE~common_name))%>%
      filter(common_name%in%c(what,'Other'))%>%  #add 'other' species to account for all shots
      group_by(sheet_no,LL.config,common_name)%>%
      summarise(Weight=sum(tw,na.rm=T),
                Effort=max(Effort))%>%
      ungroup()%>%
      spread(common_name,Weight,fill=0)%>%  #add 0 records
      gather(common_name,Weight,-c(sheet_no,LL.config,Effort))%>%
      filter(!common_name=='Other')%>%
      left_join(DATA%>%distinct(common_name,Taxa),by='common_name')%>%
      mutate(cpue=Weight/Effort,
             cpue1=cpue*LL.scale,   # (comparable cpue 3000 m GN = 1000 m LL)
             colr=ifelse(Taxa=='Teleost',"firebrick",
                         ifelse(Taxa=='Elasmobranch',"steelblue",NA)))%>%
      filter(!is.na(Effort))%>%
      filter(Effort>0)%>%
      filter(common_name%in%what)%>%
      group_by(common_name,LL.config)%>%
      mutate(Nominal=mean(cpue1),
             SD=sd(cpue1),
             N=n(),
             SE=SD/sqrt(N))%>%
      ungroup()%>%
      ggplot(aes(x=LL.config, y=cpue1)) + 
      geom_boxplot(outlier.size = 1,aes(fill=Taxa),alpha=.4)+ 
      coord_flip()+
      facet_wrap(~common_name,ncol=2,scales='free_x')+
      ylab(paste("Catch rate per session (kg per ",LL.scale," hook hour)",sep=''))+xlab("")+ 
      theme_PA(strx.siz=12,leg.siz=12,axs.t.siz=12,axs.T.siz=14)+
      theme(legend.position="none")+
      scale_fill_manual(values=c(Teleost="firebrick",Elasmobranch="steelblue"))+
      #stat_summary(fun.data=MeanSE, geom="errorbar", color="darkgreen",alpha=1,aes(width=0.5))+
      stat_summary(fun=mean, geom="point", shape=21, size=2, color="darkgreen", fill="darkgreen")+
      scale_y_sqrt()+scale_x_discrete(position = "top")
    return(p)
  }
  
  Display.these=DATA%>%     #select what species to display
    filter(common_name%in%Main.species & method=="LL" &
             !is.na(hooksize) &!is.na(hooktype)&!is.na(wiretrace))%>%
    group_by(common_name)%>%
    tally()%>%ungroup()%>%filter(n>=20)%>%pull(common_name)
  fn.fig(le.paste("Observer/catch_rates/CPUE.main.sp_LL.configuration_y_sqrt"),2400,2000)
  fn.LL.config(what=Display.these,
               LL.scale=cpue.scaler)
  dev.off()
  
  #1.c  Longline catch rate by shot and fisher to inspect learning behavior
  fn.ll.cpue.shot=function(what,LL.scale)
  {
    p=DATA%>%
      filter(!is.na(zone) & method=="LL")%>%
      mutate(Effort=n.hooks*soak.time,
             common_name=case_when(!common_name%in%what~'Other',
                                   TRUE~common_name))%>%
      filter(common_name%in%c(what,'Other'))%>%  #add 'other' species to account for all shots
      group_by(sheet_no,boat,common_name)%>%
      summarise(Weight=sum(tw,na.rm=T),
                Effort=max(Effort))%>%
      ungroup()%>%
      spread(common_name,Weight,fill=0)%>%  #add 0 records
      group_by(boat)%>%
      mutate(Shot=1:n())%>%
      ungroup()%>%
      gather(common_name,Weight,-c(sheet_no,boat,Shot,Effort))%>%
      filter(!common_name=='Other')%>%
      left_join(DATA%>%distinct(common_name,Taxa),by='common_name')%>%
      mutate(cpue=Weight/Effort,
             cpue1=cpue*LL.scale,
             colr=ifelse(Taxa=='Teleost',"firebrick",
                         ifelse(Taxa=='Elasmobranch',"steelblue",
                                NA)))%>%
      filter(!is.na(Effort))%>%
      filter(Effort>0)%>%
      filter(!is.na(boat))%>%
      mutate(boat=as.character(as.numeric(as.factor(boat))))%>%
      ggplot(aes(x=Shot, y=cpue1,color=boat)) + 
      geom_point(size=2)+
      geom_line(alpha=0.5,linetype = "dashed")+
      facet_wrap(~common_name,scales='free')+
      ylab(paste("Catch rate per session (kg per ",LL.scale," hook hour)",sep=''))+
      theme_PA(strx.siz=12,leg.siz=12,axs.t.siz=12,axs.T.siz=14)+
      theme(legend.position="top")+scale_y_sqrt()
    return(p)
  }
  fn.fig(le.paste("Observer/catch_rates/CPUE.main.sp_by.shot_vessel"),2400,2000)
  fn.ll.cpue.shot(what=Main.species,LL.scale=300)
  dev.off()
  
  #1.d. Standardization
  do.standard=TRUE
  if(do.standard)
  {
    fn.reshape.cpue=function(d,Where,resvar)
    {
      d1=d%>%
        mutate(Effort=NA,
               Effort=ifelse(method=="GN",1000*net_length*soak.time,  #net length in m
                             ifelse(method=="LL",Effort.hook.combo.infered,  #effort by snood combo   
                                    NA)),
               common_name=case_when(!common_name%in%Where~'Other',
                                     TRUE~common_name))%>%
        filter(common_name%in%c(Where,'Other'))%>%  #add 'other' species to account for all shots
        group_by(sheet_no,method,common_name,hooktype,hooksize,wiretrace,zone)%>%
        summarise(Weight=sum(!!sym(resvar),na.rm=T),
                  Effort=max(Effort))%>%
        ungroup()%>%
        spread(common_name,Weight,fill=0)%>%  #add 0 records
        gather(common_name,Weight,-c(sheet_no,method,Effort,hooktype,hooksize,wiretrace,zone))%>%
        filter(!common_name=='Other')%>%
        left_join(d%>%distinct(common_name,Taxa),by='common_name')%>%
        mutate(cpue=Weight/Effort,
               cpue1=ifelse(method=="GN",cpue*cpue.scaler,   # (overall hook:gillnet is 1:3, Rory pers com)
                            ifelse(method=="LL",cpue*cpue.scaler,
                                   NA)),
               colr=ifelse(Taxa=='Teleost',"firebrick",
                           ifelse(Taxa=='Elasmobranch',"steelblue",NA)))
      return(d1)
    }
    #cpue in kg
    cpue.data.GN=fn.reshape.cpue(d=DATA%>%filter(method=="GN"),
                                 Where=Main.species,
                                 resvar='tw')%>%
      filter(!is.na(Effort))%>%
      filter(Effort>0)
    
    cpue.data.LL=fn.reshape.cpue(d=DATA%>%filter(method=="LL"),
                                 Where=Main.species,
                                 resvar='tw')%>%
      filter(!is.na(Effort))%>%
      filter(Effort>0)
    
    cpue.data.LL%>%
      ggplot(aes(wiretrace,cpue1))+
      stat_summary(fun.data=MeanSE, geom="errorbar", color="darkgreen",alpha=1,aes(width=0.5))+
      stat_summary(fun=mean, geom="point", shape=21, size=2, color="darkgreen", fill="darkgreen")+
      facet_wrap(~common_name, scales='free')+ylab('Catch rate (Kg per 1 m hour or 1 hook hour)')
    do.snood.shark.only=FALSE
    if(do.snood.shark.only)
    {
      Main.shark.species=Main.species[1:4]
      cpue.stand.snood=vector('list',length(Main.shark.species))
      names(cpue.stand.snood)=Main.shark.species
      for(m in 1:length(cpue.stand.snood))
      {
        D=cpue.data.LL%>%
          filter(common_name==Main.shark.species[m] & !is.na(zone))%>%
          filter(!is.na(wiretrace))%>%
          mutate(wiretrace=as.factor(wiretrace))
        cpue.stand.snood[[m]]=cpue.stand.fun(d=D,Formula=formula(cpue1~wiretrace))  
        as_flextable(cpue.stand.snood[[m]]$mod)
      }
    }
    
    
    #cpue in numbers
    cpue.data.GN.number=fn.reshape.cpue(d=DATA%>%filter(method=="GN"),
                                        Where=Main.species,
                                        resvar='number')%>%
      filter(!is.na(Effort))%>%
      filter(Effort>0)
    
    cpue.data.LL.number=fn.reshape.cpue(d=DATA%>%filter(method=="LL"),
                                        Where=Main.species,
                                        resvar='number')%>%
      filter(!is.na(Effort))%>%
      filter(Effort>0)
    
    cpue.data.LL.number%>%
      ggplot(aes(wiretrace,cpue1))+
      stat_summary(fun.data=MeanSE, geom="errorbar", color="darkgreen",alpha=1,aes(width=0.5))+
      stat_summary(fun=mean, geom="point", shape=21, size=2, color="darkgreen", fill="darkgreen")+
      facet_wrap(~common_name, scales='free')+ylab('Catch rate (Numbers per 1 m hour or 1 hook hour)')
    
    
    #1.b.1 fit GAM
    cpue.stand.out.GN=vector('list',length(Main.species))
    names(cpue.stand.out.GN)=Main.species
    cpue.stand.out.LL=cpue.stand.out.GN
    
    GN.form=formula(cpue1~zone)
    LL.form=formula(cpue1~zone+hooktype+hooksize+wiretrace)
    
    system.time({
      for(m in 1:length(Main.species))
      {
        print(paste(Main.species[m],'GN GAM-------------'))
        D=cpue.data.GN%>%
          filter(common_name==Main.species[m] & !is.na(zone))
        TAB=D%>%group_by(zone)%>%summarise(min=min(cpue1),max=max(cpue1))%>%filter(max>0)%>%pull(zone)
        D=D%>%
          filter(zone%in%TAB)%>%
          mutate(zone=as.factor(zone))
        cpue.stand.out.GN[[m]]=cpue.stand.fun(d=D,Formula=GN.form)  
        rm(TAB)
        
        print(paste(Main.species[m],'LL GAM-------------'))
        D=cpue.data.LL%>%
          filter(common_name==Main.species[m] & !is.na(zone))%>%
          filter(!is.na(hooktype))%>%
          filter(!is.na(wiretrace))%>%
          filter(!is.na(hooksize))
        if(nrow(D)>0)
        {
          TAB=D%>%group_by(zone)%>%summarise(min=min(cpue1),max=max(cpue1))%>%filter(max>0)%>%pull(zone)
          D=D%>%
            filter(zone%in%TAB)%>%
            mutate(wiretrace=as.factor(wiretrace),
                   hooktype=as.factor(hooktype),
                   hooksize=factor(hooksize),
                   zone=as.factor(zone))
          form=LL.form
          if(length(TAB)==1) form=formula(cpue1~hooktype+hooksize+wiretrace)
          if(nrow(D)>0 & !Main.species[m]=="Queen snapper" )cpue.stand.out.LL[[m]]=cpue.stand.fun(d=D,Formula=form)  
          rm(TAB)
          
        }
      }
    })   #takes 30 secs
    #gam.check(cpue.stand.out[[m]])
    
    
    #1.b.2 export anova table
    fn.wrp=function(NM,ext)le.paste(paste("Observer/catch_rates/GAM_Anova",paste(NM,ext,sep=''),sep='/'))
    for(m in 1:length(Main.species))
    {
      NM=Main.species[m]
      
      #GN
      print(paste(Main.species[m],'GN GAM Anova-------------'))
      ft <- as_flextable(cpue.stand.out.GN[[m]]$mod)
      save_as_image(ft, path = fn.wrp(NM,'.GN.png'))
      #save_as_docx(ft,path =  fn.wrp(NM,'.GN.docx'))
      rm(ft)
      
      #LL
      print(paste(Main.species[m],'LL GAM Anova-------------'))
      if(!is.null(cpue.stand.out.LL[[m]]$mod))
      {
        ft <- as_flextable(cpue.stand.out.LL[[m]]$mod)
        save_as_image(ft, path = fn.wrp(NM,'.LL.png'))
        #save_as_docx(ft,path = fn.wrp(NM,'.LL.docx'))
        rm(ft)
      }
      rm(NM)
      
    }
    
    
    #1.b.3 display term effect
    fn.sig.sp=function(mod,Pred,nm)
    {
      if(!is.null(mod))
      {
        a=summary(mod)$p.pv
        a=a[grep(paste(Pred, collapse="|"),names(a))]
        a=subset(a,a<0.05)
        if(length(a)>0) return(nm)
      }
    }
    Get.hook.type.Pred=vector('list',length(Main.species))
    Get.snood.Pred=Get.hook.size.Pred=Get.hook.type.Pred
    for(m in 1:length(Main.species))
    {
      Get.hook.size.Pred[[m]]=fn.sig.sp(mod=cpue.stand.out.LL[[m]]$mod,Pred="hooksize",nm=names(cpue.stand.out.LL)[m])
      Get.hook.type.Pred[[m]]=fn.sig.sp(mod=cpue.stand.out.LL[[m]]$mod,Pred="hooktype",nm=names(cpue.stand.out.LL)[m])
      Get.snood.Pred[[m]]=fn.sig.sp(mod=cpue.stand.out.LL[[m]]$mod,Pred="wiretrace",nm=names(cpue.stand.out.LL)[m])
    }
    Get.hook.size.Pred=unlist(Get.hook.size.Pred[!sapply(Get.hook.size.Pred,is.null)])
    Get.hook.type.Pred=unlist(Get.hook.type.Pred[!sapply(Get.hook.type.Pred,is.null)])
    Get.snood.Pred=unlist(Get.snood.Pred[!sapply(Get.snood.Pred,is.null)])
    
    Store.zone.method=vector('list',length(Main.species))
    names(Store.zone.method)=Main.species
    Store.hook.type=Store.hook.size=Store.hook.snood=Store.zone.method
    
    for(m in 1:length(Main.species))
    {
      SP=Main.species[m]
      
      #zone-method effect                        
      GN=pred.fun(mod=cpue.stand.out.GN[[m]]$mod,biascor="NO",PRED=c('zone'))%>%
        mutate(method="GN",
               species=SP)
      LL=NULL
      if(!is.null(cpue.stand.out.LL[[m]]))
      {
        if('zone'%in%labels(terms(cpue.stand.out.LL[[m]]$mod)))
        {
          LL=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('zone'))%>%
            mutate(method="LL",
                   species=SP)
        }
        
        
        #Hook type
        if(Main.species[m]%in%Get.hook.type.Pred)
        {
          Store.hook.type[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('hooktype'))%>%
            mutate(species=SP)
        }
        
        #Hook size
        if(Main.species[m]%in%Get.hook.size.Pred)
        {
          Store.hook.size[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('hooksize'))%>%
            mutate(species=SP)
        }
        
        #Snood
        if(Main.species[m]%in%Get.snood.Pred)
        {
          Store.hook.snood[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('wiretrace'))%>%
            mutate(species=SP)
        }
        
      }
      Store.zone.method[[m]]=rbind(GN,LL)
    }
    
    #Method-zone
    fn.tit=function(x) paste("Standardized catch rate (kg per ",x," m hour or ",x," hook hour)",sep='')
    fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_Method.zone_y_sqrt"),2600,1600)
    out=fn.barplot.cpue(d=do.call(rbind,Store.zone.method)%>%
                          mutate(method.zone=paste(method,zone,sep='-'),
                                 species=factor(species,levels=Main.species),
                                 x=method.zone,
                                 y=response,
                                 fill=method.zone,
                                 facet=species),
                        YLAB=fn.tit(cpue.scaler),
                        XLAB='Fishing method and zone',
                        cex=12,
                        Rotate="Yes",
                        Relative="No",
                        Y_sqrt="YES")
    dev.off()
    write.csv(out,le.paste("Observer/catch_rates/Standardized_Catch rates_Method.zone.csv"),row.names = F)
    
    #Hooksize  
    d=do.call(rbind,Store.hook.size)
    if(!is.null(d))
    {
      fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_hooksize"),2000,2400)
      out=fn.barplot.cpue(d=d%>%
                            mutate(species=factor(species,levels=Main.species),
                                   x=hooksize,
                                   y=response,
                                   fill=hooksize,
                                   facet=species),
                          YLAB="Relative standardized catch rate",
                          XLAB='Hook size',
                          cex=16,
                          Relative="Yes")
      dev.off()
    }
    
    
    #Hooktype
    d=do.call(rbind,Store.hook.type)
    if(!is.null(d))
    {
      fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_hooktype"),2000,2400)
      out=fn.barplot.cpue(d=d%>%
                            mutate(species=factor(species,levels=Main.species),
                                   x=hooktype,
                                   y=response,
                                   fill=hooktype,
                                   facet=species),
                          YLAB="Relative standardized catch rate",
                          XLAB='Hook type',
                          cex=16,
                          Relative="Yes")
      dev.off()
    }
    
    
    #Snood
    d=do.call(rbind,Store.hook.snood)
    if(!is.null(d))
    {
      fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_snood"),2000,2400)
      out=fn.barplot.cpue(d=d%>%
                            mutate(species=factor(species,levels=Main.species),
                                   x=wiretrace,
                                   y=response,
                                   fill=wiretrace,
                                   facet=species),
                          YLAB="Relative standardized catch rate",
                          XLAB='Snood type',
                          cex=16,
                          Relative="Yes")
      dev.off()
    }
    
    
    #Test snood characteristics separately
    #note: no different than combined gam
    do.seprt=FALSE
    if(do.seprt)
    {
      fn.test.longline=function(d,Where,var,efforvar)
      {
        d1=d%>%  
          mutate(Effort=!!sym(efforvar),
                 common_name=case_when(!common_name%in%Where~'Other',
                                       TRUE~common_name))%>%
          filter(common_name%in%c(Where,'Other'))%>%  #add 'other' species to account for all shots
          group_by(sheet_no,method,common_name,!!sym(var))%>%
          summarise(Weight=sum(tw,na.rm=T),
                    Effort=max(Effort))%>%
          ungroup()%>%
          spread(common_name,Weight,fill=0)%>%  #add 0 records
          gather(common_name,Weight,-c(sheet_no,method,Effort,!!sym(var)))%>%
          filter(!common_name=='Other')%>%
          left_join(d%>%distinct(common_name,Taxa),by='common_name')%>%
          mutate(cpue=Weight/Effort,
                 cpue1=cpue*cpue.scaler,
                 colr=ifelse(Taxa=='Teleost',"firebrick",
                             ifelse(Taxa=='Elasmobranch',"steelblue",NA)))%>%
          filter(!is.na(Effort))%>%
          filter(Effort>0)%>%
          mutate(Term=factor(!!sym(var)))
        
        this.sp=subset(Main.species,!Main.species%in%c("Blue groper","Queen snapper"))  
        MODS=vector('list',length(this.sp))
        names(MODS)=this.sp
        form=formula(paste('cpue',var,sep = '~'))
        for(s in 1:length(MODS))  MODS[[s]]=gam(form,dat=d1%>%filter(common_name==names(MODS)[s]),family=tw,method="REML")
        #for(s in 1:length(MODS))  MODS[[s]]=glm(form,dat=d1%>%filter(common_name==names(MODS)[s] &cpue1>0),family='gaussian')
        
        return(list(data=d1,MODS=MODS))
        
      }
      
      Wire.mod=fn.test.longline(d=DATA%>%filter(method=="LL"),Where=Main.species,
                                var='wiretrace',efforvar='Effort.wire.infered')
      Hooksize.mod=fn.test.longline(d=DATA%>%filter(method=="LL"),Where=Main.species,
                                    var='hooksize',efforvar='Effort.hooksize.infered')
      Hooktype.mod=fn.test.longline(d=DATA%>%filter(method=="LL"),Where=Main.species,
                                    var='hooktype',efforvar='Effort.hooktype.infered')
      as_flextable(Hooktype.mod$MODS[[6]])
      Hooktype.mod$data%>%ggplot(aes(Term,cpue1))+geom_boxplot()+facet_wrap(~common_name)
      
    }
  }
  
  
  #2. Main discarded   
  Main.discards=DATA%>%
    filter(!is.na(Code))%>%
    group_by(common_name,retainedflag)%>%
    tally()%>%
    filter(retainedflag=="No")%>%
    arrange(-n)%>%
    ungroup()%>%
    filter(!common_name%in%Main.species)%>%
    mutate(Cumsum=cumsum(n)/sum(n))%>%
    filter(Cumsum<.9)%>%
    pull(common_name)
  
  if("Guitarfish & shovelnose rays"%in%Main.discards)Main.discards=Main.discards[-match("Guitarfish & shovelnose rays",Main.discards)]
  
  
  #2.a. By method-zone
  fn.fig(le.paste("Observer/catch_rates/CPUE.main.sp.discarded_GN_LL_y_sqrt"),2400,2000)
  fn.method.zone(what=Main.discards,
                 GN.scale=cpue.scaler,
                 LL.scale=cpue.scaler)
  dev.off()
  
  #2.b. by LL configuration
  Display.these=DATA%>%     #select what species to display
    filter(common_name%in%Main.discards & method=="LL" &
             !is.na(hooksize) &!is.na(hooktype)&!is.na(wiretrace))%>%
    group_by(common_name)%>%
    tally()%>%ungroup()%>%filter(n>=20)%>%pull(common_name)
  fn.fig(le.paste("Observer/catch_rates/CPUE.main.sp.discarded_LL.configuration_y_sqrt"),2400,2200)
  fn.LL.config(what=Display.these,
               LL.scale=cpue.scaler)
  dev.off()
  
  #2.c. Standardization
  Do.disc.cpue=FALSE
  if(Do.disc.cpue)
  {
    # This.disc=DATA%>%
    #   filter(retainedflag=='No')%>%
    #   group_by(common_name)%>%
    #   tally()%>%
    #   filter(n>=20)%>%
    #   filter(!common_name=='')%>%
    #   pull(common_name)
    # This.disc=subset(This.disc,!This.disc%in%c(Main.species,"Stingrays"))
    This.disc=c("Buffalo bream","Dusky morwong","Port Jackson","Southern eagle ray")
    
    cpue.data.GN=fn.reshape.cpue(d=DATA%>%filter(method=="GN"),Where=This.disc,resvar='tw')
    cpue.data.LL=fn.reshape.cpue(d=DATA%>%filter(method=="LL"),Where=This.disc,resvar='tw')
    
    #5.c.1 fit GAM
    cpue.stand.out.GN=vector('list',length(This.disc))
    names(cpue.stand.out.GN)=This.disc
    cpue.stand.out.LL=cpue.stand.out.GN
    
    GN.form=formula(cpue1~zone)
    LL.form=formula(cpue1~zone+hooktype+hooksize+wiretrace)
    
    system.time({
      for(m in 1:length(This.disc))
      {
        print(paste(This.disc[m],'GN GAM-------------'))
        D=cpue.data.GN%>%
          filter(common_name==This.disc[m])%>%
          mutate(zone=as.factor(zone))
        
        if(!This.disc[m]%in%c("Fiddler ray","Guitarfish & shovelnose rays"))cpue.stand.out.GN[[m]]=cpue.stand.fun(d=D,Formula=GN.form)  
        
        
        print(paste(This.disc[m],'LL GAM-------------'))
        D=cpue.data.LL%>%
          filter(common_name==This.disc[m])%>%
          filter(!is.na(hooktype))%>%
          filter(!is.na(wiretrace))%>%
          filter(!is.na(hooksize))%>%
          mutate(wiretrace=as.factor(wiretrace),
                 hooktype=as.factor(hooktype),
                 hooksize=factor(hooksize),
                 zone=as.factor(zone))
        form=LL.form
        if(This.disc[m]=="Fiddler ray")form=formula(cpue1~hooktype+hooksize+wiretrace)
        if(nrow(D)>0)cpue.stand.out.LL[[m]]=cpue.stand.fun(d=D,Formula=form) 
        
      }
    }) 
    
    #5.b.2 export anova table
    for(m in 1:length(This.disc))
    {
      NM=This.disc[m]
      
      #GN
      print(paste(This.disc[m],'GN GAM Anova-------------'))
      if(!is.null(cpue.stand.out.GN[[m]]$mod))
      {
        ft <- as_flextable(cpue.stand.out.GN[[m]]$mod)
        save_as_image(ft, path = fn.wrp(NM,'.GN.png'))
        save_as_docx(ft,path =  fn.wrp(NM,'.GN.docx'))
        rm(ft)
      }
      
      
      #LL
      print(paste(This.disc[m],'LL GAM Anova-------------'))
      if(!is.null(cpue.stand.out.LL[[m]]$mod))
      {
        ft <- as_flextable(cpue.stand.out.LL[[m]]$mod)
        save_as_image(ft, path = fn.wrp(NM,'.LL.png'))
        save_as_docx(ft,path = fn.wrp(NM,'.LL.docx'))
        rm(ft)
      }
      rm(NM)
      
    }
    
    #5.b.3 display term effect
    Store.zone.method=vector('list',length(This.disc))
    names(Store.zone.method)=This.disc
    Store.hook.type=Store.hook.size=Store.hook.snood=Store.zone.method
    
    Get.hook.type.Pred=vector('list',length(Main.species))
    Get.snood.Pred=Get.hook.size.Pred=Get.hook.type.Pred
    for(m in 1:length(This.disc))
    {
      Get.hook.size.Pred[[m]]=fn.sig.sp(mod=cpue.stand.out.LL[[m]]$mod,Pred="hooksize",nm=names(cpue.stand.out.LL)[m])
      Get.hook.type.Pred[[m]]=fn.sig.sp(mod=cpue.stand.out.LL[[m]]$mod,Pred="hooktype",nm=names(cpue.stand.out.LL)[m])
      Get.snood.Pred[[m]]=fn.sig.sp(mod=cpue.stand.out.LL[[m]]$mod,Pred="wiretrace",nm=names(cpue.stand.out.LL)[m])
    }
    Get.hook.size.Pred=unlist(Get.hook.size.Pred[!sapply(Get.hook.size.Pred,is.null)])
    Get.hook.type.Pred=unlist(Get.hook.type.Pred[!sapply(Get.hook.type.Pred,is.null)])
    Get.snood.Pred=unlist(Get.snood.Pred[!sapply(Get.snood.Pred,is.null)])
    
    
    for(m in 1:length(This.disc))
    {
      SP=This.disc[m]
      
      #zone-method effect                        
      GN=NULL
      if(!is.null(cpue.stand.out.GN[[m]]$mod))
      {
        GN=pred.fun(mod=cpue.stand.out.GN[[m]]$mod,biascor="NO",PRED=c('zone'))%>%
          mutate(method="GN",
                 species=SP)
      }
      
      LL=NULL
      if(!is.null(cpue.stand.out.LL[[m]]))
      {
        if('zone'%in%all.vars(cpue.stand.out.LL[[m]]$mod$formula))  LL=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('zone'))%>%
            mutate(method="LL",
                   species=SP)
        
        #Hook type
        if(This.disc[m]%in%Get.hook.type.Pred)
        {
          Store.hook.type[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('hooktype'))%>%
            mutate(species=SP)
        }
        
        #Hook size
        if(This.disc[m]%in%Get.hook.size.Pred)
        {
          Store.hook.size[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('hooksize'))%>%
            mutate(species=SP)
        }
        
        #Snood
        if(This.disc[m]%in%Get.snood.Pred)
        {
          Store.hook.snood[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('wiretrace'))%>%
            mutate(species=SP)
        }
        
      }
      Store.zone.method[[m]]=rbind(GN,LL)
    }
    
    #Method-zone
    fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_Method.zone_discards_y_sqrt"),2600,1600)
    out=fn.barplot.cpue(d=do.call(rbind,Store.zone.method)%>%
                          mutate(method.zone=paste(method,zone,sep='-'),
                                 species=factor(species,levels=This.disc),
                                 x=method.zone,
                                 y=response,
                                 fill=method.zone,
                                 facet=species),
                        YLAB=fn.tit(cpue.scaler),
                        XLAB='Fishing method and zone',
                        cex=10,
                        Rotate="Yes",
                        Relative="No",
                        Y_sqrt='YES')
    dev.off()
    write.csv(out,le.paste("Observer/catch_rates/Standardized_Catch rates_Method.zone_discards.csv"),row.names = F)
    
    #Hooksize
    d=do.call(rbind,Store.hook.size)
    if(!is.null(d))
    {
      fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_hooksize_discards"),2000,2400)
      out=fn.barplot.cpue(d=d%>%
                            mutate(species=factor(species,levels=This.disc),
                                   x=hooksize,
                                   y=response,
                                   fill=hooksize,
                                   facet=species),
                          YLAB="Relative standardized catch rate",
                          XLAB='Hook size',
                          cex=16,
                          Relative="Yes")
      dev.off()
    }
    
    #Hooktype
    d=do.call(rbind,Store.hook.type)
    if(!is.null(d))
    {
      fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_hooktype_discards"),2000,2400)
      out=fn.barplot.cpue(d=d%>%
                            mutate(species=factor(species,levels=This.disc),
                                   x=hooktype,
                                   y=response,
                                   fill=hooktype,
                                   facet=species),
                          YLAB="Relative standardized catch rate",
                          XLAB='Hook type',
                          cex=16,
                          Relative="Yes")
      dev.off()
      
    }
    
    #Snood
    d=do.call(rbind,Store.hook.snood)
    if(!is.null(d))
    {
      fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_snood_discards"),2000,2400)
      out=fn.barplot.cpue(d=d%>%
                            mutate(species=factor(species,levels=This.disc),
                                   x=wiretrace,
                                   y=response,
                                   fill=wiretrace,
                                   facet=species),
                          YLAB="Relative standardized catch rate",
                          XLAB='Snood type',
                          cex=16,
                          Relative="Yes")
      dev.off()
    }
  }
}



#---------Observer retained vs discarded ------------ 
if(do.observer.retained.vs.discarded)
{
  #Export table of retained or discarded numbers, size and release condition by method
  fn.tab=function(what)
  {
    d=DATA%>%filter(method==what)
    dum=d%>%distinct(common_name,Taxa)
    
    Out_table_Retained=d%>%
      #filter(!is.na(retainedflag))%>%
      group_by(common_name,retainedflag,CAES_Code)%>%
      tally()%>%
      ungroup%>%
      spread(retainedflag,n,fill=0)%>%
      rename(unk=`<NA>`)%>%
      mutate(n=No+Yes+unk)%>%
      arrange(-n)%>%
      dplyr::select(-n)
    Out_table_disc.size=d%>%
      filter(retainedflag=='No')%>%
      group_by(common_name)%>%
      summarise(min.size=round(min(Size,na.rm=T)),
                max.size=round(max(Size,na.rm=T)))%>%
      mutate(Discarded.size.range=paste(min.size,max.size,sep='-'))%>%
      filter(!is.infinite(min.size) | !is.infinite(max.size))%>%
      dplyr::select(-min.size, -max.size)
    Out_table_Retained=Out_table_Retained%>%
      left_join(Out_table_disc.size,by='common_name')%>%
      dplyr::select(-CAES_Code)%>%
      ungroup()%>%
      filter(!common_name=='')%>%
      filter(!common_name=='Unidentified')%>%
      dplyr::rename(Discarded=No,
                    Retained=Yes)%>%
      relocate(Retained, .before = Discarded)
    
    Rel.cond=d%>%
      group_by(common_name,rel.cond)%>%
      summarise(N=sum(number))%>%
      filter(!(is.na(rel.cond)|rel.cond==''))%>%
      mutate(rel.cond=paste('Condition',rel.cond,sep='.'))%>%
      spread(rel.cond,N,fill=0)%>%
      mutate(Tot=Condition.0+Condition.1+Condition.2+Condition.3,
             Condition.0=paste(round(100*Condition.0/Tot,1),'%',sep=''),
             Condition.1=paste(round(100*Condition.1/Tot,1),'%',sep=''),
             Condition.2=paste(round(100*Condition.2/Tot,1),'%',sep=''),
             Condition.3=paste(round(100*Condition.3/Tot,1),'%',sep=''))%>%
      dplyr::select(-Tot)
    
    Out_table_Retained=Out_table_Retained%>%
      left_join(Rel.cond,by='common_name')
    
    Out_table_Retained=Out_table_Retained%>%
      left_join(dum,by="common_name")%>%
      mutate(n=Retained+Discarded+unk)%>%
      ungroup()%>%
      arrange(Taxa,-n)%>%
      dplyr::select(-Taxa,-n)
    return(Out_table_Retained)
  }
  out=fn.tab(what="GN")
  write.csv(out,le.paste("Observer/retained/Retained_species_table_GN.csv"),row.names=F)  
  out=fn.tab(what="LL")
  write.csv(out,le.paste("Observer/retained/Retained_species_table_LL.csv"),row.names=F)  
  
  
  
  #Plot percentage retained and discarded by gear
  #a. number of individuals
  p1=DATA%>%
    filter(!is.na(retainedflag))%>%
    ggplot(aes(x=as.factor(method), fill=as.factor(retainedflag)))+
    geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
    geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
              stat="count", position=position_dodge(0.9), vjust=-0.5)+
    scale_y_continuous(labels = scales::percent)+
    theme_PA(lgT.siz=14,leg.siz=12,axs.t.siz=12,axs.T.siz=14)+
    ylab("Percentage")+xlab('Method')+labs(fill='Retained') 
  
  
  p2=DATA%>%
    filter(retainedflag=='No' & !(rel.cond==''|is.na(rel.cond)))%>%
    group_by(rel.cond,method)%>%
    summarise(N=sum(number))%>%
    group_by(method)%>%
    mutate(sum=N/sum(N),
           ymax=cumsum(N),
           ymin=c(0, head(ymax, n=-1)))%>%
    arrange(method)%>%
    ggplot(aes(x=1,y=sum,fill=rel.cond)) + 
    geom_bar(stat="identity",width=2) + 
    coord_polar(theta='y')+
    facet_wrap(~ method)+
    scale_fill_manual(values=c("deepskyblue2",'khaki3',"tomato4","forestgreen"))+
    scale_x_continuous(limits=c(-1,2.5))+
    ggtitle(label ="",
            subtitle = "Release condition for discarded individuals")+
    theme_void() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          strip.text = element_text(size = 16),
          plot.subtitle = element_text(size = 17,face = "bold",hjust=0.25))
  
  fn.fig(le.paste("Observer/retained/GN_LLretained_discarded_individuals"),2100,2400)
  ggarrange(p1,p2, ncol = 1, heights=c(1.25,1))
  dev.off()
  
  
  
  #Stats
  fn.glm.retained=function(d,Formula,Pred,cex,XLAB,Rotate=NULL,Anova.title,do.plot)
  {
    d=d%>%
      mutate(Retained=ifelse(retainedflag=='Yes',1,
                             ifelse(retainedflag=='No',0,
                                    NA)))%>%
      filter(!is.na(Retained))%>%
      mutate(Gear=ifelse(method=="GN","Gillnet",LL.config))%>%
      filter(!grepl("NA",Gear))%>%
      mutate(Gear=as.factor(Gear),
             method=as.factor(method),
             zone=as.factor(zone))
    
    #rum model
    mod=gam(Formula,dat=d,family = "binomial")
    
    #export anova table
    ft <- as_flextable(mod)
    save_as_image(ft, path = le.paste(paste("Observer/retained/ANOVA_",Anova.title,".png",sep='')))
    rm(ft)
    
    #Plot term effect
    if(do.plot)
    {
      p=pred.fun(mod=mod,biascor="NO",PRED=Pred)%>%
        mutate(x=!!sym(Pred),
               y=prob,
               fill=!!sym(Pred))%>%
        ggplot(aes(x,y,fill=fill))+
        geom_bar(stat="identity")+
        geom_errorbar(aes(x, ymin=lower.CL, ymax=upper.CL),colour='black',width=0.25)+
        theme_PA(strx.siz=cex-2,lgT.siz=cex,leg.siz=cex-2,axs.t.siz=cex-3,axs.T.siz=cex)+
        theme(legend.position='none')+
        ylab("Probability of retaining an individual")+
        xlab(XLAB)
      if(!is.null(Rotate))p=p+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      fn.fig(le.paste(paste("Observer/retained/Standardized_retained_",Pred,sep='')),2400,2000)
      print(p)
      dev.off()
    }
    
  }
  #Method effect
  fn.glm.retained(d=DATA%>%
                    filter(!is.na(retainedflag)),
                  Formula=formula(Retained~method+zone),
                  Pred='method',
                  cex=18,
                  XLAB="Method",
                  Anova.title="Method",
                  do.plot=T)
  #Longline configuration effect
  fn.glm.retained(d=DATA%>%
                    filter(!is.na(retainedflag) & method=='LL')%>%
                    filter(!is.na(hooktype) & !is.na(wiretrace) & !is.na(hooksize))%>%
                    mutate(hooktype=as.factor(hooktype),
                           wiretrace=as.factor(wiretrace),
                           hooksize=factor(hooksize)),
                  Formula=formula(Retained~hooktype+hooksize+wiretrace+zone),
                  Pred=c('hooksize','hooktype','wiretrace'),
                  cex=18,
                  XLAB="Longline configuration",
                  Anova.title="longine.config",
                  do.plot=F)
  
  
  #b. number of species
  fn.fig(le.paste("Observer/retained/GN_LLretained_discarded_species"),2400,1600)
  DATA%>%
    filter(!is.na(retainedflag))%>%
    mutate(SP.GR=paste(species,method))%>%
    distinct(species,method,retainedflag,SP.GR)%>%
    ggplot(aes(x=as.factor(method), fill=as.factor(retainedflag)))+
    geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
    geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
              stat="count", position=position_dodge(0.9), vjust=-0.5)+
    scale_y_continuous(labels = scales::percent)+
    theme_PA(lgT.siz=14,leg.siz=12,axs.t.siz=12,axs.T.siz=14)+
    ylab("Percentage")+xlab('Method')+labs(fill='Retained') 
  dev.off()
  
}


#---------Hook baiting, and number of hook loss ------------
if(do.Hook.baiting.loss)
{
  #1. Lost hooks (overall tally)
  LL.sessions=DATA%>%
    filter(method=='LL')%>%
    distinct(sheet_no)%>%
    arrange(sheet_no)%>%
    mutate(Session=1:n())
  
  
  Lost.huk=DATA[grep('hook',tolower(DATA$comments.hdr)),]%>%
    distinct(sheet_no,method,n.hooks,comments.hdr)%>%
    mutate(Lost.hk= str_extract(comments.hdr, "[[:digit:]]+"))
  uniK=DATA%>%
    distinct(sheet_no,method,n.hooks,comments.hdr)%>%
    mutate(Lost.hk=0)%>%
    filter(method=='LL'& !sheet_no%in%Lost.huk$sheet_no)
  Lost.huk=rbind(Lost.huk,uniK)%>%
    filter(method=='LL' & !is.na(n.hooks))%>%
    mutate(Lost.hk=as.numeric(Lost.hk),
           Kept.hk=n.hooks-Lost.hk)%>%
    dplyr::select(-comments.hdr)%>%
    gather("Lost", "Percent", -c(sheet_no,method,n.hooks))%>%
    mutate(Percent=100*Percent/n.hooks)%>%
    filter(n.hooks>0)%>%
    left_join(LL.sessions,by='sheet_no')%>%
    mutate(Lost=ifelse(Lost=='Lost.hk','Lost','Not lost'))%>%
    arrange(Session)
  
  Plost=Lost.huk%>%
    ggplot(aes(Session,Percent,fill=Lost))+
    geom_col()+
    geom_text(data = Lost.huk%>%filter(!is.na(Percent))%>%distinct(Session,.keep_all=T),
              aes(y=110,label = paste("n=",n.hooks)),angle=65,size=2.5)+
    ylim(0,120)+
    ylab("Percentage of lost hooks/snoods")+
    theme_PA(leg.siz=12,axs.t.siz=10,axs.T.siz=11)+
    theme(legend.position = "top",
          legend.title = element_blank())+xlab('')
  
  #2. Lost snoods
  #note: only use circular for this analysis as Eb tend to bend so cannot tell if 'broken' refers to
  #       a broken or bent hook or broken snood
  Broken.snoods=Lost.snoods%>%
    filter(!is.na(Sheet.no))%>%
    map_if(is.numeric,~ifelse(is.na(.x),0,.x))%>%
    data.frame%>%
    mutate(Mono=C10.M+C12.M+C14.M,
           Wire=C10.W+C12.W+C14.W)%>%
    arrange(Sheet.no)%>%
    mutate(Session=1:n(),
           Tot=Mono+Wire)%>%
    left_join(DATA%>%
                filter(method=='LL')%>%
                distinct(sheet_no,n.hooks),
              by=c('Sheet.no'='sheet_no'))
  
  
  Average.lost.by.Mono=round(100*sum(Broken.snoods$Mono)/sum(Broken.snoods$Tot,na.rm=T),0)
  Average.lost.by.Wire=round(100*sum(Broken.snoods$Wire)/sum(Broken.snoods$Tot,na.rm=T),0)
  
  
  #3. baiting efficiency  
  Baiting=DATA%>%
    filter(method=="LL")%>%
    distinct(sheet_no,.keep_all=T)%>%
    mutate(Efficiency=100*n.hooks/hooks.baited,
           Efficiency=ifelse(Efficiency>100,100,Efficiency))%>%
    left_join(LL.sessions,by='sheet_no')
  P.baitn.eff=Baiting%>%
    ggplot(aes(Session,Efficiency))+
    geom_point(size=3,color="forestgreen")+
    ylim(0,100)+
    theme_PA(axs.t.siz=10,axs.T.siz=11)+
    theme(legend.position = "top",
          legend.title = element_blank())+
    ylab("Deployment efficiency")+xlab('')
  
  
  #4. baiting time   
  Baiting.time=Hook.combos[,c('sheet_no','baiting.time','baiting.crew','hooks.baited')]%>%
    filter(!is.na(baiting.time) & !is.na(baiting.crew))%>%
    filter(!baiting.time=="NA" & !is.na(hooks.baited))%>%
    left_join(Baiting%>%distinct(sheet_no,Session),by='sheet_no')%>%
    mutate(baiting.time=as.numeric(baiting.time),
           baiting.crew=as.numeric(baiting.crew),
           hook.per.hour_person=60*(hooks.baited/baiting.crew)/baiting.time)
  Avrg.baitime=mean(Baiting.time$hook.per.hour_person,na.rm=T)
  P.baitn.time=Baiting.time%>%
    ggplot(aes(Session,hook.per.hour_person))+
    geom_hline(yintercept = Avrg.baitime,
               col="steelblue",alpha=.6,size=2)+
    geom_text(aes(2,Avrg.baitime*1.2),label="Average",col="steelblue",size=5
              ,nudge_x = 3,nudge_y = -5)+
    geom_point(size=3,color="forestgreen")+
    xlim(0,max(Baiting$Session))+
    theme_PA(axs.t.siz=10,axs.T.siz=11)+
    theme(legend.position = "top",
          legend.title = element_blank())+
    ylab("Baited hooks per person-hour")+xlab('')+
    expand_limits(x = 0, y = 0)  
  
  
  fig=ggarrange(ggarrange(P.baitn.eff,P.baitn.time,
                          nrow=1,ncol=2),Plost,nrow=2) 
  
  fn.fig(le.paste("Observer/Hook baiting and lost"),2400,2400)
  annotate_figure(fig,bottom = text_grob("Session", size = 12))
  dev.off()
  
  
  # fn.lost.huk.trace=function(with.catch,deployed)
  # {
  #   d=deployed%>%
  #     dplyr::select(-c(Date,baiting.time,baiting.crew,Comments))%>%
  #     gather(Snood,N,-c(sheet_no,hooks.baited))%>%
  #     mutate(wiretrace.deployed=sub(".*\\.", "", Snood),
  #            wiretrace=case_when(wiretrace.deployed=='M'~'Mono',
  #                                wiretrace.deployed=='W'~'Wire'),
  #            hook.size=gsub(".*?([0-9]+).*", "\\1", Snood),
  #            hook.type=sub("([0-9]+).*", "", Snood))
  #   
  #   d1=d%>%
  #     group_by(sheet_no,wiretrace)%>%
  #     summarise(N.deployed=sum(N))
  #   
  #   with.catch=with.catch%>%
  #     filter(!is.na(wiretrace))%>%
  #     group_by(sheet_no,wiretrace)%>%
  #     summarise(N.with.catch=sum(number))
  #   
  #   p=d1%>%
  #     left_join(with.catch,by=c('sheet_no','wiretrace'))%>%
  #     mutate(prop.with.catch=N.with.catch/N.deployed,
  #            shot=as.numeric(substr(sheet_no,4,6)))%>%
  #     ggplot(aes(shot,prop.with.catch))+
  #     geom_line()+geom_point()+
  #     facet_wrap(~wiretrace)
  #   print(p)
  #   
  #   
  # }
  # fn.lost.huk.trace(with.catch=DATA%>%filter(method=='LL' & Taxa=='Teleost'),deployed=Hook.combos)
  # fn.lost.huk.trace(with.catch=DATA%>%filter(method=='LL' & Taxa=='Elasmobranch'),deployed=Hook.combos)
  
}

#---------Blood lactate ------------
if(do.Blood.lactate)
{
  Lactate=DATA%>%
    filter(!is.na(lactate) & common_name=="Port Jackson")%>%
    mutate(lactate=as.numeric(lactate),
           rel.cond=ifelse(is.na(rel.cond),'unknown',rel.cond))
  
  Mn=mean(Lactate$lactate,na.rm=T)
  SE=sd(Lactate$lactate,na.rm=T)/sqrt(nrow(Lactate))
  
  
  fn.fig("C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/Satellite_tagging/Outputs/Port Jackson/Lactate",
         2400,2400)
  Lactate%>%
    ggplot(aes(tl,lactate,color=rel.cond))+
    geom_point(aes(size=soak.time))+
    theme_PA(leg.siz=12,axs.t.siz=12,axs.T.siz=14)+
    theme(legend.position = "top")+
    xlab('Total length (cm)')+
    ylab( expression(paste("Lactate (mmol L", ' '^{-1},")", sep = "")))+
    #scale_color_continuous(guide = 'none')+
    scale_color_discrete(name ='Release condition')+
    scale_size_continuous(name ="Soak time (hours)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0,1.1*max(Lactate$lactate)))+
    geom_hline(yintercept = Mn,alpha=0.5,size=1.5,color="forestgreen")+
    geom_hline(yintercept = Mn-1.96*SE,alpha=0.5,size=1,color="forestgreen",linetype = 2)+
    geom_hline(yintercept = Mn+1.96*SE,alpha=0.5,size=1,color="forestgreen",linetype = 2)
  
  dev.off()
  
}


#---------Longline selectivity ------------
if(do.Longline.selectivity)
{
  #function for predicting selectivity
  pred.normal.fixed=function(l,k,m,sigma) exp(-((l-k*m)^2)/(2*(sigma)^2))
  pred.normal.prop=function(l,m,a1,a2) exp(-(((l- a1*m)^2)/(2*a2*m^2)))
  pred.gamma=function(l,m,k,alpha) ((l/((alpha-1)*k*m))^(alpha-1))*exp(alpha-1-(l/(k*m)))
  pred.lognormal=function(l,m,m1,mu,sigma) (1/l)*exp(mu+(log(m/m1))-((sigma^2)/2)-((log(l)-mu-log(m/m1))^2)/(2*(sigma^2)))
  
  #function for predicting selectivity 
  out.sel=function(d,BEST,Fixed.equal.power)
  {
    if(Fixed.equal.power) DAT=d
    id=match(BEST$Model,names(DAT))
    
    #predict selectivity
    #Plotlen
    dat=data.frame(TL.cm=DAT[[id]]$plotlens)
    Pars=d[[id]]$gear.pars
    if(BEST$Model=="norm.loc")
    {
      k=Pars[match("k",rownames(Pars)),1]
      sigma=Pars[match("sigma",rownames(Pars)),1] 
      dat$'10'=pred.normal.fixed(l=dat$TL.cm,k,m=10,sigma)
      dat$'12'=pred.normal.fixed(l=dat$TL.cm,k,m=12,sigma)
      dat$'14'=pred.normal.fixed(l=dat$TL.cm,k,m=14,sigma)
    }
    if(BEST$Model=="norm.sca")
    {
      a1=Pars[match("k1",rownames(Pars)),1]
      a2=Pars[match("k2",rownames(Pars)),1] 
      dat$'10'=pred.normal.prop(l=dat$TL.cm,m=10,a1,a2)
      dat$'12'=pred.normal.prop(l=dat$TL.cm,m=12,a1,a2)
      dat$'14'=pred.normal.prop(l=dat$TL.cm,m=14,a1,a2)
    }
    if(BEST$Model=="gamma")
    {
      k=Pars[match("k",rownames(Pars)),1]
      alpha=Pars[match("alpha",rownames(Pars)),1] 
      dat$'10'=pred.gamma(l=dat$TL.cm,m=10,k,alpha)
      dat$'12'=pred.gamma(l=dat$TL.cm,m=12,k,alpha)
      dat$'14'=pred.gamma(l=dat$TL.cm,m=14,k,alpha)
    }
    if(BEST$Model=="lognorm")
    {
      m1=min(DAT[[id]]$meshsizes)
      mu=Pars[grep('mu1',rownames(Pars)),1]
      sigma=Pars[grep('sigma',rownames(Pars)),1] 
      dat$'10'=pred.lognormal(l=dat$TL.cm,m=10,m1,mu,sigma)
      dat$'12'=pred.lognormal(l=dat$TL.cm,m=12,m1,mu,sigma)
      dat$'14'=pred.lognormal(l=dat$TL.cm,m=14,m1,mu,sigma)
    }
    return(dat)
  }
  
  #wrapping function
  fn.LL.sel=function(d,Min.sample,Min.hooks,Size.Interval,convrt.this,Fitfunction,
                     Rtype,Min.length,Max.length)
  {
    #Table 1. Number of observations
    Table1=d%>%
      filter(!is.na(Size) &!is.na(hooksize) & !is.na(common_name) & !common_name=="Unidentified")%>%
      group_by(CAES_Code,common_name,scientific_name,hooksize)%>%
      tally()%>%
      spread(hooksize,n,fill=0)%>%
      data.frame%>%
      arrange(CAES_Code)%>%
      rename_with(~str_remove(., 'X'))%>%
      dplyr::select(-CAES_Code)%>%
      rename('Common name'=common_name,
             'Scientific name'=scientific_name)
    ft <- flextable(Table1)
    save_as_docx(ft,path = le.paste('Selectivity_LL/Table1.docx'))
    
    #Map
    do.map=FALSE
    if(do.map)
    {
      library("rnaturalearth")
      world <- ne_countries(scale = "medium", returnclass = "sf")
      Map.dat=d%>%distinct(sheet_no,.keep_all=T)
      Long.range=range(Map.dat$mid.long,na.rm=T)
      Lat.range=range(Map.dat$mid.lat,na.rm=T)
      Poly=data.frame(x=c(Long.range,rev(Long.range)),y=c(rep(Lat.range[1],2),rep(Lat.range[2],2)))
      Inset=ggplot(data = world) +
        geom_sf(color = "black", fill = "white") +
        coord_sf(xlim =c(Long.range[1],153) , ylim = c(-43,-11), expand = T)+
        theme(axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank())+ 
        annotate(geom="text", x=134, y=-24, label="Australia",size=12)+
        geom_polygon(data=Poly,aes(x,y),fill='black',alpha=.35)
      p=ggplot(data = world) +
        geom_sf(color = "black", fill = "grey60") +
        coord_sf(xlim =Long.range , ylim = Lat.range, expand = T) +
        xlab("Longitude") + ylab("Latitude")+
        geom_point(data=Map.dat,aes(x=mid.long, y=mid.lat,size=10),shape=21,alpha=0.4,fill="brown4")+
        theme(legend.title = element_text(size = 12),
              legend.text = element_text(size = 10),
              legend.position = "none",
              legend.key=element_blank(),
              legend.background=element_blank(),
              legend.direction = "vertical",
              legend.box = "horizontal",
              axis.text.x = element_text(size = 16),
              axis.text.y = element_text(size = 15.5),
              axis.title.x = element_text(size = 24),
              axis.title.y = element_text(size = 24),
              plot.margin=unit(c(.1,.1,.1,.1),"cm"))
      p+annotation_custom(ggplotGrob(Inset),xmin = 115, xmax = 119, ymin = -32, ymax = -30.5)
      ggsave(le.paste('Selectivity_LL/Figure 1.tiff'), width = 8,height = 12,dpi = 300, compression = "lzw")
      
    }
    
    #Select species with enough observations
    Tabl=d%>%
      filter(!is.na(Size) & !is.na(common_name))%>%
      group_by(common_name,method)%>%
      tally()%>%
      spread(method,n,fill=0)%>%
      filter(LL>Min.sample)%>%
      pull(common_name)
    d=d%>%filter(common_name%in%Tabl & hooksize%in%c(10,12,14))
    TAB=table(d$common_name,d$hooksize)
    TAB[TAB<Min.sample]=0
    TAB[TAB>=Min.sample]=1
    d=d%>%
      filter(common_name%in%names(which(rowSums(TAB)>=Min.hooks)) & !is.na(hooktype))
    
    #Convert FL to TL
    d=d%>%
      mutate(a_FL.to.TL=ifelse(common_name=="Port Jackson",1.105265,a_FL.to.TL),
             b_FL.to.TL=ifelse(common_name=="Port Jackson",0.4579532,b_FL.to.TL),
             tl=ifelse(common_name=="Port Jackson" & tl>150,NA,tl),
             tl=ifelse(common_name%in% convrt.this & is.na(tl),
                       fl*a_FL.to.TL+b_FL.to.TL,tl))%>%
      filter(!is.na(tl))
    
    #Check increase in total length with hook size
    colfunc <- colorRampPalette(c("red","orange"))
    cols=colfunc(length(unique(d$hooktype)))
    names(cols)=sort(unique(d$hooktype))
    size.vs.hook=d%>%
      group_by(hooksize,hooktype,common_name)%>%
      summarise(Mean=mean(tl),
                SD=sd(tl))%>%
      mutate(Hook=as.character(hooktype))
    size.vs.hook%>%
      mutate(hooksize=ifelse(hooktype=='Circular',hooksize+.15,hooksize))%>%
      ggplot(aes(x=hooksize, y= Mean, colour=Hook)) + 
      geom_errorbar(aes(ymin= Mean-SD, ymax= Mean+SD), width=.1) +
      geom_point(size=3) +
      ylab("Mean total length (+/-SD)") + xlab("Hook size") +
      facet_wrap(vars(common_name), scales = "free_y")+
      scale_color_manual("Hook type",values=cols)+
      theme_PA(Ttl.siz=18,strx.siz=16,leg.siz=16,axs.t.siz=13,axs.T.siz=20)+
      theme(legend.position="top",
            legend.title = element_blank())
    ggsave(le.paste('Selectivity_LL/Figure_S1.tiff'), width = 10,height = 10, dpi = 300, compression = "lzw")
    
    #drop if size not increasing with hook size
    size.vs.hook.sp=size.vs.hook%>%
      dplyr::select(-SD,-Hook)%>%
      mutate(hooksize=paste("Size.",hooksize,sep=''))%>%
      spread(hooksize,Mean)%>%
      mutate(Keep=ifelse(Size.14>Size.12,'Yes',"No"),
             Keep=ifelse(Size.12>Size.10 & Keep=='Yes','Yes',"No"))%>%
      filter(Keep=="Yes")%>%
      pull(common_name)
    #d=d%>%filter(common_name%in%unique(size.vs.hook.sp))
    
    #add size clase
    d1=d%>%
      mutate(size.class=Size.Interval*floor(tl/Size.Interval)+Size.Interval/2)%>%
      dplyr::select(common_name,hooksize,hooktype,size.class,tl,fl,Size)
    
    #Display size frequency
    sp=unique(d1$common_name)
    library(lemon)
    fig2=function(d1)
    {
      cols=colfunc(length(unique(d1$hooksize)))
      p=d1 %>%
        ggplot( aes(x=TL, fill=hooksize)) +
        geom_histogram(binwidth = 5, alpha=0.8,colour='grey40',size=.1)  +
        facet_wrap(vars(common_name), scales = "free")+
        xlab("Total length (cm)")+ ylab("Frequency")+scale_fill_manual(values=cols)+
        theme_PA(strx.siz=18,leg.siz=16,axs.t.siz=16,axs.T.siz=20)+
        theme(legend.position="top",
              legend.title=element_blank())
      print(p)
    }
    fig2(d1=d%>%
           mutate(hooksize=as.factor(paste(hooksize,"/0",sep='')),
                  TL=tl)%>%
           filter(common_name%in%sp))
    ggsave(le.paste('Selectivity_LL/Figure 2.tiff'), width = 10,height = 10, dpi = 300, compression = "lzw")
    
    #calculate selectivity by species based on Millar & Holst 1997
    hk.typ=unique(d1$hooktype)
    PlotLens=seq(Min.length+Size.Interval/2,Max.length-Size.Interval/2,by=Size.Interval)
    Store.sel=vector('list',length(sp))
    names(Store.sel)=sp
    Store.tab=Store.sel
    for(i in 1:length(sp))
    {
      Store=vector('list',length(hk.typ))
      names(Store)=hk.typ
      store.tab=Store
      for(h in 1:length(hk.typ))
      {
        #Tabulate observations by mid size class and hook size   
        tab=d1%>%
          filter(common_name==sp[i] & hooktype==hk.typ[h])%>%
          group_by(hooksize,size.class,common_name)%>%
          summarise(n=n())
        Hooksize=as.numeric(unique(tab$hooksize)) 
        tab=tab%>%
          spread(hooksize,n,fill=0)%>%
          data.frame
        
        #Fit Miller model with equal fishing power
        pwr=rep(1,length(Hooksize))
        Equal.power=vector('list',length(Rtype))
        names(Equal.power)=Rtype
        if(Fitfunction=='gillnetfit')
        {
          for(f in 1:length(Equal.power))
          {
            Equal.power[[f]]=gillnetfit(data=as.matrix(tab%>%dplyr::select(-'common_name')),
                                        meshsizes=Hooksize,
                                        type=Rtype[f],
                                        rel=pwr,
                                        plots=c(F,F),
                                        plotlens=PlotLens,
                                        plotlens_age=100,
                                        details=T)
          }
        }
        Store[[h]]=Equal.power
        store.tab[[h]]=tab
      }
      Store.sel[[i]]=Store
      Store.tab[[i]]=store.tab
    }
    
    #Tabulate model fit
    fn.rnd=function(x) sprintf(round(x,2), fmt = '%#.2f')
    Table.mod.fit=vector('list',length(sp))
    names(Table.mod.fit)=sp
    for(s in 1:length(sp))
    {
      dummy=vector('list',length(hk.typ))
      for(h in 1:length(hk.typ))
      {
        Tab=data.frame(Model=names(Store.sel[[s]][[h]]))%>%
          mutate(Equal_Param1=NA,
                 Equal_Param2=NA,
                 Equal_Deviance=NA,
                 hooktype=hk.typ[h])%>%
          relocate(hooktype)
        for(f in 1:length(Rtype))
        {
          Tab$Equal_Deviance[f]=fn.rnd(Store.sel[[s]][[h]][[f]]$fit.stats['model_dev'])
          PaR=fn.rnd(Store.sel[[s]][[h]][[f]]$gear.pars[1:2,'estimate'])
          errOr=fn.rnd(Store.sel[[s]][[h]][[f]]$gear.pars[1:2,'s.e.'])
          Tab$Equal_Param1[f]=paste(PaR[1]," (",errOr[1],")",sep='')
          Tab$Equal_Param2[f]=paste(PaR[2]," (",errOr[2],")",sep='')
          rm(PaR,errOr)
        }
        dummy[[h]]=Tab
      }
      Table.mod.fit[[s]]=do.call(rbind,dummy)
    }
    Table.mod.fit=do.call(rbind,Table.mod.fit)
    Table.mod.fit$Species=sub("*\\.[0-9]", "", rownames(Table.mod.fit))
    Table.mod.fit=Table.mod.fit%>%
      relocate(Species)%>%
      mutate(Model=ifelse(Model=="norm.loc","Normal (fixed spread)",
                          ifelse(Model=="norm.sca","Normal (prop. spread)",
                                 ifelse(Model=="gamma","Gamma",
                                        ifelse(Model=="lognorm","Lognormal",
                                               Model)))))
    Table.mod.fit$Species[duplicated(Table.mod.fit$Species)]=""
    Table.mod.fit[is.na(Table.mod.fit)] <- ""
    Table.mod.fit[Table.mod.fit=="NaN"] <- ""
    row.names(Table.mod.fit)=NULL
    ft <- flextable(Table.mod.fit)
    save_as_docx(ft,path = le.paste('Selectivity_LL/Table2.docx'))
    
    
    #select best fit
    Best.fit=vector('list',length(sp))
    names(Best.fit)=sp
    for(i in 1:length(sp))
    {
      Store=vector('list',length(hk.typ))
      names(Store)=hk.typ
      for(h in 1:length(hk.typ))
      {
        Tab=data.frame(Model=names(Store.sel[[i]][[h]]),
                       Equal_dev=NA)
        for(f in 1:length(Rtype))
        {
          Tab$Equal_dev[f]=Store.sel[[i]][[h]][[f]]$fit.stats['model_dev']
        }
        Store[[h]]=Tab[which.min(Tab[,2]),]%>%mutate(Fishing.power="Equal.power")%>%rename(Dev=Equal_dev)
      }
      Best.fit[[i]]=Store
    }
    
    
    #Plot fit residuals
    fn.plt.FigS3=function(h)
    {
      for(s in 1:length(sp))
      {
        for(f in 1:length(Store.sel[[s]][[h]]))
        {
          with(Store.sel[[s]][[h]][[f]],
               {
                 MAIN=""
                 plot.resids(devres,meshsizes,lens,title=MAIN)
               })
          if(s==1)
          {
            MAIN=with(Store.sel[[s]][[h]][[f]],
                      ifelse(type=="norm.loc","Normal (fixed spread)",
                             ifelse(type=="norm.sca","Normal (prop. spread)",
                                    ifelse(type=="gamma","Gamma",
                                           ifelse(type=="lognorm","Lognormal",NA)))))
            mtext(MAIN,3,cex=.95)
          }
        }
        mtext( sp[s],4,cex=1,las=3)
      }
    }
    tiff(file=le.paste("Selectivity_LL/Figure.S3_Ezb.tiff"),width = 2100, height = 2400,units = "px", res = 300, compression = "lzw")    
    par(mfrow=c(length(sp),length(Rtype)),mar=c(1.5,1.2,.2,.3),oma=c(1.75,2,1,1),mgp=c(1,.5,0),las=1)
    fn.plt.FigS3(h=1)
    mtext("Total length (mm)",1,outer=T,line=.35,cex=1.25)
    mtext("Hook size (/0)",2,outer=T,line=.35,cex=1.25,las=3)
    dev.off()
    
    tiff(file=le.paste("Selectivity_LL/Figure.S3_Circular.tiff"),width = 2100, height = 2400,units = "px", res = 300, compression = "lzw")    
    par(mfrow=c(length(sp),length(Rtype)),mar=c(1.5,1.2,.2,.3),oma=c(1.75,2,1,1),mgp=c(1,.5,0),las=1)
    fn.plt.FigS3(h=2)
    mtext("Total length (mm)",1,outer=T,line=.35,cex=1.25)
    mtext("Hook size (/0)",2,outer=T,line=.35,cex=1.25,las=3)
    dev.off()
    
    #Compare observed VS predicted distribution
    Cols.type=c('blue','brown','forestgreen','red')
    names(Cols.type)=Rtype
    fn.freq.obs.pred1=function(Sizes,Obs,dd,hook.names)
    {
      #Calculated expected population frequency
      N.fish.caught=vector('list',length(dd))
      names(N.fish.caught)=names(dd)
      for(f in 1:length(dd))
      {
        Lns=dd[[f]]$lens
        iid=match(Lns,dd[[f]]$plotlens)
        Pred.sel=dd[[f]]$rselect[iid,]
        Total.sel=rowSums(Pred.sel)
        Total.obs=rowSums(Obs)
        Rel.num.in.pop=sapply(Total.obs/Total.sel,function(x) max(x,0.1))
        Rel.prop.in.pop=Rel.num.in.pop/sum(Rel.num.in.pop)
        N.mesh=colSums(Obs)
        Expnd.N.mesh=matrix(rep(N.mesh,each=nrow(Pred.sel)),ncol=ncol(Pred.sel))
        Expnd.Rel.prop.in.pop=matrix(rep(Rel.prop.in.pop,ncol(Pred.sel)),ncol=ncol(Pred.sel))
        Sum.prod=colSums(Pred.sel*Expnd.Rel.prop.in.pop)
        Sum.prod=matrix(rep(Sum.prod,each=nrow(Pred.sel)),ncol=ncol(Pred.sel))
        N.fish.caught[[f]]=(Expnd.N.mesh*Expnd.Rel.prop.in.pop*Pred.sel)/Sum.prod
      }
      
      #Plot by mesh
      n=ncol(Obs)
      for(i in 1:ncol(Obs))
      {
        plot(Sizes,Obs[,i],type='h',ylab='',xlab='', lwd = 4,col=rgb(.5,.5,.5,alpha=.5))
        for(m in 1:length(N.fish.caught))
        {
          lines(Sizes,N.fish.caught[[m]][,i],col=Cols.type[m],lwd=1.5)
        }
        if(hook.names) mtext(paste(substr(names(Obs[i]),2,5),'/0',sep=''),3,cex=.9)
      }
    }
    
    tiff(file=le.paste("Selectivity_LL/Figure.S4_EZb.tiff"),width = 2400, height = 2200,units = "px", res = 300, compression = "lzw")    
    par(mfrow=c(length(sp),length(unique(d$hooksize))),mar=c(1.5,1.2,.2,.65),oma=c(1.5,2.5,.8,1.2),mgp=c(1,.5,0),las=1,xpd=T)
    for(s in 1:length(sp))
    {
      h=1
      if(s==1) HK.nm=TRUE else HK.nm=FALSE
      fn.freq.obs.pred1(Sizes=Store.tab[[s]][[h]]%>%pull(size.class),
                        Obs=Store.tab[[s]][[h]]%>%dplyr::select(-c(size.class,common_name)),
                        dd=Store.sel[[s]][[h]],
                        hook.names=HK.nm)
      if(s==1)legend("topright",names(Cols.type),bty='n',text.col=Cols.type,cex=1.25)
      mtext(sp[s],4,line=.5,cex=1,las=3)
    }
    mtext("Frequency", side = 2,outer=T, line = 1,las=3,cex=1.2)
    mtext("Total length (cm)",1,outer=T,line=.4,cex=1.2)
    dev.off()
    
    tiff(file=le.paste("Selectivity_LL/Figure.S4_Circular.tiff"),width = 2400, height = 2200,units = "px", res = 300, compression = "lzw")    
    par(mfrow=c(length(sp),length(unique(d$hooksize))),mar=c(1.5,1.2,.2,.65),oma=c(1.5,2.5,.8,1.2),mgp=c(1,.5,0),las=1,xpd=T)
    for(s in 1:length(sp))
    {
      h=2
      if(s==1) HK.nm=TRUE else HK.nm=FALSE
      fn.freq.obs.pred1(Sizes=Store.tab[[s]][[h]]%>%pull(size.class),
                        Obs=Store.tab[[s]][[h]]%>%dplyr::select(-c(size.class,common_name)),
                        dd=Store.sel[[s]][[h]],
                        hook.names=HK.nm)
      if(s==1)legend("topright",names(Cols.type),bty='n',text.col=Cols.type,cex=1.25)
      mtext(sp[s],4,line=.5,cex=1,las=3)
    }
    mtext("Frequency", side = 2,outer=T, line = 1,las=3,cex=1.2)
    mtext("Total length (cm)",1,outer=T,line=.4,cex=1.2)
    dev.off()
    
    #Predict selectivity by species and hook type
    Pred.sel=vector('list',length(sp))
    names(Pred.sel)=sp
    for(s in 1:length(sp))
    {
      Store=vector('list',length(hk.typ))
      names(Store)=hk.typ
      for(h in 1:length(hk.typ))
      {
        dummy=out.sel(d=Store.sel[[s]][[h]],
                      BEST=Best.fit[[s]][[h]],
                      Fixed.equal.power=TRUE)
        
        dummy=dummy%>%
          gather(Hook.size,Selectivity,-TL.cm)%>%
          mutate(Hooktype=hk.typ[h],
                 Species=sp[s])
        
        Store[[h]]=dummy
        rm(dummy)
      }
      Pred.sel[[s]]=do.call(rbind,Store)
    }
    Pred.sel=do.call(rbind,Pred.sel)
    
    #Display selectivity
    p.list=vector('list',length(hk.typ))
    for(h in 1:length(hk.typ))
    {
      p.list[[h]]=Pred.sel%>%
        filter(Hooktype==hk.typ[h])%>%
        mutate(Hook.size=as.factor(paste(Hook.size,'/0',sep='')))%>%
        ggplot(aes(x=TL.cm,y=Selectivity,col=Hook.size))+
        geom_line(size=1.15)+
        facet_wrap(~Species,nrow=1)+
        labs(x="",y="",subtitle=hk.typ[h])+
        theme_PA(Sbt.siz=22,strx.siz=20,leg.siz=20,axs.t.siz=16,axs.T.siz=20)+
        theme(legend.position = "top",
              legend.title = element_blank())
    }
    fig=ggarrange(plotlist=p.list,
                  nrow = 2,
                  common.legend=TRUE)
    annotate_figure(fig,
                    bottom = text_grob("Total length (cm)",size = 20),
                    left = text_grob("Relative selectivity",size = 20,rot = 90))
    ggsave(le.paste("Selectivity_LL/Relative selectivity.tiff"),width = 12,height = 8,compression = "lzw")
  }
  
  fn.LL.sel(d=DATA%>%filter(method=='LL' & !is.na(Size)),
            Min.sample=30,  
            Min.hooks=2,
            Size.Interval=5,
            convrt.this=c("Dusky shark","Gummy shark","Port Jackson"),
            Fitfunction='gillnetfit',
            Rtype=c("norm.loc","norm.sca","gamma","lognorm"),
            Min.length=20,
            Max.length=300) 
}


#---------General tables and plots of PA underwater Video ------------ 
SP.group.levels=c("Invertebrates","Scalefish","Sharks","Rays","Marine mammals","Seabirds","Turtles")
TEP.groups=c("Marine mammals","Seabirds","Reptiles","Turtles")
if(do.general.underwater)
{

  #Table Mean soak time by gear
  Mean.soak.time=DATA%>%
                  distinct(sheet_no,.keep_all=T)%>%
                  group_by(method)%>%
                  summarise(Mean=mean(soak.time,na.rm=T),
                            MIN=min(soak.time,na.rm=T),
                            MAX=max(soak.time,na.rm=T))%>%
                  arrange(method)
  write.csv(Mean.soak.time,le.paste("Video/underwater/Table_Mean soak time by gear.csv"),row.names = F)
  
  #Table number of shots, camera deployments and hours of footage by gear
  Table.underwater.shots.hours.deployments=data.frame(
    method=c('Gillnet','Longline'),
    Shots=c(length(unique(Video.net.interaction$sheet_no)),length(unique(Video.longline.interaction$sheet_no))),
    Camera.deployments=c(length(unique(Video.net.interaction$OpCode)),length(unique(Video.longline.interaction$OpCode))),
    Hours.of.footage=c(hours.underwater.gn,hours.underwater.ll))
  write.csv(Table.underwater.shots.hours.deployments,le.paste("Video/underwater/Table_Shots_deployments_hours.csv"),row.names = F)
  

  #Combine data sets
  #note: must treat 'swim past' and 'swim through' as  MaxN per shot
  Dat.comb=rbind(Video.longline.interaction%>%
                   distinct(OpCode,sheet_no,Method,Interaction,Number,SP.group,
                            Family,Genus,Species,taxa,taxa.to.mutate,Code),
                 Video.net.interaction%>%
                   distinct(OpCode,sheet_no,Method,Interaction,Number,SP.group,
                            Family,Genus,Species,taxa,taxa.to.mutate,Code))%>%
            filter(!taxa%in%c("no fish","no haul","snail"))%>%
    filter(!Interaction=='Missing data')%>%
    filter(!Interaction%in%NA)%>%
    mutate(Method=capitalize(Method))
  
  Dat.comb.swim=Dat.comb%>%filter(grepl('Swim',Interaction))
  Dat.comb=Dat.comb%>%filter(!grepl('Swim',Interaction))
  
  Dat.comb.swim=Dat.comb.swim%>%
    group_by(OpCode,sheet_no,Method,Interaction,Number,SP.group,
             Family,Genus,Species,taxa,taxa.to.mutate,Code)%>%
    mutate(MaxN=max(Number,na.rm=T))%>%
    ungroup()%>%
    mutate(MaxN=ifelse(MaxN=='-Inf',NA,MaxN))%>%
    group_by(sheet_no,Method,Interaction,SP.group,Family,Genus,Species,taxa,taxa.to.mutate,Code)%>%
    filter(MaxN==max(MaxN,na.rm=T))%>%
    ungroup()%>%
    dplyr::select(-MaxN)
  
  Dat.comb=rbind(Dat.comb,Dat.comb.swim)%>%
    mutate(Interaction=ifelse(grepl('Caught',Interaction),'Caught',Interaction))
  
  #Table of species and behaviours 
  Tabl1=Dat.comb%>%
    group_by(taxa,Code,SP.group,Interaction)%>%
    summarise(Number=sum(Number,na.rm=T))%>%
    ungroup%>%
    spread(Interaction,Number,fill='')%>%
    arrange(SP.group,taxa)%>%
    left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by='Code')%>%
    mutate(COMMON_NAME=case_when(is.na(COMMON_NAME) & Code==25154008 ~'Seven legged starfish',
                                 is.na(COMMON_NAME) & Code==23607901 ~'Cuttlefish',
                                 is.na(COMMON_NAME) & Code==23617909 ~'Squid',
                                 is.na(COMMON_NAME) & Code==41112006 ~'Humpback whale',
                                 is.na(COMMON_NAME) & Code==37038902 ~'Stingarees',
                                 is.na(COMMON_NAME) & Code==37337915 ~'Trevallies',
                                 is.na(COMMON_NAME) & Code==37408000 ~'Blennies',
                                 is.na(COMMON_NAME) & Code==37311919 ~'Seaperches',
                                 is.na(COMMON_NAME) & Code==37337916 ~'Seaperches',
                                 is.na(COMMON_NAME) & Code==37337917 ~'Trevallies',
                                 is.na(COMMON_NAME) & Code==37365905 ~'Butterflyfishes',
                                 is.na(COMMON_NAME) & Code==37372907 ~'Damselfishes',
                                 is.na(COMMON_NAME) & Code==37337901 ~'Trevallies',
                                 is.na(COMMON_NAME) & Code==37465902 ~'Leatherjackets',
                                 is.na(COMMON_NAME) & Code==37465921 ~'Leatherjackets',
                                 is.na(COMMON_NAME) & Code==37259902 ~'Pineapplefishes',
                                 is.na(COMMON_NAME) & Code==37099999 ~'Osteichthyes',
                                 is.na(COMMON_NAME) & Code==37390901 ~'Grubfishes',
                                 is.na(COMMON_NAME) & Code==37357903 ~'Bullseyes',
                                 is.na(COMMON_NAME) & Code==37461916 ~'Righteye flounders',
                                 is.na(COMMON_NAME) & Code==37365916 ~'Angelfishes',
                                 is.na(COMMON_NAME) & Code==37372918 ~'Damselfishes',
                                 is.na(COMMON_NAME) & Code==37460919 ~'Sand flounders',
                                 is.na(COMMON_NAME) & Code==37330904 ~'Whitings',
                                 is.na(COMMON_NAME) & Code==37467912 ~'Toadfishes',
                                 is.na(COMMON_NAME) & Code==37337904 ~'Trevallies',
                                 is.na(COMMON_NAME) & Code==37337907 ~'Trevallies',
                                 is.na(COMMON_NAME) & Code==39020000 ~'Sea turtle',
                                 TRUE~COMMON_NAME))%>%
    distinct(COMMON_NAME,taxa,.keep_all = T)%>%
    rename(Scientific.name=taxa)%>%
    dplyr::select(-Code)%>%
    relocate(SP.group,COMMON_NAME,Scientific.name)
  write.csv(Tabl1,le.paste("Video/underwater/Table_species & interactions.csv"),row.names = F)
  
  these.codes=c(37018003,37017003,37017001,37018007,
                37377004,37384002,37320004,37353001)
  these.codes.discards=c(37007001,37039001)
  
  Tabl1=Dat.comb%>%
    filter(Code%in%c(these.codes.discards,these.codes))%>%
    mutate(Discarded=ifelse(Code%in%these.codes.discards,'Discarded','Retained'))%>%
    group_by(Discarded,taxa,Code,Interaction)%>%
    summarise(Number=sum(Number,na.rm=T))%>%
    ungroup%>%
    spread(Interaction,Number,fill='')%>%
    arrange(Discarded,taxa)%>%
    left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by='Code')%>%
    rename(Scientific.name=taxa)%>%
    dplyr::select(-Code)%>%
    relocate(Discarded,COMMON_NAME,Scientific.name)
  write.csv(Tabl1,le.paste("Video/underwater/Table_species & interactions_main species.csv"),row.names = F)
  
  
  
  #1. number of events 
    #1.1. by gear and species group          
  Tab=Dat.comb%>%
    filter(!SP.group%in%c('Invertebrates',TEP.groups))%>%
    filter(!SP.group%in%NA)%>%
    mutate(Number=1,
           Method=capitalize(Method))%>%
    group_by(Method,Interaction,SP.group)%>%
    tally(Number)%>%
    filter(!is.na(Interaction))%>%
    mutate(Interaction=capitalize(tolower(Interaction)),
           SP.group=factor(SP.group,levels=SP.group.levels))
  write.csv(Tab%>%spread(Interaction,n,fill='')%>%arrange(Method,SP.group),
            le.paste("Video/underwater/Interactions_number.events_sqrt.transf_by.group.csv"),row.names = F)
  
  Tab%>%
    ggplot(aes(fill=Method, y=n, x=Interaction)) + 
    geom_bar(position="dodge", stat="identity")+
    geom_text(aes(label = n, group = Method),
              position = position_dodge(width = 1),
              vjust = 0, hjust = 1, size = 5,color='black')+
    coord_flip() + scale_y_sqrt()+
    facet_wrap(~SP.group,dir='h',scales='free_x')+ 
    theme_PA(strx.siz=17,leg.siz=18,axs.t.siz=14,axs.T.siz=16)+ 
    theme(legend.position = "top",
          strip.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          axis.text=element_text(size=14),
          axis.title=element_text(size=16))+
    xlab('')+ylab('Number of events')+ guides(color = guide_legend(nrow = 1))
  ggsave(le.paste("Video/underwater/Interactions_number.events_sqrt.transf_by.group.tiff"),
         width = 12,height = 10,compression = "lzw")
  
  
    #1.2 Species caught by gear        
  Tab=Dat.comb%>%
    filter(!SP.group%in%c('Invertebrates',TEP.groups))%>%
    filter(!SP.group%in%NA)%>%
    filter(Interaction=="Caught")%>%
    mutate(Number=1,
           Method=capitalize(Method))%>%
    group_by(Method,SP.group,taxa)%>%
    tally(Number)%>%
    mutate(SP.group=factor(SP.group,levels=SP.group.levels))
  write.csv(Tab%>%spread(Method,n,fill='')%>%arrange(SP.group),
            le.paste("Video/underwater/Interactions_number.events_species.caught.by.gear.csv"),row.names = F)
  
  
    #1.3. by gear
  Tab=Dat.comb%>%
    filter(!SP.group%in%c('Invertebrates',TEP.groups))%>%
    filter(!SP.group%in%NA)%>%
    filter(!Interaction=='Missing data')%>%
    mutate(Number=1,
           Method=capitalize(Method))%>%
    group_by(Method,Interaction)%>%
    tally(Number)%>%
    filter(!is.na(Interaction))%>%
    mutate(Interaction=capitalize(tolower(Interaction)))
  write.csv(Tab%>%spread(Interaction,n,fill='')%>%arrange(Method),
            le.paste("Video/underwater/Interactions_number.events_sqrt.transf.csv"),row.names = F)
  Tab_total_non_ETPs=Tab%>%group_by(Interaction)%>%summarise(n=sum(n))%>%rename(Non_ETPs=n)
  
  Tab%>%
    ggplot(aes(fill=Method, y=n, x=Interaction)) + 
    geom_bar(position="dodge", stat="identity")+
    geom_text(aes(label = n, group = Method),
              position = position_dodge(width = 1),
              vjust = 0, hjust = 1, size = 5,color='black')+
    
    coord_flip() + scale_y_sqrt()+
    facet_wrap(~Method,dir='h',scales='free_x')+ 
    theme_PA(strx.siz=17,leg.siz=18,axs.t.siz=14,axs.T.siz=16)+ 
    theme(legend.position = "top",
          strip.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          axis.text=element_text(size=14),
          axis.title=element_text(size=16))+
    xlab('')+ylab('Number of events')+ guides(color = guide_legend(nrow = 1))
  ggsave(le.paste("Video/underwater/Interactions_number.events_sqrt.transf.tiff"),width = 12,height = 8,compression = "lzw")
  
  
  #1.3. ETPs by gear
   Dat.comb.ETPs=Dat.comb%>%
    filter(SP.group%in%TEP.groups | Code%in%TEPS_Shark.rays)%>%
    mutate(Interaction=capitalize(tolower(Interaction)),
           Species=case_when(taxa=='Bathytoshia brevicaudata' ~ 'Smooth stingray',
                             taxa=='Carcharias taurus' ~ 'Grey nurse shark',
                             taxa=='Carcharodon carcharias' ~ 'White shark',
                             taxa=='Cheloniidae spp' ~ 'Sea turtle',
                             taxa=='Megaptera novaeangliae' ~ 'Humpback whale',
                             taxa=='Phalacrocoracidae' ~ 'Cormorants',
                             taxa=='Neophoca cinerea' ~ 'Australian sea-lion'))
  
  TEPS.cols1=TEPS.cols
  names(TEPS.cols1)[match('Green turtle',names(TEPS.cols1))]='Sea turtle'
  TEPS.cols1=TEPS.cols1[match(unique(Dat.comb.ETPs$Species),names(TEPS.cols1))]
  Dat.comb.ETPs=Dat.comb.ETPs%>%
    mutate(Species=factor(Species,levels=names(TEPS.cols1)))
  
    #1.3.1 barplot  
  Tab=Dat.comb.ETPs%>%
    group_by(Method,Interaction,Species)%>%
    tally(Number)
  write.csv(Tab%>%spread(Interaction,n,fill='')%>%arrange(Method,Species),
            le.paste("Video/underwater/Interactions_number.events_ETPs.csv"),row.names = F)
  
  write.csv(Tab_total_non_ETPs%>%
              left_join(Tab%>%group_by(Interaction)%>%summarise(n=sum(n))%>%rename(ETPs=n),
                        by='Interaction')%>%
              mutate(ETPs=ifelse(is.na(ETPs),0,ETPs),
                     Percent_ETP=100*ETPs/(ETPs+Non_ETPs)),
            le.paste("Video/underwater/Table_Percent ETPs_event_interactions.csv"),row.names = F)
  
  Tab%>%
    filter(!is.na(Interaction))%>%
    mutate(Interaction=capitalize(tolower(Interaction)))%>%
    ggplot(aes(x=Interaction, y=n, fill=Species)) + 
    geom_bar(position="stack", stat="identity")+
    geom_text(aes(label = n),
              hjust = 0.5, vjust = 0, position = position_stack(vjust = 0.5),
              size = 5,color='black')+
    coord_flip()+
    facet_wrap(~Method,dir='h',scales='free_x')+ 
    theme_PA(str.siz=16,strx.siz=16,leg.siz=17,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "top",
          legend.justification='left',
          legend.direction='horizontal',
          legend.title = element_blank())+
    xlab('')+ylab('Number of events')+
    scale_y_continuous(breaks = integer_breaks())+
    guides(fill = guide_legend(nrow = 2))+
    scale_x_discrete(position = "top")+
    scale_fill_manual(values=TEPS.cols1, drop=FALSE)
  ggsave(le.paste("Video/underwater/Interactions_number.events_ETPs.tiff"),width = 12,height = 8,compression = "lzw")
  
    #1.3.2 Mosaic plot
  fn.mosaic=function(d)
  {
    TAB=table(d%>%pull(Interaction))
    lab=c(TAB)
    lab=paste(names(TAB)," (n= ",lab,")",sep='')
    names(lab)=names(TAB)
    
    p=d%>%
      ggplot() +
      geom_mosaic(aes(x = product(Species), fill=Species), na.rm=TRUE) + 
      labs(x = "",y='', title=unique(d$Method))+
      facet_wrap(~Interaction,labeller = labeller(Interaction=lab),nrow=1) +
      labs(fill = "")+
      scale_fill_manual(values=TEPS.cols1, drop=FALSE)+
      theme_PA()+
      theme(legend.position = 'top',axis.text.x=element_blank(), 
            axis.ticks.x=element_blank(), 
            axis.text.y=element_blank(), 
            axis.ticks.y=element_blank())
    return(p)
  }
  p1=fn.mosaic(d=Dat.comb.ETPs%>%filter(Method=='Gillnet'))
  p2=fn.mosaic(d=Dat.comb.ETPs%>%filter(Method=='Longline'))
  ggarrange(plotlist=list(p1+rremove("xlab")+rremove("ylab"),
                          p2+rremove("xlab")+rremove("ylab")),
            nrow = 2,
            common.legend=TRUE)
  
  ggsave(le.paste("Video/underwater/Interactions_number.events_ETPs_mosaic.tiff"), 
         width = 9,height = 6,compression = "lzw")
  
  
  #2. Number of individuals  
    #2.1. by gear and species group
  Dat.comb%>%
    filter(!SP.group%in%c('Invertebrates',TEP.groups))%>%
    filter(!SP.group%in%NA)%>%
    filter(!Interaction=='Missing data')%>%
    mutate(Method=capitalize(Method))%>%
    group_by(Method,Interaction,SP.group)%>%
    summarise(n=sum(Number,na.rm=T))%>%
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
  Dat.comb%>%
    filter(!SP.group%in%c('Invertebrates',TEP.groups))%>%
    filter(!SP.group%in%NA)%>%
    filter(!Interaction=='Missing data')%>%
    mutate(Method=capitalize(Method))%>%
    group_by(Method,Interaction)%>%
    summarise(n=sum(Number,na.rm=T))%>%
    filter(!is.na(Interaction))%>%
    mutate(Interaction=capitalize(tolower(Interaction)))%>%
    ggplot(aes(fill=Method, y=n, x=Interaction)) + 
    geom_bar(position="dodge", stat="identity")+
    coord_flip() + scale_y_sqrt()+
    facet_wrap(~Method,dir='h',scales='free_x')+ 
    theme_PA(strx.siz=17,leg.siz=18,axs.t.siz=12,axs.T.siz=16)+
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
  d=Dat.comb%>%
    filter(!SP.group%in%c('Invertebrates',TEP.groups))%>%
    filter(!SP.group%in%NA)%>%
    filter(!Interaction=='Missing data')%>%
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
  if(do.Abbeys)
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
    write.csv(Abbey.data.chapter.1_species,handl_OneDrive('Analyses/Parks Australia/outputs/Data for Abbey/Abbey.data.chapter.1_species.csv'),row.names = F)
    write.csv(Abbey.data.chapter.1_SP.group,handl_OneDrive('Analyses/Parks Australia/outputs/Data for Abbey/Abbey.data.chapter.1_SP.group.csv'),row.names = F)
    write.csv(Abbey.data.chapter.1_Retain.group,handl_OneDrive('Analyses/Parks Australia/outputs/Data for Abbey/Abbey.data.chapter.1_Retain.group.csv'),row.names = F)
    
  }
  
  
  #4. Lolipop approach
  
    #4.1 Lollipop graph of % of interaction types by gear 
  df=Dat.comb%>%
    filter(!SP.group%in%c('Invertebrates',TEP.groups))%>%
    filter(!SP.group%in%NA)%>%
    filter(!Interaction=='Missing data')
  df=pct_routine(df, Method, Interaction)
  ggplot(df)+
    geom_linerange(aes(x = Interaction, ymin = 0, ymax = pct, colour = Method), 
                   position = position_dodge(width = 1))+
    geom_point(aes(x = Interaction, y = pct, colour = Method),
               position = position_dodge(width = 1))+
    coord_flip()+ylab('Percentage')
  ggsave(le.paste("Video/underwater/Interactions_lollipop.tiff"), 
         width = 6,height = 6,compression = "lzw")
  
    #4.2 Mosaic plot
  do.mosaic=FALSE #wrong, not using MaxN
  if(do.mosaic)
  {
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
           width = 9,height = 6,compression = "lzw")
    
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
           width = 9,height = 6,compression = "lzw")
    
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
  ggarrange(plotlist=list(out.under.n$p.mdsGN+rremove("xlab")+rremove("ylab"),
                          out.under.n$p.mdsLL+rremove("xlab")+rremove("ylab")),
            ncol = 1,
            common.legend=TRUE)
  ggsave(le.paste("Video/underwater/interactions_MDS.tiff"),
         width = 5,height = 8,compression = "lzw")
  
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
  
  
  
  #fn.test.underwtr(what='Events',trans='squareroot')
  
}



#---------Analyse PA Habitat ------------
if(do.habitat)
{
  #aca: missing, combine Santiago's and Jacks and update figures
  #Santiago's data in M:\Production Databases\Shark\ParksAustralia_2019\Habitat Analysis\TMobs
  if(!exists('Video.habitat'))  #original data put together by Jack
  {
    Video.habitat<- read_excel(handl_OneDrive("Parks Australia/2019_project/Data/cameras/Gillnet_longline habitat.xlsx"), 
                                    sheet = "gillnet habitat")
    Video.habitat.LL<- read_excel(handl_OneDrive("Parks Australia/2019_project/Data/cameras/Gillnet_longline habitat.xlsx"), 
                                    sheet = "longline habitat")
  }
  # 1. Underwater habitat classification
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
  
  
  # 3. Deck camera Habitat interactions (gillnets only)
  Video.habitat.deck=Video.habitat.deck%>%
    data.frame%>%
    mutate(Period=tolower(Period),
           SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DIPRD.code,1,sep = "\\/"),'GN'),
                              Period=='longline'~str_remove(word(DIPRD.code,2,sep = "\\/"),'LL')),
           Percentage.cover=ifelse(Percentage.cover=='<1',1,Percentage.cover),
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
                         Period))   #wrongfully allocated method
  Video.habitat.deck=Video.habitat.deck%>%
    mutate(net_length=ifelse(is.na(net_length) & SHEET_NO %in% unique(DATA_PA%>%filter(skipper%in%c('N.STOKKE'))%>%pull(sheet_no)),
                             1800,net_length))
  

  fn.habitat.damg.deck=function(Gear)  
  {
    d=Video.habitat.deck%>%
      filter(Period==Gear)%>%
      group_by(Frame,SHEET_NO,net_length,Species)%>%
      summarise(Percentage.cover=sum(Percentage.cover,na.rm=T))%>%
      group_by(Frame,SHEET_NO,net_length)%>%
      mutate(Total.damage=sum(Percentage.cover,na.rm=T))%>%
      spread(Species,Percentage.cover,fill = 0)%>%
      mutate(Dist.roller.spreader=distance.roller.spreader)%>%
      data.frame
    
    #add 0 habitat damage frames for reviewed sheet_NO
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
    N.km.analysed=with(DATA_PA%>%filter(sheet_no%in%unique(d$SHEET_NO)),sum(net_length,na.rm=T))
    p2=d%>%
      mutate(Damage=ifelse(Total.damage>0,"Damage","No damage"))%>%
      group_by(Damage)%>%
      tally()%>%
      filter(!is.na(Damage))%>%
      mutate(Percent=100*round(n/sum(n),2),
             ymax=cumsum(Percent),
             ymin = c(0, head(ymax, n=-1)),
             labelPosition =(ymax + ymin) / 2)
    
    p2=p2%>%
      ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Damage)) +
      geom_rect() +
      geom_text(x=3.5,aes(y=labelPosition, label=paste(Percent,"%")),size = 8,check_overlap = TRUE)+
      scale_fill_manual(values=c("brown2","steelblue"))+ 
      coord_polar(theta="y") + xlim(c(2, 4)) + 
      labs(subtitle = 'Percentage of frames with damage',
           caption = paste(paste0('(each frame is ~',distance.roller.spreader,' m of net; '),
                           paste0('',N.km.analysed,' km of net reviewed)'),sep=''))+
      theme_void() +
      theme(legend.position = "right",
            legend.title = element_blank(),
            legend.text = element_text(size = 16),
            plot.caption =  element_text(size = 14,hjust=0.25),
            plot.title = element_text(size = 20, face = "bold",hjust=-920),
            plot.subtitle = element_text(size = 17,face = "italic",hjust=0.3))
    
    
    #Pie chart of different habitat damage categories
    d3=d.no.zeros%>%
      dplyr::select(-Frame,-Total.damage,-SHEET_NO,-net_length,-Dist.roller.spreader)%>%
      gather(Species,Percent)%>%
      group_by(Species)%>%
      summarise(Percent=sum(Percent))%>%
      mutate(label=paste(round(100*Percent/sum(Percent),1),'%',sep=''),
             ymax=cumsum(Percent),
             ymin = c(0, head(ymax, n=-1)),
             labelPosition =(ymax + ymin) / 2)
    p3=d3%>%
      ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Species)) +
      geom_rect(show.legend = FALSE) +
      scale_fill_manual(values=c("deepskyblue2","darkorange1","tomato4","forestgreen",'khaki3'))+ 
      coord_polar(theta="y") + xlim(c(2, 4)) +
      labs(subtitle = 'Damage categories')+
      theme_void() +
      theme(legend.position = "right",
            legend.title = element_blank(),
            legend.text = element_text(size = 16),
            plot.title = element_text(size = 20, face = "bold",hjust=-920),
            plot.subtitle = element_text(size = 17,face = "italic",hjust=0.3))+
      geom_label_repel(x=3.5,aes(y=labelPosition,label=label,angle = 90),color='white',size = 5,show.legend = FALSE)

    #Histogram of proportion of net damage
    Seq.int=10
    p4=d.no.zeros%>%
      dplyr::select(-Frame,-Total.damage,-SHEET_NO,-net_length,-Dist.roller.spreader)%>%
      gather(Species,Percent)%>%
      filter(Percent>0)%>%
      mutate(Percent.bin=Seq.int*round(Percent/Seq.int))%>%
      ggplot( aes(x=Percent.bin)) +
      geom_histogram( aes(fill=Species),col="black", binwidth = Seq.int)+
      ggtitle(label ="",
              subtitle ="  Percentage damage per frame")+
      theme_classic()+
      theme(legend.position = 'top',
            legend.title = element_blank(),
            legend.text=element_text(size=12),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16),
            plot.title = element_text(size = 20, face = "bold"),
            plot.subtitle = element_text(size = 17,face = "italic",hjust=-.15))+
      xlab("Percentage")+
      ylab("Number of frames")+
      scale_fill_manual(values=c("deepskyblue2","darkorange1","tomato4","forestgreen",'khaki3'))+
      scale_x_continuous(breaks = seq(0, 70, Seq.int),labels=seq(5, 75, Seq.int))
     
    
    
    return(list(p2=p2, p3=p3, p4=p4))
  }
  Out.damg_GN=fn.habitat.damg.deck(Gear='gillnet')
  (Out.damg_GN$p2 + Out.damg_GN$p3) / Out.damg_GN$p4 +
    plot_layout(widths = c(4, 4.5))
  ggsave(le.paste("Video/deck.cameras/Habitat.interactions.tiff"),
         width = 10,height = 7,compression = "lzw")
  
}


#---------Dropouts, depredation, Gaffing & Position in water column (catch around weight or float) ---------  

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
         gaffed=capitalize(tolower(gaffed)),
         Code=case_when(is.na(Code) & grepl(paste(c('sealion','Sea Lion'),collapse = '|'),original.dropout) ~41131005,
                        TRUE~Code))

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
         Period=case_when(grepl(paste(c('gillne','gn'),collapse = '|'),Period) ~ 'gillnet',
                          grepl(paste(c('long','ll'),collapse = '|'),Period) ~ 'longline',
                          TRUE~Period),
         Drop.out=ifelse(is.na(Drop.out),'No',Drop.out),    
         Drop.out=tolower(Drop.out),
         Dropout.condition=tolower(Dropout.condition),
         dropout=capitalize(Drop.out),
         SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DPIRD.code,1,sep = "\\/"),'GN'),
                            Period=='longline'~str_remove(word(DPIRD.code,2,sep = "\\/"),'LL')))
Video.subsurface=Video.subsurface%>%filter(!Period=='')

Video.subsurface.comments=Video.subsurface.comments%>%  
        data.frame%>%
        mutate(Period=tolower(Period),
               Period=case_when(grepl('gn',Period) ~ 'gillnet',
                                grepl('ll',Period) ~ 'longline',
                                Period=="gillnert" ~ 'gillnet',
                                TRUE~Period),
               Code=as.numeric(str_trim(Code)),
               Code=ifelse(grepl('shearwater',comment) & Code==40128000,40041050,Code),
               SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DPIRD.code,1,sep = "\\/"),'GN'),
                                  Period=='longline'~str_remove(word(DPIRD.code,2,sep = "\\/"),'LL')))
Video.subsurface.comments=Video.subsurface.comments%>%
                    mutate(Code=ifelse(is.na(Code) & taxa=='Neophoca cinerea', 41131005,Code),
                           Period=ifelse(original.condition=='australian sea lion dead','gillnet',Period),
                           Interaction=interaction,
                           Interaction=case_when(grepl('diving',original.condition)~'diving',
                                                 original.condition%in%c('australian sea lion dead','Seabird dead')~ 'Drop out (dead)',
                                                 TRUE~Interaction))

if(do.dropouts.gaffing.water.column)
{
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
             Period=capitalize(Period))
    d=d%>% 
      left_join(All.species.names%>%
                  filter(Code%in%unique(d$Code))%>%
                  dplyr::select(COMMON_NAME,Code),by="Code")%>%  
      mutate(COMMON_NAME=factor(COMMON_NAME,levels=names(sort(these.drop.out.sp))))
    
    vals=c("#00BFC4","#F8766D","#7CAE00")
    if(var=="dropout") vals=c("#7CAE00","#F8766D","#00BFC4")
    if(var=="Gaffed") vals=c("#F8766D","#00BFC4","#7CAE00")
    
    
    p=d%>%
      filter(!is.na(dummy))%>%
      ggplot(aes(fill=dummy, y=n, x=COMMON_NAME)) + 
      geom_bar(position="stack", stat="identity")+
      coord_flip() +
      facet_wrap(~Period,dir='h')+ 
      theme_PA(Ttl.siz=18,Sbt.siz=16,str.siz=18,strx.siz=18,
               cap.siz=10,lgT.siz=14,leg.siz=16,axs.t.siz=14,axs.T.siz=16)+
      theme(legend.position = LegPos,
            legend.title = element_blank(),
            axis.text.y = element_text(colour = Col.vec),
            plot.title = element_text(hjust = -.4))+
      xlab('')+ylab('Number of events')
    if(var=="dropout")
    {
      vals=c("#7CAE00","#F8766D","#00BFC4")
      p=p+
        scale_fill_manual(values=vals[1:length(unique(d$dummy))])
    }
    if(var=="Dropout.condition")
    {
      vals=c("#7CAE00","#F8766D","#00BFC4")
      p=p+
        scale_fill_manual(values=vals[1:length(unique(d$dummy))])
    }
    if(!is.null(TITLE))p=p+ggtitle(TITLE)
    
    return(p)
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
  write.csv(fun.table.ret.disc(d=Video.camera2.deck%>%filter(!is.na(Code))),
            le.paste("Video/deck.cameras/Drop.out.events_table_camera2.csv"),row.names = F)
  
  write.csv(fun.table.ret.disc(d=Video.subsurface%>%filter(!is.na(Code))),
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
  p1=fn.plt.dropouts(d=Video.subsurface%>%
                       filter(!(Genus=='Heterodontus' & Gaffed=='yes'))%>%
                       filter(!is.na(Gaffed))%>%
                       filter(dropout=='Yes')%>%
                       mutate(Gaffed=capitalize(Gaffed),
                              Gaffed=ifelse(Gaffed=="Yes","Gaffed","Lost"),
                              Gaffed=factor(Gaffed,levels=c("Lost","Gaffed"))),
                     TITLE="Subsurface camera",
                     LegPos="top",
                     var='Gaffed',
                     MinN=1)
  p1=p1+theme(plot.title=element_text(hjust = -.25))
  #ggsave(le.paste("Video/deck.cameras/Drop.out.events_gaffing_subsurface.tiff"),width = 10,height = 8,compression = "lzw")
  
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
    filter(!is.na(water.column))
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
  
  Depredation=c('Caught while predating','Predated on')  
  #Depredation=c(Depredation,'Feeding') # From Sarah "The 'Feeding' behaviour should have only been used in the longlines as it 
  # indicated when a fish fed from the bait on the longline hooks"
  Depredation.species.net=Video.net.interaction%>%
    filter(Interaction%in%Depredation)%>%
    dplyr::select(original.interaction,Interaction,Interaction2,taxa.to.mutate,Comment)%>%
    arrange(Interaction)
  Depredation.species.line=Video.longline.interaction%>%
    filter(Interaction%in%Depredation)%>%
    dplyr::select(original.interaction,Interaction,Interaction2,taxa.to.mutate,Comment)%>%
    arrange(Interaction)
  
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
  
  Observed.underwater.gillnet_m.gn.hour=sum(All.underwater.shts%>%filter(Method=='Gillnet')%>%pull(Effort))
  Observed.underwater.longline_hook.hour=sum(All.underwater.shts%>%filter(Method=='Longline')%>%pull(Effort))
  
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
              soak.time=max(soak.time),
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
    filter(Interaction%in%c(Depredation,"Dropout"))%>%
    mutate(Number=1,
           Method=capitalize(Method),
           Interaction=ifelse(Interaction%in%Depredation,
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
                              filter(Interaction=='Dropout')%>%
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
    mutate(Rate=round(Rate,6),
           Observed.underwater.effort=ifelse(Method=='Gillnet',Observed.underwater.gillnet_m.gn.hour,
                                             ifelse(Method=='Longline',Observed.underwater.longline_hook.hour,
                                                    NA)),
           Observed.underwater.effort.units=ifelse(Method=='Gillnet','m.gn.hour',
                                                   ifelse(Method=='Longline','hook.hour',
                                                          NA)))
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
    filter(!(method=='LL' & n.hooks==0))%>%
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
    dplyr::select(sheet_no,method,Effort)%>%
    filter(Effort>0)
  
  
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
  Observed.deck2.gillnet_km.gn.hour=sum(All.shts%>%
                                          filter(sheet_no%in%unique(Video.camera2.deck$SHEET_NO) & method=='Gillnet')%>%
                                          pull(Effort))/1000
  Observed.deck2.longline_hook.hour=sum(All.shts%>%
                                          filter(sheet_no%in%unique(Video.camera2.deck$SHEET_NO) & method=='Longline')%>%
                                          pull(Effort))
  write.csv(Drop.out.rate_deck2%>%
              mutate(Observed.deck2.effort_Gillnet_km.gn.hour=Observed.deck2.gillnet_km.gn.hour,
                     Observed.deck2.effort_Longline_hook.hour=Observed.deck2.longline_hook.hour),
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
  Observed.subsurface.gillnet_km.gn.hour=sum(All.shts%>%
                                               filter(sheet_no%in%unique(Video.subsurface$SHEET_NO) & method=='Gillnet')%>%
                                               pull(Effort))/1000
  Observed.subsurface.longline_hook.hour=sum(All.shts%>%
                                               filter(sheet_no%in%unique(Video.subsurface$SHEET_NO) & method=='Longline')%>%
                                               pull(Effort))
  
  
  write.csv(Drop.out.rate_subsurface%>%
              mutate(Observed.subsurface.effort_Gillnet_km.gn.hour=Observed.subsurface.gillnet_km.gn.hour,
                     Observed.subsurface.effort_Longline_hook.hour=Observed.subsurface.longline_hook.hour),
            le.paste("Video/deck.cameras/Rate_Drop.out_subsurface.csv"),row.names=F)
  
}



#---------Analysis of Deck 1 camera VS observers --------------------------------------------------------------------
if(do.camera.VS.observer)
{
  # Get video shots with full battery life 
  D1.good.ones <- D1.cam.battery %>%
    filter(str_detect(Full.LL, "(?i)y")|str_detect(Full.GN, "(?i)y")) %>% 
    mutate( LL = ifelse(str_detect(`DIPRD code`, "(?i)ll"), as.character(`DIPRD code`), as.character(NA)),
            GN = ifelse(str_detect(`DIPRD code`, "(?i)gn"), as.character(`DIPRD code`), as.character(NA)),
            sheet_no = ifelse(is.na(LL), as.character(GN), as.character(LL))) %>%
    filter(!sheet_no=="GNPA0103")
  D2.good.ones <- D2.cam.battery %>% filter(str_detect(Full.LL, "(?i)y")|str_detect(Full.GN, "(?i)y")) %>% 
    mutate(LL = ifelse(str_detect(DPIRD.code, "(?i)ll"), as.character(DPIRD.code), as.character(NA)),
           GN = ifelse(str_detect(DPIRD.code, "(?i)gn"), as.character(DPIRD.code), as.character(NA)),
           sheet_no = ifelse(is.na(LL), as.character(GN), as.character(LL)))%>%
    filter(!sheet_no=="GNPA0103")
  
  # Write csv for in meta but not df and vice versa
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
  
  if(approach.camera.observer.comps=='Original')
  {
    Video.camera1.deck=Video.camera1.deck%>%
      rename(DIPRD.code='DIPRD code')%>%
      mutate(Period=ifelse(is.na(Period) & DIPRD.code=='GNPA003/LLPA0004','gillnet',Period),
             DIPRD.code=ifelse(DIPRD.code=='GNPA003/LLPA0004','GNPA0003/LLPA0004',DIPRD.code),
             Period=tolower(Period),
             Period=ifelse(Period%in%c('longlien','ll','longlinr'),'longline',
                           ifelse(Period%in%c('gillnet 2','gn'),'gillnet',
                                  Period)),
             method=ifelse(Period=='gillnet','GN',
                           ifelse(Period=='longline','LL',
                                  NA)),
             meshed=tolower(meshed),
             meshed=ifelse(meshed=='yes','gilled',meshed))          
    
    Video.camera1=rbind(Video.camera1.deck_extra.records%>%
                          rename(DIPRD.code='DIPRD code')%>%
                          dplyr::select(DIPRD.code,Code,Period,number,condition),
                        Video.camera1.deck%>%
                          mutate(Code=ifelse(Genus=="Kyphosus" & Species=="spp",'37361903',Code))%>%
                          dplyr::select(DIPRD.code,Code,Period,number,condition))%>%
      data.frame%>%
      mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                                Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                                Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                                Code >=3.7046e7 & Code<3.900e7 ~"Scalefish",
                                Code ==99999999 ~"Scalefish",
                                Code >=3.900e7 & Code<=4.0e+07 ~"Turtles",
                                Code >=4.1e+07 & Code<=4.115e+07 ~"Marine mammals",
                                Code >=4.0e+07 & Code<4.1e+07 ~"Seabirds",
                                Code >=1.2e7 & Code<3.7e7 ~"Invertebrates",
                                Code >=1.1e7 & Code<1.2e7 ~"Rock/reef structure",
                                Code >=5.4e7 & Code<5.49e7 ~"Macroalgae",
                                Code == 10000910 ~"Sponges"),
             Period=tolower(Period),
             Period=ifelse(Period=='gn','gillnet',
                           ifelse(Period=='ll','longline',Period)),
             sheet_no=case_when(Period=='gillnet'~str_remove(word(DIPRD.code,1,sep = "\\/"),'GN'),
                                Period=='longline'~str_remove(word(DIPRD.code,2,sep = "\\/"),'LL')))%>%
      left_join(DATA_PA,by='sheet_no')%>%
      mutate(Data.set="camera",
             Period=ifelse(Period=='gillnet' & method=='LL','longline',
                           ifelse(Period=='longline' & method=='GN','gillnet',Period)))
    
  }
  if(approach.camera.observer.comps=='Sarah')
  {
    Video.camera1 <-rbind(Video.camera1.deck_extra.records%>%
                            rename(DIPRD.code='DIPRD code')%>%
                            dplyr::select(DIPRD.code,Code,Period,number,condition),Video.camera1.deck %>%
                            dplyr::select(DIPRD.code,Code,Period,number,condition)) %>% 
      data.frame%>%
      separate(DIPRD.code, into = c("GN", "LL"), sep = "/", remove = FALSE) %>% 
      mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                                Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                                Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                                Code >=3.7046e7 & Code<3.900e7 ~"Scalefish",
                                Code ==99999999 ~"Scalefish",
                                Code >=3.900e7 & Code<=4.0e+07 ~"Turtles",
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
  }
  
  #1. Export data for Abbey
  if(do.Abbeys)
  {
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
    Abbey.data.chapter.2_species=Abbey.data.chapter.2%>%
      group_by(sheet_no,Method,Data.set,Code)%>%
      summarise(Number=sum(Number))%>%
      spread(Code,Number,fill = 0)%>%
      data.frame%>%
      left_join(add.effort,by=c('sheet_no','Method'))
    Abbey.data.chapter.2_SP.group=Abbey.data.chapter.2%>%
      group_by(sheet_no,Method,Data.set,SP.group)%>%
      summarise(Number=sum(Number))%>%
      spread(SP.group,Number,fill = 0)%>%
      data.frame%>%
      left_join(add.effort,by=c('sheet_no','Method'))
    Abbey.data.chapter.2_Retain.group=Abbey.data.chapter.2%>%
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
    write.csv(Abbey.data.chapter.2_species,handl_OneDrive('Analyses/Parks Australia/outputs/Data for Abbey/Abbey.data.chapter.2_species.csv'),row.names = F)
    write.csv(Abbey.data.chapter.2_SP.group,handl_OneDrive('Analyses/Parks Australia/outputs/Data for Abbey/Abbey.data.chapter.2_SP.group.csv'),row.names = F)
    write.csv(Abbey.data.chapter.2_Retain.group,handl_OneDrive('Analyses/Parks Australia/outputs/Data for Abbey/Abbey.data.chapter.2_Retain.group.csv'),row.names = F)
    
  }
  
  #2 Barplot of observer VS camera
  EM.vs.OM.DATA=DATA%>%
    mutate(COMMON_NAME=case_when(grepl(paste(c("whaler","dusky shark"),collapse = '|'),tolower(COMMON_NAME))~'Whalers',
                                 grepl(paste(c("blacktip sharks","Common blacktip"),collapse = '|'),tolower(COMMON_NAME))~'Blacktip sharks',
                                 grepl('wobbegong',tolower(COMMON_NAME))~'Wobbegongs',
                                 grepl('hammerhead',tolower(COMMON_NAME))~'Hammerheads',
                                 grepl('stingray',tolower(COMMON_NAME))~'Stingrays',
                                 grepl('angel',tolower(COMMON_NAME))~'Angel sharks',
                                 grepl('catshark',tolower(COMMON_NAME))~'Catsharks',
                                 grepl('boxfish',tolower(COMMON_NAME))~'Boxfishes',    #NEW
                                 grepl('parrotfish',tolower(COMMON_NAME))~'Parrotfishes',   #NEW
                                 grepl('boarfish',tolower(COMMON_NAME))~'Boarfishes',   #NEW
                                 grepl('gurnard',tolower(COMMON_NAME))~'Gurnards',
                                 grepl('jacket',tolower(COMMON_NAME))~'Leatherjackets',
                                 grepl('guitarfish',tolower(COMMON_NAME))~'Guitarfish & shovelnose rays',
                                 grepl(paste(c("gummy shark","whiskery shark"),collapse = '|'),tolower(COMMON_NAME))~"Gummy/Whiskery sharks",
                                 TRUE~COMMON_NAME))%>%
    mutate(Size=case_when(is.na(Size) & taxa=='Elasmobranch'~fl,
                          is.na(Size) & taxa=='Teleost'~tl,
                          is.na(Size) & CAES_Code %in% 35000:39100 ~ dw,
                          TRUE~Size),
           Size=ifelse(is.na(Size) & is.na(fl) & CAES_Code%in%(13000:13016),tl,Size),
           Size=ifelse(is.na(Size) & CAES_Code %in% 35000:39100,dw,Size))
  
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
    mutate(retainedflag=ifelse(COMMON_NAME%in%Reset.disc.ret_RET,'Yes','No'))%>%
    filter(taxa%in%c('Elasmobranch','Teleost'))
  
  EM.vs.OM.DATA=EM.vs.OM.DATA%>%
    mutate(sheet_no=ifelse(sheet_no=="PA1109",'PA0109',sheet_no))  #same shot but data sheet split in 2
  
  EM.vs.OM.Video.camera1=Video.camera1%>%
    filter(SP.group%in%c('Rays','Scalefish','Sharks'))%>%
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
                                 grepl('boxfish',tolower(COMMON_NAME))~'Boxfishes',    #NEW
                                 grepl('parrotfish',tolower(COMMON_NAME))~'Parrotfishes',   #NEW
                                 grepl('boarfish',tolower(COMMON_NAME))~'Boarfishes',   #NEW
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
    N.other=length(This.sp.other)
    d=rbind(Cam,Obs)%>%
      mutate(Method.hour=ifelse(method=='Longline',paste(method,LGN.LL,sep=''),
                                ifelse(method=='Gillnet',paste(method,LGN.GN,sep=''),
                                       NA)))%>%
      filter(!is.na(method))%>%
      mutate(COMMON_NAME=ifelse(!COMMON_NAME%in%This.sp.other,COMMON_NAME,paste0("Other (n=",N.other,' species)')))
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
    
    return(p)
  }
  
  #Commercial species
  This.sp=TAB.comm%>%pull(COMMON_NAME)
  This.sp.other=TAB.comm%>%filter(Percent>=95)%>%pull(COMMON_NAME)
  id.Othersharks=match("Other sharks",This.sp)
  if(length(id.Othersharks)) This.sp.other=c("Other sharks",This.sp.other)  
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
  This.sp.other=TAB.disc%>%filter(Percent>=95)%>%pull(COMMON_NAME)
  id.Othersharks=match("Other sharks",This.sp)
  if(length(id.Othersharks)) This.sp.other=c("Other sharks",This.sp.other)
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
  
  fig=ggarrange(p1+rremove("xlab")+ylim(0,650), p2+rremove("xlab")+ylim(0,650),
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
    
    dd=dd%>%
      group_by(sheet_no)%>%
      mutate(Total.catch=sum(Observer,na.rm=T))
    
    mod <- glm(Camera ~ Observer + method , family="gaussian", data=dd)
    #mod <- glm(Camera ~ Observer + method +Total.catch, family="gaussian", data=dd)
    mod_mean.size <- glm(Camera ~ Observer + method +Mean.size, family="gaussian", data=dd%>%filter(!is.na(Mean.size)))
    mod_species <- glm(Camera ~ Observer + method + COMMON_NAME, family="gaussian", data=dd)
    Over.dispesion=check_overdispersion(mod)
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
      geom_point(size=2.5,alpha = 0.5)+
      facet_wrap(~method,scales='free')+
      geom_ribbon(data=Preds,aes(ymin = low, ymax = high,fill=method),alpha = 0.2)+
      geom_line(data=Preds,aes(Observer,Camera))+
      geom_line(aes(Observer,Observer),linetype=2,color='black')+
      theme_PA(Ttl.siz=18,Sbt.siz=17,str.siz=16,strx.siz=16,
               leg.siz=18,axs.t.siz=16,axs.T.siz=18)+
      theme(legend.position = 'none',
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
    
    return(list(mod=mod,mod_mean.size=mod_mean.size,mod_species=mod_species,p=p,Over.dispesion=Over.dispesion,COEF=COEF))
  }
  lolipop=function(x,disc.sp)
  {
    x=x%>%
      ungroup()%>%
      mutate(COMMON_NAME = fct_reorder(COMMON_NAME, N))
    CLR=ifelse(levels(x$COMMON_NAME)%in%disc.sp,'firebrick4','darkgreen')
    p=x%>%
      ggplot()+
      geom_linerange(aes(x = COMMON_NAME, ymin = 0, ymax =  Mean, colour = method), 
                     position = position_dodge(width = 0),show.legend=F)+
      geom_hline(yintercept = 0,linetype=2)+
      geom_errorbar(aes(ymin = xmin,ymax = xmax,x=COMMON_NAME),width = .125,show.legend=F)+
      geom_point(aes(x = COMMON_NAME, y = Mean, fill = method),pch=21,size=3,
                 position = position_dodge(width = 0))+
      geom_text(aes(x = COMMON_NAME, y = Mean,label = paste("n=",N,sep='')),
                size=5,alpha=.8,hjust=-0.25, vjust=0)+
      coord_flip()+
      facet_wrap(~method)+
      theme_PA(Ttl.siz=18,Sbt.siz=17,str.siz=16,strx.siz=20,
               leg.siz=14,axs.t.siz=14,axs.T.siz=18)+
      theme(legend.position = "none",
            axis.text.y = element_text(hjust = 1, colour = CLR))+
      ylab("Percentage diference")+xlab('')+ylim(-100,100)+
      geom_text(y = 70, x =15 ,label = '% more in EM',
                size=7,angle=90,hjust=-0.1, vjust=0,color=alpha('grey',0.35))+
      geom_segment(x = 15, y = 50, xend = 15, yend = 85, color=alpha('grey',0.35),
                   arrow = arrow(length = unit(0.25, "cm"), type = "closed"))+
      geom_text(y = -70, x =15 ,label = '% more in OM',
                size=7,angle=90,hjust=-0.1, vjust=0,color=alpha('grey',0.35))+
      geom_segment(x = 15, y = -50, xend = 15, yend = -85, color=alpha('grey',0.35),
                   arrow = arrow(length = unit(0.25, "cm"), type = "closed"))
    
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
      filter(!COMMON_NAME=="")%>%
      mutate(method=ifelse(method=="GN","Gillnet",
                    ifelse(method=="LL","Longline",
                    method)),
             n=ifelse(is.na(n),0,n))
    
    #add mean size
    Mean.size.sheet=OBS%>%
      group_by(COMMON_NAME,sheet_no)%>%
      summarise(Mean.size.sheet=mean(Size,na.rm=T))
    Mean.size.sp=OBS%>%
      group_by(COMMON_NAME)%>%
      summarise(Mean.size.sp=mean(Size,na.rm=T))%>%
      ungroup()%>%
      mutate(Mean.size.sp=case_when(is.na(Mean.size.sp) & COMMON_NAME=='Brownspotted wrasse'~40,
                                    is.na(Mean.size.sp) & COMMON_NAME=='Green moray'~50,
                                    is.na(Mean.size.sp) & COMMON_NAME=='Guitarfish & shovelnose rays'~150,
                                    is.na(Mean.size.sp) & COMMON_NAME=='Other sharks'~100,
                                    is.na(Mean.size.sp) & COMMON_NAME=='Spearfish Remora'~40,
                                    is.na(Mean.size.sp) & COMMON_NAME=='Western stingaree'~45,
                                    TRUE~Mean.size.sp))
    d=d%>%
        left_join(Mean.size.sheet,by=c('COMMON_NAME','sheet_no'))%>%
        left_join(Mean.size.sp,by=c('COMMON_NAME'))%>%
        mutate(Mean.size=ifelse(!is.na(Mean.size.sheet),Mean.size.sheet,Mean.size.sp))
    
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
    
    #Fit glm
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
    Per.dif=lolipop(x=Per.diff,disc.sp=Discarded.species)
    Per.dif.com=lolipop(x=Per.diff%>%filter(!COMMON_NAME%in%Discarded.species),
                        disc.sp=Discarded.species)
    Per.dif.dis=lolipop(x=Per.diff%>%filter(COMMON_NAME%in%Discarded.species),
                        disc.sp=Discarded.species)
    
    
    
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
                Per.diff.table=Per.diff,
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
  write.csv(Out$out.for.Jack,le.paste("Camera_v_Observer/out.for.Jack.csv"),row.names=F)
  
  #export Raw data comparison
  Out$p.ind
  ggsave(le.paste("Camera_v_Observer/raw.tiff"),width = 12,height = 10,compression = "lzw")
  
  Out$p.sp
  ggsave(le.paste("Camera_v_Observer/raw_species.tiff"),width = 12,height = 10,compression = "lzw")
  
  
  #export GLMS
  ft <- as_flextable(Out$mod_all.species$mod)
  save_as_docx(ft, path = le.paste("Camera_v_Observer/GLM_mod_all.species.docx"))
  save_as_image(ft, path = le.paste("Camera_v_Observer/GLM_mod_all.species.png"))
  
  ft <- as_flextable(Out$mod_all.species$mod_mean.size)   
  save_as_docx(ft, path = le.paste("Camera_v_Observer/GLM_mod_all.species_mean.size.docx"))
  save_as_image(ft, path = le.paste("Camera_v_Observer/GLM_mod_all.species_mean.size.png"))
  
  ft <- as_flextable(Out$mod_all.species$mod_species)   
  save_as_docx(ft, path = le.paste("Camera_v_Observer/GLM_mod_all.species_species.docx"))
  save_as_image(ft, path = le.paste("Camera_v_Observer/GLM_mod_all.species_species.png"))
  
  
  ft <- as_flextable(Out$mod_commercial.species$mod)
  save_as_docx(ft, path = le.paste("Camera_v_Observer/GLM_mod_commercial.species.docx"))
  save_as_image(ft, path = le.paste("Camera_v_Observer/GLM_mod_commercial.species.png"))
  
  ft <- as_flextable(Out$mod_discarded.species$mod)
  save_as_docx(ft, path = le.paste("Camera_v_Observer/GLM_mod_discarded.species.docx"))
  save_as_image(ft, path = le.paste("Camera_v_Observer/GLM_mod_discarded.species.png"))
  
  fig=ggarrange(Out$mod_all.species$p+rremove("xlab")+rremove("ylab"),
                Out$mod_commercial.species$p+rremove("xlab")+rremove("ylab"),
                Out$mod_discarded.species$p+rremove("xlab")+rremove("ylab"),
                ncol = 1, nrow = 3,
                common.legend=TRUE)
  annotate_figure(fig,
                  bottom = text_grob("Number of individuals reported by observer",size = 18),
                  left = text_grob("Number of individuals recorded by camera",size = 18,rot = 90))
  ggsave(le.paste("Camera_v_Observer/GLM_preds_by.data.set.tiff"),width = 8,height = 12,compression = "lzw")
  
  Out$mod_all.species$p+labs(subtitle='') 
  ggsave(le.paste("Camera_v_Observer/GLM_preds.tiff"),width = 6,height = 6,compression = "lzw")
  
  #export Percentage difference
  write.csv(Out$Per.diff.table%>%arrange(COMMON_NAME),
            le.paste("Camera_v_Observer/Percentage.difference.csv"),row.names = F)
  Out$Per.dif
  ggsave(le.paste("Camera_v_Observer/Percentage.difference.tiff"),width = 10,height = 12,compression = "lzw")
  
  Out$Per.dif.com
  ggsave(le.paste("Camera_v_Observer/Percentage.difference_com.tiff"),width = 10,height = 12,compression = "lzw")
  
  Out$Per.dif.dis
  ggsave(le.paste("Camera_v_Observer/Percentage.difference_disc.tiff"),width = 10,height = 12,compression = "lzw")
  
  
  #export MDS
  fig=ggarrange(Out$p.GN, Out$p.LL,
                ncol = 1, nrow = 2,
                common.legend=TRUE)
  ggsave(le.paste("Camera_v_Observer/MDS.tiff"),width = 8,height = 12,compression = "lzw")
  
  
  #export Permanovas
  write.csv(Out$adon.GN,le.paste("Camera_v_Observer/Permanova_GN.csv"),row.names = T)
  write.csv(Out$adon.LL,le.paste("Camera_v_Observer/Permanova_LL.csv"),row.names = T)
  
}
   

#---------Analysis of Deck 2 camera VS observers --------------------------------------------------------------------
if(do.camera.VS.observer)
{
  # Deck 2  (over roller)                      
  if(approach.camera.observer.comps=='Original')
  {
    Video.camera2=Video.camera2.deck%>%
      mutate(Code=ifelse(Genus=="Kyphosus" & Species=="spp",'37361903',Code))%>%
      dplyr::select(DPIRD.code,Code,Period)%>%
      mutate(number=1)%>%
      data.frame%>%
      mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                                Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                                Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                                Code >=3.7046e7 & Code<3.900e7 ~"Scalefish",
                                Code ==99999999 ~"Scalefish",
                                Code >=3.900e7 & Code<=4.0e+07 ~"Turtles",
                                Code >=4.1e+07 & Code<=4.115e+07 ~"Marine mammals",
                                Code >=4.0e+07 & Code<4.1e+07 ~"Seabirds",
                                Code >=1.2e7 & Code<3.7e7 ~"Invertebrates",
                                Code >=1.1e7 & Code<1.2e7 ~"Rock/reef structure",
                                Code >=5.4e7 & Code<5.49e7 ~"Macroalgae",
                                Code == 10000910 ~"Sponges"),
             Period=tolower(Period),
             Period=ifelse(Period=='gn','gillnet',
                           ifelse(Period=='ll','longline',Period)),
             sheet_no=case_when(Period=='gillnet'~str_remove(word(DPIRD.code,1,sep = "\\/"),'GN'),
                                Period=='longline'~str_remove(word(DPIRD.code,2,sep = "\\/"),'LL')))%>%
      left_join(DATA_PA,by='sheet_no')%>%
      mutate(Data.set="camera",
             Period=ifelse(Period=='gillnet' & method=='LL','longline',
                           ifelse(Period=='longline' & method=='GN','gillnet',Period)))
    
  }
  if(approach.camera.observer.comps=='Sarah')
  {
    Video.camera2=Video.camera2.deck%>%
      mutate(Code=ifelse(Genus=="Kyphosus" & Species=="spp",'37361903',Code))%>%
      dplyr::select(DPIRD.code,Code,Period)%>%
      mutate(number=1)%>%
      data.frame%>%
      separate(DPIRD.code, into = c("GN", "LL"), sep = "/", remove = FALSE) %>% 
      mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                                Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                                Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                                Code >=3.7046e7 & Code<3.900e7 ~"Scalefish",
                                Code ==99999999 ~"Scalefish",
                                Code >=3.900e7 & Code<=4.0e+07 ~"Turtles",
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
      mutate(sheet_no = substr(sheet_no, 3,8),
             method=ifelse(is.na(method),capitalize(Period),method))
  }
  
  EM.vs.OM.Video.camera2=Video.camera2%>%
    filter(SP.group%in%c('Rays','Scalefish','Sharks'))%>%
    mutate(Code=as.numeric(Code))%>%
    left_join(All.species.names%>%
                dplyr::select(COMMON_NAME,Code)%>%
                distinct(Code,.keep_all=T),
              by="Code")%>%
    mutate(method=ifelse(method=="GN","Gillnet",
                         ifelse(method=="LL","Longline",
                                method)))%>%
    mutate(COMMON_NAME=case_when(grepl(paste(c("whaler","dusky shark"),collapse = '|'),tolower(COMMON_NAME))~'Whalers',
                                 grepl(paste(c("blacktip sharks","Common blacktip"),collapse = '|'),tolower(COMMON_NAME))~'Blacktip sharks',
                                 grepl('wobbegong',tolower(COMMON_NAME))~'Wobbegongs',
                                 grepl('hammerhead',tolower(COMMON_NAME))~'Hammerheads',
                                 grepl('stingray',tolower(COMMON_NAME))~'Stingrays',
                                 grepl('angel',tolower(COMMON_NAME))~'Angel sharks',
                                 grepl('catshark',tolower(COMMON_NAME))~'Catsharks',
                                 grepl('boxfish',tolower(COMMON_NAME))~'Boxfishes',    
                                 grepl('parrotfish',tolower(COMMON_NAME))~'Parrotfishes',   
                                 grepl('boarfish',tolower(COMMON_NAME))~'Boarfishes',   
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
  This.sp.other=TAB.comm%>%filter(Percent>=95)%>%pull(COMMON_NAME)
  id.Othersharks=match("Other sharks",This.sp)
  if(length(id.Othersharks)) This.sp.other=c("Other sharks",This.sp.other)
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
  This.sp.other=TAB.disc%>%filter(Percent>=95)%>%pull(COMMON_NAME)
  id.Othersharks=match("Other sharks",This.sp)
  if(length(id.Othersharks)) This.sp.other=c("Other sharks",This.sp.other)
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
  
  fig=ggarrange(p1+rremove("xlab")+ylim(0,650), p2+rremove("xlab")+ylim(0,650),
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
  write.csv(Out$out.for.Jack,le.paste("Camera2_v_Observer/out.for.Jack.csv"),row.names=F)
  
  #export Raw data comparison
  Out$p.ind
  ggsave(le.paste("Camera2_v_Observer/raw.tiff"),width = 12,height = 10,compression = "lzw")
  
  Out$p.sp
  ggsave(le.paste("Camera2_v_Observer/raw_species.tiff"),width = 12,height = 10,compression = "lzw")
  
  
  #export GLMS
  ft <- as_flextable(Out$mod_all.species$mod)
  save_as_docx(ft, path = le.paste("Camera2_v_Observer/GLM_mod_all.species.docx"))
  save_as_image(ft, path = le.paste("Camera2_v_Observer/GLM_mod_all.species.png"))
  
  ft <- as_flextable(Out$mod_all.species$mod_mean.size)   
  save_as_docx(ft, path = le.paste("Camera2_v_Observer/GLM_mod_all.species_mean.size.docx"))
  save_as_image(ft, path = le.paste("Camera2_v_Observer/GLM_mod_all.species_mean.size.png"))
  
  ft <- as_flextable(Out$mod_all.species$mod_species)   
  save_as_docx(ft, path = le.paste("Camera2_v_Observer/GLM_mod_all.species_species.docx"))
  save_as_image(ft, path = le.paste("Camera2_v_Observer/GLM_mod_all.species_species.png"))
  
  
  ft <- as_flextable(Out$mod_commercial.species$mod)
  save_as_docx(ft, path = le.paste("Camera2_v_Observer/GLM_mod_commercial.species.docx"))
  save_as_image(ft, path = le.paste("Camera2_v_Observer/GLM_mod_commercial.species.png"))
  
  ft <- as_flextable(Out$mod_discarded.species$mod)
  save_as_docx(ft, path = le.paste("Camera2_v_Observer/GLM_mod_discarded.species.docx"))
  save_as_image(ft, path = le.paste("Camera2_v_Observer/GLM_mod_discarded.species.png"))
  
  fig=ggarrange(Out$mod_all.species$p+rremove("xlab")+rremove("ylab"),
                Out$mod_commercial.species$p+rremove("xlab")+rremove("ylab"),
                Out$mod_discarded.species$p+rremove("xlab")+rremove("ylab"),
                ncol = 1, nrow = 3,
                common.legend=TRUE)
  annotate_figure(fig,
                  bottom = text_grob("Number of individuals reported by observer",size = 18),
                  left = text_grob("Number of individuals recorded by camera",size = 18,rot = 90))
  ggsave(le.paste("Camera2_v_Observer/GLM_preds_by.data.set.tiff"),width = 8,height = 12,compression = "lzw")
  
  Out$mod_all.species$p+labs(subtitle='')
  ggsave(le.paste("Camera2_v_Observer/GLM_preds.tiff"),width = 6,height = 6,compression = "lzw")
  
  
  #export Percentage difference
  write.csv(Out$Per.diff.table%>%arrange(COMMON_NAME),
            le.paste("Camera2_v_Observer/Percentage.difference.csv"),row.names = F)
  Out$Per.dif
  ggsave(le.paste("Camera2_v_Observer/Percentage.difference.tiff"),width = 10,height = 12,compression = "lzw")
  
  #Out$Per.dif.com
  #ggsave(le.paste("Camera2_v_Observer/Percentage.difference_com.tiff"),width = 10,height = 12,compression = "lzw")
  
  # Out$Per.dif.dis
  # ggsave(le.paste("Camera2_v_Observer/Percentage.difference_disc.tiff"),width = 10,height = 12,compression = "lzw")
  
  
  #export MDS
  fig=ggarrange(Out$p.GN, Out$p.LL,
                ncol = 1, nrow = 2,
                common.legend=TRUE)
  ggsave(le.paste("Camera2_v_Observer/MDS.tiff"),width = 8,height = 12,compression = "lzw")
  
  
  #export Permanovas
  write.csv(Out$adon.GN,le.paste("Camera2_v_Observer/Permanova_GN.csv"),row.names = T)
  write.csv(Out$adon.LL,le.paste("Camera2_v_Observer/Permanova_LL.csv"),row.names = T)
  
  
  #Table of species
  Discarded.species=TAB.disc%>%pull(COMMON_NAME)
  Table.species=rbind(EM.vs.OM.Video.camera1%>%
                        mutate(Platform='Camera1')%>%
                        dplyr::select(method,COMMON_NAME,sheet_no,Platform),
                      EM.vs.OM.Video.camera2%>%
                        mutate(Platform='Camera2')%>%
                        dplyr::select(method,COMMON_NAME,sheet_no,Platform),
                      EM.vs.OM.DATA%>%
                        mutate(Platform='Observer')%>%
                        dplyr::select(method,COMMON_NAME,sheet_no,Platform))%>%
    filter(!COMMON_NAME=='')
  
  Tab.shots=data.frame(Number.of.shots=length(unique(EM.vs.OM.DATA$sheet_no)),
                       Number.of.individuals=nrow(Table.species))
  
  Table.species=Table.species%>%
    distinct(COMMON_NAME)%>%
    left_join(All.species.names%>%
                dplyr::select(COMMON_NAME,SCIENTIFIC_NAME,Taxa)%>%
                mutate(COMMON_NAME=case_when(COMMON_NAME%in%c('Gummy shark','Whiskery shark')~"Gummy/Whiskery sharks",
                                           COMMON_NAME%in%c('Dusky shark','Bronze whaler')~"Whalers",
                                           TRUE~COMMON_NAME),
                     SCIENTIFIC_NAME=case_when(SCIENTIFIC_NAME%in%c('Mustelus antarcticus','Furgaleus macki')~"Mustelus antarcticus/Furgaleus macki",
                                               SCIENTIFIC_NAME%in%c('Carcharhinus obscurus','Carcharhinus brachyurus')~"Carcharhinus obscurus/C. brachyurus",
                                               TRUE~SCIENTIFIC_NAME))%>%
                distinct(COMMON_NAME,.keep_all = T),
              by='COMMON_NAME')%>%
              arrange(Taxa,COMMON_NAME)
  write.csv(Table.species,le.paste("Camera_v_Observer/Table.species.csv"),row.names = F)
  write.csv(Tab.shots,le.paste("Camera_v_Observer/Tab.shots.csv"),row.names = F)
  

}


#---------Gilled or bagged ------------ 
Tab.gilled.bagged=Video.camera1.deck%>%
                    filter(Period=="Gillnet")%>%
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
write.csv(Tab.gilled.bagged,le.paste("Video/deck.cameras/Tab.gilled.bagged.csv"),row.names=F)



#---------Analyses for Abbey ------------
if(do.Abbeys)
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
  pdf(file=handl_OneDrive('Analyses/Parks Australia/outputs/Data for Abbey/Exploraty.pdf'))
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
    mutate(Method=ifelse(Method%in%c("Gn","GN"),"Gillnet",
                  ifelse(Method%in%c("Ll","LL"),"Longline",Method)),
           Method.hour=ifelse(Method=='Longline',paste(Method,LGN.LL,sep=''),
                       ifelse(Method=='Gillnet',paste(Method,LGN.GN,sep=''),
                       NA)))
  TAB=d%>%
    mutate(Name=factor(Name,levels=TEPS.names$Name))
  
  p=TAB%>%
    ggplot(aes(x=Interaction, y=n, fill=Name)) + 
    geom_bar(position="stack", stat="identity")+
    coord_flip() +
    facet_wrap(~Method.hour,dir='h')+ 
    theme_PA(str.siz=16,strx.siz=16,leg.siz=17,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = LGN,
          legend.justification='left',
          legend.direction='horizontal',
          legend.title = element_blank())+
    xlab('')+ylab('Number of events')+
    scale_y_continuous(breaks = integer_breaks())+
    guides(fill = guide_legend(nrow = 3))+
    scale_x_discrete(position = "top")
  
  if(!is.null(TITL)) p=p+ggtitle(TITL)
  if(show.all.levels) p=p+scale_fill_manual(values=TEPS.cols, drop=FALSE)
  if(!show.all.levels)
  {
    THIS=match(unique(d$Name),names(TEPS.cols))
    p=p+scale_fill_manual(values=TEPS.cols[THIS])
  }
  
  return(list(p=p,TAB=TAB%>%ungroup()))
}
#LEGN='none'
LEGN='top'

# 1.1. Underwater 
out=fn.TEPS.barplot(n.shots=rbind(Video.longline.interaction%>%
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
                                                  Interaction=="Caught while predating" ~ "Caught",
                                                  Interaction=="Bounce off" ~ "Bounced off",
                                                  Interaction== "Swim past" ~ "Swam past",
                                                  Interaction== "Swim through" ~ "Swam through",
                                                  grepl("Caught",Interaction) ~ "Caught",
                                                  TRUE~Interaction),
                            Name=factor(Name,levels=TEPS.names$Name)),  
                   hours.LL=hours.underwater.ll,
                   hours.GN=hours.underwater.gn,
                   LGN=LEGN,
                   TITL="Underwater cameras")
write.csv(out$TAB%>%dplyr::select(Method,Interaction,Name,n),le.paste("TEPS/Interactions_number.events_underwater.csv"),row.names = F)
p1=out$p
p1
ggsave(le.paste("TEPS/Interactions_number.events_underwater.tiff"),width = 12,height = 8,compression = "lzw")


# 1.2. subsurface 
out=fn.TEPS.barplot(n.shots=Video.subsurface%>%
                     distinct(SHEET_NO,Period)%>%
                     group_by(Period)%>%
                     tally()%>%
                     spread(Period,n),
                   d=rbind(Video.subsurface.comments%>%
                             filter(Code%in%TEPS.codes)%>%
                             mutate(Method=capitalize(tolower(Period)),
                                    Number=1)%>%
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
                     mutate(Name=factor(Name,levels=TEPS.names$Name),
                            Interaction=capitalize(Interaction)),
                   hours.LL=hours.subsurface.ll,
                   hours.GN=hours.subsurface.gn,
                   LGN=LEGN,
                   TITL="Subsurface camera")
write.csv(out$TAB%>%dplyr::select(Method,Interaction,Name,n),le.paste("TEPS/Interactions_number.events_subsurface.csv"),row.names = F)
p2=out$p
p2
ggsave(le.paste("TEPS/Interactions_number.events_subsurface.tiff"),width = 12,height = 8,compression = "lzw")

# 1.3. Deck 1 
out=fn.TEPS.barplot(n.shots=Video.camera1%>%
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
                     mutate(Name=factor(Name,levels=TEPS.names$Name)),
                   hours.LL=hours.deck1.ll,
                   hours.GN=hours.deck1.gn,
                   LGN=LEGN,
                   TITL="Deck camera 1")
write.csv(out$TAB%>%dplyr::select(Method,Interaction,Name,n),le.paste("TEPS/Interactions_number.events_deck1.csv"),row.names = F)
p3=out$p
p3
ggsave(le.paste("TEPS/Interactions_number.events_deck1.tiff"),width = 12,height = 8,compression = "lzw")

# 1.4. Deck 2  (pointing to roller) 
#note: condition is not recorded by this camera
if(approach.camera.observer.comps=='Original')
{
  out=fn.TEPS.barplot(n.shots=rbind(Video.camera2.deck_observations%>%distinct(Period,SHEET_NO),
                                   Video.camera2.deck%>%distinct(Period,SHEET_NO))%>%
                       distinct(SHEET_NO,Period)%>%
                       group_by(Period)%>%
                       tally()%>%
                       spread(Period,n),
                     d=rbind(Video.camera2.deck_observations%>%    
                               filter(Code%in%TEPS.codes)%>%
                               dplyr::select(Period,Genus,Species,Code,Activity),
                             Video.camera2.deck%>%
                               filter(Code%in%TEPS.codes)%>%
                               mutate(Activity=ifelse(dropout=='Yes' & gaffed=="No","Drop out",
                                                      ifelse(dropout=='Yes' & gaffed=="Yes","Drop out and gaffed",
                                                             NA)))%>%
                               dplyr::select(Period,Genus,Species,Code,Activity))%>%
                       filter(!is.na(Activity))%>%
                       left_join(TEPS.names,by="Code")%>%
                       mutate(Number=1,
                              Method=capitalize(Period),
                              Activity=capitalize(Activity))%>%
                       group_by(Method,Activity,Name,Colr,Code)%>%
                       tally(Number)%>%
                       mutate(Interaction=case_when(Activity%in%c("Feeding from gillnet",
                                                                  "Feeding from longline")~"Feeding",
                                                    TRUE~Activity))%>%
                       mutate(Name=factor(Name,levels=TEPS.names$Name)),
                     hours.LL=hours.deck2.ll,
                     hours.GN=hours.deck2.gn,
                     LGN=LEGN,
                     TITL="Deck camera 2")
  
}
if(approach.camera.observer.comps=='Sarah')
{
  ## NOTE: Activity strings changed because "passing" should be "caught"
  out=fn.TEPS.barplot(n.shots=rbind(Video.camera2.deck_observations%>%distinct(Period,SHEET_NO),
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
                               mutate(Activity = case_when(original.hooklocation == "caught feeding on Sargent Baker" ~ "Feeding",
                                                           original.dropout == "sealion feeding on macki" ~ "Feeding",
                                                           dropout=='Yes' & gaffed=="No" ~ "Drop out",
                                                           dropout=='Yes' & gaffed=="Yes" ~ "Drop out and gaffed",
                                                           Activity == "Passing" ~ "Caught",
                                                           is.na(Activity) ~ "Caught"))%>%
                               dplyr::select(Period,Genus,Species,Code,Activity))%>%
                       left_join(TEPS.names,by="Code")%>%
                       mutate(Number=1,
                              Method=capitalize(Period),
                              Activity=capitalize(Activity))%>%
                       group_by(Method,Activity,Name,Colr,Code)%>%
                       tally(Number)%>%
                       mutate(Interaction= Activity)%>%
                       mutate(Name=factor(Name,levels=TEPS.names$Name),
                              Method=ifelse(Method=='','Longline',Method)),
                     hours.LL=hours.deck2.ll,
                     hours.GN=hours.deck2.gn,
                     LGN=LEGN,
                     TITL="Deck camera 2")
  
}
write.csv(out$TAB%>%dplyr::select(Method,Interaction,Name,n),le.paste("TEPS/Interactions_number.events_deck2.csv"),row.names = F)
p4=out$p
p4
ggsave(le.paste("TEPS/Interactions_number.events_deck2.tiff"),width = 12,height = 8,compression = "lzw")

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

out=fn.TEPS.barplot(n.shots=DATA%>%
                         distinct(sheet_no,method)%>%
                         mutate(method=ifelse(method=="GN","gillnet",
                                              ifelse(method=="LL","longline",
                                                     NA)))%>%
                         group_by(method)%>%
                         tally()%>%
                         spread(method,n),
                   d=rbind(d.teps,d.data)%>%
                     mutate(Name=factor(Name,levels=TEPS.names$Name)),
                   hours.LL=NULL,
                   hours.GN=NULL,
                   LGN=LEGN,
                   TITL="Onboard observers")
write.csv(out$TAB%>%dplyr::select(Method,Interaction,Name,n),le.paste("TEPS/Interactions_number_Observers_by.gear.csv"),row.names = F)
p5=out$p
p5
ggsave(le.paste("TEPS/Interactions_number_Observers_by.gear.tiff"), width = 12,height = 8,compression = "lzw")


#3. Combine all figures
fig=ggarrange(p1+rremove("xlab"),
              p2+rremove("xlab"),
              p4+rremove("xlab"),
              p3+rremove("xlab"), 
              p5+rremove("xlab"),
              ncol = 1, nrow = 5,
              common.legend=TRUE)
annotate_figure(fig,bottom = text_grob("Number of events",size = 18))   
ggsave(le.paste("TEPS/Interactions.tiff"), width = 15,height = 13.5,compression = "lzw")


#4. By shot
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


#5. check ASLs
check.ASL=FALSE
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
  ASL_shape.file=readOGR(handl_OneDrive("Data/Mapping/Closures/ASL_closures/ASL_Closures.shp")) #south coast only
  
  plot(ASL_shape.file,col="chartreuse3")
  
}


#---------Analyse PA socio-economic survey ------------
# Consider for paper:
#          Destination of catch: weight average by volume (or proportion) of catch from TDGDL to
#               give more weight to representative players (e.g. A. Soumelidis VS Alan Miles)
#       Report operating fishers and also number of licenses to show latent effort


#some manipulations
Survey.fishers=Survey.fishers%>%  
                    data.frame%>%
                    filter(if_any(everything(), ~ !is.na(.)))
Survey.processor=Survey.processor%>%    
                    data.frame%>%
                    filter(if_any(everything(), ~ !is.na(.)))

#amend responses not summing to 100%
Survey.fishers[paste("Q.19.",letters[1:11],sep='')]=100*Survey.fishers[paste("Q.19.",letters[1:11],sep='')]/rowSums(Survey.fishers[paste("Q.19.",letters[1:11],sep='')])
Survey.processor[paste("Q.16.",letters[1:8],sep='')]=100*Survey.processor[paste("Q.16.",letters[1:8],sep='')]/rowSums(Survey.processor[paste("Q.16.",letters[1:8],sep='')])


#1. Display distribution of all responses 
fn.plt.hist=function(d,Var)
{
  p=d%>%
    mutate(x=!!sym(Var))%>%
    ggplot(aes(x))+
    geom_histogram(fill="forestgreen")+
    xlab(Var)+ylab("")+
    theme_PA(leg.siz=Cex.res,axs.t.siz=Cex.res,axs.T.siz=Cex.res+4)+
  theme(legend.position = "none")+
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
  print(p)
}
fn.plt.barplt=function(d,Var)
{
  d=d%>%mutate(x=!!sym(Var))%>%group_by(x)%>%tally()%>%filter(!is.na(x))
  p=d%>%
    ggplot(aes(x=x,y=n))+
    geom_bar(stat="identity",aes(fill=x))+
    xlab(Var)+ylab("")+
    theme_PA(leg.siz=Cex.res,axs.t.siz=Cex.res,axs.T.siz=Cex.res+4)+
    theme(legend.position = "none")+
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
  print(p)
}
Cex.res=12

  # 1.1 Fishers questionnaire
Personal.questions.fisher=c("Inteview","Q.1","Q.2","Q.4","Q.5")
Comments.fisher="Q.30"
Fishers.list=list(c("Ownership", "Employment", "Demographics"),
                  "Catch",
                  "Revenue",
                  c("Costs", "Other"),
                  "Bus. Exp. location")
names(Fishers.list)=Fishers.list
for(l in 1:length(Fishers.list)) 
{
  if(length(Fishers.list[[l]])>1) names(Fishers.list)[l]=paste(Fishers.list[[l]],collapse="_")
}
for(l in 1:length(Fishers.list))
{
  Fishers.list[[l]]=Survey.fishers.metadata%>%
                        filter(Section%in%Fishers.list[[l]])%>%
                        pull(Question_no)
  Fishers.list[[l]]=subset(Fishers.list[[l]],!Fishers.list[[l]]%in%Personal.questions.fisher,Comments.fisher)
}
for(l in 1:length(Fishers.list))
{
  a=Fishers.list[[l]]
  D.lis=vector('list',length(a))
  for(s in 1:length(D.lis))
  {
    dummy=Survey.fishers[,a[s]]
    if(is.character(dummy)|is.logical(dummy))D.lis[[s]]=fn.plt.barplt(d=Survey.fishers,Var=a[s])
    if(is.numeric(dummy))D.lis[[s]]=fn.plt.hist(d=Survey.fishers,Var=a[s])
    rm(dummy)
  }
  optimum.disp=n2mfrow(length(D.lis))
  fig=ggarrange(plotlist=compact(D.lis),
            nrow=optimum.disp[1],
            ncol = optimum.disp[2])
  annotate_figure(fig,left = text_grob("Frequency", rot=90,size=Cex.res+4))
  ggsave(le.paste(paste("Socio-economics/raw_responses/Responses_fishers_",names(Fishers.list)[l],".tiff",sep='')),
         width = 10,height = 10,compression = "lzw")
}

  # 1.2 Processor questionnaire
Personal.questions.proc=c("Inteview","Q.1","Q.2.a")
Comments.proc="Q.26"
Proc.list=list(c("Ownership", "Employment", "Demographics"),
                  "Catch",
                  "Revenue",
                  c("Costs", "Other"),
                  "Bus. Exp. location")
names(Proc.list)=Proc.list
for(l in 1:length(Proc.list))
{
  if(length(Proc.list[[l]])>1) names(Proc.list)[l]=paste(Proc.list[[l]],collapse="_")
}
for(l in 1:length(Proc.list))
{
  Proc.list[[l]]=Survey.processor.metadata%>%
    filter(Section%in%Proc.list[[l]])%>%pull(Question_no)
  Proc.list[[l]]=subset(Proc.list[[l]],!Proc.list[[l]]%in%Personal.questions.proc,Comments.proc)
}
for(l in 1:length(Proc.list))
{
  a=Proc.list[[l]]
  D.lis=vector('list',length(a))
  for(s in 1:length(D.lis))
  {
    dummy=Survey.processor[,a[s]]
    if(is.character(dummy)|is.logical(dummy))D.lis[[s]]=fn.plt.barplt(d=Survey.processor,Var=a[s])
    if(is.numeric(dummy))D.lis[[s]]=fn.plt.hist(d=Survey.processor,Var=a[s])
    rm(dummy)
  }
  optimum.disp=n2mfrow(length(D.lis))
  fig=ggarrange(plotlist=compact(D.lis),
                nrow=optimum.disp[1],
                ncol = optimum.disp[2])
  annotate_figure(fig,left = text_grob("Frequency", rot=90,size=Cex.res+4))
  ggsave(le.paste(paste("Socio-economics/raw_responses/Responses_processors_",names(Proc.list)[l],".tiff",sep='')),
         width = 10,height = 10,compression = "lzw")
}


#2. Plot Sankey plot and social network to display flows 

  #2.1. Sankey plot
fn.sankey.plot=function(d1,d2,Other,FROM,CL)
{
  N.int=length(unique(d1$Inteview))
  df=d1%>%
    gather(Question_no,value,-Inteview)
  d=d2%>%
    filter(Question_no%in%df$Question_no)%>%
    dplyr::select(-Section)%>%
    mutate(Question=sub(".*? ", "", sub(".* following buyers", "", Question)))
  Other=Other%>%
    rename(Question=2)%>%
    mutate(Question=tolower(Question),
           Question1=case_when(grepl('processor',Question)~"Other fish processor",
                               grepl('coopt',Question)~"Fish coopt",
                               TRUE~Question),
           Question="Other")
  
  dat=left_join(df,d,by='Question_no')%>%
    left_join(Other,by=c('Question','Inteview'))%>%
    mutate(Question=ifelse(!is.na(Question1),Question1,Question))%>%
    mutate(Question=ifelse(Question=='Self-consumed',FROM,
                           ifelse(Question=='Direct to local restaurants or other retailers','Local retailer',
                                  Question)),
           From=FROM,
           To=Question)%>%
    dplyr::select(From,To,value)
  
  keep.level=dat%>%filter(value>0)%>%pull(To)
  
  dat=dat%>%
    filter(To%in%keep.level)
    
  
  p=dat%>%
    gather_set_data(1:2)%>%
    ggplot(aes(x, id = id, split = y, value = value)) +
    geom_parallel_sets(fill = CL,alpha = 0.2, axis.width = 0.1,show.legend = FALSE) +
    geom_parallel_sets_axes(axis.width = 0.6,alpha = 0.85,fill=CL,colour='transparent') +
    geom_parallel_sets_labels(colour = 'black',size=5,angle = 0)+
    theme_void()+
    theme(plot.margin=unit(c(0,-1,.1,-1), "cm"))
  
  Tab=dat%>%
    group_by(To)%>%
    summarise(Percent=sum(value))%>%
    mutate(Percent=Percent/N.int)
    
    
  return(list(p=p,Tab=Tab))
  
}

      #Fisher - catch
p1=fn.sankey.plot(d1=Survey.fishers[c('Inteview','Q.9.a','Q.9.b','Q.9.c','Q.9.d','Q.9.e','Q.9.f','Q.9.g')],
                  d2=Survey.fishers.metadata,
                  Other=Survey.fishers[c('Inteview','Q.9.g.i')],
                  FROM='Fisher',
                  CL='#F8766d')

      #Processor - catch
p2=fn.sankey.plot(d1=Survey.processor[c('Inteview','Q.6.a','Q.6.b','Q.6.c','Q.6.d','Q.6.e','Q.6.f','Q.6.g')],
                  d2=Survey.processor.metadata,
                  Other=Survey.processor[c('Inteview','Q.6.g.i')],
                  FROM='Local fish processor',
                  CL='#00BFC4')

ggarrange(p1$p,p2$p,nrow=1)
ggsave(le.paste(paste("Socio-economics","sankey.plot_catch.tiff",sep='/')),width = 12,height = 8,compression = "lzw")
write.csv(p1$Tab,le.paste(paste("Socio-economics","sankey.plot_catch_fisher.csv",sep='/')),row.names = F)
write.csv(p2$Tab,le.paste(paste("Socio-economics","sankey.plot_catch_processor.csv",sep='/')),row.names = F)


  #2.2. Social network
fn.social.net.all=function(d1,d2,Other,dummy,sklr,Percent=100,show.lgn=FALSE,
                           LGN.title=FALSE,LGN.loc,XLIM,YLIM)
{
  df=d1%>%
    gather(Question_no,value,-Inteview)
  d=d2%>%
    filter(Question_no%in%df$Question_no)%>%
    dplyr::select(-Section)%>%
    mutate(Question=sub(".*? ", "", sub(".* following buyers", "", Question)))
  dat=left_join(df,d,by='Question_no')
  if(!is.null(Other))
  {
    Other=Other%>%
      rename(Question=2)%>%
      mutate(Question=tolower(Question),
             Question1=case_when(grepl('processor',Question)~"Other fish processor",
                                 TRUE~Question),
             Question="Other")
    dat=dat%>%
      left_join(Other,by=c('Question','Inteview'))%>%
      mutate(Question=ifelse(!is.na(Question1),Question1,Question))
  }
  dat=dat%>%
    mutate(Question=case_when(Question=='Self-consumed'~dummy,
                              Question=='Direct to public'~'Public',
                              Question=='Local fish processor'~'Local processor',
                              Question=='Other fish processor'~'Other processor',
                              Question=='geraldton coopt'~'Fish coopt',
                              Question=='Direct to local restaurants or other retailers'~'Local retailer',
                              grepl("FTE",Question)~'FTEs',
                              grepl("a whole year",Question)~'Employees per year',
                              TRUE~Question),
           from = paste(dummy,Inteview),
           to=Question,
           to=ifelse(to=='Fisher',from,to),
           value=value/Percent,
           col=Inteview)%>%
    dplyr::select(from,to,value,col)%>%
    filter(value>0)%>%
    mutate(to=ifelse(grepl('Fish processor',from) & to=='Fish processor',from,to),
           to=capitalize(to))
  
  
  net <- graph_from_data_frame(d=dat, directed=T) 
  E(net)$width <- E(net)$value*sklr  #change edge width to proportional to weight
  COLS=colorRampPalette(c("cadetblue","chartreuse4","darkgreen"))
  COLS=COLS(length(unique(dat$col)))
  E(net)$color <- as.character(COLS[dat$col])
  #V(net)$color <- as.character(COLS[dat$col])
  #X=layout_with_gem(net)
  #X=layout_with_mds(net)
  X=layout_with_kk(net)
  plot(net,
       vertex.label.font=1,
       vertex.label=V(net)$label,
       vertex.color=adjustcolor("tomato", alpha.f = .99),
       vertex.label.color='black',
       vertex.label.dist = -2,
       vertex.label.cex=1.6,
       edge.arrow.size=1.15,
       edge.curved=0.25,
       edge.color = E(net)$color,
       layout=X,
       rescale = FALSE,
       ylim=YLIM,
       xlim=XLIM,
       asp = 0)
  if(show.lgn)
  {
    LGN=range(dat$value)
    legend(LGN.loc,paste(LGN*Percent),lty=1,lwd=LGN*sklr,bty='n',cex=1.25,title=LGN.title)
  }
}
    #2.2. Catch
      #2.2.1 Fisher
fn.fig(le.paste(paste("Socio-economics","social.network_catch_all_fisher",sep='/')),2400,2400)
par(mfrow=c(1,1),par(mar=c(1,0,2,0)))
fn.social.net.all(d1=Survey.fishers[c('Inteview','Q.9.a','Q.9.b','Q.9.c','Q.9.d','Q.9.e','Q.9.f','Q.9.g')],
                  d2=Survey.fishers.metadata,
                  Other=Survey.fishers[c('Inteview','Q.9.g.i')],
                  dummy='Fisher',
                  sklr=7,
                  show.lgn=TRUE,
                  LGN.title="Flow %",
                  LGN.loc='bottomright',
                  XLIM=c(-1.5,2.25),
                  YLIM=c(-1.5,1.65))
dev.off()

      #2.2.2 Processor
fn.fig(le.paste(paste("Socio-economics","social.network_catch_all_processor",sep='/')),2400,2400)
par(mfrow=c(1,1),par(mar=c(1,0,2,0)))
fn.social.net.all(d1=Survey.processor[c('Inteview','Q.6.a','Q.6.b','Q.6.c','Q.6.d','Q.6.e','Q.6.f','Q.6.g')],
                  d2=Survey.processor.metadata,
                  Other=Survey.processor[c('Inteview','Q.6.g.i')],
                  dummy='Fish processor',
                  sklr=10,
                  show.lgn=TRUE,
                  LGN.title="Flow %",
                  LGN.loc='bottomright',
                  XLIM=c(-1.5,2),
                  YLIM=c(-1.2,1.3))
dev.off()



do.network.summary=FALSE   #not used
if(do.network.summary)
{
  setwd(le.paste(paste("Socio-economics","network",sep='/')))
  fn.social.net=function(d1,d2,Other,out,dummy)
  {
    df=d1%>%
      gather(Question_no,value,-Inteview)
    d=d2%>%
      filter(Question_no%in%df$Question_no)%>%
      dplyr::select(-Section)%>%
      mutate(Question=sub(".*? ", "", sub(".* following buyers", "", Question)))
    Other=Other%>%
      rename(Question=2)%>%
      mutate(Question=tolower(Question),
             Question1=case_when(grepl('processor',Question)~"Other fish processor",
                                 TRUE~Question),
             Question="Other")
    
    dat=left_join(df,d,by='Question_no')%>%
      left_join(Other,by=c('Question','Inteview'))%>%
      dplyr::select(-Question_no,-Inteview)%>%
      mutate(Question=ifelse(!is.na(Question1),Question1,Question))%>%
      mutate(value=value/sum(value))%>%
      group_by(Question)%>%
      summarise(value=100*sum(value))%>%
      mutate(Question=ifelse(Question=='Self-consumed',dummy,
                      ifelse(Question=='Direct to local restaurants or other retailers','Local retailer',
                      Question)),
             value=ifelse(value==0 & Question==dummy,1e-4,value))%>%
      filter(value>0)%>%
      ungroup()
    
    nodes=data.frame(id = dat$Question,
                     label = dat$Question,
                     value=dat$value/100)
    
    edges <- data.frame(from = dummy, to = dat$Question,
                        width = 15*dat$value/100)%>%
      filter(width>1e-03)
    nodes$value=1
    nodes$font.size=20
    
    Net=visNetwork(nodes, edges, height = "600px", width = "100%")%>% 
      visEdges(arrows = "to")%>%
      visLayout(randomSeed=666)%>%
      visHierarchicalLayout(direction='LR',nodeSpacing = 150)
    
    html_name <- tempfile(tmpdir=le.paste("Socio-economics"),fileext = "net.html")
    visSave(Net, html_name)
    webshot(html_name, zoom = 4, 
            file = le.paste(paste("Socio-economics","network",paste(out,".jpeg",sep=''),sep='/')))
    
    
  }
  #2.1. Catch
    #2.1.1 Fisher
  fn.social.net(d1=Survey.fishers[c('Inteview','Q.9.a','Q.9.b','Q.9.c','Q.9.d','Q.9.e','Q.9.f','Q.9.g')],
                d2=Survey.fishers.metadata,
                Other=Survey.fishers[c('Inteview','Q.9.g.i')],
                out='Fisher_catch',
                dummy='Fisher')
  
    #2.1.2. Processor
  fn.social.net(d1=Survey.processor[c('Inteview','Q.6.a','Q.6.b','Q.6.c','Q.6.d','Q.6.e','Q.6.f','Q.6.g')],
                d2=Survey.processor.metadata,
                Other=Survey.processor[c('Inteview','Q.6.g.i')],
                out='Processor_catch',
                dummy='Local fish processor')
}


#3. Shark body parts
Body.parts=rbind(Survey.fishers[,c('Q.10','Q.14')]%>%
                   dplyr::rename_at(vars(c('Q.10','Q.14')), ~ c("Q1","Q2"))%>%
                    mutate(Dat="Fisher")%>%
                   filter(!is.na(Q1)),
                Survey.processor[,c('Q.7.a','Q.11')]%>%
                  dplyr::rename_at(vars(c('Q.7.a','Q.11')), ~ c("Q1","Q2"))%>%
                        mutate(Dat="Processor")%>%
                  filter(!is.na(Q1)))
fn.body.part=function(Q,Var)
{
  p=Body.parts%>%
    ggplot(aes(!!sym(Q)))+
    geom_bar(aes(fill=!!sym(Q)))+
    facet_wrap(~Dat)+
    xlab(Var)+ylab("Frequency")+
    theme_PA(strx.siz=Cex.res+2,leg.siz=Cex.res+1,axs.t.siz=Cex.res+1,axs.T.siz=Cex.res+4)+
    theme(legend.position = "none")+
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) #integer axis
  print(p)
}
p1=fn.body.part(Q="Q1",
             Var=Survey.fishers.metadata%>%filter(Question_no=='Q.10')%>%pull(Question))
p2=fn.body.part(Q="Q2",
                #Var=Survey.fishers.metadata%>%filter(Question_no=='Q.14')%>%pull(Question)
                Var="Do you sell other shark products in addition to flesh and fins?")
ggarrange(p1, p2, ncol = 1, nrow = 2)
ggsave(le.paste("Socio-economics/Question_body.parts.tiff"),width = 10,height = 10,compression = "lzw")

#4. Fisher costs and revenue
colfunc <- colorRampPalette(c("gold", "firebrick"))

Fill.cols.cost.rev=colfunc(11)
names(Fill.cols.cost.rev)=c('Less than 50000','50000 to 99999',
                            '100000 to 149999','150000 to 199999',
                            '200000 to 249999','250000 to 299999',
                            '300000 to 349999','350000 to 399999',
                            '400000 to 449999','450000 to 499999',
                            '500000 or more')
Fishr.cost.rev=Survey.fishers[,c('Q.11','Q.16')]%>%
  dplyr::rename_at(vars(c('Q.11','Q.16')), ~ c("Q1","Q2"))%>%
  mutate(Dat="Fisher",
         Q1=factor(Q1,levels=names(Fill.cols.cost.rev)),
         Q2=factor(Q2,levels=names(Fill.cols.cost.rev)))%>%
  filter(!is.na(Q2))
fn.cost.rev=function(Q,Var)
{
  p=Fishr.cost.rev%>%
    ggplot(aes(!!sym(Q)))+
    geom_bar(aes(fill=!!sym(Q)))+
    xlab(Var)+ylab("Frequency")+
    theme_PA(strx.siz=Cex.res+2,leg.siz=Cex.res+1,axs.t.siz=Cex.res+1,axs.T.siz=Cex.res+4)+
    theme(legend.position = "none")
  print(p)
}
p1=fn.cost.rev(Q="Q1",
               Var=Survey.fishers.metadata%>%filter(Question_no=='Q.11')%>%pull(Question))+
  scale_fill_manual(values =Fill.cols.cost.rev)
p2=fn.cost.rev(Q="Q2",
               Var=Survey.fishers.metadata%>%filter(Question_no=='Q.16')%>%pull(Question))+
  scale_fill_manual(values =Fill.cols.cost.rev)
ggarrange(p1, p2, ncol = 1, nrow = 2)
ggsave(le.paste("Socio-economics/Question_revenue_costs.tiff"),width = 14,height = 10,compression = "lzw")


#5. Average price per species
fn.average.barplt=function(d.f,d.f_m,Dat.f,d.p,d.p_m,Dat.p,REMUV,REMUV.p,XLAB,YLAB,Rotate=0,Cex=12)
{
  #fisher
  Mean=d.f%>%summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  SD=d.f%>%summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))
  d.f=data.frame(Q=names(Mean),Mean=unlist(Mean),SD=unlist(SD))
  d.f_m=d.f_m%>%
    mutate(Question=str_remove(Question, REMUV))
  if(any(grepl('\\?',d.f_m$Question))) d.f_m=d.f_m%>%mutate(Question=sub(".*? ", "",Question))
  d.f=left_join(d.f,d.f_m,by=c('Q'="Question_no"))%>%
    mutate(Dat=Dat.f)
  
  #processor
  Mean=d.p%>%summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  SD=d.p%>%summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))
  d.p=data.frame(Q=names(Mean),Mean=unlist(Mean),SD=unlist(SD))
  d.p_m=d.p_m%>%
    mutate(Question=str_remove(Question, REMUV.p))
  if(any(grepl('\\?',d.p_m$Question))) d.p_m=d.p_m%>%mutate(Question=sub(".*? ", "",Question))
  d.p=left_join(d.p,d.p_m,by=c('Q'="Question_no"))%>%
          mutate(Dat=Dat.p)
  if(any(grepl('shark',d.p$Question)))d.p=d.p%>%mutate(Dat=paste(Dat.p,' (',sub(".*_", "", Q),')',sep=''))
  
  
  d=rbind(d.f,d.p)%>%
    mutate(Question=case_when(Question=='Sandbar (thick skin) shark'~'Sandbar shark',
                              Question=='Vessel repairs and maintenance'~'Vess. repairs',
                              Question=='Vessel lease/mortgage'~'Vess. lease/mortgage',
                              Question=='Fuel and lubricants'~'Fuel/lubricants',
                              Question=='Processing plant lease/mortgage'~'Proc. plant lease/mortgage',
                              Question=='Gear repairs, maintenance and replacement'~'Equip. repairs',
                              Question=='Crew (including skipper; salaries, superannuation, etc.)'~'Crew',
                              grepl('Persons working in processing plant',Question)~'Personnel',
                              grepl('Administration',Question)~'Admin.',
                              Question=='Sourcing fish (purchasing from fisher or other processor)'~'Sourcing fish',
                              Question=='Equipment repairs, maintenance and replacement'~'Equip. repairs',
                              TRUE~Question),
           Question=factor(Question,
                           levels=unique(Question)))
  if(any(grepl('shark',d$Question))) d=d%>%mutate(Col=ifelse(grepl(paste(c('shark','Shark'),collapse="|"),Question),"steelblue","firebrick"))
  if(!any(grepl('shark',d$Question))) d=d%>%mutate(Col=Question)
  
  p=d%>%
    ggplot(aes(x=Question,y=Mean,fill=Col))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                  position=position_dodge(0.05))+
    facet_wrap(~Dat,ncol=1,scales='free')+
    xlab(XLAB)+ylab(YLAB)+
    theme_PA(strx.siz=Cex+3,axs.t.siz=Cex+2,axs.T.siz=Cex+6)+
    theme(legend.position = "none")+
    scale_x_discrete(labels = label_wrap(10))
  if(any(grepl('shark',d$Question)))p=p+scale_fill_manual(values=c("firebrick","steelblue"))
  if(Rotate>0) p=p+theme(axis.text.x = element_text(angle = Rotate, vjust = 1, hjust=1))
  
  print(p)
  write.csv(d%>%
              dplyr::select(Question,Mean,SD,Dat)%>%
              rename(Species=Question,
                     Price_mean=Mean,
                     Price.sd=SD,
                     Source=Dat)%>%
              mutate(Comments=ifelse(grepl('Fisher',Source),'AUD per kg of trunks (sharks) or whole fish (scalefish)',
                              ifelse(grepl('Processor',Source),'AUD per kg of fillets',
                                     NA))),
            le.paste("Socio-economics/Price.per.kg.csv"),row.names = F) 
}
Chunk=c(paste(paste('Q.10',letters[1:11],sep='.'),'retail',sep='_'),paste(paste('Q.10',letters[1:11],sep='.'),'wholesale',sep='_'))
fn.average.barplt(d.f=Survey.fishers[,paste('Q.13',letters[1:11],sep='.')],
                  d.f_m=Survey.fishers.metadata%>%filter(Question_no%in%paste('Q.13',letters[1:11],sep='.')),
                  Dat.f="Fisher",
                  d.p=Survey.processor[,Chunk],
                  d.p_m=Survey.processor.metadata%>%filter(Question_no%in%Chunk),
                  Dat.p="Processor",
                  REMUV="What is the average price you receive per kg of ",
                  REMUV.p="What is the average price you receive per kg of ",
                  XLAB='Price per kg',
                  YLAB="AUD",
                  Rotate=0,
                  Cex=18)
ggsave(le.paste("Socio-economics/Question_price.per.kg.tiff"),width = 16,height = 16,compression = "lzw")


#5. Costs 
fn.average.barplt(d.f=Survey.fishers[paste("Q.19.",letters[1:11],sep='')],
                  d.f_m=Survey.fishers.metadata%>%filter(Question_no%in%paste("Q.19.",letters[1:11],sep='')),
                  Dat.f="Fisher",
                  d.p=Survey.processor[paste("Q.16.",letters[1:8],sep='')],
                  d.p_m=Survey.processor.metadata%>%filter(Question_no%in%paste("Q.16.",letters[1:8],sep='')),
                  Dat.p="Processor",
                  REMUV="What percentage of your annual fishing costs were due to?",
                  REMUV.p="What percentage of your annual processing costs were due to?",
                  XLAB='',
                  YLAB="Percentage of total annual costs",
                  Rotate=0,
                  Cex=12)
ggsave(le.paste("Socio-economics/Question_Costs.tiff"),width = 13,height = 10,compression = "lzw")


#6. Business expenditure location
fn.average.barplt(d.f=Survey.fishers[paste("Q.20.",letters[1:11],sep='')],
                  d.f_m=Survey.fishers.metadata%>%filter(Question_no%in%paste("Q.20.",letters[1:11],sep='')),
                  Dat.f="Fisher",
                  d.p=Survey.processor[paste("Q.17.",letters[1:8],sep='')],
                  d.p_m=Survey.processor.metadata%>%filter(Question_no%in%paste("Q.17.",letters[1:8],sep='')),
                  Dat.p="Processor",
                  REMUV='What percentage of your annual costs is spent within your local government area?',
                  REMUV.p="What percentage of your annual processing costs is spent within your local government area?",
                  XLAB='',
                  YLAB="Percentage of costs spent within local government area",
                  Rotate=0)
ggsave(le.paste("Socio-economics/Question_Expenditure_location.tiff"),width = 12,height = 10,compression = "lzw")


#7. Tabulate responses for input into Report's body text
N_contacted=28  #number of fishers that are participation in TDGDLF and processors contacted by phone
write.csv(N_contacted,le.paste(paste('Socio-economics/N_contacted.csv',sep='')),row.names = F)
N_answered=length(unique(Survey.processor$Inteview))+length(unique(Survey.fishers$Inteview))
write.csv(N_answered,le.paste(paste('Socio-economics/N_answered.csv',sep='')),row.names = F)

fn.tbl.location.zone=function(d)
{
  return(d%>%
           mutate(Zone=case_when(Loc%in%c('Lancelin')~"West Coast",
                                 Loc%in%c('Augusta','Bunbury','Busselton','Hamelin Bay','Dunsborough')~"Zone 1",
                                 Loc%in%c('Albany','Esperance','Esoerance')~"Zone 2"))%>%
           group_by(Zone)%>%tally())
  
}
Resp_zone_fisher=fn.tbl.location.zone(d=Survey.fishers%>%dplyr::select(Q.20.l)%>%rename(Loc=Q.20.l))
write.csv(Resp_zone_fisher,le.paste(paste('Socio-economics/Resp_zone_fisher.csv',sep='')),row.names = F)
Resp_zone_proc=fn.tbl.location.zone(d=Survey.processor%>%dplyr::select(Q.17.i)%>%rename(Loc=Q.17.i))
write.csv(Resp_zone_proc,le.paste(paste('Socio-economics/Resp_zone_proc.csv',sep='')),row.names = F)


fun.tabl.out=function(d1,d2,outfile)
{
  d=d1%>%map(tabyl,show_na=F)
  for(n in 1:length(d))
  {
    x=d2%>%filter(Question_no==names(d)[n])%>%pull(Question)
    y=names(d)[n]
    d[[n]]=d[[n]]%>%
      mutate(Question_no=y,
             Question=x)%>%
      rename(Response='.x[[i]]')%>%
      dplyr::select(Question_no,Question,Response,n,percent)
  }
  write.csv(do.call(rbind,d),le.paste(paste('Socio-economics/Tabulate_',outfile,'.csv',sep='')),row.names = F)
  
}
fun.tabl.out(d1=Survey.fishers[c('Q.6','Q.6.a','Q.7','Q.8',
                                   'Q.15',paste('Q',21:24,sep='.'))],
             d2=Survey.fishers.metadata,
             outfile='fisher')

fun.tabl.out(d1=Survey.processor[c('Q.3','Q.4','Q.12',
                                 paste('Q',18:21,sep='.'))],
             d2=Survey.processor.metadata,
             outfile='processor')


#8. Histograms of income percent, ownership and demographics
nwords <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}
fn.plt.this=function(d,Var,d2,nline=32)
{
  Xtitl=d2$Question
  if(nchar(Xtitl)>nline)
  {
    Xtitl=paste(c(word(Xtitl,1,5),word(Xtitl,6,nwords(Xtitl))),collapse="\n ")
    #Xtitl=paste(substring(Xtitl, c(1,nline+2), c(nline,nchar(Xtitl))),collapse="\n ")
  }
  expression()
  dummy=d[,Var]
  if(is.character(dummy)|is.logical(dummy))
  {
    p=d%>%
      mutate(x=!!sym(Var))%>%
      group_by(x)%>%
      tally()%>%
      filter(!is.na(x))%>%
      ggplot(aes(x=x,y=n))+
      geom_bar(stat="identity",aes(fill=x))+
      ylab("")+
      xlab(Xtitl)+
      theme_PA(leg.siz=Cex.res,axs.t.siz=Cex.res+1,axs.T.siz=Cex.res+4)
  } 
  if(is.numeric(dummy))
  {
    p=d%>%
      mutate(x=!!sym(Var))%>%
      ggplot(aes(x))+
      geom_histogram(fill="forestgreen")+
      ylab("")+
      xlab(Xtitl)+
      theme_PA(leg.siz=Cex.res,axs.t.siz=Cex.res+1,axs.T.siz=Cex.res+4)+
      scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
  }
  print(p+theme(legend.position = "none",plot.title.position = 'plot')+
          scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))))
}
  #Fishers
ld=c('Q.25','Q.26','Q.29')
ll.fis=vector('list',length(ld))
for(l in 1:length(ld))
{
  
  ll.fis[[l]]=fn.plt.this(d=Survey.fishers,
                          Var=ld[l],
                          d2=Survey.fishers.metadata%>%filter(Question_no==ld[l]))
}

  #Processors
ld=c('Q.22','Q.23','Q.25')
ll.proc=vector('list',length(ld))
for(l in 1:length(ld))
{
  
  ll.proc[[l]]=fn.plt.this(d=Survey.processor,
                           Var=ld[l],
                           d2=Survey.processor.metadata%>%filter(Question_no==ld[l]))
}
ll=do.call(c, list(ll.fis, ll.proc))
fig=ggarrange(plotlist=compact(ll),nrow=2,ncol = 3)
annotate_figure(fig,left = text_grob("Frequency", rot=90,size=Cex.res+6))
ggsave(le.paste("Socio-economics/Question_Income.percent_owner_family.tiff"),width = 14,height = 10,compression = "lzw")



#9. Fishery valuation - overall market value through the supply chain 
#note: use Rogers 2017 multipliers to calculate overall value of fishery
Avrg.ktch=read.csv(le.paste("Socio-economics/Avrg.ktch.csv"))
Total.ktch.per.boat=read.csv(le.paste("Socio-economics/Total.ktch.per.boat.csv"))

  
    #Add species names
Avrg.ktch=Avrg.ktch%>%
  left_join(All.species.names%>%
              distinct(CAES_Code,.keep_all=T)%>%
              dplyr::select(COMMON_NAME,Taxa,CAES_Code),
            by=c('SPECIES'='CAES_Code'))%>%
  mutate(COMMON_NAME=case_when(SPECIES==22998~'Shark fin',
                               SPECIES==441025~'Shark Mackerel',
                               TRUE~COMMON_NAME),
         Taxa=case_when(SPECIES==22998~'Elasmobranch',
                        SPECIES==441025~'Teleost',
                        TRUE~Taxa))%>%
  filter(Taxa%in%c('Elasmobranch','Teleost'))%>%
  rename(Avg.annual.landed_kg=Tot)


  #9.1 Get average prices by species and sector
fn.get.sp.price=function(d,d_m,Chunk,REMUV,do.what)
{
  if(do.what=='min')d=d[,Chunk]%>%summarise(across(where(is.numeric), ~ min(.x, na.rm = TRUE)))
  if(do.what=='max')d=d[,Chunk]%>%summarise(across(where(is.numeric), ~ max(.x, na.rm = TRUE)))
  d=data.frame(Q=names(d),Mean=unlist(d))
  
  d_m=d_m%>%
    filter(Question_no%in%Chunk)%>%
    mutate(Question=str_remove(Question, REMUV))
  if(any(grepl('\\?',d_m$Question))) d_m=d_m%>%mutate(Question=sub(".*? ", "",Question))
  d_m=d_m%>%
    rename(Species=Question)%>%
    mutate(Species=ifelse(Species=='Sandbar (thick skin) shark','Sandbar shark',Species))%>%
    dplyr::select(-Section)
  
  return(d%>%left_join(d_m,by=c('Q'='Question_no')))
}
  #Fishers
#note: Mean (min,max) is the mean $ per kg of trunks (sharks) or whole fish (scalefish)
Fisher.price.min=fn.get.sp.price(d=Survey.fishers,
                             d_m=Survey.fishers.metadata,
                             Chunk=paste('Q.13',letters[1:11],sep='.'),
                             REMUV='What is the average price you receive per kg of ',
                             do.what='min')%>%  
  dplyr::select(-Q) 
Fisher.price.max=fn.get.sp.price(d=Survey.fishers,
                                 d_m=Survey.fishers.metadata,
                                 Chunk=paste('Q.13',letters[1:11],sep='.'),
                                 REMUV='What is the average price you receive per kg of ',
                                 do.what='max')%>%  
  dplyr::select(-Q) 

Fisher.price=full_join(Fisher.price.min%>%rename(Min=Mean),
                       Fisher.price.max%>%rename(Max=Mean),
                       by='Species')
  #Processors
#note: Mean (min,max) is the mean $ per kg of fillets
Processor.price.min=fn.get.sp.price(d=Survey.processor,
                                d_m=Survey.processor.metadata,
                                Chunk=c(paste(paste('Q.10',letters[1:11],sep='.'),'retail',sep='_'),
                                        paste(paste('Q.10',letters[1:11],sep='.'),'wholesale',sep='_')),
                                REMUV='What is the average price you receive per kg of ',
                                do.what='min')%>%
  mutate(Sector=ifelse(grepl('retail',Q),'retail','wholesale'))%>%
  dplyr::select(-Q)
Processor.price.max=fn.get.sp.price(d=Survey.processor,
                                    d_m=Survey.processor.metadata,
                                    Chunk=c(paste(paste('Q.10',letters[1:11],sep='.'),'retail',sep='_'),
                                            paste(paste('Q.10',letters[1:11],sep='.'),'wholesale',sep='_')),
                                    REMUV='What is the average price you receive per kg of ',
                                    do.what='max')%>%
  mutate(Sector=ifelse(grepl('retail',Q),'retail','wholesale'))%>%
  dplyr::select(-Q)

Processor.price=full_join(Processor.price.min%>%rename(Min=Mean),
                          Processor.price.max%>%rename(Max=Mean),
                          by=c('Species','Sector'))

  #Fish & Chip 

  #ABARES
PRICES_abares=PRICES_abares%>%
                mutate(dolar.per.kg=Beach.Price..Adjusted.,
                       dolar.per.kg=as.numeric(gsub("\\$", "", dolar.per.kg)),
                       SPECIES=ASA.Species.Code)%>%
                dplyr::select(SPECIES,dolar.per.kg)



  #9.2 Get values by sector  
MC.sims=1000

#note: this assumes the entire catch is sold only by a given sector
fn.value=function(price_survey,annual.ktch,price_abares=NULL,
                         wastage.fillet.shrk=NULL,wastage.bellyflap.shrk=NULL,
                         wastage.fillet.tel=NULL)
{
  Add.bronzie=price_survey%>%
              filter(Species=='Dusky shark')%>%
              mutate(Species='Bronze whaler')
  price_survey=price_survey%>%
    mutate(Species=case_when(Species=='Shark fins'~'Shark fin',
                             Species=='Dhufish'~'West Australian dhufish',
                             Species=='Snapper'~'Pink snapper',
                             TRUE~Species))
  price_survey=rbind(price_survey,Add.bronzie)
  
  Scalefish.price=price_survey%>%filter(Species=='Other scalefish')%>%pull(Mean)
  Shark.price=price_survey%>%filter(Species=='Other sharks and rays')%>%pull(Mean)
  
  if(!is.null(price_abares))
  {
    d=left_join(annual.ktch,price_survey,by=c('COMMON_NAME'='Species'))%>%
      left_join(price_abares%>%mutate(SPECIES=as.integer(SPECIES)),by='SPECIES')%>%
      mutate(Price=case_when(is.na(Mean) ~dolar.per.kg,
                             TRUE~Mean),
             Price=case_when(is.na(Price) & Taxa=='Elasmobranch' ~ Shark.price,
                             is.na(Price) & Taxa=='Teleost' ~ Scalefish.price,
                             TRUE~Price))
  }else
  {
    d=left_join(annual.ktch,price_survey,by=c('COMMON_NAME'='Species'))%>%
      mutate(Price=case_when(is.na(Mean) & Taxa=='Elasmobranch' ~ Shark.price,
                             is.na(Mean) & Taxa=='Teleost' ~ Scalefish.price,
                             TRUE~Mean))
  }
  
  if(is.null(wastage.fillet.shrk))
  {
    d=d%>%mutate(Revenue=Avg.annual.landed_kg*Price)
  }else
  {
    d=d%>%
      mutate(Revenue.fillet=case_when(Taxa=='Elasmobranch'~Avg.annual.landed_kg*Price*wastage.fillet.shrk,
                                      Taxa=='Teleost'~Avg.annual.landed_kg*Price*wastage.fillet.tel),
             Revenue.fillet=case_when(COMMON_NAME=='Shark fin' ~ Avg.annual.landed_kg*Price,
                                      TRUE ~ Revenue.fillet),
             Revenue.belly=case_when(Taxa=='Elasmobranch'~Avg.annual.landed_kg*Price*wastage.bellyflap.shrk,
                                     Taxa=='Teleost'~Avg.annual.landed_kg*Price*0),
             Revenue.belly=case_when(COMMON_NAME=='Shark fin' ~ 0,
                                     TRUE~Revenue.belly),
             Revenue=Revenue.fillet+Revenue.belly)
  }
  
  Value.in.millions=sum(d%>%pull(Revenue))/1e6
  
  return(Value.in.millions)
}
  #9.2.1 Fishers (if no price available from survey, the use Abares, ie the 'PRICE' dataset)
Value.to.fisher_millions.min=fn.value(price_survey=Fisher.price.min,
                                      annual.ktch=Avrg.ktch,
                                      price_abares=PRICES) 
Value.to.fisher_millions.max=fn.value(price_survey=Fisher.price.max,
                                      annual.ktch=Avrg.ktch,
                                      price_abares=PRICES) 

Value.to.fisher_millions.MC=vector('list',MC.sims)
for(m in 1:MC.sims)
{
  price.sample=Fisher.price%>%mutate(Mean=NA) 
  for(x in 1:nrow(price.sample)) price.sample$Mean[x]=with(price.sample,runif(1,Min[x],Max[x]))
  price.sample=price.sample%>%dplyr::select(Mean,Species)
  Value.to.fisher_millions.MC[[m]]=fn.value(price_survey=price.sample,
                                            annual.ktch=Avrg.ktch,
                                            price_abares=PRICES)
}


  #9.2.2 Processors 
Value.to.processor_millions_retail.min=fn.value(price_survey=Processor.price.min%>%
                                                     filter(Sector=='retail'),
                                                annual.ktch=Avrg.ktch,
                                                wastage.fillet.shrk=Trunk.wastage$Trunk.to.fillet,  
                                                wastage.bellyflap.shrk=Trunk.wastage$Trunk.to.belly.flat,
                                                wastage.fillet.tel=Scalefish.to.fillet)
Value.to.processor_millions_wholesale.min=fn.value(price_survey=Processor.price.min%>%
                                                     filter(Sector=='wholesale'),
                                                   annual.ktch=Avrg.ktch,
                                                   wastage.fillet.shrk=Trunk.wastage$Trunk.to.fillet,  
                                                   wastage.bellyflap.shrk=Trunk.wastage$Trunk.to.belly.flat,
                                                   wastage.fillet.tel=Scalefish.to.fillet)
Value.to.processor_millions_retail.max=fn.value(price_survey=Processor.price.max%>%
                                                  filter(Sector=='retail'),
                                                annual.ktch=Avrg.ktch,
                                                wastage.fillet.shrk=Trunk.wastage$Trunk.to.fillet,  
                                                wastage.bellyflap.shrk=Trunk.wastage$Trunk.to.belly.flat,
                                                wastage.fillet.tel=Scalefish.to.fillet)
Value.to.processor_millions_wholesale.max=fn.value(price_survey=Processor.price.max%>%
                                                     filter(Sector=='wholesale'),
                                                   annual.ktch=Avrg.ktch,
                                                   wastage.fillet.shrk=Trunk.wastage$Trunk.to.fillet,  
                                                   wastage.bellyflap.shrk=Trunk.wastage$Trunk.to.belly.flat,
                                                   wastage.fillet.tel=Scalefish.to.fillet)


Value.to.processor_millions_retail.MC=Value.to.processor_millions_wholesale.MC=vector('list',MC.sims)
for(m in 1:MC.sims)
{
  price.sample=Processor.price%>%mutate(Mean=NA) 
  for(x in 1:nrow(price.sample)) price.sample$Mean[x]=with(price.sample,runif(1,Min[x],Max[x]))
  price.sample=price.sample%>%dplyr::select(Mean,Species,Sector)
  
  Value.to.processor_millions_retail.MC[[m]]=fn.value(price_survey=price.sample%>%
                                                        filter(Sector=='retail'),
                                                      annual.ktch=Avrg.ktch,
                                                      wastage.fillet.shrk=Trunk.wastage$Trunk.to.fillet,  
                                                      wastage.bellyflap.shrk=Trunk.wastage$Trunk.to.belly.flat,
                                                      wastage.fillet.tel=Scalefish.to.fillet)
  
  Value.to.processor_millions_wholesale.MC[[m]]=fn.value(price_survey=price.sample%>%
                                                           filter(Sector=='wholesale'),
                                                         annual.ktch=Avrg.ktch,
                                                         wastage.fillet.shrk=Trunk.wastage$Trunk.to.fillet,  
                                                         wastage.bellyflap.shrk=Trunk.wastage$Trunk.to.belly.flat,
                                                         wastage.fillet.tel=Scalefish.to.fillet)
}


  #9.2.3 Fish & Chip

    #Sharks
Shark.price.list=Shark.price.list%>%
  mutate(Price.per.kg.min=Price.per.fillet.min*1000/fillet.weight,
         Price.per.kg.max=Price.per.fillet.max*1000/fillet.weight)
fn.value.fish.chip_shk=function(price,annual.ktch,wastage.fillet.shrk,wastage.bellyflap.shrk,do.what)
{
  if(do.what=='min')
  {
    Shark.price=price%>%filter(Species=='Shark')%>%pull(Price.per.kg.min)
    d=left_join(annual.ktch,price,by=c('COMMON_NAME'='Species'))%>%
      mutate(Price=case_when(is.na(Price.per.kg.min) & Taxa=='Elasmobranch' ~ Shark.price,
                             TRUE~Price.per.kg.min))
  }
  if(do.what=='max')
  {
    Shark.price=price%>%filter(Species=='Shark')%>%pull(Price.per.kg.max)
    d=left_join(annual.ktch,price,by=c('COMMON_NAME'='Species'))%>%
      mutate(Price=case_when(is.na(Price.per.kg.max) & Taxa=='Elasmobranch' ~ Shark.price,
                             TRUE~Price.per.kg.max))
  }
  if(do.what=='mean')
  {
    Shark.price=price%>%filter(Species=='Shark')%>%pull(Price.per.kg.mean)
    d=left_join(annual.ktch,price,by=c('COMMON_NAME'='Species'))%>%
      mutate(Price=case_when(is.na(Price.per.kg.mean) & Taxa=='Elasmobranch' ~ Shark.price,
                             TRUE~Price.per.kg.mean))
  }
  d=d%>%
    mutate(Revenue.fillet=case_when(Taxa=='Elasmobranch'~Avg.annual.landed_kg*Price*wastage.fillet.shrk),
           Revenue.belly=case_when(Taxa=='Elasmobranch'~Avg.annual.landed_kg*Price*wastage.bellyflap.shrk),
           Revenue=Revenue.fillet+Revenue.belly)
  
  Value.in.millions=sum(d%>%pull(Revenue))/1e6
  
  return(Value.in.millions)
}
Value.to.fish.chip_millions_shk.min=fn.value.fish.chip_shk(price=Shark.price.list,
                                                       annual.ktch=Avrg.ktch%>%filter(Taxa=='Elasmobranch' & !COMMON_NAME=="Shark fin"),
                                                       wastage.fillet.shrk=Trunk.wastage$Trunk.to.fillet,
                                                       wastage.bellyflap.shrk=Trunk.wastage$Trunk.to.belly.flat,
                                                       do.what='min')
Value.to.fish.chip_millions_shk.max=fn.value.fish.chip_shk(price=Shark.price.list,
                                                           annual.ktch=Avrg.ktch%>%filter(Taxa=='Elasmobranch' & !COMMON_NAME=="Shark fin"),
                                                           wastage.fillet.shrk=Trunk.wastage$Trunk.to.fillet,
                                                           wastage.bellyflap.shrk=Trunk.wastage$Trunk.to.belly.flat,
                                                           do.what='max')

Value.to.fish.chip_millions_shk.MC=vector('list',MC.sims)
for(m in 1:MC.sims)
{
  price.sample=Shark.price.list%>%mutate(Price.per.kg.mean=NA) 
  for(x in 1:nrow(price.sample)) price.sample$Price.per.kg.mean[x]=with(price.sample,runif(1,Price.per.kg.min[x],Price.per.kg.max[x]))
  price.sample=price.sample%>%dplyr::select(Price.per.kg.mean,Species)
  Value.to.fish.chip_millions_shk.MC[[m]]=fn.value.fish.chip_shk(price=price.sample,
                                                                 annual.ktch=Avrg.ktch%>%filter(Taxa=='Elasmobranch' & !COMMON_NAME=="Shark fin"),
                                                                 wastage.fillet.shrk=Trunk.wastage$Trunk.to.fillet,
                                                                 wastage.bellyflap.shrk=Trunk.wastage$Trunk.to.belly.flat,
                                                                 do.what='mean')
}


    #Scalefish 
fn.value.fish.chip_tel=function(beach.price,multiplier,annual.ktch)
{
  beach.price=beach.price%>%
    mutate(Species=case_when(Species=='Dhufish'~'West Australian dhufish',
                             Species=='Snapper'~'Pink snapper',
                             TRUE~Species))
  Scalefish.price=beach.price%>%filter(Species=='Other scalefish')%>%pull(Mean)
  
  d=left_join(annual.ktch,multiplier,by=c('COMMON_NAME'='Species'))%>%
    mutate(Multiplier=case_when(is.na(Multiplier) & Taxa=='Teleost' ~ Other.scalefish.price.multiplier,
                                TRUE~Multiplier))%>%
    left_join(beach.price,by=c('COMMON_NAME'='Species'))%>%
    mutate(Price=case_when(is.na(Mean) & Taxa=='Teleost' ~ Scalefish.price,
                           TRUE~Mean))%>%
    mutate(Revenue=Avg.annual.landed_kg*Price*Multiplier)
  
  Value.in.millions=sum(d%>%pull(Revenue))/1e6
  
  return(Value.in.millions)
}
Value.to.fish.chip_millions_tel.min=fn.value.fish.chip_tel(beach.price=Fisher.price.min,
                                                       multiplier=FSRDGVP.multipliers,
                                                       annual.ktch=Avrg.ktch%>%filter(Taxa=='Teleost'))
Value.to.fish.chip_millions_tel.max=fn.value.fish.chip_tel(beach.price=Fisher.price.max,
                                                           multiplier=FSRDGVP.multipliers,
                                                           annual.ktch=Avrg.ktch%>%filter(Taxa=='Teleost'))

Value.to.fish.chip_millions_tel.MC=vector('list',MC.sims)
for(m in 1:MC.sims)
{
  price.sample=Fisher.price%>%mutate(Mean=NA) 
  for(x in 1:nrow(price.sample)) price.sample$Mean[x]=with(price.sample,runif(1,Min[x],Max[x]))
  price.sample=price.sample%>%dplyr::select(Mean,Species)
  Value.to.fish.chip_millions_tel.MC[[m]]=fn.value.fish.chip_tel(beach.price=price.sample,
                                                                 multiplier=FSRDGVP.multipliers,
                                                                 annual.ktch=Avrg.ktch%>%filter(Taxa=='Teleost'))
}

    #Total
Value.to.fish.chip_millions.min=Value.to.fish.chip_millions_shk.min+Value.to.fish.chip_millions_tel.min
Value.to.fish.chip_millions.max=Value.to.fish.chip_millions_shk.max+Value.to.fish.chip_millions_tel.max

Value.to.fish.chip_millions.MC=vector('list',MC.sims)
for(m in 1:MC.sims) Value.to.fish.chip_millions.MC[[m]]=Value.to.fish.chip_millions_shk.MC[[m]]+Value.to.fish.chip_millions_tel.MC[[m]]
  

  #9.3 Get Proportion Processor sold retail and wholesale
fn.get.proportion.ktch=function(d,d_m,Chunk)
{
  d=d[,Chunk]%>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  d=data.frame(Q=names(d),Mean=unlist(d))
  
  d_m=d_m%>%
    filter(Question_no%in%Chunk)%>%
    mutate(Group=case_when(Question_no=='Q.6.a'~'Retail',
                           Question_no=='Q.6.b'~'Fish.chips',
                           TRUE~'Wholesale'))%>%
    dplyr::select(-Section)
  
  d=d%>%left_join(d_m,by=c('Q'='Question_no'))%>%
    filter(!Q%in%c('Q.6.c'))%>%
    group_by(Group)%>%
    summarise(Proportion=sum(Mean)/100)%>%
    ungroup()
  return(d)
}
Prop.ktch.processor=fn.get.proportion.ktch(d=Survey.processor,
                                           d_m=Survey.processor.metadata,
                                           Chunk=paste('Q.6',letters[1:7],sep='.'))
Prop.ktch.processor$Proportion=Prop.ktch.processor$Proportion/sum(Prop.ktch.processor$Proportion)

  #9.4 Calculate actual value considering Processor and Fish & Chips proportion of the catch
fn.out=function(x) Prop.ktch.processor%>%filter(Group==x)%>%pull(Proportion)
Fishery.value.min=Value.to.processor_millions_retail.min * fn.out('Retail') + 
                  Value.to.processor_millions_wholesale.min *Scale.factor* fn.out('Wholesale') +
                  Value.to.fish.chip_millions.min * fn.out('Fish.chips')
Fishery.value.max=Value.to.processor_millions_retail.max * fn.out('Retail') + 
                  Value.to.processor_millions_wholesale.max *Scale.factor* fn.out('Wholesale') +
                  Value.to.fish.chip_millions.max * fn.out('Fish.chips')

Fishery.value.MC=vector('list',MC.sims)
for(m in 1:MC.sims)
{
  Fishery.value.MC[[m]]=Value.to.processor_millions_retail.MC[[m]] * fn.out('Retail') + 
                        Value.to.processor_millions_wholesale.MC[[m]] *Scale.factor* fn.out('Wholesale') +
                        Value.to.fish.chip_millions.MC[[m]] * fn.out('Fish.chips')
}

Value.MC=data.frame(Fisher=unlist(Value.to.fisher_millions.MC),  
                    Fishery=unlist(Fishery.value.MC))
Median=Value.MC%>%summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE)))
CI.low=Value.MC%>%summarise(across(where(is.numeric), ~ quantile(.x, probs=0.025,na.rm = TRUE)))
CI.up=Value.MC%>%summarise(across(where(is.numeric), ~ quantile(.x, probs=0.975,na.rm = TRUE)))

write.csv(data.frame(Sector=c('Fisher','Fishery'),
                     Value_millions.per.year.min=c(Value.to.fisher_millions.min,Fishery.value.min),
                     Value_millions.per.year.max=c(Value.to.fisher_millions.max,Fishery.value.max),
                     CI_lower=unlist(CI.low),
                     Median=unlist(Median),
                     CI_upper=unlist(CI.up))%>%mutate(Delta.CV=Median-CI_lower),
          le.paste("Socio-economics/Fishery.value.csv"),row.names = F)


#10. Fishery valuation - GVA     
#10.1 Fishery revenue
GVA.revenue.min=Value.to.fisher_millions.min*1e6
GVA.revenue.max=Value.to.fisher_millions.max*1e6

GVA.revenue.MC=vector('list',MC.sims)
for(m in 1:MC.sims) GVA.revenue.MC[[m]]=Value.to.fisher_millions.MC[[m]]*1e6


#10.2 Total number of vessels in TDGDLF
N.vessels=sum(TDGDLF.vessels$Freq)

#10.3 Overall costs  
Avg.ves.annual.ktch=mean(Total.ktch.per.boat%>%filter(Tot>1000)%>%pull(Tot))
Kst.question=str_extract_all(Survey.fishers$Q.16, "[[:digit:]]+")
names(Kst.question)=Survey.fishers$Q.4
Kst.question=Kst.question[!is.na(Kst.question)]
for(k in 1:length(Kst.question))
{
  d=Kst.question[[k]]
  if(length(d)==1)d=data.frame(Min=as.numeric(d[1]),Max=NA)
  if(length(d>1))d=data.frame(Min=as.numeric(d[1]),Max=as.numeric(d[2]))
  d$BoatName=tolower(names(Kst.question)[k])
  Kst.question[[k]]=d
  rm(d)
}
Kst.question=do.call(rbind,Kst.question)%>%
  filter(!BoatName=="NA")

Kst.question=Kst.question%>%
  left_join(rbind(Total.ktch.per.boat,
                  data.frame(BoatName='st gerard; doreen',
                             Tot=mean(Total.ktch.per.boat%>%
                                        filter(BoatName%in%c("st gerard m","dorreen"))%>%pull(Tot)))),
            by='BoatName')%>%
  filter(!is.na(Tot))%>%
  mutate(Avg.ves.annual.ktch=Avg.ves.annual.ktch,
         Min.scaled=Min*Avg.ves.annual.ktch/Tot,
         Max.scaled=Max*Avg.ves.annual.ktch/Tot)%>%
  filter(!BoatName=='planjak ii')  #remove from cost estimating as not representative, too few trips

Annual.cost.min=min(Kst.question$Min.scaled,na.rm=T) #min & max annual cost for average vessel (scaled by the catch)
Annual.cost.max=max(Kst.question$Max.scaled,na.rm=T)

Annual.cost.MC=vector('list',MC.sims)
for(m in 1:MC.sims)
{
  dummy=Kst.question$Min
  for(x in 1:length(dummy))dummy[x]=with(Kst.question,runif(1,Min.scaled[x],Max.scaled[x]))
  Annual.cost.MC[[m]]=mean(dummy)
}
  

#10.4 Costs per vessel 
Kost.props.minus.wage=Survey.fishers[,paste('Q.19',letters[c(1:4,6:11)],sep='.')]
GVA.costs.min=sum(apply(Kost.props.minus.wage,2,mean,na.rm=T)*Annual.cost.min/100)
GVA.costs.max=sum(apply(Kost.props.minus.wage,2,mean,na.rm=T)*Annual.cost.max/100)
GVA.costs.MC=vector('list',MC.sims)
for(m in 1:MC.sims)
{
  GVA.costs.MC[[m]]=sum(apply(Kost.props.minus.wage,2,mean,na.rm=T)*Annual.cost.MC[[m]]/100)
}

#10.5 Taxes per vessel
GVA.taxes.min=min(c(Survey.fishers$Q.18.a,Survey.fishers$Q.18.b),na.rm=T)
GVA.taxes.max=max(c(Survey.fishers$Q.18.a,Survey.fishers$Q.18.b),na.rm=T)
dummy=c(Survey.fishers$Q.18.a,Survey.fishers$Q.18.b)
dummy=dummy[!is.na(dummy)]
GVA.taxes.MC=vector('list',MC.sims)
for(m in 1:MC.sims) GVA.taxes.MC[[m]]=runif(1,min(dummy),max(dummy))



#10.6 Wages per vessel
GVA.wages.min=mean(Survey.fishers$Q.19.e,na.rm=T)*Annual.cost.min/100
GVA.wages.max=mean(Survey.fishers$Q.19.e,na.rm=T)*Annual.cost.max/100
dummy=Survey.fishers$Q.19.e
dummy=dummy[!is.na(dummy)]
GVA.wages.MC=vector('list',MC.sims)
for(m in 1:MC.sims) GVA.wages.MC[[m]]=runif(1,min(dummy),max(dummy))*Annual.cost.MC[[m]]/100



#10.7 Total GVA
GVA.fn=function(Fishery.revenue,Costs.per.vsl,N.ves)
{
  GVA=Fishery.revenue - Costs.per.vsl*N.ves 
  return(GVA/1e6)
}
GVA.min=GVA.fn(Fishery.revenue=GVA.revenue.min,
               Costs.per.vsl=GVA.costs.min,
               N.ves=N.vessels)
GVA.max=GVA.fn(Fishery.revenue=GVA.revenue.max,
               Costs.per.vsl=GVA.costs.max,
               N.ves=N.vessels)
GVA.MC=vector('list',MC.sims)
for(m in 1:MC.sims)
{
  GVA.MC[[m]]=GVA.fn(Fishery.revenue=GVA.revenue.MC[[m]],
                     Costs.per.vsl=GVA.costs.MC[[m]],
                     N.ves=N.vessels)
}
Value=unlist(GVA.MC)
Median=median(Value)
CI.low=quantile(Value, probs=0.025,na.rm = TRUE)
CI.up=quantile(Value, probs=0.975,na.rm = TRUE)
write.csv(data.frame(Range=c('Min','Max'),
                     GVA=round(c(GVA.min,GVA.max),2),
                     Units='AUD.million')%>%
                      mutate(CI_lower=round(CI.low,1),
                             Median=round(Median,1),
                             CI_upper=round(CI.up,1),
                             Delta.CV=round(Median-CI_lower,1)),
          le.paste("Socio-economics/Fishery.value_GVA.csv"),row.names = F)


 #Fisher income
Min.income=4e4
Fishr.income=function(Annual.revenue,Annual.cost,Annual.tax,Annual.wages)  
{
  return(Annual.revenue-Annual.cost-Annual.tax-Annual.wages)
}
Fisher.annual.income.MC=vector('list',MC.sims)
for(m in 1:MC.sims)
{
  xx=Fishr.income(Annual.revenue=GVA.revenue.MC[[m]]/N.vessels,
                  Annual.cost=GVA.costs.MC[[m]],
                  Annual.tax=GVA.taxes.MC[[m]],
                  Annual.wages=GVA.wages.MC[[m]]) 
  if(xx>Min.income)Fisher.annual.income.MC[[m]]=xx
}
Fisher.annual.income.MC=compact(Fisher.annual.income.MC) 

Value=unlist(Fisher.annual.income.MC)
Median=median(Value)
CI.low=quantile(Value, probs=0.025,na.rm = TRUE)
CI.up=quantile(Value, probs=0.975,na.rm = TRUE)
write.csv(data.frame(Sector='Fisher')%>%
                      mutate(CI_lower=round(CI.low,1),
                             Median=round(Median,1),
                             CI_upper=round(CI.up,1),
                             Delta.CV=round(Median-CI_lower,1)),
          le.paste("Socio-economics/Fisher.income.csv"),row.names = F)




#11. Fishery valuation - Employment

#Fisher
FTE_fisher.min=min(Survey.fishers$Q.7,na.rm=T)
FTE_fisher.max=max(Survey.fishers$Q.7,na.rm=T)
dummy=Survey.fishers$Q.7
dummy=dummy[!is.na(dummy)]
FTE_fisher.MC=vector('list',MC.sims)
for(m in 1:MC.sims) FTE_fisher.MC[[m]]=runif(1,min(dummy),max(dummy))

Total.employees_fisher.min=min(Survey.fishers$Q.8,na.rm=T)
Total.employees_fisher.max=max(Survey.fishers$Q.8,na.rm=T)
dummy=Survey.fishers$Q.8
dummy=dummy[!is.na(dummy)]
Total.employees_fisher.MC=vector('list',MC.sims)
for(m in 1:MC.sims) Total.employees_fisher.MC[[m]]=runif(1,min(dummy),max(dummy))

#Processor
FTE_proc.min=min(Survey.processor$Q.3,na.rm=T)
FTE_proc.max=max(Survey.processor$Q.3,na.rm=T)
dummy=Survey.processor$Q.3
dummy=dummy[!is.na(dummy)]
FTE_proc.MC=vector('list',MC.sims)
for(m in 1:MC.sims) FTE_proc.MC[[m]]=runif(1,min(dummy),max(dummy))

Total.employees_proc.min=min(Survey.processor$Q.4,na.rm=T)
Total.employees_proc.max=max(Survey.processor$Q.4,na.rm=T)
dummy=Survey.processor$Q.4
dummy=dummy[!is.na(dummy)]
Total.employees_proc.MC=vector('list',MC.sims)
for(m in 1:MC.sims) Total.employees_proc.MC[[m]]=runif(1,min(dummy),max(dummy))

#total FTE and total employees across fishery
Total.employees_fishery.MC=Total.FTE_fishery.MC=Total.employees_fisher.MC
for(m in 1:MC.sims)
{
  Total.FTE_fishery.MC[[m]]=FTE_fisher.MC[[m]]*N.vessels
  Total.employees_fishery.MC[[m]]=Total.employees_fisher.MC[[m]]*N.vessels
}
Value=unlist(Total.employees_fishery.MC)
Median_emp=median(Value)
CI.low_emp=quantile(Value, probs=0.025,na.rm = TRUE)
CI.up_emp=quantile(Value, probs=0.975,na.rm = TRUE)

  
Value=unlist(Total.FTE_fishery.MC)
Median_FTE=median(Value)
CI.low_FTE=quantile(Value, probs=0.025,na.rm = TRUE)
CI.up_FTE=quantile(Value, probs=0.975,na.rm = TRUE)


write.csv(data.frame(Sector=c('Fisher','Processor'),
                     Average_FTE.min=round(c(FTE_fisher.min,FTE_proc.min)),
                     Average_FTE.max=round(c(FTE_fisher.max,FTE_proc.max)),
                     Average_All.min=round(c(Total.employees_fisher.min,Total.employees_proc.min)),
                     Average_All.max=round(c(Total.employees_fisher.max,Total.employees_proc.max)))%>%
            mutate(CI_lower_FTE=c(round(CI.low_FTE,1),NA),    #for processors, I need the total number of processors
                   Median_FTE=c(round(Median_FTE,1),NA),
                   CI_upper_FTE=c(round(CI.up_FTE,1),NA),
                   Delta.CV_FTE=c(round(Median_FTE-CI_lower_FTE,1)),
                   CI_lower_emp=c(round(CI.low_emp,1),NA),    
                   Median_emp=c(round(Median_emp,1),NA),
                   CI_upper_emp=c(round(CI.up_emp,1),NA),
                   Delta.CV_emp=c(round(Median_emp-CI_lower_emp,1))),
          le.paste("Socio-economics/Employees.csv"),row.names = F)


#12. Vertical integration
Total.ktch.per.boat.and.skipper=read.csv(le.paste("Socio-economics/Total.ktch.per.boat_skipper.csv"))
Integrated.business=Total.ktch.per.boat.and.skipper%>%
  group_by(BoatName)%>%
  summarise(Tot=sum(Tot))%>%
  ungroup()%>%
  left_join(Fisher.and.processor, by='BoatName')%>%
  mutate(Integrated=ifelse(!is.na(Fisher),'Fisher & Processor','Fisher only'))%>%
  arrange(-Tot)%>%
  mutate(position=1:n(),
         Tot.percent=100*Tot/sum(Tot))
percent.int=round(Integrated.business%>%
              group_by(Integrated)%>%
              summarise(percent=sum(Tot.percent))%>%
              filter(Integrated=='Fisher & Processor')%>%
              pull(percent))
Integrated.business%>%
  ggplot(aes(position,Tot.percent,fill=Integrated))+
  geom_col()+
  ylab('Percent of total reported catch')+xlab('Vessel')+
  theme_PA(leg.siz=Cex.res+10,axs.t.siz=Cex.res+5,axs.T.siz=Cex.res+8)+
  theme(legend.position = "top",
        legend.title=element_blank(),
        plot.caption = element_text(size=14))+
  labs(caption = paste(paste("Fisher & Processor (",percent.int,"%),",sep=''),
                       paste("Fisher only (",100-percent.int,"%)",sep='')))
ggsave(le.paste("Socio-economics/Vertical_integration.tiff"),width = 9,height = 10,compression = "lzw")

TDGDLF_processors=nrow(Fisher.and.processor)+1 #add Adam Soumelidis who does not currently own boat by process mostly TDGDLF catch
write.csv(TDGDLF_processors,le.paste(paste("Socio-economics","Processors_TDGDLF.csv",sep='/')),row.names = F)


#13. All WA Processors 
#note: processor's return do not include catch caught by the processor (e.g. SCIMONE's own caught catch not included 
#     in processors return; Buckridge and Soulos only processing his catch so doesn't even appear in the processor returns)
WA.proc.shark=WA.processors.landings%>%
                filter(SpeciesCode<50000)%>%
  left_join(WA.processors,by=c('LicNum'="Licnum"))
Total.processing.shark=length(WA.proc.shark%>%distinct(SITE_NAME)%>%pull(SITE_NAME))+2 #Soulos and Buckridge
Total.processing=nrow(WA.processors.landings%>%distinct(LicNum))+2 #Soulos and Buckridge
write.csv(Total.processing.shark,le.paste(paste("Socio-economics","Processors_allWA_shark.csv",sep='/')),row.names = F)
write.csv(Total.processing,le.paste(paste("Socio-economics","Processors_allWA.csv",sep='/')),row.names = F)



#14. Map of economic and social contribution 
Do.map.location=FALSE
if(Do.map.location)
{
  #MISSING: input the calculated GVA, social value, FTEs, etc by location
  SKLr=50
  #Economic contribution
  D=data.frame(lon.port=c(117.8837,115.1579),
               lat.port=c(-35.0269,-34.3166),
               Quantity=c(150000,350000))%>%   #the quantity to plot (GVA, FTEs, etc)
    mutate(Prop=Quantity/sum(Quantity))
  p1=plt.map(D=D,
             Title="Economic contributon",
             Scaler=SKLr)
  Bubble.ref=data.frame(lon.port=c(128,128),
                        lat.port=c(-35.75,-33.75),
                        Prop=c(0.25,0.50),
                        Text=c(1e5,5e5))
  p1=p1+geom_point(data = Bubble.ref, mapping = aes(x = lon.port, y = lat.port),
                   size=Bubble.ref$Prop*SKLr, colour = grDevices::adjustcolor("steelblue", alpha=0.6))+
    geom_text(data=Bubble.ref,aes(x = lon.port, y = lat.port,label = paste('$',Text,sep='')),
              fontface="bold",color="white",size=5)
  
  #Social contribution
  D=data.frame(lon.port=c(117.8837,115.1579),
               lat.port=c(-35.0269,-34.3166),
               Quantity=c(5,7))%>%   #the quantity to plot (GVP, FTEs, etc)
    mutate(Prop=Quantity/sum(Quantity))
  p2=plt.map(D=D,
             Title='Social contribution',
             Scaler=SKLr)
  
  Bubble.ref=data.frame(lon.port=c(128,128),
                        lat.port=c(-35.75,-33.75),
                        Prop=c(0.25,0.50),
                        Text=c(5,10))
  p2=p2+geom_point(data = Bubble.ref, mapping = aes(x = lon.port, y = lat.port),
                   size=Bubble.ref$Prop*SKLr, colour = grDevices::adjustcolor("steelblue", alpha=0.6))+
    geom_text(data=Bubble.ref,aes(x = lon.port, y = lat.port,label = paste(Text,'FTEs')),
              fontface="bold",color="white",size=5)
  
  ggarrange(p1, p2, ncol = 1, nrow = 2)
  ggsave(le.paste("Socio-economics/Map_contributions.tiff"),width = 7,height = 10,compression = "lzw")
  
}

#15. Compare Survey reply and catch * price
Compare.survey.VS.reported=Survey.fishers[,c('Q.2','Q.11')]%>%
  filter(!is.na(Q.11))%>%
  mutate(Revenue.survey.min=as.numeric(read.table(text = Q.11, fill = TRUE)[[1]]),
         Revenue.survey.max=read.table(text = Q.11, fill = TRUE)[[3]],
         Revenue.survey.max=ifelse(Revenue.survey.max=="more",NA,Revenue.survey.max),
         Revenue.survey.max=as.numeric(Revenue.survey.max))%>%
  filter(!Q.2%in%c("Gregory Sharp","Nils Stokke"))  #no vessel name currently or no catch (Nils) so cannot track catch


Average.revenue.per.landed.kg=Avrg.ktch%>%mutate(Species=COMMON_NAME,   #note: using average catch across fleet, not vessel-specific catch hence discrepancies
                                                 Species=case_when(
                                                   !COMMON_NAME%in%c("Gummy shark","Dusky shark",
                                                                     "Whiskery shark","Sandbar shark") & SPECIES<40000~"Other sharks and rays",
                                                   !COMMON_NAME%in%c("Snapper","Dhufish","Queen snapper","Blue groper") & SPECIES>40000~"Other scalefish",
                                                   TRUE~Species),
                                                 Species=ifelse(SPECIES==22998,'Shark fins',Species))%>%
  left_join(Fisher.price,by='Species')%>%
  mutate(Mean.price=(Min+Max)/2,
         Revenue=Avg.annual.landed_kg*Mean.price)
Average.revenue.per.landed.kg=sum(Average.revenue.per.landed.kg$Revenue)/sum(Average.revenue.per.landed.kg$Avg.annual.landed_kg)

Compare.survey.VS.reported=Compare.survey.VS.reported%>%
  left_join(data.frame(Q.2=c("Jeff Cooke","Markus Branderhorst",
                             "Michael Tonkin","Michael Tonkin","Peter Warrilow",
                             "Steve McWhirter","Storm Manstead"),
                       BoatName=c("tracey lea","marrabundi",
                                  "doreen","st gerard m","planjak ii",
                                  "elizabeth maria ii","fatal attraction"))%>%
              left_join(Total.ktch.per.boat,by='BoatName')%>%
              mutate(Average.annual.revenue=Average.revenue.per.landed.kg*Tot),
            by='Q.2')

#---------Analyse PA economics of Longline vs Gillnet ------------

#1. Revenue from catching one tonne
  #1.1. catch composition
Composition_GN=read.csv(le.paste("Socio-economics/catch.per.shot/Composition_GN.csv"))
Composition_LL=read.csv(le.paste("Socio-economics/catch.per.shot/Composition_LL.csv"))

  #1.2. price per species ($ per kg of trunk for sharks or whole for scalefish)  
This=c("Q.4",paste('Q.13',letters[1:11],sep='.'))
oldnames = paste('Q.13',letters[1:11],sep='.')
newnames=c("Gummy shark","Dusky shark","Whiskery shark","Sandbar shark","fins","Other sharks",
           "Pink snapper","West Australian dhufish","Queen snapper","Blue groper","Other scalefish")
Price.species=Survey.fishers[,This]%>%
    rename_at(vars(oldnames), ~ newnames)%>%
  dplyr::select(-Q.4)%>%
  gather(common_name.economics,Price.per.kg)%>%
  mutate(Price.per.kg=ifelse(is.na(Price.per.kg) & common_name.economics=='fins',16.5,
                             ifelse(is.na(Price.per.kg) & common_name.economics=='Other scalefish',5.4,
                             Price.per.kg)))%>%  #added manually as not provided
  group_by(common_name.economics)%>%
  summarise(Price.per.kg=mean(Price.per.kg,na.rm=T))

Price.LL.to.GN=mean(c(1,1+1.4*.1,1+1.4*.1))   #from Additional.questions$Q.29 & Additional.questions$Q.30. 
                                      # LL-caught gets 42% better price if gillnet caught damaged by sealice.
                                      # Sealice damage was not quantified by it is not very common so set at 10% 

  #1.3. calculate revenue
sp.pris=unique(Price.species$common_name.economics)
Rev_LL=sum(Composition_LL%>%
          mutate(Name=case_when(!COMMON_NAME%in%sp.pris & Taxa=='Elasmobranch'~'Other sharks',
                                !COMMON_NAME%in%sp.pris & Taxa=='Teleost'~'Other scalefish',
                                TRUE~COMMON_NAME))%>%
          left_join(Price.species,by=c('Name'='common_name.economics'))%>%
          mutate(Revenue.per.ton=1000*prop*Price.per.kg*Price.LL.to.GN)%>%pull(Revenue.per.ton))

Rev_GN=sum(Composition_GN%>%
             mutate(Name=case_when(!COMMON_NAME%in%sp.pris & Taxa=='Elasmobranch'~'Other sharks',
                                   !COMMON_NAME%in%sp.pris & Taxa=='Teleost'~'Other scalefish',
                                   TRUE~COMMON_NAME))%>%
             left_join(Price.species,by=c('Name'='common_name.economics'))%>%
             mutate(Revenue.per.ton=1000*prop*Price.per.kg)%>%pull(Revenue.per.ton))

#2. Costs to catch one tonne

  #2.1. number of fishing days required to catch a tonne of fish
Ndays_one.tone_GN=read.csv(le.paste("Socio-economics/catch.per.shot/Ndays_one.tone_GN.csv"))
Ndays_one.tone_LL=read.csv(le.paste("Socio-economics/catch.per.shot/Ndays_one.tone_LL.csv"))

Ndays_one.tone_LL_S1=Ndays_one.tone_LL%>%
  mutate(shot=2,
         fishing.day.1.ton_Catch=fishing.day.1.ton_Catch/2,
         fishing.day.1.ton_Catch.lower=fishing.day.1.ton_Catch.lower/2,
         fishing.day.1.ton_Catch.upper=fishing.day.1.ton_Catch.upper/2)
Ndays_one.tone_LL=rbind(Ndays_one.tone_LL,Ndays_one.tone_LL_S1)  

  #2.2. Overall parameters (from questionnaires)  
Annual.cost.GN=mean(c(Kst.question$Min.scaled,Kst.question$Max.scaled),na.rm=T)  #mean annual costs for average vessel
dis.q=c('Q.6','Q.7','Q.10','Q.19','Q.20','Q.25','Q.26','Q.27','Q.28','Q.29','Q.30')

Add_d=Additional.questions[,dis.q]%>%data.frame
Additional.questions.metadata%>%filter(Question_no%in%dis.q)%>%data.frame
Annual.LL.days=round(mean(c(150,150,170,120)))  #from Add_d$Q.6
Annual.GN.days=round(mean(c(100,120,140,95)))   #from Add_d$Q.7 

  #2.3 crew costs to catch one tonne    
Annual.cost.crew_GN=mean(Survey.fishers$Q.19.e,na.rm=T)*Annual.cost.GN/100
Annual.cost.crew_LL=Annual.cost.crew_GN

Annual.n.fte_GN=mean(Survey.fishers$Q.7)
Annual.n.fte_LL=Annual.n.fte_GN

n.crew_LL=mean(c(2,2,3))+mean(c(1,0,1)) #from Add_d$Q.26, Add_d$Q.27
n.crew_GN=mean(c(2,2,3))  #from Add_d$Q.25

crew.cost=function(an.crew.kst,an.ftes,an.days,n.crew.day,n.day_one.ton)
{
  Lower=((an.crew.kst/an.ftes)/an.days)*n.crew.day*n.day_one.ton$fishing.day.1.ton_Catch.lower
  Mean=((an.crew.kst/an.ftes)/an.days)*n.crew.day*n.day_one.ton$fishing.day.1.ton_Catch
  Upper=((an.crew.kst/an.ftes)/an.days)*n.crew.day*n.day_one.ton$fishing.day.1.ton_Catch.upper
  return(list(Lower=Lower,Mean=Mean,Upper=Upper))
}
Cost_crew_LL=crew.cost(an.crew.kst=Annual.cost.crew_LL,
                       an.ftes=Annual.n.fte_LL,
                       an.days=Annual.LL.days,
                       n.crew.day=n.crew_LL,
                       n.day_one.ton=Ndays_one.tone_LL%>%filter(shot==1))
Cost_crew_LL_S1=crew.cost(an.crew.kst=Annual.cost.crew_LL,
                          an.ftes=Annual.n.fte_LL,
                          an.days=Annual.LL.days,
                          n.crew.day=n.crew_LL,
                          n.day_one.ton=Ndays_one.tone_LL%>%filter(shot==2))
Cost_crew_GN=crew.cost(an.crew.kst=Annual.cost.crew_GN,
                          an.ftes=Annual.n.fte_GN,
                          an.days=Annual.GN.days,
                          n.crew.day=n.crew_GN,
                          n.day_one.ton=Ndays_one.tone_GN)

  
  #2.4. gear repair costs to catch one tonne   
Annual.cost.gear.rep_GN=mean(Survey.fishers$Q.19.c,na.rm=T)*Annual.cost.GN/100
Annual.cost.gear.rep_LL_MB=5000  # Markus Branderhorst survey  
Annual.cost.gear.rep_LL=Annual.cost.gear.rep_LL_MB *(Avg.ves.annual.ktch/Total.ktch.per.boat%>%     #for average vessel
                                                       filter(BoatName=='marrabundi')%>%pull(Tot))

gear.rep.cost=function(an.gear.rep.kst,an.days,n.day_one.ton)
{
  Lower=(an.gear.rep.kst/an.days)*n.day_one.ton$fishing.day.1.ton_Catch.lower
  Mean=(an.gear.rep.kst/an.days)*n.day_one.ton$fishing.day.1.ton_Catch
  Upper=(an.gear.rep.kst/an.days)*n.day_one.ton$fishing.day.1.ton_Catch.upper
  return(list(Lower=Lower,Mean=Mean,Upper=Upper))
}
Cost_gear.rep_LL=gear.rep.cost(an.gear.rep.kst=Annual.cost.gear.rep_LL,
                                  an.days=Annual.LL.days,
                                  n.day_one.ton=Ndays_one.tone_LL%>%filter(shot==1))
Cost_gear.rep_LL_S1=gear.rep.cost(an.gear.rep.kst=Annual.cost.gear.rep_LL,
                                  an.days=Annual.LL.days,
                                  n.day_one.ton=Ndays_one.tone_LL%>%filter(shot==2)) 
Cost_gear.rep_GN=gear.rep.cost(an.gear.rep.kst=Annual.cost.gear.rep_GN,
                                  an.days=Annual.GN.days,
                                  n.day_one.ton=Ndays_one.tone_GN)
 

  #2.5. bait costs to catch one tonne (longline only)
Bait.AUD_kg=mean(c(1,3,2.5,3.5,2.5)) #from Add_d$Q.19
Bait.kg.per.hook=mean(c(8/250,10/250,70/1000,35/750)) #from Add_d$Q.20
Bait.AUD.per.hook=Bait.kg.per.hook*Bait.AUD_kg
bait.cost=function(n.huk.day,n.shot.day,bait.price.huk,n.day_one.ton)
{
  Lower=n.huk.day*n.shot.day*bait.price.huk*n.day_one.ton$fishing.day.1.ton_Catch.lower
  Mean=n.huk.day*n.shot.day*bait.price.huk*n.day_one.ton$fishing.day.1.ton_Catch
  Upper=n.huk.day*n.shot.day*bait.price.huk*n.day_one.ton$fishing.day.1.ton_Catch.upper
  return(list(Lower=Lower,Mean=Mean,Upper=Upper))
}
Cost_bait_one.tonne=bait.cost(n.huk.day=Ndays_one.tone_LL$HOOKS[1],
                                      n.shot.day=Ndays_one.tone_LL$shot[1],
                                      bait.price.huk=Bait.AUD.per.hook,
                                      n.day_one.ton=Ndays_one.tone_LL%>%filter(shot==1))

Cost_bait_one.tonne_S1=bait.cost(n.huk.day=Ndays_one.tone_LL$HOOKS[1],
                                      n.shot.day=Ndays_one.tone_LL$shot[2],
                                      bait.price.huk=Bait.AUD.per.hook,
                                      n.day_one.ton=Ndays_one.tone_LL%>%filter(shot==2))

  #2.6 longline relative fuel cost for catching one tonne  
add.fuel=FALSE
if(add.fuel)
{
  fuel.rel.colst.LL=mean(c(1,1,1,2))-1  #M. Branderhorst leaves engine running and consumes double the fuel than gillnetting. Others don't
  Annual.cost.fuel_GN=mean(Survey.fishers$Q.19.d,na.rm=T)*Annual.cost.GN/100
  Annual.cost.fuel_LL=Annual.cost.fuel_GN
  LL.rel.fuel.cost=function(an.fuel.kst,an.days,fuel.rel.kst,n.day_one.ton)
  {
    Lower=(an.fuel.kst/an.days)*n.day_one.ton$fishing.day.1.ton_Catch.lower*fuel.rel.kst
    Mean=(an.fuel.kst/an.days)*n.day_one.ton$fishing.day.1.ton_Catch*fuel.rel.kst
    Upper=(an.fuel.kst/an.days)*n.day_one.ton$fishing.day.1.ton_Catch.upper*fuel.rel.kst
    return(list(Lower=Lower,Mean=Mean,Upper=Upper))
  }
  Cost_rel.fuel_LL=LL.rel.fuel.cost(an.fuel.kst=Annual.cost.fuel_LL,
                                    an.days=Annual.LL.days,
                                    fuel.rel.kst=fuel.rel.colst.LL,
                                    n.day_one.ton=Ndays_one.tone_LL%>%filter(shot==1))
  Cost_rel.fuel_LL_S1=LL.rel.fuel.cost(an.fuel.kst=Annual.cost.fuel_LL,
                                       an.days=Annual.LL.days,
                                       fuel.rel.kst=fuel.rel.colst.LL,
                                       n.day_one.ton=Ndays_one.tone_LL%>%filter(shot==2))
}
  

  #2.7 calculate total costs to catch one tonne
fn.tot.kst=function(Kru,Gr,Bei=NULL)
{
  Lower=Kru$Lower + Gr$Lower
  if(!is.null(Bei)) Lower=Lower+ Bei$Lower
  
  Mean=Kru$Mean + Gr$Mean
  if(!is.null(Bei)) Mean=Mean+ Bei$Mean
  
  Upper=Kru$Upper + Gr$Upper
  if(!is.null(Bei)) Upper=Upper+ Bei$Upper
  
  return(data.frame(Lower=Lower,Mean=Mean,Upper=Upper))
}
Cost_LL=fn.tot.kst(Kru=Cost_crew_LL,Gr=Cost_gear.rep_LL,Bei=Cost_bait_one.tonne) 
Cost_LL_S1=fn.tot.kst(Kru=Cost_crew_LL_S1,Gr=Cost_gear.rep_LL_S1,Bei=Cost_bait_one.tonne_S1)
Cost_GN=fn.tot.kst(Kru=Cost_crew_GN,Gr=Cost_gear.rep_GN)

if(add.fuel)
{
  Cost_LL=Cost_LL%>%mutate(Lower=Lower+Cost_rel.fuel_LL$Lower,
                           Mean=Mean+Cost_rel.fuel_LL$Mean,
                           Upper=Upper+Cost_rel.fuel_LL$Upper) 
  Cost_LL_S1=Cost_LL_S1%>%mutate(Lower=Lower+Cost_rel.fuel_LL_S1$Lower,
                           Mean=Mean+Cost_rel.fuel_LL_S1$Mean,
                           Upper=Upper+Cost_rel.fuel_LL_S1$Upper)
}


#3. Longline relative performance from catching one tonne   
#note: baiting time is considerable, only captured thru number of shots per day; GN has 0 baiting time
Relative_comparison=rbind(Cost_LL,Cost_LL_S1,Cost_GN)%>%
                      mutate(Method=c('LL','LL','GN'),
                             Scenario=c('Base','S1','Base'),
                             Rev=c(Rev_LL,Rev_LL,Rev_GN),
                             Lower.Rev.minus.ksts=Rev-Lower,
                             Mean.Rev.minus.ksts=Rev-Mean,
                             Upper.Rev.minus.ksts=Rev-Upper)
Lwr.in=Relative_comparison%>%filter(Method=='GN')%>%pull(Lower.Rev.minus.ksts)
Mean.in=Relative_comparison%>%filter(Method=='GN')%>%pull(Mean.Rev.minus.ksts)
Upr.in=Relative_comparison%>%filter(Method=='GN')%>%pull(Upper.Rev.minus.ksts)
Relative_comparison=Relative_comparison%>%
                      mutate(Lower.Rel.Per=Lower.Rev.minus.ksts/Lwr.in,
                             Mean.Rel.Per=Mean.Rev.minus.ksts/Mean.in,
                             Upper.Rel.Per=Upper.Rev.minus.ksts/Upr.in)

write.csv(Relative_comparison%>%
            filter(Method=='LL')%>%
            dplyr::select(Scenario,Lower.Rel.Per,Mean.Rel.Per,Upper.Rel.Per),
          le.paste("Socio-economics/longline.vs.gillnet/Relative_comparison.csv"),row.names = F)


#4. Output parameter inputs
Out.pars=data.frame(
           Longline.price.relative.to.gillnet=Price.LL.to.GN,
           Bait.kg.per.hook=Bait.kg.per.hook,
           Bait.AUD_kg=Bait.AUD_kg,
           Bait.AUD.per.hook=Bait.AUD.per.hook,
           Hooks.per.day=unique(Ndays_one.tone_LL$HOOKS),
           Shots.per.day_s1=Ndays_one.tone_LL$shot[1],
           Shots.per.day_s2=Ndays_one.tone_LL$shot[2],
           Annual.cost.gear.rep=Annual.cost.gear.rep_GN,
           Annual.cost.gear.rep_LL=Annual.cost.gear.rep_LL,
           daily.crew_LL=n.crew_LL,
           daily.crew_GN=n.crew_GN,
           Annual.n.ftes=Annual.n.fte_GN,
           Annual.cost.crew=Annual.cost.crew_GN,
           Annual.LL.days=Annual.LL.days,
           Annual.GN.days=Annual.GN.days,
           Ndays_1_tone_GN=Ndays_one.tone_GN$fishing.day.1.ton_Catch,
           Ndays_1_tone_LL=Ndays_one.tone_LL$fishing.day.1.ton_Catch[1],
           Ndays_1_tone_LL_S1=Ndays_one.tone_LL$fishing.day.1.ton_Catch[2])
write.csv(Out.pars,le.paste("Socio-economics/longline.vs.gillnet/Relative_comparison_input_pars.csv"),row.names = F)



Not.used=FALSE
if(Not.used)
{
  #1. Calculate revenue
  
  #1.1 get catch weight per trip
  trunk.ratio=1.59   #average total weight to trunk ratio from catch and effort returns
  Percent.fin.of.livewt=0.03   #used in SOFAR and by Eva Lai
  fin.to.trunk.ratio=Percent.fin.of.livewt*trunk.ratio
  
  #fin.to.trunk.ratio=0.05     #adopted by NMFS (1993) fide in Biery & Pauly 2012
  
  Retained.catch=DATA%>%filter(retainedflag=='Yes' & !is.na(boat))%>%
    dplyr::select(date,sheet_no,line_no,method,boat,taxa,species,common_name,
                  tl,fl,number,tw,a_weight,b_weight)%>%
    group_by(common_name)%>%
    mutate(tw.dummy=mean(tw,na.rm=T),
           tw=ifelse(is.na(tw),tw.dummy,tw))%>%
    ungroup()%>%
    filter(!is.na(tw))%>%
    arrange(date)%>%
    mutate(weight=ifelse(taxa=='Teleost',tw,
                         ifelse(taxa=='Elasmobranch',tw/trunk.ratio,
                                NA)),
           common_name.economics=case_when(common_name%in%c(Main.species,"fins")~common_name,
                                           !common_name%in%c(Main.species,"fins") & taxa=='Elasmobranch'~"Other sharks",
                                           !common_name%in%c(Main.species,"fins") & taxa=='Teleost'~"Other scalefish"))%>%
    arrange(date)%>%
    mutate(diff_days=difftime(date,lag(date,n=1),units="days"),
           diff_days=ifelse(is.na(diff_days),0,diff_days),
           delta_days=ifelse(diff_days>1,1,0),
           trip=1+cumsum(delta_days))
  
  #1.2 add price per species from questionnaires
  This=c("Q.4",paste('Q.13',letters[1:11],sep='.'))
  oldnames = paste('Q.13',letters[1:11],sep='.')
  newnames=c("Gummy shark","Dusky shark","Whiskery shark","Sandbar shark","fins","Other sharks",
             "Pink snapper","West Australian dhufish","Queen snapper","Blue groper","Other scalefish")
  d.f=Survey.fishers[,This]%>%
    mutate(boat=case_when(Q.4=="Tracey Lea"~"E35",
                          Q.4=="Elizabeth Maria II"~"E007",
                          Q.4=="Viking Legend"~"F399"))%>%
    rename_at(vars(oldnames), ~ newnames)
  d.f=d.f%>%
    dplyr::select(-Q.4)%>%
    gather(common_name.economics,Price.per.kg,-boat)%>%
    mutate(Price.per.kg=ifelse(is.na(Price.per.kg) & common_name.economics=='fins',16.5,
                               ifelse(is.na(Price.per.kg) & common_name.economics=='Other scalefish',5.4,
                                      Price.per.kg)))  #added manually as not provided
  
  Retained.catch=Retained.catch%>%left_join(d.f,by=c('common_name.economics','boat'))%>%
    mutate(revenue= Price.per.kg*weight)
  
  #1.3 add fins and aggregate revenue by trip
  Retained.catch.fins=Retained.catch%>%
    filter(taxa=='Elasmobranch')%>%
    group_by(boat,method,trip)%>%
    summarise(weight=sum(weight))%>%
    ungroup()%>%
    mutate(weight=weight*fin.to.trunk.ratio,
           common_name.economics='fins')%>%
    left_join(d.f,by=c('common_name.economics','boat'))%>%
    mutate(revenue= Price.per.kg*weight)%>%
    dplyr::select(-c(weight,Price.per.kg))
  
  Revenue=rbind(Retained.catch.fins,
                Retained.catch%>%
                  group_by(boat,method,trip,common_name.economics)%>%
                  summarise(revenue=sum(revenue)))%>%
    group_by(boat,method,trip)%>%
    summarise(revenue=sum(revenue))%>%
    ungroup()
  
  #2. Calculate costs
  #Knuckey et al page 8: auto longline daily costs (bait, fuel, crew, stores): 2,192
  #         they assume 200 fishing days per year

}
