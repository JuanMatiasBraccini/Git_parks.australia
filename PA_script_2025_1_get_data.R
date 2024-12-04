#Missing: Victoria lats and longs

library(tidyverse)
library(stringr)
library(Hmisc)
library(data.table)
library(ozmaps)
library(sf)

# Read in data sets ------------------------------------------------------
User="Matias"
#User="Agustin"
if(!exists('handl_OneDrive'))
{
  if(User=="Matias")
  {
    source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')
  }
  if(User=="Agustin")
  {
    handl_OneDrive=function(x)paste('your path',x,sep='/')
  }
}
  

paz=handl_OneDrive('Parks Australia/2025_project/Data/Data sets')
dirs <- list.dirs(path=paz, full.names = FALSE, recursive = FALSE)

fn.create.list=function(vec) sapply(vec,function(x) NULL)  
Data.set=fn.create.list(dirs)
dodgy.ones=c("Braccini sharks EM extract.csv","Braccini sharks wildlife interaction extract.csv")
for(d in 1:length(Data.set))
{
  files <- list.files(path=paste(paz,dirs[d],sep='/'), pattern=".csv")
  dumi=fn.create.list(files)
  names(dumi)=str_remove(names(dumi),'.csv')
  for(i in 1:length(dumi))
  {
    if(files[i]%in%dodgy.ones)
    {
      dumi[[i]]=read.csv(paste(paz,dirs[d],files[i],sep='/'))
    }else
    {
      dumi[[i]]=fread(paste(paz,dirs[d],files[i],sep='/'),data.table=FALSE,fill=TRUE) 
    }
      
  }
  Data.set[[d]]=dumi
  rm(dumi)
  print(c('Bring in data for -------------',names(Data.set)[d]))
}


# Manipulate data sets ------------------------------------------------------
#add lats and longs to data sets reporting only grid
for(d in 1:length(Data.set))
{
  if(names(Data.set)[d]=="NSW")
  {
    dummy1=Data.set[[d]]$`Sharks and Rays 16-10-2024`%>%rename(Grid=GridCode)
    dummy2=Data.set[[d]]$`Grid 1 degree lat NW corner`
    dummy=dummy1%>%
            left_join(dummy2%>%dplyr::select(Grid,Latitude,Longitude),
                      by='Grid')
    Data.set[[d]]=list(NSW=dummy)
  }
  if(names(Data.set)[d]=="QLD")
  {
    dummy1=Data.set[[d]]$Grid
    dummy2=Data.set[[d]]$Grids_LatLongs%>%
      rename(Latitude=CentreLatitude,
             Longitude=CentreLongitude)
    dummy=dummy1%>%
      left_join(dummy2%>%dplyr::select(Grid,Latitude,Longitude),
                by='Grid')
    Data.set[[d]]=list(QLD=dummy)
    
  }
  if(names(Data.set)[d]=="SA")
  {
    dummy1=Data.set[[d]]$`8384-2324 MSF SHARK SPECIES`%>%
      mutate(Area=case_when(Area=="8"~if_else(runif(n()) <.5, "8A","8B"),    
                            Area=="10"~if_else(runif(n()) <.5, "10A","10B"),
                            Area=="40"~if_else(runif(n()) <.5, "40A","40B"),
                            Area=="44"~if_else(runif(n()) <.5, "44A","44B"),
                            TRUE~Area))
    dummy1.effort=Data.set[[d]]$`83484-2324 MSF BDAYS`
    dummy1=dummy1%>%left_join(dummy1.effort,by=c('Year','Month','Gear','Area'))
    
    dummy2=Data.set[[d]]$Area_lat_long%>%rename(Area=Block)
    
    
    dummy=dummy1%>%
      left_join(dummy2%>%dplyr::select(Area,Latitude,Longitude),
                by='Area')
    Data.set[[d]]=list(SA=dummy)
    
  }
  if(names(Data.set)[d]=="Vic")
  {
    dummy1=Data.set[[d]]$Vic_shark_data_missing_zero_catch%>%   
                mutate(Species=ifelse(nchar(CatchKg)>5,'Shark',
                                      ifelse(CatchKg%in%c('gummy'),'Shark',
                                             Species)),
                       SpeciesCode=ifelse(nchar(CatchKg)>5,CatchKg,
                                   ifelse(CatchKg%in%c('gummy'),'gummy',
                                   ifelse(Species=='Rays' & SpeciesCode=='685','Rays',
                                          SpeciesCode))),
                       CatchKg=ifelse(nchar(CatchKg)>5,NA,
                                    ifelse(CatchKg%in%c('gummy'),NA,
                                           CatchKg)),
                       CatchKg=as.numeric(CatchKg),
                       Species=capitalize(tolower(paste(SpeciesCode,Species))))%>%
                mutate(Species=ifelse(Species%in%c('Rays rays','Rays na'),'Rays',
                               ifelse(Species=='Southern eagle shark','Southern eagle ray',
                                      Species)))%>%
                filter(!Species%in%c('Shark na','Na na','Other(unspecified) shark','Rays',
                                     'Other skates and rays','Skates and rays na',
                                     'Skates and rays shark','Unspecified stingaree',
                                     'School and gummy shark'))%>%
                group_by(FiscalYear,Species,Gridset,AreaCode)%>%
                summarise(CatchKg=sum(CatchKg,na.rm=T))%>%
                ungroup()%>%
                data.frame()
      
    dummy2=dummy1%>%                            #Missing, add real Lat and Long (ask Justin, some Gridset shape files missing)
              distinct(Gridset,AreaCode,.keep_all = FALSE)%>%      
              mutate(Latitude=sample(seq(-40.5,-39,by=0.1),n(),replace=T),
                     Longitude=sample(seq(142,148.5,by=0.1),n(),replace=T))%>%
              distinct(Gridset,AreaCode,.keep_all = T)
      dummy=dummy1%>%
              left_join(dummy2%>%dplyr::select(Gridset,AreaCode,Latitude,Longitude),by=c('Gridset','AreaCode'))
    Data.set[[d]]=list(Vic=dummy)
  }
  if(names(Data.set)[d]=="NT")
  {
    dummy=Data.set[[d]]$logbook_shark_location%>%
                            rename(Latitude=E_Decimal_Lat,
                                   Longitude=E_Decimal_Long,
                                   Species=C_STANDARD_NAME)
    Data.set[[d]]=list(NT=dummy)
  }
  print(c('Add lat long for --------------',names(Data.set)[d]))
}

#reset colnames
for(d in 1:length(Data.set))
{
  if(class(Data.set[[d]])=='list')
  {
    for(i in 1:length(Data.set[[d]]))
    {
      colnames(Data.set[[d]][[i]])=capitalize(tolower(colnames(Data.set[[d]][[i]])))
    }
  }
  print(c('Reset colnames for --------------',names(Data.set)[d]))
}

#tweak some variable names
for(d in 1:length(Data.set))
{
  if(names(Data.set)[d]=="AFMA")
  {
    for(i in 1:length(Data.set[[d]]))
    {
      Data.set[[d]][[i]]=Data.set[[d]][[i]]%>%
        rename_with(tolower)%>% rename_with(capitalize)%>%
        rename_at(vars(matches(paste(c("Fshy","Fishery.name"),collapse='|'))), ~ 'Fishery')%>%
        rename_at(vars(matches(paste(c("Spc_name","Scientific.name"),collapse='|'))), ~ 'Scientific.name')%>%
        rename_at(vars(matches("gear")), ~ 'Gear')%>%
        rename_at(vars(matches("year")), ~ 'Year')%>%
        rename_at(vars(matches("lat")), ~ 'Latitude')%>%
        rename_at(vars(matches("long")), ~ 'Longitude')%>%
        rename_at(vars(matches("Standard.name")), ~ 'Species')%>%
        mutate(Longitude=abs(Longitude),
               Latitude=-abs(Latitude))
    }
  }
  print(c('Tweak  variables for --------------',names(Data.set)[d]))
}

#add Fishery and Species
for(d in 1:length(Data.set))
{
  for(i in 1:length(Data.set[[d]]))
  {
    if(!"Fishery"%in%colnames(Data.set[[d]][[i]]))
    {
      if(names(Data.set)[d]=="MAFFRI") Data.set[[d]][[i]]$Fishery='SESSF'
      if(names(Data.set)[d]=="QLD") Data.set[[d]][[i]]$Fishery='QLD'
      if(names(Data.set)[d]=="SA") Data.set[[d]][[i]]$Fishery='MSF'
      if(names(Data.set)[d]=="Vic") Data.set[[d]][[i]]$Fishery='Vic'
      if(names(Data.set)[d]=="WA") Data.set[[d]][[i]]$Fishery=Data.set[[d]][[i]]$Method
      if(names(Data.set)[d]=="NT") Data.set[[d]][[i]]$Fishery='NT'
    }
    
    if(!"Species"%in%colnames(Data.set[[d]][[i]]))
    {
      if(names(Data.set)[d]=="AFMA") Data.set[[d]][[i]]$Species=Data.set[[d]][[i]]$Std_name
      if(names(Data.set)[d]=="NSW") Data.set[[d]][[i]]$Species=Data.set[[d]][[i]]$Commonname
      if(names(Data.set)[d]=="SA") Data.set[[d]][[i]]$Species=Data.set[[d]][[i]]$`Species name`
      if(names(Data.set[[d]])[i]=="Observers") Data.set[[d]][[i]]$Species=Data.set[[d]][[i]]$Common_name
    }
    
    Data.set[[d]][[i]]=Data.set[[d]][[i]]%>%mutate(Species=capitalize(tolower(Species)))
  }
  print(c('Add Fishery and Species for --------------',names(Data.set)[d]))
}



# Display data sets ------------------------------------------------------
hndl.out=handl_OneDrive('Analyses/Parks Australia/outputs/2025 Project/')
  
#locations per data set
for(d in 1:length(Data.set))
{
  for(i in 1:length(Data.set[[d]]))
  {
    Data.set[[d]][[i]]%>%
      mutate(Latitude=round(Latitude),
             Longitude=round(Longitude))%>%
      distinct(Fishery,Latitude, Longitude)%>%
      ggplot(aes(Longitude,Latitude))+
      geom_point(shape = 19)+
      facet_wrap(~Fishery)+
      theme(strip.text = element_text(size = 7))
    ggsave(paste0(hndl.out,'Display data sets/plot1_',names(Data.set)[d],'_',names(Data.set[[d]])[i],'.tiff'),
           width = 6,height = 6,compression = "lzw")
    
  }
}

#number of individuals per species
for(d in 1:length(Data.set))
{
  for(i in 1:length(Data.set[[d]]))
  {
    n=length(unique(Data.set[[d]][[i]]$Species))
    NN=10
    if(n>60) NN=5
    Data.set[[d]][[i]]%>%
      group_by(Species)%>%
      tally()%>%
      ggplot(aes(Species,n))+
      geom_bar(stat='identity')+
      theme(axis.text.y=element_text(size = NN),
            strip.text = element_text(size = 7))+
      coord_flip()+
      scale_y_log10()+ylab('Log scale')+xlab('')
    ggsave(paste0(hndl.out,'Display data sets/plot2_',names(Data.set)[d],'_',names(Data.set[[d]])[i],'.tiff'),
           width = 6,height = 7,compression = "lzw")
    
  }
}

#Parks Australia milestone report November 2024
dumi1=fn.create.list(names(Data.set))
for(d in 1:length(Data.set))
{
  dumi=fn.create.list(names(Data.set[[d]]))
  for(i in 1:length(dumi))
  {
    dumi[[i]]=Data.set[[d]][[i]]%>%
      dplyr::select(Species,Latitude,Longitude)%>%
      mutate(Data.set=names(Data.set)[d])
  }
  dumi1[[d]]=do.call(rbind,dumi)
}
dumi1=do.call(rbind,dumi1)

#map
sf_oz <- ozmap_data("states")
ggplot(sf_oz) + 
  geom_point(data=dumi1%>%
               mutate(Latitude=round(Latitude),
                      Longitude=round(Longitude))%>%
               distinct(Data.set,Latitude, Longitude),
             aes(Longitude,Latitude),
             shape = 19,size=1.5)+
  geom_sf(fill = "darkorange2")+
  facet_wrap(~Data.set)+
  theme(strip.text = element_text(size = 12))+
  xlim(110,157)+ylim(-46,-10)
ggsave(paste0(hndl.out,'Display data sets/Parks_milestone1_Nov2024_map.tiff'),
       width = 7,height = 7,compression = "lzw")

#tables
write.csv(dumi1%>%group_by(Data.set)%>%tally(),
          paste0(hndl.out,'Display data sets/Parks_milestone1_Nov2024_Table_records.csv'),row.names = F) 

A=dumi1%>%group_by(Data.set,Species)%>%tally()%>%ungroup()%>%mutate(N=1)%>%group_by(Data.set)%>%tally()
write.csv(A,
          paste0(hndl.out,'Display data sets/Parks_milestone1_Nov2024_Table_species.csv'),row.names = F) 

