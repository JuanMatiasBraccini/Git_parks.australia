library(tidyverse)
library(stringr)
library(Hmisc)
library(data.table)

# Read in data sets ------------------------------------------------------
User="Matias"
if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')

paz=handl_OneDrive('Parks Australia/2025_project/Data/Data sets')
dirs <- list.dirs(path=paz, full.names = FALSE, recursive = FALSE)

Data.set=vector('list',length(dirs))
names(Data.set)=dirs
dodgy.ones=c("Braccini sharks EM extract.csv","Braccini sharks wildlife interaction extract.csv")
for(d in 1:length(Data.set))
{
  files <- list.files(path=paste(paz,dirs[d],sep='/'), pattern=".csv")
  dumi=vector('list',length(files))
  names(dumi)=str_remove(files,'.csv')
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
    Data.set[[d]]=dummy
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
    Data.set[[d]]=dummy
    
  }
  if(names(Data.set)[d]=="SA")
  {
    dummy1=Data.set[[d]]$`8384-2324 MSF SHARK SPECIES`%>%
      mutate(Area=case_when(Area=="8"~sample(c("8A","8B"),1),    #ACA this will only sample 1 value, make it work. Also Do Victoria
                            Area=="10"~sample(c("10A","10B"),1),
                            Area=="40"~sample(c("40A","40B","40C"),1),
                            Area=="44"~sample(c("44A","44B"),1),
                            TRUE~Area))
    dummy1.effort=Data.set[[d]]$`83484-2324 MSF BDAYS`
    dummy1=dummy1%>%left_join(dummy1.effort,by=c('Year','Month','Gear','Area'))
    
    dummy2=Data.set[[d]]$Area_lat_long%>%rename(Area=Block)
    
    
    dummy=dummy1%>%
      left_join(dummy2%>%dplyr::select(Area,Latitude,Longitude),
                by='Area')
    Data.set[[d]]=dummy
    
  }
  
  if(names(Data.set)[d]=="Vic")
  {
    dummy=Data.set[[d]]$Vic_shark_data_missing_zero_catch%>%   #aca, aggregate by year and grid, add grid lat long
                mutate(CatchKg=ifelse(nchar(CatchKg)>5,NA,
                                    ifelse(CatchKg%in%c('gummy'),NA,
                                           CatchKg)),
                       CatchKg=as.numeric(CatchKg))%>%
                group_by(FiscalYear,SpeciesCode)%>%
                summarise(CatchKg=sum(CatchKg,na.rm=T))
    
    Data.set[[d]]=dummy
  }
}

#tweak some variable names
for(d in 1:length(Data.set))
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
                          mutate(Longitude=abs(Longitude),
                                 Latitude=-abs(Latitude))
  }
}

# Plot data sets ------------------------------------------------------
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
    ggsave(handl_OneDrive(paste0('Parks Australia/2025_project/Data/plot1_',names(Data.set)[d],
                                 '_',names(Data.set[[d]])[i],'.tiff')),width = 6,height = 6,compression = "lzw")
    
  }
}

#number of species
for(d in 1:length(Data.set))
{
  for(i in 1:length(Data.set[[d]]))
  {
    n=length(unique(Data.set[[d]][[i]]$Scientific.name))
    NN=10
    if(n>60) NN=5
    Data.set[[d]][[i]]%>%
      group_by(Scientific.name)%>%
      tally()%>%
      ggplot(aes(Scientific.name,n))+
      geom_bar(stat='identity')+
      theme(axis.text.y=element_text(size = NN),
            strip.text = element_text(size = 7))+
      coord_flip()+
      scale_y_log10()+ylab('Log scale')+xlab('')
    ggsave(handl_OneDrive(paste0('Parks Australia/2025_project/Data/plot2_',names(Data.set)[d],
                                 '_',names(Data.set[[d]])[i],'.tiff')),width = 6,height = 6,compression = "lzw")
    
  }
}

