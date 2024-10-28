library(tidyverse)
library(rlang)
library(MASS)
library(janitor)
library(lazyeval)

options(stringsAsFactors = FALSE,dplyr.summarise.inform = FALSE) 

source("data_cleaning_functions.R")
source("data_cleaning_constants.R")

#########################Ref sheet for CAAB code matching
{
hndl.Sarah='M:\\Production Databases\\Shark\\ParksAustralia_2019'
setwd(hndl.Sarah)
ref <- read.delim("CodeMatchingPA19.txt", sep = "\t") %>% 
  unite(taxa, GENUS, SPECIES, sep = " ", remove = FALSE, na.rm = TRUE) %>% 
  mutate(
    refCode = as.integer(CAAB.CODE),
    taxa = str_trim(taxa, side = "left")) %>% 
  dplyr::select(taxa, refCode)
Event.Mes.data.dump <- 'Abbey_Sarah'
}

#### NOTE: CAAB codes are assigned based on MatchCAABFUN() which does a left_join() with a reference .CSV
#### unknown fish are given dummy CAAB code: 99999999

###########################-------------Underwater----------------###################################
if(Event.Mes.data.dump=='Abbey_Sarah')
{
  #1. read in  data
  #1.1. gillnet
  setwd(paste0(hndl.Sarah,'/EMOutputs/Gillnet'))
  filenames <- list.files(pattern = '*.csv')
  dummy.GN <- lapply(filenames, read.csv, skip = 4)
  #1.2. longline
  setwd(paste0(hndl.Sarah,'/EMOutputs/Longline'))
  filenames <- list.files(pattern = '*.csv')
  dummy.LL <- lapply(filenames, read.csv, skip = 4)
  
  #2. put data in standard format
  #2.1. gillnet
  Video.net.interaction = Video.net.maxN = Video.net.obs = vector('list', length(dummy.GN))
  
  for (i in 1:length(dummy.GN))
  {
    dummy.GN[[i]] <- RenameColumn(dummy.GN[[i]])
    if (!'Position' %in% names(dummy.GN[[i]]))
      dummy.GN[[i]]$Position = NA
    Video.net.interaction[[i]] <- dummy.GN[[i]] %>%
      OrigUW() %>% 
      filter(is.na(MaxN)) %>%
      CategoriseComment() %>%
      CategoriseEscape() %>% 
      dplyr::select(all_of(interaction.names)) %>%
      mutate(
        Number = ifelse(Number == 'AD', NA, Number),
        No.haul = Alt.species == "no haul",
        No.fish = Alt.species == "no fish",
        Method = "Gillnet",
        Interaction = AssignInteractions(Interaction)
      ) %>% 
      ASL(Alt.species) %>% 
      Humpback(Alt.species) %>% 
      filter(
        !Alt.species %in% c(
          "squid",
          "unknown fish",
          "cuttlefish",
          "baitfish",
          "sea hare",
          "commorant",
          "crab",
          "seven legged starfish"
        ))
    
    Video.net.maxN[[i]] <- dummy.GN[[i]] %>%
      RenameColumn() %>%
      OrigUW() %>% 
      CategoriseComment() %>%
      dplyr::select(all_of(maxn.names)) %>%
      mutate(
        Species=ApplySpecies(Species, Alt.species),
        Method = "Gillnet",
      )
    
    Video.net.obs[[i]] = dummy.GN[[i]] %>%
      RenameColumn() %>%
      OrigUW() %>% 
      CategoriseComment() %>%
      mutate(
        observation = Alt.species,
        No.haul = Alt.species == "no haul",
        No.fish = Alt.species == "no fish",
        code = Code,
        Method = "Gillnet",
        Species=ApplySpecies(Species, Alt.species)
      ) %>%
      dplyr::select(all_of(observation.names))
  }
  
  Video.net.interaction <- do.call(rbind, Video.net.interaction) %>%
    MatchCAABFUN() %>% 
    filter(!Species %in% c("", " ") &
             No.haul == FALSE & No.fish == FALSE)
  Video.net.maxN = do.call(rbind, Video.net.maxN) %>%
    MatchCAABFUN() %>% 
    filter(!is.na(MaxN))
  Video.net.obs = do.call(rbind, Video.net.obs) %>%
    MatchCAABFUN() %>% 
    filter(!observation == '') %>% 
    filter(!Species %in% c("sea lion", "Whale")) %>% 
    mutate(code = Code)
  
  #2.2. longline
  Video.longline.interaction = Video.longline.maxN = Video.longline.obs =
    vector('list', length(dummy.LL))
  for (i in 1:length(dummy.LL))
  {
    dummy.LL[[i]] <- RenameColumn(dummy.LL[[i]])
    if (!'Position' %in% names(dummy.LL[[i]]))
      dummy.LL[[i]]$Position = NA
    Video.longline.interaction[[i]] <-  dummy.LL[[i]] %>%
      OrigUW() %>% 
      filter(is.na(MaxN)) %>%
      CategoriseComment() %>%
      CategoriseEscape() %>% 
      dplyr::select(all_of(interaction.names)) %>%
      mutate(
        Number = ifelse(Number == 'AD', NA, Number),
        No.haul = Alt.species == "no haul",
        No.fish = Alt.species == "no fish",
        Species=ApplySpecies(Species, Alt.species),
        Method = "Longline",
        Interaction = AssignInteractions(Interaction)
      ) %>% 
      Turtle(Alt.species) %>% 
      filter(
        !Alt.species %in% c(
          "squid",
          "Octopus",
          "cuttlefish",
          "bird",
          "snail",
          "unknown fish",
          "baitfish"
        ))
    
    Video.longline.maxN[[i]] = dummy.LL[[i]] %>%
      RenameColumn() %>%
      OrigUW() %>% 
      CategoriseComment() %>%
      dplyr::select(all_of(maxn.names)) %>%
      mutate(
        Method = "Longline",
        Species=ApplySpecies(Species, Alt.species)
      )
    
    Video.longline.obs[[i]] = dummy.LL[[i]] %>%
      RenameColumn() %>%
      OrigUW() %>% 
      CategoriseComment() %>%
      mutate(
        observation = Alt.species,
        No.haul = Alt.species == "no haul",
        No.fish = Alt.species == "no fish",
        code = Code,
        Method = "Longline",
        Species=ApplySpecies(Species, Alt.species)
      ) %>%
      filter(!Species == "turtle") %>% 
      dplyr::select(all_of(observation.names))
  }
  Video.longline.interaction = do.call(rbind, Video.longline.interaction) %>%
    MatchCAABFUN() %>%
    filter(!Species %in% c(" ", "") &
             No.haul == FALSE & No.fish == FALSE)
  Video.longline.maxN = do.call(rbind, Video.longline.maxN) %>%
    MatchCAABFUN() %>%
    filter(!is.na(MaxN)) %>%
    rename(Max.N = MaxN)
  Video.longline.obs = do.call(rbind, Video.longline.obs) %>%
    MatchCAABFUN() %>%
    filter(!observation == '') %>%
    rename(Observation = observation,
           optcode = OpCode) %>% 
    mutate(code = Code)
  
  
}

###########################-------------Deck2----------------###################################
setwd(paste0(hndl.Sarah,'/EMOutputs/Deck2'))

# Read in data
dummy.d2 <- list()  
deck2filenames <- dir(pattern="*.csv")

for (i in 1:length(deck2filenames))
{
  dummy.d2[[i]] <- read.csv(deck2filenames[i], skip=4)
}
# Clean with User Defined functions
Deck.2.fish <-  Deck.2.obs <-  vector('list', length(dummy.d2))
for (i in 1:length(dummy.d2))
{
  Deck.2.fish[[i]] <- dummy.d2[[i]] %>% 
    DeckTwoColumns() %>% 
    OrigD2() %>% 
    RemoveWhitespace(hooklocation, hookloc.and.comments) %>% 
    rename(`hook distance to float/weight`=hooklocation) %>% 
    HookLocation() %>% 
    CategoriseGaffed() %>%
    CategoriseDropout() %>% 
    separate("Curtin opcode", c("Region", "DPIRD code", "Position"), sep="_", remove=FALSE) %>% 
    CategoriseRegion() %>% 
    CategorisePeriod(Period) %>% 
    mutate(Position = "Deck#2",
           Species=ApplySpecies(Species, Alt.species),
           comment = as.character(Alt.species),
           Activity = as.character(NA),
           Stage = as.character(NA),
           Number = as.integer(NA)) %>%
    ASL(Alt.species) %>% 
    dplyr::select(all_of(deck.2.fish.names)) 
  
  Deck.2.obs[[i]] <- Deck.2.fish[[i]] %>% 
    mutate(
      comment = case_when(!Alt.species == "" ~ as.character(Alt.species),
                          original.hooklocation %in% deck.2.observations ~ as.character(original.hooklocation),
                          TRUE ~ as.character(NA)),
      Activity = as.character("Passing"),
      Stage = as.character("AD"),
      Number = as.integer(1)) %>% 
    filter(!is.na(comment)) %>% 
    filter(!comment == "sea lion") %>% 
  dplyr::select(all_of(deck.2.observations.names))
  
  }

Video.camera2.deck <- do.call(rbind, Deck.2.fish) %>% 
  filter(!Alt.species %in% c(
    "bird",
    "sea hare",
    "crab",
    "crayfish",
    "unknown fish",
    "cuttlefish")) %>%
  MatchCAABFUN()
Video.camera2.deck_observations <- do.call(rbind, Deck.2.obs) %>%
  MatchCAABFUN()

###########################-------------Deck1----------------###################################
setwd(paste0(hndl.Sarah,'/EMOutputs/Deck1'))

# Read in data
dummy.d1 <- list()  
deck1filenames <- dir(pattern="*.csv")
for (i in 1:length(deck1filenames))
{
  dummy.d1[[i]] <- read.csv(deck1filenames[i], skip=4)
}
Deck.1.fish <-  Deck.1.habitat <- Deck.1.obs <-  vector('list', length(dummy.d1))
for (i in 1:length(dummy.d1))
{
  Deck.1.fish[[i]] <- dummy.d1[[i]] %>%
    DeckOneColumns() %>%
    separate("curtin opcode", c("Region", "DIPRD code", "Position"), sep="_", remove=FALSE) %>%
    OrigD1() %>% 
    CategoriseRegion() %>% 
    CategoriseCondition(original.condition, condition) %>% 
    CategoriseRetained() %>% 
    CategoriseMeshed() %>% 
    mutate(Position = "Deck#1",
           Species=ApplySpecies(Species, Alt.species),
           number = as.integer(1)) %>%
    filter(is.na(`Percentage cover`)) %>%
    CategorisePeriod(Period) %>% 
    dplyr::select(all_of(deck.1.fish.names))
  
  Deck.1.habitat[[i]] <- dummy.d1[[i]] %>%
    DeckOneColumns() %>%
    separate("curtin opcode", c("Region", "DIPRD code", "Position"), sep="_", remove=FALSE) %>%
    CategoriseRegion() %>% 
    mutate(
      `Curtin opcode` = `curtin opcode`
    ) %>% 
    mutate(Position = "Deck#1") %>%
    CategorisePeriod(Period) %>% 
    CategoriseMeshed() %>% 
    filter(!is.na(`Percentage cover`)) %>%
    filter(!Period == "Longline") %>% 
    dplyr::select(all_of(deck.1.habitat.names))
  
  Deck.1.obs[[i]] <- Deck.1.fish[[i]] %>%
    mutate(
      number = as.integer(1),
      meshed = case_when(!is.na(Alt.species) ~ as.character(Alt.species),
                         Alt.species == "" ~ as.character(NA),
                          original.meshed %in% deck.1.observations ~ as.character(original.meshed),
                          TRUE ~ as.character(NA))) %>%
    filter(!meshed =="") %>% 
    unite(`RegionDIPRD codePosition`, remove = FALSE) %>% 
    dplyr::select(all_of(deck.1.observations.names))
}


Video.camera1.deck <- do.call(rbind, Deck.1.fish) %>% 
  filter(Alt.species == "") %>% 
  MatchCAABFUN()
Video.habitat.deck <- do.call(rbind, Deck.1.habitat)
Video.camera1.deck_extra.records <- do.call(rbind, Deck.1.obs) %>% 
  MatchCAABFUN()

###########################-------------Subsurface----------------###################################
setwd(paste0(hndl.Sarah,'/EMOutputs/Subsurface'))
# Read in data
dummy.ss <- list()  
ssfilenames <- dir(pattern="*.csv")
for (i in 1:length(ssfilenames))
{
  dummy.ss[[i]] <- read.csv(ssfilenames[i], skip=4)
}
SS.fish <- SS.obs <-  vector('list', length(dummy.ss))
for (i in 1:length(dummy.ss)){
  SS.fish[[i]] <- dummy.ss[[i]] %>%
    SSColumns() %>% 
    mutate(Region = str_extract(`DPIRD code`, "[^_]+"))%>%
    OrigSS() %>% 
    CategoriseSSDropout() %>% 
    CategoriseSSGaffed() %>% 
    CategoriseCondition(original.condition, `Dropout condition`) %>%
    mutate(Species=ApplySpecies(Species, Alt.species)) %>%
    separate(`DPIRD code`, into = str_c("meta", 1:4), sep="_", remove = FALSE) %>%
    mutate(
      `DPIRD code` = ifelse(meta2 == "bay", as.character(meta3), as.character(meta2))) %>%
    ASL(Alt.species) %>%
    dplyr::select(all_of(subsurface.names))
  
  SS.obs[[i]] <- SS.fish[[i]] %>%
    mutate(
      comment = case_when(!Alt.species == "" ~ as.character(Alt.species),
                          original.gaffed %in% subsurface.observations ~ as.character(original.gaffed),
                          TRUE ~ as.character(NA)),
      interaction = as.character(Interaction)) %>% 
    unite(OpCode, c("Region", "DPIRD code", "Position"), sep = "_", remove = FALSE) %>% 
    filter(!is.na(comment)) %>% 
    dplyr::select(all_of(subsurface.observations.names))
    
}
print("NOTE: There were 35 warnings THIS IS FINE AND EXPECTED")
Video.subsurface <- do.call(rbind, SS.fish) %>%
  filter(!Alt.species %in% c(
    "unknown fish",
    "bird",
    "baitfish")) %>%
  MatchCAABFUN()
Video.subsurface.comments <- do.call(rbind, SS.obs) %>% 
  MatchCAABFUN()

