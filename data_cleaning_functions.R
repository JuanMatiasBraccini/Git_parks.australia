# Functions used in data_cleaning_PA.Rmd
### fucntion names are defined LikeThis

####################################Underwater#####################################
{
# Assign Interactions Function
AssignInteractions <- function(numerical.interaction){
  
  case_when(numerical.interaction==1 ~'Swim Past',
            numerical.interaction==2 ~'Swim Through',
            numerical.interaction==3 ~'Attracted',
            numerical.interaction==4 ~'Bounce Off',
            numerical.interaction==5 ~'Avoid',
            numerical.interaction==6 ~'Caught-Gilled/hooked',
            numerical.interaction==7 ~'Caught-bagged',
            numerical.interaction==8 ~'Escape',
            numerical.interaction==9 ~'Feeding',
            numerical.interaction==10 ~'Predated on',
            numerical.interaction==11 ~'Bait feeding',
            numerical.interaction==12 ~'Caught while predating',
            TRUE~"Missing data")
}


# Rename Column Function (underwater)
RenameColumn <- function(df) {
  new.names = case_when(
    str_detect(names(df),"(?i)escape|esape") ~"Escape",
    str_detect(names(df),"(?i)max") ~"MaxN",
    str_detect(names(df),"(?i)interact") ~ "Interaction",
    str_detect(names(df),"(?i)method") ~ "Method",
    names(df)=="Time..mins."~"Time (mins)",
    names(df)=="Period.time..mins."~"Period time (mins)",
    TRUE~as.character(names(df))
  )
  old.names <- names(df)
  names(df)[names(df)==old.names] <- new.names
  
  return(df)
}

# Duplicate cols that will be mutated
OrigUW <- function(df){
  ret <- df %>% mutate(
    original.escape = Escape,
    original.maxn = MaxN,
    original.interaction = Interaction
  )
  return(ret)
}
# Categorise Comment Function (Underwater)
## Using regex notation to capture variation in comments
CategoriseComment <- function(df) {
  ret <- df %>% mutate(
    Alt.species = case_when(
      str_detect(Escape, "(?i)startfish") ~ "seven legged starfish",
      str_detect(Escape, "(?i)squid") ~ "squid",
      str_detect(Escape, "(?i)cuttle") ~ "cuttlefish",
      str_detect(Escape, "(?i)unidentifiable|unknown|UNKNONW|unsure") ~ "unknown fish",
      str_detect(Escape, "(?i)seal") ~ "sea lion",
      str_detect(Escape, "(?i)bird|sear") ~ "bird",
      str_detect(Escape, "(?i)bait|^school") ~ "baitfish",
      str_detect(Escape, "(?i)commernat|comorant") ~ "commorant",
      str_detect(Escape, "(?i)garnard") ~ "gurnard",
      str_detect(Escape, "Aplysia punctata") ~ "sea hare",
      str_detect(Escape, "(?i)haul|dark|stops") ~ "no haul",
      str_detect(Escape, "(?i)no fish") ~ "no fish",
      str_detect(Escape, "^\\d|\\<|reef") ~ "",
      is.na(Escape) ~ "",
      Escape %in% drop2 ~ '',
      TRUE ~ as.character(Escape)))
  return(ret)
}

# Apply species
ApplySpecies <- function(common.species, alternative.species){
  Species = case_when(
    common.species == "" ~ as.character(alternative.species),
    is.na(common.species) ~ as.character(alternative.species),
    TRUE ~ as.character(common.species)
  )
  return(Species)
}

# Categorise escape
CategoriseEscape <- function(df){
  ret <- df %>% mutate(
    Escape = as.numeric(str_extract(original.escape, "(0|[1-9]\\d*)(\\.\\d+)?"))
  )
}

}


##################################Deck2###########################################
{
# Rename Deck Column
## Using regex notation to capture variation in column names
DeckTwoColumns <- function(df) {
  new.names =
    case_when(
      str_detect(names(df), "(?i)^prox|^dist|^mesh|^near|^hook")~"hooklocation",
      str_detect(names(df), "(?i)^drop")~"dropout",
      str_detect(names(df), "(?i)^gaff")~"gaffed",
      names(df)=="OpCode"~"Curtin opcode",
      str_detect(names(df), "Period.time.")~"Period time (mins)",
      TRUE~as.character(names(df)))
  old.names <- names(df)
  names(df)[names(df)==old.names] <- new.names
  
  return(df)
}

## duplicate original cols for reference
OrigD2 <- function(df){
  ret <- df %>% mutate(
    original.hooklocation = hooklocation,
    original.dropout = dropout,
    original.gaffed = gaffed
  )
  return(ret)
}

## Remove Whitespace
## input dataframe, col in which you want to rm whitespace, new col name
RemoveWhitespace <- function(df, varnam, outname) {
  passvar <- rlang::enquo(varnam)
  df <- df %>%
    dplyr::mutate(fnoutput = gsub("\\s+", "",!!passvar)) %>%
    rename({{outname}} := fnoutput)
  return(df)
}

# Categorise Hook Location
## Using regex notation to capture variation in comments
HookLocation <- function(df) {
  ret <- df %>% mutate(
    `hook distance to float/weight` = case_when(
      hookloc.and.comments %in% deck.2.observations~ as.character(NA),
      hookloc.and.comments==""~ as.character(NA),
      is.na(hookloc.and.comments)~ as.character(NA),
      str_detect(hookloc.and.comments, "(?i)^1w") ~ "1w",
      str_detect(hookloc.and.comments, "(?i)^2w") ~ "2w",
      str_detect(hookloc.and.comments, "(?i)^3w") ~ "3w",
      str_detect(hookloc.and.comments, "(?i)^4w") ~ as.character(NA),
      str_detect(hookloc.and.comments, "(?i)^1f") ~ "1f",
      str_detect(hookloc.and.comments, "(?i)^2f") ~ "2f",
      str_detect(hookloc.and.comments, "(?i)^3f") ~ "3f",
      str_detect(hookloc.and.comments, "(?i)^4f") ~ as.character(NA),
      str_detect(hookloc.and.comments, "(?i)^bag|gil") ~ as.character(NA),
      TRUE ~ "ERROR"
    )
  )
  return(ret)
}

# Categorise Gaffed
CategoriseGaffed <- function(df) {
  df %>% mutate(binary.gaffed = ifelse(str_detect(gaffed, "(?i)^y"), "yes", "no"),
                gaffed = binary.gaffed)
}
# Categorise Dropout
CategoriseDropout <- function(df) {
  ret <- df %>% mutate(Alt.species = case_when(
    str_detect(dropout, "(?i)bird|shear") ~ "bird",
    str_detect(dropout, "(?i)cuttle") ~ "cuttlefish",
    str_detect(dropout, "(?i)cray") ~ "crayfish",
    str_detect(dropout, "(?i)unknown") ~ "unknown fish",
    str_detect(dropout, "(?i)cuttle") ~ "cuttlefish",
    str_detect(dropout, "sea hare") ~ "sea hare",
    str_detect(hookloc.and.comments, "seahare") ~ "sea hare",
    str_detect(dropout, "^sealion|^Sea Lion") ~ "sea lion",
    str_detect(dropout, "^blue manner") ~ "crab",
    TRUE ~ ""),
    depredated=case_when(dropout == "depredated" ~ TRUE,
                         hookloc.and.comments =="depredated" ~ TRUE,
                         TRUE ~ FALSE),
    dropout = case_when(
      str_detect(dropout, "(?i)^y|Sea Lion y") ~ "Yes",
      is.na(dropout) ~ "No",
      TRUE ~ "No"
  ))
  return(ret)
}

}


##################################Deck1##########################################
{
# Rename Deck 1 Column
## Using regex notation to capture variation in column names
DeckOneColumns <- function(df) {
  new.names =
    case_when(
      str_detect(names(df), "(?i)^condition|^alive")~"condition",
      str_detect(names(df), "(?i)^reta")~"retained",
      str_detect(names(df), "(?i)^bag|^mesh|cover")~"meshed",
      names(df)=="OpCode"~"curtin opcode",
      names(df)=="Depth"~"Depth (M)",
      names(df)=="Number"~"number",
      str_detect(names(df), "Period.time.")~"Period time (mins)",
      TRUE~as.character(names(df)))
  old.names <- names(df)
  names(df)[names(df)==old.names] <- new.names
  
  return(df)
}

# Define percent cover
## Make a new col named percent cover that includes all numerical values in the meshed col

# Duplicate cols to keep original cols for reference
OrigD1 <- function(df){
  ret <- df %>% mutate(
    original.meshed = meshed,
    original.condition = condition,
    original.retained = retained
  )
  return(ret)
}

# Define condition
## ^a=alive, ^y=alive
## ^d=dead, ^n=dead
#input dataframe, variable name, and desired output variable name
CategoriseCondition <- function(df, varnam, outnam){
  ret <- df %>% mutate({{outnam}} := case_when(
    str_detect({{varnam}}, "(?i)dead|^d|^n") ~ "dead",
    str_detect({{varnam}}, "(?i)^a|^y") ~ "alive",
    TRUE ~ as.character(NA)),
  )
  return(ret)
}

#Define retained
## ^y=yes, ^n=no
CategoriseRetained <- function(df){
  ret <- df %>% mutate(categorise.retained = case_when(
    str_detect(retained, "(?i)^y") ~ "yes",
    str_detect(retained, "(?i)^n") ~ "no",
    TRUE ~ as.character(NA)),
    retained = categorise.retained
  )
  return(ret)
}

# Categorise meshed
# Two layers of functions as there is lots of stuff in meshed col
# Create ref col to store original meshed data 
OrigMesh <- function(df){
  ret <- df %>% mutate(
    original.meshed = meshed
  )
  return(ret)
}

# create just bagged/gilled in "meshed"
ActualMeshed <- function(df){
  ret <- df %>% mutate(
    meshed = case_when(
      str_detect(meshed, "(?i)illed") ~ "gilled",
      str_detect(meshed, "(?i)bagg") ~ "bagged",
      meshed == "G" ~ "gilled",
      meshed == "B" ~ "bagged")
  )
  return(ret)
}

# create alt.species
AltSpecies <- function(df, varnam){
  ret <- df %>% mutate(
    Alt.species = case_when(
      str_detect({{varnam}}, "(?i)unknown") ~ "unknown fish",
      str_detect({{varnam}}, "(?i)shell|mollusc") ~ "shell",
      str_detect({{varnam}}, "(?i)crab") ~ "crab",
      str_detect({{varnam}}, "(?i)bird|shearwater") ~ "bird",
      str_detect({{varnam}}, "(?i)baitfish") ~ "baitfish",
      str_detect({{varnam}}, "(?i)lion") ~ "sea lion",
      {{varnam}} == "crayfish" ~ "crayfish",
      TRUE ~ as.character(""))  
  )
  return(ret)
}



# create depredated
Depredate <- function(df, varnam){
  ret <- df %>% mutate(
    depredated = ifelse(str_detect({{varnam}}, "(?i)depredate"), TRUE, as.character(NA))
  )
}

# Create habitat
Habitat <- function(df){
  ret <- df %>% mutate(
    `Percentage cover` = ifelse(str_detect(original.meshed, "\\d"), as.character(original.meshed), as.character(NA))
  )
}

# 2 layer function
CategoriseMeshed <- function(df){
  ret <- df %>% 
    OrigMesh() %>% 
    ActualMeshed() %>% 
    AltSpecies(original.meshed) %>% 
    Depredate(original.meshed) %>% 
    Habitat() %>% 
    filter(!original.meshed %in% c("jack gets stingray barb", "craypot"))
  return(ret)
}

# Categorise Region
CategoriseRegion <- function(df){
  ret <- df %>% mutate(
    Region = case_when(
      str_detect(Region, "(?i)lance") ~ "Lancelin",
      str_detect(Region, "(?i)peac") ~ "Peacefulbay",
      TRUE ~ as.character(Region)
    )
  )
  return(ret)
}
# define comments

# Categorise Periods
CategorisePeriod <- function(df, varnam){
  ret <- df %>% mutate(
    Period = case_when(
      str_detect({{varnam}}, "(?i)^l") ~ "Longline",
      str_detect({{varnam}}, "(?i)^g") ~ "Gillnet",
      TRUE ~ as.character({{varnam}})
    )
  )
  return(ret)
}

}


#########################Subsurface######################################
{
# Rename SS Column
## Using regex notation to capture variation in column names
SSColumns <- function(df) {
  new.names =
    case_when(
      str_detect(names(df), "(?i)gaffed")~"Gaffed",
      str_detect(names(df), "(?i)condition")~"Dropout condition",
      str_detect(names(df), "(?i)drop")~"Drop out",
      names(df)=="OpCode"~"DPIRD code",
      str_detect(names(df), "Period.time.")~"Period time (mins)",
      TRUE~as.character(names(df)))
  old.names <- names(df)
  names(df)[names(df)==old.names] <- new.names
  
  return(df)
}

# Another two layer function
# Duplicate cols to create ref cols
OrigSS <- function(df){
  ret <- df %>% mutate(
    original.gaffed = Gaffed,
    original.condition = `Dropout condition`,
    original.dropout = `Drop out`
  )
  return(ret)
}

ActualGaffed <- function(df){
  ret <- df %>% mutate(
    Gaffed = case_when(
      str_detect(Gaffed, "(?i)^y") ~ "yes",
      str_detect(Gaffed, "(?i)^n") ~ "no",
      TRUE ~ as.character(NA)
    )
  )
  return(ret)
}
# Categorise interaction
# from gaffed column
CategoriseInteraction <- function(df){
  ret <- df %>% 
    mutate(
      Interaction = str_extract(original.gaffed, "\\d{1,2}")
    )
  return(ret)
}

CategoriseSSDropout <- function(df){
  ret <- df %>% mutate(
    `Drop out` = case_when(
      str_detect(original.dropout, "(?i)^y") ~ "yes",
      str_detect(original.dropout, "(?i)^n") ~ "no",
      TRUE ~ as.character(NA)
    )
  )
  return(ret)
}

# Define comments
SSComments <- function(df){
  ret <- df %>% mutate(
    comment = ifelse(original.gaffed %in% c("LINE BROKE","GN BROKE","TO DARK","LL BROKE",
                                            "LL picked up again","camera taken out half way",
                                            "NET SNAPPED","longline snapped","broken line",
                                            "end before finish","END BEFORE FINISHED","thrown over",
                                            "THROWN OVER", "VIDEO ENDS BEFORE FINISHING HAUL", "line snapped"), 
                     as.character(original.gaffed), as.character(NA))
  )
}


# 2 layer function
CategoriseSSGaffed <- function(df){
  ret <- df %>%
    ActualGaffed %>%
    AltSpecies(`Dropout condition`) %>%
    CategoriseInteraction() %>% 
    Depredate(original.gaffed) %>%
    SSComments() %>% 
    mutate(Position = "subsurface",
           Interaction = AssignInteractions(Interaction),
           Interaction = ifelse(Interaction == "Missing data", as.character(NA), as.character(Interaction)))
  return(ret)
  
}

}


###########################################ASL,Humpback,Turtle#########################
{
# Assign ASL, turtle and Humpback Family Genus Species
ASL <- function(df, varnam){
  ret <- df %>% mutate(
    Family = ifelse({{varnam}} == "sea lion", as.character("Otariidae"), as.character(Family)),
    Genus = ifelse({{varnam}} == "sea lion", as.character("Neophoca"), as.character(Genus)),
    Species = ifelse({{varnam}} == "sea lion", as.character("cinerea"), as.character(Species))
  )
  return(ret)
}
Humpback <- function(df, varnam){
  ret <- df %>% mutate(
    Family = ifelse({{varnam}} == "Whale", as.character("Balaenopteridae"), as.character(Family)),
    Genus = ifelse({{varnam}} == "Whale", as.character("Megaptera"), as.character(Genus)),
    Species = ifelse({{varnam}} == "Whale", as.character("novaeangliae"), as.character(Species))
  )
  return(ret)
}
Turtle <- function(df, varnam){
  ret <- df %>% mutate(
    Family = ifelse({{varnam}} == "turtle", as.character("Cheloniidae"), as.character(Family)),
    Genus = ifelse({{varnam}} == "turtle", as.character("Cheloniidae"), as.character(Genus)),
    Species = ifelse({{varnam}} == "turtle", as.character("spp"), as.character(Species))
  )
  return(ret)
}

}


###################################CAAB Macth#########################################
{
# Write a 2 level function to apply CAAB code to species
  MutateForCAAB <- function(df) {
    ret <- df %>% unite(taxa.to.mutate, Genus, Species, sep = " ", remove = FALSE, na.rm = TRUE) %>%
      mutate(
      Species = case_when(
        taxa.to.mutate %in% c("Carcharhinus brachyurus", "Carcharhinus spp","Carcharhinus obscurus", "Carcharhinus sp1") ~ "obscurus",
        Species %in% c("sp2", "sp", "sp1") ~ "spp",
        taxa.to.mutate == "Centroberyx affinis" ~ "gerrardi",
        taxa.to.mutate %in% c("Kyphosus sydneyanus", "Kyphosus bigibbus") ~ "sydneyanus",
        TRUE ~ as.character(Species))) %>%
      unite(taxa, Genus, Species, sep = " ", remove = FALSE, na.rm = TRUE) %>%
      mutate(taxa = str_trim(taxa, side = "left"))
    return(as.data.frame(ret))
  }
  

MatchCAAB <- function(df){
  ret <- left_join(df, ref, by = "taxa") %>% 
    mutate(Code = as.integer(refCode))
    # mutate(Code = ifelse(is.na(Code), as.integer(refCode), as.integer(Code)))
  return(ret)
}


# Returns df with macthed CAAB codes and a new "refCode" col
MatchCAABFUN <- function(df){
  ret <- df %>% MutateForCAAB() %>% 
    MatchCAAB()
  return(ret)
}

}




################################CPUE
# CPUE as n fish caught/fishing hours * n hooks
CPUE <- function(n.catch, effort.hours, n.hooks){
  n.catch/(effort.hours*n.hooks)
}

  
# Assess and combine species groups
columnselect<-function(df){
  df %>% dplyr::select(Family, Genus, Species, Code)
}
# # List data frames
# dfs <- list(
#   Video.longline.interaction,
#   Video.longline.maxN,
#   Video.net.interaction,
#   Video.net.maxN,
#   Video.camera2.deck,
#   Video.camera1.deck,
#   Video.subsurface
# )
# 
# # MAke all species list
# mylist <- lapply(X = dfs, FUN = columnselect)
# All.species <- data.table::rbindlist(mylist) %>%
#   mutate(
#     Species = ifelse(
#       Species %in% c("sp2", "sp", "sp1"),
#       as.character("spp"),
#       as.character(Species)
#     ),
#     Genus = ifelse(Genus == "", as.character(Family), Genus)
#   ) %>%
#   unite(taxa,
#         Genus,
#         Species,
#         sep = " ",
#         remove = FALSE,
#         na.rm = TRUE) %>%
#   mutate(taxa = str_trim(taxa, side = "left"))
# All.taxa <- as.data.frame(distinct(All.species, taxa))
# myspecieslist <- sort(table(All.species$taxa))
# myspp <- All.species %>% filter(str_detect(taxa, "(?i)spp")) %>% distinct(taxa)
