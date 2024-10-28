
## Constants used in data_cleaning_PA
### Variables are defined like.this
################################################Underwater############################################  
# Start with column names that we want
## Interaction df
interaction.names <-  c(
  "OpCode",
  "Frame",
  "Time (mins)",
  "Period time (mins)",
  "Period",
  "TapeReader",
  "Depth",
  "Comment",
  "Method",
  "Position",
  "Family",
  "Genus" ,
  "Species" ,
  "Code" ,
  "Number",
  "Interaction",
  "Escape",
  "Alt.species",
  "original.escape",
  "original.maxn",
  "original.interaction"
)
## MaxN df
maxn.names <-  c(
  "OpCode",
  "Frame",
  "Time (mins)",
  "Period time (mins)",
  "Period",
  "TapeReader",
  "Depth",
  "Family",
  "Genus" ,
  "Species" ,
  "Code",
  "MaxN",
  "Escape",
  "Method",
  "Alt.species",
  "original.maxn"
)
## Observation df
observation.names <-  c(
  "OpCode",
  "Frame",
  "Time (mins)",
  "Period time (mins)",
  "Period",
  "TapeReader",
  "Depth",
  "Number",
  "observation",
  "code",
  "Code",
  "Method",
  "Alt.species",
  "original.escape",
  "Family",
  "Genus" ,
  "Species"
)


# comments (from underwater escape column) that we don't want to include in a df
## Two different sets. Use in various places
## drop1
drop1 <- c(
  "Camera Onboard",
  "example of swell conditions for fis",
  "looking at camera",
  "no fish",
  "No Fish Seen",
  "reef structure 20cm squre rippedup",
  "Retrevial begins",
  "Retrevial Started",
  "retrival start",
  "ripped up macro algae",
  "rock dislodged",
  "rocks dislodged",
  'end-no haul',
  "end no haul",
  "end before haul",
  'CAMERA STOPS',
  "end",
  "ended before haul",
  "finish before haul",
  "no haul",
  " no haul",
  "?",
  "snoode broke on haul 170"
)
## drop2
drop2 <-  c(
  "Camera Onboard",
  "example of swell conditions for fis",
  "looking at camera",
  "reef structure 20cm squre rippedup",
  "Retrevial begins","Retrevial Started",
  "retrival start",
  "ripped up macro algae",
  "rock dislodged",
  "rocks dislodged",
  "?",
  "snoode broke on haul 170",
  "snoode broke on haul 170mins later",
  "line cut",
  "attacked camera",
  "attracted to float",
  "attacks float",
  "snoode broke on haul",
  "attrached to flaot",
  "Attacks camera",
  "snoode broke during retreival"
)

##########################################################Deck 1############################################
## Column names that we want
deck.1.fish.names <-
  c(
    "curtin opcode",
    "Region",
    "DIPRD code",
    "Position",
    "Frame",
    "Period",
    "Period time (mins)",
    "TapeReader",
    "Depth (M)",
    "Method",
    "Family",
    "Genus",
    "Species",
    "Code",
    "number",
    "condition",
    "retained",
    "meshed",
    "original.meshed",
    "Alt.species",
    "depredated",
    "original.condition",
    "original.retained"
  )
deck.1.habitat.names <-
  c(
    "Curtin opcode",
    "Region",
    "DIPRD code",
    "Position",
    "Frame",
    "Period",
    "Period time (mins)",
    "TapeReader",
    "Depth (M)",
    "Method",
    "Family",
    "Genus",
    "Species",
    "Code",
    "Percentage cover",
    "original.meshed"
  )
deck.1.observations.names <- 
  c(
    "RegionDIPRD codePosition",
    "Region",
    "DIPRD code",
    "Position",
    "Frame",
    "Period",
    "Period time (mins)",
    "TapeReader",
    "Depth (M)",
    "Method",
    "Family",
    "Genus",
    "Species",
    "Code",
    "number",
    "condition",
    "retained",
    "meshed",
    "original.meshed",
    "Alt.species"
  )

deck.1.observations <-
  c(
    "gilled not measured",
    "not measured",
    "measured twice",
    "depredated not measured",
    "caught inside mako shark gill"
  )

#########################################################Deck 2#############################################################
deck.2.fish.names <-
  c(
    "Curtin opcode",
    "Region",
    "DPIRD code",
    "Position",
    "Frame",
    "Period",
    "Period time (mins)",
    "TapeReader",
    "Depth",
    "Method",
    "Family",
    "Genus",
    "Species",
    "Code",
    "hook distance to float/weight",
    "dropout",
    "gaffed",
    "Filename",
    "hookloc.and.comments",
    "Alt.species",
    "depredated",
    "original.hooklocation",
    "original.dropout",
    "original.gaffed",
    "Number",
    "Stage",
    "Activity"
  )
deck.2.observations.names <-
  c(
    "Curtin opcode",
    "Region",
    "DPIRD code",
    "Position",
    "Frame",
    "Period",
    "Period time (mins)",
    "TapeReader",
    "Depth",
    "Method",
    "Family",
    "Genus",
    "Species",
    "Code",
    "Number",
    "Stage",
    "Activity",
    "comment",
    "original.hooklocation",
    "original.dropout",
    "original.gaffed",
    "Alt.species"
  )

deck.2.observations <-
  c(
    "caught feeding on Sargent Baker",
    "tagged and released",
    "4min gap between vidsdidn't see fish on dec",
    "depredated",
    "caughtfeedingonSargentBaker",
    "2",
    "taggedandreleased",
    "4mingapbetweenvidsdidn'tseefishondec"
  )

######################################################Subsurface################################################
subsurface.names <- 
  c(
    "Region",
    "DPIRD code",
    "Position",
    "Period",
    "Period time (mins)",
    "Depth",
    "Family",
    "Genus",
    "Species",
    "Code",
    "comment",
    "Drop out",
    "Dropout condition",
    "Interaction",
    "Gaffed",
    "original.gaffed",
    "original.dropout",
    "original.condition",
    "Alt.species",
    "depredated"
  )

subsurface.observations.names <- 
  c(
    "OpCode",
    "Region",
    "DPIRD code",
    "Position",
    "Period",
    "Period time (mins)",
    "Depth",
    "Family",
    "Genus",
    "Species",
    "Code",
    "comment",
    "Dropout condition",
    "Drop out",
    "interaction",
    "original.gaffed",
    "original.dropout",
    "original.condition",
    "Alt.species"
  )

subsurface.observations <-
  c(
    "LINE BROKE",
    "GN BROKE",
    "TO DARK",
    "LL BROKE",
    "LL picked up again",
    "camera taken out half way",
    "VIDEO ENDS BEFORE FINISHING HAUL",
    "line snapped",
    "NET SNAPPED",
    "longline snapped",
    "broken line",
    "end before finish",
    "END BEFORE FINISHED",
    "thrown over",
    "THROWN OVER"
  )