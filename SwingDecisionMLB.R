install.packages("tidyverse")
install.packages("readr")
library(tidyverse)
library(readr)

MLBultimate <- read_csv("mlb_data.csv")
View(MLBultimate)
# get dodgers hitter data
#LADultimate <- read_csv("Dodgers_Data.csv")
LADultimate1 <- MLBultimate %>%
  filter(home_team == "LAD" & inning_topbot == "Bot")
LADultimate2 <- MLBultimate %>%
  filter(away_team == "LAD" & inning_topbot == "Top")

View(LADultimate1)
View(LADultimate2)

LADultimate <- rbind(LADultimate1,LADultimate2)

unique(MLBultimate$plate_x)   # list of Unique Pitches in TaggedPitchType 
# FF 4-Seam Fastball
# SL Slider
# CU Curveball
# FS Split-Finger
# FC Cutter
# CH Changeup
# SI Sinker (synonymous with FT)
# KC Knuckle Curve

# OMIT:
# ST
# KN Knuckleball
# CS
# PO
# SV
# FA
# EP
# SC
# FO
# FT

unique <- filter(MLBultimate, events == "")
nrow(unique)
View(unique)


unique <- unique %>%
  select(stand)
View(unique)

unique(MLBultimate$pitch_name)


unique <- MLBultimate %>%
  select(description)
View(unique)

unique(MLBultimate$description)

###################################################################
####### FUNCTION FOR FILTERING PITCHES AND CREATING RATINGS #######
###################################################################


####  SIMILAR PITCH 1 wond read for straight pitches and horizontal moving pitches :()



PitchRating <- function(pitcherside, batterside, pitch, velo, x, y) {
  # switch x value so that it is equivalent to Catcher's view and keep code the same
  x = x * -1
  
  # Test if pitch it outside bounds, and return null
  if(x > .92 | x < -.92 | y > 4 | y < 1) {
    return("outside zone")
  } 
  
  
  # now filter by pitch type i.e horizontal moving or not
  
  ## FIX FOR HANDEDNESS OF BATTER ##
  else if (pitch == "4-Seam Fastball" | pitch == "Changeup" | pitch == "Split-Finger") {
    #filter similar pitch for batter == batter
    SimilarPitch1 <- MLBultimate %>% 
      filter(pitch_name == pitch) %>%
      filter(stand == batterside) %>%
      filter(release_speed > (velo - 2) & release_speed < (velo + 2)) %>%
      filter(plate_z > (y - .25) & plate_z < (y + .25)) %>%
      filter(plate_x > (x - .25) & plate_x < (x + .25))
    #View(SimilarPitch1)
    
    # reverse batter handedness and x value
    
    if(batterside == "R") { revbatter <- "L" }
    if(batterside == "L") { revbatter <- "R"}
    
    SimilarPitch2 <- MLBultimate %>% 
      filter(stand == revbatter) %>%
      filter(pitch_name == pitch) %>%
      filter(release_speed > (velo - 2) & release_speed < (velo + 2)) %>%
      filter(plate_z > (y - .25) & plate_z < (y + .25)) %>%
      filter(plate_x > (-x - .25) & plate_x < (-x + .25))
    #View(SimilarPitch2)
    
    SimilarPitch <- rbind(SimilarPitch1,SimilarPitch2)
    SimilarPitch <- SimilarPitch %>%
      filter(description == "hit_into_play" | description == "foul_tip" | description == "swinging_strike" | description == "foul" | description == "swinging_strike_blocked")
    #View(SimilarPitch)
    if (nrow(SimilarPitch) == 0) { return("no similar pitches") }
    
  }
  
  
  # if horizontal moving make two tables and merge
  else if (pitch == "Curveball" | pitch == "Slider" | pitch == "Cutter" | pitch == "Sinker") {
    SimilarPitch1 <- MLBultimate %>% 
      filter(p_throws == pitcherside & stand == batterside) %>%
      filter(pitch_name == pitch) %>%
      filter(release_speed > (velo - 4) & release_speed < (velo + 4)) %>%
      filter(plate_z >= (y - .25) & plate_z <= (y + .25)) %>%
      filter(plate_x >= (x - .25) & plate_x <= (x + .25))
    #View(SimilarPitch1)
    
    
    # reverse handedness for second table
    # can take this out and make pitcherThrows != pitcher if we clean the data
    
    if(pitcherside == "R") { revpitcher <- "L" }
    if(pitcherside == "L") { revpitcher <- "R" }
    if(batterside == "R") { revbatter <- "L" }
    if(batterside == "L") { revbatter <- "R"}
    
    SimilarPitch2 <- MLBultimate %>%
      filter(p_throws == revpitcher & stand == revbatter) %>%
      filter(pitch_name == pitch) %>%
      filter(release_speed > (velo - 3) & release_speed < (velo + 3)) %>%
      filter(plate_z >= (y - .25) & plate_z <= (y + .25)) %>%
      filter(plate_x >= (-x - .25) & plate_x <= (-x + .25)) 
    #View(SimilarPitch2)
    
    SimilarPitch <- rbind(SimilarPitch1,SimilarPitch2)
    SimilarPitch <- SimilarPitch %>%
      filter(description == "hit_into_play" | description == "foul_tip" | description == "swinging_strike" | description == "foul" | description == "swinging_strike_blocked")
    #View(SimilarPitch)
    if (nrow(SimilarPitch) == 0) { return("no similar pitches") }
  }
  

  
  else { return("not a common pitch") }
  
  
  
  totalbases = 0
  for (i in 1:nrow(SimilarPitch)) {
    if(!is.na(SimilarPitch$events[i])){
      if (SimilarPitch$events[i] == "single") {
        totalbases = totalbases + 1
      }
      if (SimilarPitch$events[i] == "double") {
        totalbases = totalbases + 2
      }
      if (SimilarPitch$events[i] == "triple") {
        totalbases = totalbases + 3
      }
      if (SimilarPitch$events[i] == "home_run") {
        totalbases = totalbases + 4
      }
    }
  }
  expectedbases <- totalbases/nrow(SimilarPitch)
  if(totalbases < 50) { return("not enough data") }
  
  
  # expectedbases <- (expectedbases-0.5151)*11.9
  
  return(expectedbases)
  
  
  #cat("Total Bases: ", totalbases, "\n")
  #cat("# of At Bats: ", nrow(SimilarPitch),"\n")
  #cat("Expected Bases: ", expectedbases)
  
  
  
}

PitchRating <- Vectorize(PitchRating)



###################################################################
########################### TESTING ###############################
###################################################################

PitchRating("R", "R", "Curveball", 78, 0, 2.55)
PitchRating("R", "R", "4-Seam Fastball", 90, 0, 2.55)
PitchRating("R", "R", "Curveball", 80, -.5, 1.9)
PitchRating("R", "R", "4-Seam Fastball", 70, 0, 2.55)



#### Clean DODGERS ULTIMATE FOR NA VALUES in our parameters ####

unique(LADultimate$plate_x)
unique(LADultimate$plate_z)
unique(LADultimate$release_speed)
unique(LADultimate$p_throws)

for(i in 1:nrow(LADultimate)) { 
  if(is.null(LADultimate[i,]$stand)){
    cat("YES ")
  }
}


LADultimate <- LADultimate[!is.na(LADultimate$plate_x), ]
LADultimate <- LADultimate[!is.na(LADultimate$plate_z), ]
LADultimate <- LADultimate[!is.na(LADultimate$release_speed), ]
LADultimate <- LADultimate[!is.na(LADultimate$pitch_name), ]


#### MUTATING RATINGS TO DODGERS Hitters ULTIMATE ####

LADultimatehead <- head(LADultimate, 20)
LADultimatehalf <- head(LADultimate, 50000)

LADultimateRatingDesc <- mutate(LADultimatehead, Rating = PitchRating(p_throws, stand, pitch_name, release_speed, plate_x, plate_z))
LADultimateRatingDesc1 <- LADultimateRatingDesc %>%
  select(p_throws, stand, pitch_name, release_speed, plate_x, plate_z, description, Rating)
View(LADultimateRatingDesc1)

LADultimateRatingDescription <- mutate(LADultimate, Rating = PitchRating(p_throws, stand, pitch_name, release_speed, plate_x, plate_z))
LADultimateRatingDescdescription1 <- LADultimateRatingDescription %>%
  select(p_throws, stand, pitch_name, release_speed, plate_x, plate_z, description, Rating)
View(LADultimateRatingDescription1)


LADultimateRatingDescriptionTEST <- mutate(LADultimatehalf, Rating = PitchRating(p_throws, stand, pitch_name, release_speed, plate_x, plate_z))
