
# CONTROL FOR NA in ALL colleges Ultimate
# CONTROL FOR EMPTY DATASET (SIMILAR PITCH EMPTY)


install.packages("tidyverse")
install.packages("readr")
library(tidyverse)
library(readr)


## Load and view All colleges Ultimate

ultimate <- read_csv("All_College_TM_19_22.csv")
UCSBultimate <- read_csv("Ultimate_UCSB_5_28_22.csv")

View(ultimate)

unique(ultimate$PlateLocSide)   # list of Unique Pitches in TaggedPitchType 
unique <- filter(ultimate, TaggedPitchType == "Cutter")
unique <- unique %>%
  select(RelSpeed)
View(unique)

# FASTBALL: 637,498 obs
# CHANGEUP : 123,799 obs
# SPLITTER : 2,793 obs   OMIT
# SINKER : 18,045
 
# CURVEBALL : 88,537
# SLIDER : 214,037
# CUTTER : 13,400


####### CLEAN UCSBultimate DATA FOR:
####### PitcherThrows, BatterSide, RelSpeed, PlateLocHeight, PlateLocSide

UCSBultimate <- UCSBultimate[!is.na(UCSBultimate$PlateLocHeight), ]
UCSBultimate <- UCSBultimate[!is.na(UCSBultimate$BatterSide), ]
UCSBultimate <- UCSBultimate[!is.na(UCSBultimate$PitcherThrows), ]
UCSBultimate <- UCSBultimate[!is.na(UCSBultimate$RelSpeed), ]

unique(UCSBultimate$PitcherThrows)
unique(UCSBultimate$BatterSide)
unique(UCSBultimate$RelSpeed)

for(i in 1:nrow(UCSBultimate)) {       # for-loop over columns
  if(is.numeric(UCSBultimate[i, ]$PlateLocSide) == FALSE) {
    print(UCSBultimate[i, ]$PlateLocSide)
  }
} 
 
UCSBultimate <- UCSBultimate %>%
  filter(BatterSide == "Right" | BatterSide == "Left")

UCSBultimate <- UCSBultimate %>%
  filter(PitcherThrows == "Right" | PitcherThrows == "Left")



test <- UCSBultimate %>% 
  filter(PitcherThrows == "RR")
View(test)



###################################################################
####### FUNCTION FOR FILTERING PITCHES AND CREATING RATINGS #######
###################################################################


PitchRating <- function(pitcher, batter, pitch, velo, x, y) {
  # Test if pitch it outside bounds, and return null
  if(x > .92 | x < -.92 | y > 4 | y < 1) {
    return(NA)
  } 
  
  
  # now filter by pitch type i.e horizontal moving or not
  
  ## FIX FOR HANDEDNESS OF BATTER ##
  else if (pitch == "Fastball" | pitch == "ChangeUp" | pitch == "Splitter" | pitch == "Sinker") {
    #filter similar pitch for batter == batter
    SimilarPitch1 <- ultimate %>% 
      filter(BatterSide == batter) %>%
      filter(TaggedPitchType == pitch) %>%
      filter(RelSpeed > (velo - 2) & RelSpeed < (velo + 2)) %>%
      filter(PlateLocHeight > (y - .25) & PlateLocHeight < (y + .25)) %>%
      filter(PlateLocSide > (x - .25) & PlateLocSide < (x + .25))
    #View(SimilarPitch1)
    
    # reverse batter handedness and x value
    
    if(batter == "Right") { revbatter <- "Left" }
    if(batter == "Left") { revbatter <- "Right"}
    
    SimilarPitch2 <- ultimate %>% 
      filter(BatterSide == revbatter) %>%
      filter(TaggedPitchType == pitch) %>%
      filter(RelSpeed > (velo - 2) & RelSpeed < (velo + 2)) %>%
      filter(PlateLocHeight > (y - .25) & PlateLocHeight < (y + .25)) %>%
      filter(PlateLocSide > (-x - .25) & PlateLocSide < (-x + .25))
    #View(SimilarPitch2)
    
    SimilarPitch <- rbind(SimilarPitch1,SimilarPitch2)
    SimilarPitch <- SimilarPitch %>%
      filter(PlayResult != "Undefined" | KorBB == "Strikeout")
    #View(SimilarPitch)
    
    
  }
  
  
  # if horizontal moving make two tables and merge
  else if (pitch == "Curveball" | pitch == "Slider" | pitch == "Cutter") {
    SimilarPitch1 <- ultimate %>% 
      filter(PitcherThrows == pitcher & BatterSide == batter) %>%
      filter(TaggedPitchType == pitch) %>%
      filter(RelSpeed > (velo - 4) & RelSpeed < (velo + 4)) %>%
      filter(PlateLocHeight >= (y - .25) & PlateLocHeight <= (y + .25)) %>%
      filter(PlateLocSide >= (x - .25) & PlateLocSide <= (x + .25))
    #View(SimilarPitch1)
    
    
    # reverse handedness for second table
    # can take this out and make pitcherThrows != pitcher if we clean the data
    
    if(pitcher == "Right") { revpitcher <- "Left" }
    if(pitcher == "Left") { revpitcher <- "Right" }
    if(batter == "Right") { revbatter <- "Left" }
    if(batter == "Left") { revbatter <- "Right"}

    SimilarPitch2 <- ultimate %>%
      filter(PitcherThrows == revpitcher & BatterSide == revbatter) %>%
      filter(TaggedPitchType == pitch) %>%
      filter(RelSpeed > (velo - 4) & RelSpeed < (velo + 4)) %>%
      filter(PlateLocHeight >= (y - .25) & PlateLocHeight <= (y + .25)) %>%
      filter(PlateLocSide >= (-x - .25) & PlateLocSide <= (-x + .25)) %>%
      filter(TaggedPitchType == pitch) 
    #View(SimilarPitch2)
    SimilarPitch <- rbind(SimilarPitch1,SimilarPitch2)
    SimilarPitch <- SimilarPitch %>%
      filter(PlayResult != "Undefined" | KorBB == "Strikeout")
    #View(SimilarPitch)
  }
  
  else { return(NA) }
  
  if (nrow(SimilarPitch) == 0) { return(NA) }
  
  totalbases = 0
  for (i in 1:nrow(SimilarPitch)) {
    if (SimilarPitch$PlayResult[i] == "Single") {
      totalbases = totalbases + 1
    }
    if (SimilarPitch$PlayResult[i] == "Double") {
      totalbases = totalbases + 2
    }
    if (SimilarPitch$PlayResult[i] == "Triple") {
      totalbases = totalbases + 3
    }
    if (SimilarPitch$PlayResult[i] == "HomeRun") {
      totalbases = totalbases + 4
    }
  }
  expectedbases <- totalbases/nrow(SimilarPitch)
  if(totalbases < 50) { return(NA) }
  
  
  expectedbases <- (expectedbases-0.5151)*11.9
  
  return(expectedbases)
  
  
  
}

PitchRating <- Vectorize(PitchRating)

# maybe shift high and low values to be about -.175-.175 and then scale to get to -5/5 or -10/10

#############################################
####### MORE SCRATCH FUNCTION WORK ##########
#############################################


PitchRating("Right", "Right", "Fastball", 94, 0, 2.55)
as.double(PitchRating("Right", "Right", "Cutter", 83, -.5, 2.6))
print(PitchRating("Left", "Left", "Curveball", 75, 1, 2))
print(as.double(PitchRating("Right", "Right", "Slider", 78, .5, 2)))
PitchRatingDesc("Right", "Right", "Fastball", 84, .07, 3.02)

## SHOW BRIAN THIS ONE
PitchRatingDesc("Right", "Right", "Cutter", 83, .1, 2.6)


###### MUTATING TABLE TO ADD RATINGS AT THE END ######

UCSBultimate <- UCSBultimate %>%
  filter(BatterTeam == "SAN_GAU" | BatterTeam == "UCSB_GAU")


ultimateRatingDesc <- mutate(UCSBultimate, Rating = PitchRating(PitcherThrows, BatterSide, TaggedPitchType, RelSpeed, PlateLocSide, PlateLocHeight))
ultimateRatingDesc1 <- ultimateRatingDesc %>%
  select(PitcherThrows, BatterSide, TaggedPitchType, RelSpeed, PlateLocSide, PlateLocHeight, KorBB, PlayResult, Rating)
View(ultimateRatingDesc1)



ultimateRating <- mutate(UCSBultimate, Rating = as.double(PitchRating(PitcherThrows, BatterSide, TaggedPitchType, RelSpeed, PlateLocSide, PlateLocHeight)))
ultimateRating1 <- ultimateRating %>%
  select(Batter, PitcherThrows, BatterSide, TaggedPitchType, RelSpeed, PlateLocSide, PlateLocHeight, KorBB, PlayResult, Rating)
View(ultimateRating1)



ultimateRatingSCALED <- mutate(UCSBultimate, Rating = as.double(PitchRating(PitcherThrows, BatterSide, TaggedPitchType, RelSpeed, PlateLocSide, PlateLocHeight)))
ultimateRatingSCALED1 <- ultimateRatingSCALED %>%
  select(PitcherThrows, BatterSide, TaggedPitchType, RelSpeed, PlateLocSide, PlateLocHeight, KorBB, PlayResult, Rating)
View(ultimateRatingSCALED)







#### LOOPS TO FIND SCALE FOR RATING

index = 0
min = 1
count = 0
for(i in 1:nrow(ultimateRating)) {       # for-loop over columns
  if(is.na(as.numeric(ultimateRating[i, ]$Rating)) == FALSE) {
    if(min > as.numeric(ultimateRating[i, ]$Rating)) {
      min <- as.numeric(ultimateRating[i, ]$Rating)
      print(as.numeric(ultimateRating[i, ]$Rating))
      index = i
    }
  }
} 
print(min)
print(index)

## Out of Zone: 18515
## Not a common pitch: 93
## Similar Pitch Empty: 6
## Low Data Numbers: 2468 
## TOTAL: 21082


for(i in 1:nrow(ultimateRatingx)) {       # for-loop over columns
  if(is.na(ultimateRatingx[i, ]$Rating) == FALSE) {
    count = count + 1
  }
} 

for(i in 1:nrow(ultimateRatingx)) {       # for-loop over columns
  as.double(ultimateRatingx[i, ]$Rating)
} 

min = 1
for(i in 1:nrow(ultimateRating)) {       # for-loop over columns
  if(is.na(ultimateRating[i, ]$Rating) == FALSE) {
    if(min > ultimateRating[i, ]$Rating) {
      min <- ultimateRating[i, ]$Rating
    }
  }
} 
print(min)
max = 0
for(i in 1:nrow(ultimateRating)) {       # for-loop over columns
  if(is.na(ultimateRating[i, ]$Rating) == FALSE) {
    if(max < ultimateRating[i, ]$Rating) {
      max <- ultimateRating[i, ]$Rating
      print(i)
      print(ultimateRating[i, ]$TaggedPitchType)
      print(ultimateRating[i, ]$RelSpeed)
      print(ultimateRating[i, ]$PlateLocSide)
      print(ultimateRating[i, ]$PlateLocHeight)
      print(max)
    }
  }
} 
print(max)

hist(ultimateRating$Rating)





#################################################################
####### CALCULATING METRIC PER PLAYER AND SCALING RATINGS #######
#################################################################

ultimateRating <- ultimateRating[!is.na(ultimateRating$PitchCall), ]
ultimateRating1 <- ultimateRating1[order(ultimateRating$Batter), ]
UCSBHitters <- table(ultimateRating$Batter)
View(UCSBHitters)
UCSBHitters <- as.data.frame(UCSBHitters)
colnames(UCSBHitters) <- c('Player','# of Pitches Seen')
UCSBHitters <- mutate(UCSBHitters, Total = 0)
UCSBHitters <- mutate(UCSBHitters, "Total/Num" = 0)

UCSBHitters

for(i in 1:nrow(UCSBHitters)){
  UCSBHitters$Total <- 0
}


for(i in 1:nrow(ultimateRating)){
  rating <- ultimateRating[i,173]
  rating <- (rating-0.5151)*11.9
  
  
  if(is.na(ultimateRating[i, ]$Rating) == TRUE) { }
  else if(ultimateRating[i,]$PitchCall == "InPlay" || ultimateRating[i,]$PitchCall == "FoulBall" || ultimateRating[i,]$PitchCall == "StrikeSwinging" || ultimateRating[i,]$PitchCall == "Inplay"){
    UCSBHitters[UCSBHitters$Player == toString(ultimateRating[i,]$Batter), 3] <- (UCSBHitters[UCSBHitters$Player == toString(ultimateRating[i,]$Batter), 3]) + rating
  }
  else if(ultimateRating[i,]$PitchCall == "BallCalled" || ultimateRating[i,]$PitchCall == "StrikeCalled" || ultimateRating[i,]$PitchCall == "BallinDirt") {
    UCSBHitters[UCSBHitters$Player == toString(ultimateRating[i,]$Batter), 3] <- (UCSBHitters[UCSBHitters$Player == toString(ultimateRating[i,]$Batter), 3]) - rating
  }
  else { }
}


unique(ultimate$PitchCall)

table(ultimateRating$PitchCall)
table(ultimateRating1$Batter)




### EXTRA FUNCTIONS FOR RATONG DESCRIPTIPONS

PitchRating <- function(pitcher, batter, pitch, velo, x, y) {
  # Test if pitch it outside bounds, and return null
  if(x > .92 | x < -.92 | y > 4 | y < 1) {
    return("OUT OF ZONE")
  } 
  
  
  # now filter by pitch type i.e horizontal moving or not
  
  ## FIX FOR HANDEDNESS OF BATTER ##
  else if (pitch == "Fastball" | pitch == "ChangeUp" | pitch == "Splitter" | pitch == "Sinker") {
    #filter similar pitch for batter == batter
    SimilarPitch1 <- ultimate %>% 
      filter(BatterSide == batter) %>%
      filter(TaggedPitchType == pitch) %>%
      filter(RelSpeed > (velo - 2) & RelSpeed < (velo + 2)) %>%
      filter(PlateLocHeight > (y - .25) & PlateLocHeight < (y + .25)) %>%
      filter(PlateLocSide > (x - .25) & PlateLocSide < (x + .25))
    #View(SimilarPitch1)
    
    # reverse batter handedness and x value
    
    if(batter == "Right") { revbatter <- "Left" }
    if(batter == "Left") { revbatter <- "Right"}
    
    SimilarPitch2 <- ultimate %>% 
      filter(BatterSide == revbatter) %>%
      filter(TaggedPitchType == pitch) %>%
      filter(RelSpeed > (velo - 2) & RelSpeed < (velo + 2)) %>%
      filter(PlateLocHeight > (y - .25) & PlateLocHeight < (y + .25)) %>%
      filter(PlateLocSide > (-x - .25) & PlateLocSide < (-x + .25))
    #View(SimilarPitch2)
    
    SimilarPitch <- rbind(SimilarPitch1,SimilarPitch2)
    SimilarPitch <- SimilarPitch %>%
      filter(PlayResult != "Undefined" | KorBB == "Strikeout")
    #View(SimilarPitch)
    
    
  }
  
  
  # if horizontal moving make two tables and merge
  else if (pitch == "Curveball" | pitch == "Slider" | pitch == "Cutter") {
    SimilarPitch1 <- ultimate %>% 
      filter(PitcherThrows == pitcher & BatterSide == batter) %>%
      filter(TaggedPitchType == pitch) %>%
      filter(RelSpeed > (velo - 4) & RelSpeed < (velo + 4)) %>%
      filter(PlateLocHeight >= (y - .25) & PlateLocHeight <= (y + .25)) %>%
      filter(PlateLocSide >= (x - .25) & PlateLocSide <= (x + .25))
    #View(SimilarPitch1)
    
    
    # reverse handedness for second table
    # can take this out and make pitcherThrows != pitcher if we clean the data
    
    if(pitcher == "Right") { revpitcher <- "Left" }
    if(pitcher == "Left") { revpitcher <- "Right" }
    if(batter == "Right") { revbatter <- "Left" }
    if(batter == "Left") { revbatter <- "Right"}
    
    SimilarPitch2 <- ultimate %>%
      filter(PitcherThrows == revpitcher & BatterSide == revbatter) %>%
      filter(TaggedPitchType == pitch) %>%
      filter(RelSpeed > (velo - 4) & RelSpeed < (velo + 4)) %>%
      filter(PlateLocHeight >= (y - .25) & PlateLocHeight <= (y + .25)) %>%
      filter(PlateLocSide >= (-x - .25) & PlateLocSide <= (-x + .25)) %>%
      filter(TaggedPitchType == pitch) 
    #View(SimilarPitch2)
    SimilarPitch <- rbind(SimilarPitch1,SimilarPitch2)
    SimilarPitch <- SimilarPitch %>%
      filter(PlayResult != "Undefined" | KorBB == "Strikeout")
    #View(SimilarPitch)
  }
  
  else { return("NOT A COMMON PITCH") }
  
  if (nrow(SimilarPitch) == 0) { return("SIMILAR PITCH EMPTY") }
  
  totalbases = 0
  for (i in 1:nrow(SimilarPitch)) {
    if (SimilarPitch$PlayResult[i] == "Single") {
      totalbases = totalbases + 1
    }
    if (SimilarPitch$PlayResult[i] == "Double") {
      totalbases = totalbases + 2
    }
    if (SimilarPitch$PlayResult[i] == "Triple") {
      totalbases = totalbases + 3
    }
    if (SimilarPitch$PlayResult[i] == "HomeRun") {
      totalbases = totalbases + 4
    }
  }
  expectedbases <- totalbases/nrow(SimilarPitch)
  if(totalbases < 50) { return("LOW DATA NUMBERS") }
  
  return(expectedbases)
  
}



PitchRatingDesc <- function(pitcher, batter, pitch, velo, x, y) {
  # Test if pitch it outside bounds, and return null
  if(x > .92 | x < -.92 | y > 4 | y < 1) {
    return("OUT OF ZONE")
  } 
  
  
  # now filter by pitch type i.e horizontal moving or not
  
  ## FIX FOR HANDEDNESS OF BATTER ##
  else if (pitch == "Fastball" | pitch == "ChangeUp" | pitch == "Splitter" | pitch == "Sinker") {
    #filter similar pitch for batter == batter
    SimilarPitch1 <- ultimate %>% 
      filter(BatterSide == batter) %>%
      filter(TaggedPitchType == pitch) %>%
      filter(RelSpeed > (velo - 2) & RelSpeed < (velo + 2)) %>%
      filter(PlateLocHeight > (y - .25) & PlateLocHeight < (y + .25)) %>%
      filter(PlateLocSide > (x - .25) & PlateLocSide < (x + .25))
    #View(SimilarPitch1)
    
    # reverse batter handedness and x value
    
    if(batter == "Right") { revbatter <- "Left" }
    if(batter == "Left") { revbatter <- "Right"}
    
    SimilarPitch2 <- ultimate %>% 
      filter(BatterSide == revbatter) %>%
      filter(TaggedPitchType == pitch) %>%
      filter(RelSpeed > (velo - 2) & RelSpeed < (velo + 2)) %>%
      filter(PlateLocHeight > (y - .25) & PlateLocHeight < (y + .25)) %>%
      filter(PlateLocSide > (-x - .25) & PlateLocSide < (-x + .25))
    #View(SimilarPitch2)
    
    SimilarPitch <- rbind(SimilarPitch1,SimilarPitch2)
    SimilarPitch <- SimilarPitch %>%
      filter(PlayResult != "Undefined" | KorBB == "Strikeout")
    #View(SimilarPitch)
    
    
  }
  
  
  # if horizontal moving make two tables and merge
  else if (pitch == "Curveball" | pitch == "Slider" | pitch == "Cutter") {
    SimilarPitch1 <- ultimate %>% 
      filter(PitcherThrows == pitcher & BatterSide == batter) %>%
      filter(TaggedPitchType == pitch) %>%
      filter(RelSpeed > (velo - 4) & RelSpeed < (velo + 4)) %>%
      filter(PlateLocHeight >= (y - .25) & PlateLocHeight <= (y + .25)) %>%
      filter(PlateLocSide >= (x - .25) & PlateLocSide <= (x + .25))
    #View(SimilarPitch1)
    
    
    # reverse handedness for second table
    # can take this out and make pitcherThrows != pitcher if we clean the data
    
    if(pitcher == "Right") { revpitcher <- "Left" }
    if(pitcher == "Left") { revpitcher <- "Right" }
    if(batter == "Right") { revbatter <- "Left" }
    if(batter == "Left") { revbatter <- "Right"}
    
    SimilarPitch2 <- ultimate %>%
      filter(PitcherThrows == revpitcher & BatterSide == revbatter) %>%
      filter(TaggedPitchType == pitch) %>%
      filter(RelSpeed > (velo - 4) & RelSpeed < (velo + 4)) %>%
      filter(PlateLocHeight >= (y - .25) & PlateLocHeight <= (y + .25)) %>%
      filter(PlateLocSide >= (-x - .25) & PlateLocSide <= (-x + .25)) %>%
      filter(TaggedPitchType == pitch) 
    #View(SimilarPitch2)
    SimilarPitch <- rbind(SimilarPitch1,SimilarPitch2)
    SimilarPitch <- SimilarPitch %>%
      filter(PlayResult != "Undefined" | KorBB == "Strikeout")
    #View(SimilarPitch)
  }
  
  else { return("NOT A COMMON PITCH") }
  
  if (nrow(SimilarPitch) == 0) { return("SIMILAR PITCH EMPTY") }
  
  totalbases = 0
  for (i in 1:nrow(SimilarPitch)) {
    if (SimilarPitch$PlayResult[i] == "Single") {
      totalbases = totalbases + 1
    }
    if (SimilarPitch$PlayResult[i] == "Double") {
      totalbases = totalbases + 2
    }
    if (SimilarPitch$PlayResult[i] == "Triple") {
      totalbases = totalbases + 3
    }
    if (SimilarPitch$PlayResult[i] == "HomeRun") {
      totalbases = totalbases + 4
    }
  }
  expectedbases <- totalbases/nrow(SimilarPitch)
  if(totalbases < 50) { return("LOW DATA NUMBERS") }
  
  #return(expectedbases)
  
  
  cat("Total Bases: ", totalbases, "\n")
  cat("# of At Bats: ", nrow(SimilarPitch),"\n")
  
  
  
  
  cat("Expected Bases: ", expectedbases)
  
  
  
  ### test by writing out Total bases and total number of pitches seen 
  ### make a bunch of test cases with various high volume and low volume 
  ### pitches to find threshold that we will use
  
  # add post condition for small data set with PlayResult != "Undefined"
  
  
  
  
  
}



