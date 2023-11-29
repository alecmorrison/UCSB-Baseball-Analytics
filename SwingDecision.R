install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
library(readr)


table <- read_csv("All_College_TM_19_22.csv")

head(table)

is.data.frame(table)

View(table)


##################SCTRATCH WORK###########################


#Fastballs <- filter(table, TaggedPitchType == "Fastball")
#View(Fastballs)

#Speed <- filter(Fastballs, RelSpeed >= 87 & RelSpeed <= 95)
#View(Speed)

#cutter <- filter(table, TaggedPitchType == "Curveball")
#View(cutter)



# Left <- filter(table, BatterSide == "Left")
# Right <- filter(table, BatterSide == "Right")
# x = nrow(Left) + nrow(Right)

## we can omit the 32 undefined ones


##########################################################


# Function to filter by similar pitch Type
# AND calculate Rating for each pitch by Expected bases and SLG formula

PitchRating <- function(pitchtype, minspeed, maxspeed, xmin, xmax, ymin, ymax) {
  
  # POSS STANDARDIZE PITCH LOCATIONS TO MAKE INPUT "lowinside", "lowoutside" etc. FIRST
  
  
  #### IF (|xmin| == |xmax|), then filter LIKE SO:
  
  if (abs(xmin) == abs(xmax)) {
    defined <- filter(table, BatterSide != "Undefined")
    Pitch <- filter(defined, TaggedPitchType == pitchtype)
    Speed <- filter(Pitch, RelSpeed >= minspeed & RelSpeed <= maxspeed)
    Location <- filter(Speed, PlateLocHeight  >= ymin & PlateLocHeight <= ymax & PlateLocSide >= xmin & PlateLocSide <= xmax)
    View(Location)
    
    # EXPECTED BASES
  
    ExpectedBases = 0
    for (i in 1:nrow(Location)) {
      if (Location$PlayResult[i] == "Single") {
        ExpectedBases = ExpectedBases + 1
      }
      if (Location$PlayResult[i] == "Double") {
        ExpectedBases = ExpectedBases + 2
      }
      if (Location$PlayResult[i] == "Triple") {
        ExpectedBases = ExpectedBases + 3
      }
      if (Location$PlayResult[i] == "HomeRun") {
        ExpectedBases = ExpectedBases + 4
      }
    }
    return(ExpectedBases)
  
  }
  
  
  #### ELSE (meaning its an inside or outside pitch so reverse for right/left hitters)
  
  ##### STILL NEED TO FIX X COORDS FOR LEFT/RIGHT HANDED #######
  
  else {
  
  # FILTER FOR RIGHT HANDED HITTERS
    Right <- filter(table, BatterSide == "Right")
    RPitch <- filter(Right, TaggedPitchType == pitchtype)
    RSpeed <- filter(RPitch, RelSpeed >= minspeed & RelSpeed <= maxspeed)
    RLocation <- filter(RSpeed, PlateLocHeight  >= ymin & PlateLocHeight <= ymax & PlateLocSide >= xmin & PlateLocSide <= xmax)
    View(RLocation)
  
  
  
  # FILTER FOR LEFT HANDED HITTERS
    Left <- filter(table, BatterSide == "Left")
    LPitch <- filter(Left, TaggedPitchType == pitchtype)
    LSpeed <- filter(LPitch, RelSpeed >= minspeed & RelSpeed <= maxspeed)
    LLocation <- filter(LSpeed, PlateLocHeight  >= ymin & PlateLocHeight <= ymax & PlateLocSide >= xmin & PlateLocSide <= xmax)
    View(LLocation)
  
  
  # EXPECTED BASES FOR RIGHTIES
  
    RExpectedBases = 0
    for (i in 1:nrow(RLocation)) {
      if (RLocation$PlayResult[i] == "Single") {
        RExpectedBases = RExpectedBases + 1
      }
      if (RLocation$PlayResult[i] == "Double") {
        RExpectedBases = RExpectedBases + 2
      }
      if (RLocation$PlayResult[i] == "Triple") {
        RExpectedBases = RExpectedBases + 3
      }
      if (RLocation$PlayResult[i] == "HomeRun") {
        RExpectedBases = RExpectedBases + 4
      }
    }
 
  
  
  #### EXPECECTED BASES FOR LEFTIES
    LExpectedBases = 0
    for (i in 1:nrow(LLocation)) {
      if (LLocation$PlayResult[i] == "Single") {
        LExpectedBases = LExpectedBases + 1
      }
      if (LLocation$PlayResult[i] == "Double") {
        LExpectedBases = LExpectedBases + 2
      }
      if (LLocation$PlayResult[i] == "Triple") {
        LExpectedBases = LExpectedBases + 3
      }
      if (LLocation$PlayResult[i] == "HomeRun") {
        LExpectedBases = LExpectedBases + 4
      }
    }
    
    
    ExpectedBases = LExpectedBases + RExpectedBases
    return(ExpectedBases)
    
    
  }
  
}



### TESTS 

PitchRating("Fastball", 87, 95, -.5, .5, 2.4, 2.6)

PitchRating("Fastball", 87, 95, -.5, .5, 3.4, 3.6)







