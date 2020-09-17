##################             Fish Positioning Data Set Up          ##############################
# this script reads in the raw x,y pixel coordinates of top left, top right, bottom left, and bottom right
# corners of the culvert as well as the coordinates of the fish position taken from screen grabs from underwater camera footage
# The raw pixel coordinates are rescaled and normalized for each photo to allow for comparisons and plotting (two separate scripts)
# The photo file name is matched to a database that associates the content of the photograph with fish species, turtle presence and other information related to the record.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set WD to folder where this script is stored

# checking that all required packages are installed and installing any that are missing
list.of.packages <- c("ggplot2", "dplyr", "readxl", "tidyr", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table) # used to combine list of data frames into one data frame
library(tidyr) # used for gather function
library(dplyr)
library(ggplot2)
library(readxl)




####   Prep   ####
# Read in detailed data about each observation
Observations = read_xlsx("../VideoObservations/VideoBehaviorsDatasheet.xlsx", na = "NA") # for each photo file name, this includes species, direction, size of school, etc.
Observations$PhotFileName = as.character(Observations$PhotFileName)
Observations = subset(Observations, is.na(Species) | Species != "UNKNOWN") # remove one record where species of animal attacked could not be identified and records where a fish did not enter culvert
Observations$Species = factor(as.character(Observations$Species))
# If CatchSuccess = "NA" when turtle was present, it means there was no attack attempted.




### Read in coordinates/positions data for each image files
file.list = list.files("Data/Raw/")

## Building the Raw DataFrame from the list of log files
RawCoord = NULL # create a blank data frame that you can fill in with data from the list of files created above

# For loop to
for (file.n in 1:length(file.list)) {      
  
  temp <- read.csv(paste("Data/Raw/", file.list[file.n], sep=""), header=T)      # reading the file  
  temp$FileName <- basename(file.list[file.n]) #include the name of the source file as the value in a new variable named "File"                                                                 
  RawCoord <- rbind(RawCoord, temp)                 # combine data of different files by binding the rows   --temp refers to the current datafile that was just read into the dataframe
}        
rm(temp) # remove this object, no longer needed

length(file.list) == nlevels(factor(RawCoord$FileName)) # All files accounted for if TRUE



####   Setting up initial database   ####
# Identify which of the 5 points of interest each coordinate pair is associated with (when recording coordinates, they were always recorded in the order: top-left, top-right, bottom-left, bottom-right corners, then center mass of fish)
RawCoord$Name = ifelse(RawCoord$X.1 == 1, "TopLeft",
                       ifelse(RawCoord$X.1 == 2, "TopRight", 
                              ifelse(RawCoord$X.1 == 3, "BottomLeft",
                                     ifelse(RawCoord$X.1 == 4, "BottomRight", 
                                            ifelse(RawCoord$X.1 == 5, "Fish", "OOPS") 
                                     )
                              )
                       )
)
# for each photo, location of all 4 corners of culvert + location of the centroid of the fish were recorded

# Position data are .csv files and image files are .jpg files with same name. Need to remove file extensions in order to match on file name.
Observations$PhotFileName = as.character(Observations$PhotFileName)
Observations$PhotFileName2 = substr(Observations$PhotFileName, 1, (nchar(Observations$PhotFileName)-4)) # remove .jpg file extension

RawCoord$FileName2 = substr(RawCoord$FileName, 1, (nchar(RawCoord$FileName)-4)) # remove .csv file extension

RawCoord = RawCoord %>%
  group_by(FileName2) %>%
  mutate(RowID = row_number()) # RowID shouldn't exceed 5 for any photo file. If it does, there's a mistake that will cause a problem



RawCoord = select(RawCoord, X, Y, Name, FileName2) # subset the coordinates data frame to only include variables of interest
# split the data frame of coordinates into a list of data frames where each data frame is all coordinates of one observation/record (5 points from one photo)
out <- split(RawCoord, f = RawCoord$FileName2) # list of data frames where each observation record is its own data frame. The data frame consists of x,y coordinates for each of the 5 points of interest, name of the point, and original file name


# Calculate distance matrix for each observation, store as a list of distance matrices that corresponds to the list of observations in "out"
DistList = lapply(out, function(x) dist(x)) #list of distance matrices cooresponding to each photo file in "out" list
  # warnings are OK


### Convert each distance matrix to a dataframe and store in a list of DFs where each dataframe is all of the distances between points for a single observation
# Distance matrix outputs will have distances between these points in this order.
DistNames = c('TopLeft-to-TopRight', 'TopLeft-to-BottomLeft', 'TopLeft-to-BottomRight', 'TopLeft-to-Fish',
              'TopRight-to-BottomLeft', 'TopRight-to-BottomRight', 'TopRight-to-Fish',
              'BottomLeft-to-BottomRight', 'BottomLeft-to-Fish',
              'BottomRight-to-Fish') # names of distance measurements 

#  Function to convert a distance matrix to a dataframe - apply to list of distance matrices
MatrixToDF = function(x, Names) { # x = dataframe, names = vector of names corresponding to the distance pairs
  Distance = as.numeric(x) # convert distance matrix to a numeric vector
  data = data.frame(Distance, Names)
}


Final = lapply(DistList, MatrixToDF, DistNames) # create final data frame by applying the matrix to data frame conversion function
# now need to add file name to each record in this data frame

dt<-rbindlist(out) # from data.table package - make a dataframe of the file names from the original coordinates list
dt1 = dt %>% group_by(FileName2) %>% slice(1) %>% select(FileName2) # only keep one record for each file name, just want an ordered list of file names
dt1 = pull(dt1, FileName2) # convert from Tibble object to a vector so we can apply the names in the next step

DistancesTable = rbindlist(Final) # Make final distances data table of observations of coordinates
DistancesTable$FileName2 = rep(dt1, each=10) # 10 records of distances for each observation, so repeat each file name 10 times, then move on to the next one
# This is a complete table of distance matrices between all 5 points of interest in each image for all images


# Now split DistancesTable back into a list by Observation for next steps
Data = split(DistancesTable, f = DistancesTable$FileName2)
# this is the list of dataframes where each dataframe is the distance matrix for a single image
# within the data frame is the distances between all pairs of points within the image (i.e. each of the 4 corners and the fish)











#####  Calculating Normalized Coordinate Pairs    #####
# Calculating normalized coordinate pairs for all 5 points of interest from each image
# by setting the distance between the bottom-left and top-right corners of the culvert
# to 100 and rescaling all other distances between all other points proportionally.
# then recalculating new coordinate pairs from common anchor points [e.g. bottom-left corner = origin (0,0)]

# Extract the distance between bottom-left and top-right corners of culvert and rescale to 100
ScaleLengths = DistancesTable %>%
  filter(Names == "TopRight-to-BottomLeft") %>% # base everything off of the bottom-left to top right diagonal 
  select(FileName2, Distance) # only need these two variables
# Now can match on the file names of this in the original distances dataframe

# Apply the diagonal length that will be set to 100 and all other distances rescaled based on that adjustment to the distances table
DistancesTable$ScaleLength = ScaleLengths$Distance[match(DistancesTable$FileName2, ScaleLengths$FileName2)]
# Calculate the adjusted distance values based on the scale adjustment
DistancesTable$DistAdj = DistancesTable$Distance*(100/DistancesTable$ScaleLength)
# This is the adjusted distances matrices for each observation
# now need to split the observations into a list of dataframes where each dataframe is its own dataframe
# then can apply function that calculates new/adjusted coordinates based on the normalized distances and bottom left = (0,0), so on..

Data = split(DistancesTable, f = DistancesTable$FileName2)





### Calculating new coordiante pairs 
# Create a blank template data frame for the coordinates of each corner and the fish (starting with 0's for all values)
Template = data.frame(Name = character(5), X = numeric(5), Y = numeric(5), FileName2 = character(5))
Template$Name = c("BottomLeft", "BottomRight", "TopLeft", "TopRight", "Fish")

Coordinates = list() # create an empty list before running a for loop to fill it
# For loop to create a list of these blank tables, one for each record
for(i in 1:length(Data)) {
  Template = data.frame(Name = character(5), X = numeric(5), Y = numeric(5), FileName2 = character(5))
  Template$Name = c("BottomLeft", "BottomRight", "TopLeft", "TopRight", "Fish")
  
  Coordinates[[i]] <- Template
}






## Calculating the new coordinates
for(i in 1:length(Coordinates)) {
  # Bottom-Left = (0,0), no need to calculate (already set as 0,0)
  
  # Calculate BottomRight coordinates
  Coordinates[[i]]$X[2] = Data[[i]]$DistAdj[8] # find the row where bottom left to bottom right distance is listed and enter it as row number
  # y cooridinate remains 0 for this record
  
  
  # Calculate TopLeft coordinates
  # first, define a needed constant for calculating top-left coordinates
  D = Data[[i]]$DistAdj[Data[[i]]$Names=="BottomLeft-to-BottomRight"]
  r1 = Data[[i]]$DistAdj[Data[[i]]$Names=="TopLeft-to-BottomLeft"]
  r2 = Data[[i]]$DistAdj[Data[[i]]$Names=="TopLeft-to-BottomRight"]
  
  q = (1/4)*sqrt( (D+r1+r2) * (D+r1-r2) * (D-r1+r2) * (-D+r1+r2) ) # calculating a constant that is used in the equation for the coordinates
  
  # letters a, b, and c were already used in quadratic equation in an earlier step. It should be ok to overwrite them here, but risks applying the wrong value if not kept up with. Safer to use a1, b1, c1 to separate them
  a1 = Coordinates[[i]]$X[Coordinates[[i]]$Name == "BottomLeft"] # this value will always be 0, but this explains where that value comes from instead of just tossing a 0 in there
  b1 = Coordinates[[i]]$Y[Coordinates[[i]]$Name == "BottomLeft"] # same as above
  c1 = Coordinates[[i]]$X[Coordinates[[i]]$Name == "BottomRight"] # this is the only one of these values that will be some value other than 0
  d1 = Coordinates[[i]]$Y[Coordinates[[i]]$Name == "BottomRight"] # this will also be 0
  #D (distance between the centers of circles, aka distance from BottomLeft to BottomRight) is reused from above
  
  Xtl1 = ((a1+c1)/2) + (((c1-a1) * (r1^2 - r2^2))/(2*D^2)) + (2*((b1-d1)/D^2)*q) # first possible value for x coordinate for top left (need to repeat with a minus sign for second possible value)
  Xtl2 = ((a1+c1)/2) + (((c1-a1) * (r1^2 - r2^2))/(2*D^2)) - (2*((b1-d1)/D^2)*q) # second possible value for x coordinate for top left (subtle change of + to -)
  
  Ytl1 = ((b1+d1)/2) + (((d1-b1) * (r1^2 - r2^2))/(2*D^2)) - (2*((a1-c1)/D^2)*q) # first possible value for y coordinate for top left (need to replace - for a + for the second possible value)
  Ytl2 = ((b1+d1)/2) + (((d1-b1) * (r1^2 - r2^2))/(2*D^2)) + (2*((a1-c1)/D^2)*q) # second possible value for y coordinate for top left (subtle cahnge of - to +)
  
  Coordinates[[i]]$X[Coordinates[[i]]$Name == "TopLeft"] = Xtl1
  Coordinates[[i]]$Y[Coordinates[[i]]$Name == "TopLeft"] = Ytl1
  
  # Top right y value = y value of Top Left (not exactly true but forces water surface to be parallel to ground. Will also push the calculated TopRight corner to be farther to the right than it actually is in real life)
  Coordinates[[i]]$Y[Coordinates[[i]]$Name == "TopRight"] = Coordinates[[i]]$Y[Coordinates[[i]]$Name == "TopLeft"]
  
  # Now need to calculate X-value of TopRight corner based off distance from TopLeft
  Coordinates[[i]]$X[Coordinates[[i]]$Name == "TopRight"] = sqrt(Data[[i]]$DistAdj[Data[[i]]$Names == "TopRight-to-BottomLeft"]^2 - (Coordinates[[i]]$Y[Coordinates[[i]]$Name=="TopRight"] - Coordinates[[i]]$Y[Coordinates[[i]]$Name=="BottomLeft"])^2)
  
  
  
  
  
  # Last, but not least, calculate coordinates of the fish. 
  
  ## setting up equations by defining complex expressions as simple letters to increase readability
  # first, define a needed constant for calculating top-left coordinates
  D = Data[[i]]$DistAdj[Data[[i]]$Names=="TopLeft-to-BottomRight"]
  r1 = Data[[i]]$DistAdj[Data[[i]]$Names=="TopLeft-to-Fish"]
  r2 = Data[[i]]$DistAdj[Data[[i]]$Names=="BottomRight-to-Fish"]
  
  q = (1/4)*sqrt( (D+r1+r2) * (D+r1-r2) * (D-r1+r2) * (-D+r1+r2) ) # calculating a constant that is used in the equation for the coordinates
  
  # letters a, b, and c were already used in quadratic equation in an earlier step. It should be ok to overwrite them here, but risks applying the wrong value if not kept up with. Safer to use a1, b1, c1 to separate them
  a1 = Coordinates[[i]]$X[Coordinates[[i]]$Name == "TopLeft"] # this value will always be 0, but this explains where that value comes from instead of just tossing a 0 in there
  b1 = Coordinates[[i]]$Y[Coordinates[[i]]$Name == "TopLeft"] # same as above
  c1 = Coordinates[[i]]$X[Coordinates[[i]]$Name == "BottomRight"] # this is the only one of these values that will be some value other than 0
  d1 = Coordinates[[i]]$Y[Coordinates[[i]]$Name == "BottomRight"] # this will also be 0
  #D (distance between the centers of circles, aka distance from BottomLeft to BottomRight) is reused from above
  
  Xf1 = ((a1+c1)/2) + (((c1-a1) * (r1^2 - r2^2))/(2*D^2)) + (2*((b1-d1)/D^2)*q) # first possible value for x coordinate for top left (need to repeat with a minus sign for second possible value)
  Xf2 = ((a1+c1)/2) + (((c1-a1) * (r1^2 - r2^2))/(2*D^2)) - (2*((b1-d1)/D^2)*q) # second possible value for x coordinate for top left (subtle change of + to -)
  
  Yf1 = ((b1+d1)/2) + (((d1-b1) * (r1^2 - r2^2))/(2*D^2)) - (2*((a1-c1)/D^2)*q) # first possible value for y coordinate for top left (need to replace - for a + for the second possible value)
  Yf2 = ((b1+d1)/2) + (((d1-b1) * (r1^2 - r2^2))/(2*D^2)) + (2*((a1-c1)/D^2)*q) # second possible value for y coordinate for top left (subtle cahnge of - to +)
  
  
  # Need to determine which of the two coordinate pairs is correct by testing if it correctly solves the distance equation for a third known location
  # Assigning the correct coordinates for the fish
  Coordinates[[i]]$X[Coordinates[[i]]$Name=="Fish"] = ifelse(is.na(sqrt(Data[[i]]$DistAdj[Data[[i]]$Names=="BottomLeft-to-Fish"]^2 - Xf1^2)), Xf2, # if this expression is NA, then it's not the right answer
                                                             ifelse(which.min(c((sqrt(Data[[i]]$DistAdj[Data[[i]]$Names=="BottomLeft-to-Fish"]^2 - Xf1^2)-Yf1), # comparing Xf1 plugged in to equation result to the estimated Yf1 value
                                                                                (sqrt(Data[[i]]$DistAdj[Data[[i]]$Names=="BottomLeft-to-Fish"]^2 - Xf2^2)-Yf2))) # comparing Xf2 plugged in to equation result to the estimated Yf2 value
                                                                    == 1, Xf1, Xf2) # if the first of these two is closer to the expected Yf1 value, then keep Xf1, if the second of these two is closer, then keep Xf2
                                                             # this allows for Yf1 to be ~0.0001 different from the result of plugging Xf1 into the equation and solving for Yf1, which is clearly better than when it is the wrong solution and Yf1 is ~30 different than calculated
  ) 
  
  
  Coordinates[[i]]$Y[Coordinates[[i]]$Name=="Fish"] = ifelse(Coordinates[[i]]$X[Coordinates[[i]]$Name=="Fish"] == Xf1, Yf1, Yf2) # keep Yf1 or Yf2 based on which of the two X values was correct.
} # end for loop
# Warnings are OK






NewData = rbindlist(Data) # make the list of individual dataframes into one dataframe

Coords = rbindlist(Coordinates) # make the list of individual dataframes into one dataframe


# Extracting file names from distance matrices
Files = NewData %>%
  group_by(FileName2) %>%
  slice(1) %>% # only keep one copy of each file name
  select(FileName2) # only keep this variable
Files = pull(Files, FileName2) # ungroup and convert to a vector

# Assign the correct file names 5 times each in order because each file name has 5 records (4 corners + 1 fish)
Coords$FileName2 = rep(Files, each=5)


# now bring in species data, etc for the observation
Observations$PhotFileName = as.character(Observations$PhotFileName)
Observations$FileName2 = substr(Observations$PhotFileName, 1, nchar(Observations$PhotFileName)-4) # remove the ".jpg" extension so the file names will match


Coords$TurtleAvailable = Observations$TurtleAvailable[match(Coords$FileName2, Observations$FileName2)]
Coords$TurtleAvailable = ifelse(Coords$TurtleAvailable == 0, "NoTurtle", "Turtle")

Coords$Species = Observations$Species[match(Coords$FileName2, Observations$FileName2)]
Coords$Direction = Observations$Direction[match(Coords$FileName2, Observations$FileName2)]
Coords$CatchSuccess = Observations$CatchSuccess[match(Coords$FileName2, Observations$FileName2)]

Coords_List = split(Coords, f = Coords$FileName2)




Files = Coords %>% group_by(FileName2) %>% slice(1)



#### Save the relevant data tables
write.csv(Coords, "Data/Normalized/FishAndCulvertCornerCoordinates_Normalized.csv") # a data frame of the normalized coordinates of the 4 corners of the culvert plus the fish for each image taken


