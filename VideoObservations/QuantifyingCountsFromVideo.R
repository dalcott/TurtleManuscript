#####################              Counts from Video Analyses            ######################
# this script takes observations recorded from watching underwater culvert action camera videos
# and quantifies number of various events that occur, durations, gaps, etc.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set WD to folder where this script is stored

# checking that all required packages are installed and installing any that are missing
list.of.packages <- c("ggplot2", "dplyr", "readxl", "ggpubr", "Hmisc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(dplyr)
library(ggplot2)
library(readxl)
library(ggpubr)
library(Hmisc)



####   Prep   ####


# Read in and set up data frames
ObsData = read_xlsx("VideoBehaviorsDatasheet.xlsx", na = "NA") # Observations recording from viewing video files
PresData = read.csv("TurtlesPresenceTable.csv") # Camera on/off times and arrival/departure times of turtles within the culvert

ObsData$PhotFileName = as.character(ObsData$PhotFileName)
ObsData$Species = factor(as.character(ObsData$Species))
ObsData$EventTime = as.character(ObsData$EventTime)
ObsData$EventTime = substr(ObsData$EventTime, nchar(ObsData$EventTime)-8, nchar(ObsData$EventTime))

PresData$StartTime = as.POSIXct(PresData$StartTime, tz = "America/New_York")






#####   Camera Time   #######
# amount of time camera was on
Cam = subset(PresData, Event != "CameraOff") # Keep anything but camera off. If turtle arrive or departure, then camera was on (in addition to camera on with no turtles present)
Cam$StartTime = as.POSIXct(Cam$StartTime, tz = "America/New_York") # make DateTime object
Cam$Duration = difftime(Cam$EndTime, Cam$StartTime, units = "hours") # duration of each event in hours
sum(Cam$Duration) # 115.5 hours of camera footage recorded
sum(subset(Cam, Turtles > 0)$Duration) #31.6 hours of turtles on camera (sometimes more than one turtle present)





#####   Turtle Presences    #####
TurtlePres = filter(PresData, Turtles > 0) # only cases where at least 1 turtle is present within the culvert
TurtlePres$EndTime = as.POSIXct(TurtlePres$EndTime, tz = "America/New_York")
TurtlePres$Duration = difftime(TurtlePres$EndTime, TurtlePres$StartTime, units = "hours")

TurtlePres$TurtleTime = ifelse(TurtlePres$Turtles > 1, TurtlePres$Duration*TurtlePres$Turtles, TurtlePres$Duration) # if there is more than one turtle in the culvert, then count the amount of time present for each turtle (i.e. if 2 turtles spend 1 hour in the culvert together, than that is 2 hours worth of turtles present in the culvert, not 1 hour)
sum(TurtlePres$Duration) #31.6 hrs of turtles on video in 2016 (i.e. amount of time of footage that contains at least 1 turtle)
sum(TurtlePres$TurtleTime) # 35.7 hrs of combined turtle occupancy time.


# Individual number of turtle occupancies observed with camera (not including PIT data when camera was off)
nrow(subset(Cam, Event == "TurtleArrival")) # any turtle arrival is one new occupancy
nrow(subset(Cam, Event == "CameraOn" & Turtles > 0)) # each turtle present before start of camera is also a new occupancy

# Toal number of individual occupancies observed with camera
nrow(subset(Cam, Event == "TurtleArrival")) + nrow(subset(Cam, Event == "CameraOn" & Turtles > 0))
# 29



## Time with 1 turtle in culvert vs time with more than 1
Solo = filter(TurtlePres, Turtles == 1)
Multi = filter(TurtlePres, Turtles > 1)
sum(Solo$Duration) # total of 27.8 hours of one turtle alone in the culvert recorded
sum(Multi$Duration) # total of 3.8 hours of more than one turtle in the culvert recorded
27.8/(27.8+3.8) # 88% of the time only one turtle present
3.8/(27.8+3.8) # 12% of the time more than one turtle present









##### Comparing turtle presences to herring counts by Date ####
# Pulling out date from datetime
TurtlePres$FootageDate = substr(as.character(TurtlePres$StartTime), 1, 10)

a = TurtlePres %>%
  group_by(FootageDate) %>%
  summarise(TotalTime = sum(as.numeric(Duration)))
a$FootageDate = as.POSIXct(as.character(a$FootageDate), tz = "America/New_York")


ggplot(data = a, aes(x = FootageDate, y = TotalTime))+
  geom_line()+
  ylab('Time Turtle Present (h)')+
  xlab('Date')+
  scale_x_datetime(date_labels = "%m/%d") #use month/day format
  # Now look at this with herring counts for those days too

# herring counts by date
Hcounts = ObsData %>% 
  filter(!is.na(SchoolSize)) %>% # only include records of herring counts
  select(FootageDate, SchoolSize) %>% # only need footage date and size of herring school
  group_by(FootageDate) %>% 
  summarise(TotalFish = sum(SchoolSize))
Hcounts$FootageDate = as.POSIXct(as.character(Hcounts$FootageDate), tz = "America/New_York")


## Combine turtle and herring counts
b = merge(a, Hcounts, all = T)
b$TotalTime = ifelse(is.na(b$TotalTime), 0, b$TotalTime) # time of turtle present on that date
b$TotalFish = ifelse(is.na(b$TotalFish), 0, b$TotalFish) # number of fish observed on that date

## Add rows of 0 values for 0 turtles and 0 herring for remaining days of camera on that had no herring or turtle presences
Cam$FootageDate = substr(Cam$StartTime, 1,10)
CamDates = Cam %>% 
  group_by(FootageDate) %>% 
  slice(1) %>% 
  select(FootageDate) %>% 
  mutate(Missing = ifelse(FootageDate %in% as.character(b$FootageDate), 0, 1)) %>% # identify which dates had video recordings but are not in data frame b
  filter(Missing == 1)  

CamDates = CamDates %>% 
  ungroup() %>% # ungroup by FootageDate so that column can be modified
  mutate(TotalFish = 0, TotalTime = 0) %>%  # create new rows with 0's for these dates
  select(FootageDate, TotalTime, TotalFish) %>% 
  mutate(FootageDate = as.POSIXct(FootageDate, tz = "America/New_York"))

b = rbind(b, CamDates) # Add the 0 records to the rest of the data frame
  

# Make hours of turtle present in culvert plot
p1 = ggplot(data = b, aes(x = FootageDate, y = TotalTime))+
  geom_line()+
  ylab('Time Turtle Present (h)')+
  xlab('Date')+
  scale_x_datetime(breaks = seq(as.POSIXct("2016-05-16 00:00"),
                                as.POSIXct("2016-06-11 00:00"), "1 week"),
                                date_labels = "%m/%d", #use month/day format
                                limits = as.POSIXct(c("2016-05-16 00:00", "2016-06-11 00:00"))
                   ) 
p1 # turtle plot

# Make herring count plot
p2 = ggplot(data = b, aes(x = FootageDate, y = TotalFish))+
  geom_line()+
  ylab('Number Herring Present')+
  xlab('Date')+
  scale_x_datetime(breaks = seq(as.POSIXct("2016-05-16 00:00"),
                                as.POSIXct("2016-06-11 00:00"), "1 week"),
                   date_labels = "%m/%d",    #use month/day format
                   limits = as.POSIXct(c("2016-05-16 00:00", "2016-06-11 00:00"))
                   ) 
p2 # herring plot
p = ggarrange(p1, p2, nrow = 2, ncol=1) # make combined plot
p














######      Turtle Strikes      #######
# counting number of hit and miss attempts of turtles attacking herring

Miss = ObsData %>%
  filter(Species == "RH") %>%
  filter(CatchSuccess == 0) %>%
  select(FootageDate, EventTime, VideoFileName, Direction, SchoolSize, CatchSuccess) %>%
  mutate(DateTime = as.POSIXct(paste(FootageDate, EventTime, sep = " "), tz = "America/New_York"))
# 123 recorded missed attempts.
  


Hit = ObsData %>%
  filter(Species == "RH") %>%
  filter(CatchSuccess == 1) %>%
  select(FootageDate, EventTime, VideoFileName, Direction, SchoolSize) %>%
  mutate(DateTime = as.POSIXct(paste(FootageDate, EventTime, sep = " "), tz = "America/New_York"))
  # 21 records of turtle capturing herring
  
nrow(Hit)/(nrow(Hit)+nrow(Miss)) #15% catch rate.



## How many times did turtle not attempt an attack?
NoTry = ObsData %>%
  filter(Species == "RH" & TurtleAvailable == 1) %>%
  filter(Attacked == 0)
nrow(NoTry) - 1 # one case in this data frame the turtle's head was above water breathing, therefore turtle was not "available" for a potential strike at the time. Do not count that record


# No Attack attempted %
(nrow(NoTry)-1)/(nrow(Hit)+nrow(Miss)+(nrow(NoTry)-1)) 
  # 21% of the time, turtles did not attempt to strike. 

# Attack attempted %
1 - (nrow(NoTry)-1)/(nrow(Hit)+nrow(Miss)+(nrow(NoTry)-1))
    # 80% of the time turtles attempted an attack



#####  Non-Herring passed turtle  #####
# How many times did a non-herring fish swim passed a turtle and how many strike attempts by turtle?
NH = ObsData %>%
  filter(!(is.na(Species))) %>% # first, eliminate NAs
  filter(Species != "RH" & TurtleAvailable == 1) # only keep non RH fish, and make sure a turtle is present
  
sum(as.numeric(NH$Attacked)) # how many times did a turtle attempt to attack a non-RH fish?
  # turtle never even attempts to attack a non River Herring fish - UNKNOWN was a small mammal, likely a muskrat, not positively ID'd due to light availability







####  Size of School passing/rejecting  ####
# what proportion of school of herring pass/reject when attacked, not attacked, no turtle present?
Her = subset(ObsData, Species == "RH") # all RH observiations
Her$PercentPass = Her$NumHerringPass/Her$SchoolSize*100 # calculate percentage of herring successfully passing on a given event
Her$PercentReject = Her$NumHerringReject/Her$SchoolSize*100 # calculate percentage of herring rejecting on a given event

HerTurt = subset(Her, TurtleAvailable == 1) # Herring cases when turtle is available
HerNoTurt = subset(Her, TurtleAvailable == 0) # Herring cases when no turtle present

TurtSum = HerTurt %>%
  mutate(Flag = ifelse(SchoolSize == 1 & CatchSuccess == 1, 1,0)) %>% # remove cases where there's only one fish in the school and that fish is caught (thus none reject and none pass)
  filter(Flag != 1) %>% 
  group_by(Attacked) %>%  # grouped by whether or not school was attacked
  summarise(MeanPass = mean(PercentPass), MedPass = median(PercentPass), 
            MeanRej = mean(PercentReject), MedReject = median(PercentReject),
            PassSD = sd(PercentPass), RejSD = sd(PercentReject)) # actually more interested in SE than SD but SE too complicated right now

NoTurtSum = HerNoTurt %>%
  filter(!is.na(NumHerringPass)) %>% # remove any NAs
  summarise(MeanPass = mean(PercentPass), MedPass = median(PercentPass), 
            MeanRej = mean(PercentReject), MedReject = median(PercentReject),
            PassSD = sd(PercentPass), RejSD = sd(PercentReject)) # actually more interested in SE than SD but SE too complicated right now

Passes = c(sum(HerNoTurt$NumHerringPass, na.rm = T),
           sum(subset(HerTurt, Attacked == 0)$NumHerringPass, na.rm = T),
           sum(subset(HerTurt, Attacked == 1)$NumHerringPass, na.rm = T)
) # no turtle, then turtle but no attack, then turtle attacks

Rejects = c(sum(HerNoTurt$NumHerringReject, na.rm = T),
            sum(subset(HerTurt, Attacked == 0)$NumHerringReject, na.rm = T),
            sum(subset(HerTurt, Attacked == 1)$NumHerringReject, na.rm = T)
) # no turtle, then turtle but no attack, then turtle attacks


Contingency = data.frame(Passes, Rejects) # make contingency table
row.names(Contingency) = c('NoTurtle', 'TurtleNoAttack', 'TurtleAttack')

chisq <- chisq.test(Contingency)
chisq # these 3 proportions are not equal, p < 0.0001




### Two sample z-test of sample proportions (repeated for two direct comparisons)
# Comparing No turtle to Turtle present-no attack
res <- prop.test(x = c(Contingency[1,1], Contingency[2,1]), n = c((Contingency[1,1]+Contingency[1,2]), (Contingency[2,1]+Contingency[2,2]))) # x = number of successes of group 1, group 2; n = samples size (successes+failures) of group 1, group 2
res # highly significant at p < 0.0001

# Comparing Turtle present-No attack to Turtle present-Attacked
res2 <- prop.test(x = c(Contingency[2,1], Contingency[3,1]), n = c((Contingency[2,1]+Contingency[2,2]), (Contingency[3,1]+Contingency[3,2])))
res2 # highly significant at p < 0.0001

