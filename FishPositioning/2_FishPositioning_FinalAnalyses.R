##########################        Fish Positioning Final Analyses           ######################
# This script performs the statistical analyses of the relative location of each fish within 
# the culvert from screen capture images taken from underwater action camera footage.
# The cross sectional position of each fish has been normalized 
# this script includes tests for multivariate normality and DBSCAN cluster analysis

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set WD to folder where this script is stored

# checking that all required packages are installed and installing any that are missing
list.of.packages <- c("dplyr", "MVN", "dbscan")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(dplyr) # for data prep
library(MVN) # for multivariate normal test
library(dbscan) # for DBSCAN analysis


####   Prep   ####
Coords = read.csv("Data/Normalized/FishAndCulvertCornerCoordinates_Normalized.csv") # this table is created in "FishPositioningSetUp_NormalizingPixelCoordinates.R"
# this contains the coordinates of 4 corners of the culvert + center mass of fish (5 points for each image taken)

Coords$FishType = ifelse(Coords$Species == "RH", "Herring", "Residents") # defining fish as herring or non-herring (aka residents)


###### Data Prep  #####
# River herring positions only
RH = Coords %>%
  filter(Species == "RH" & Name == "Fish") %>% # River herring positions only
  select(X, Y, TurtleAvailable)

# Non-River herring fish only
NonRH = Coords %>%
  filter(Species != "RH" & Name == "Fish") %>% # River herring positions only
  select(X, Y, TurtleAvailable)




RH$TurtleAvailable = factor(RH$TurtleAvailable)
NonRH$TurtleAvailable = factor(NonRH$TurtleAvailable)







#####   MVN Analysis   #####

# River herring with turtle available or not available (both included)
RH_Result = mvn(data = RH, subset = "TurtleAvailable",
             mvnTest = "hz", univariateTest = "AD", 
             univariatePlot = "histogram", multivariatePlot = "qq", 
             multivariateOutlierMethod = "adj",showOutliers = TRUE, 
             showNewData = TRUE)
# Henze-Zirkler's multivariate normality test and Anderson Darling univariate test


RH_Result$multivariateNormality
RH_Result$univariateNormality
RH_Result$Descriptives



# Resident fishes (non river herring)
NonRH_Result = mvn(data = NonRH, subset = "TurtleAvailable",
             mvnTest = "hz", univariateTest = "AD", 
             univariatePlot = "histogram", multivariatePlot = "qq", 
             multivariateOutlierMethod = "adj",showOutliers = TRUE, 
             showNewData = TRUE)

NonRH_Result$multivariateNormality
NonRH_Result$univariateNormality
NonRH_Result$Descriptives

















#######    DBSCAN analysis   #######
# Using DBSCAN cluster analysis to test for clusters in the distribution of fish locations for each of the 4 groups (i.e. turtle present vs not X resident fish vs herring)

# Further subset data by Turtle available or not (need to run each of 4 groups separately)
  # River herring first
RH_T = subset(RH, TurtleAvailable == "Turtle")
RH_N = subset(RH, TurtleAvailable == "NoTurtle")
RH_Tmatrix <- as.matrix(RH_T[,1:2]) # need a matrix to input into dbscan (only keep x and y)
RH_Nmatrix <- as.matrix(RH_N[,1:2])

  # Non-river herring/Resident fish
NH_T = subset(NonRH, TurtleAvailable == "Turtle")
NH_N = subset(NonRH, TurtleAvailable == "NoTurtle")
NH_Tmatrix <- as.matrix(NH_T[,1:2])
NH_Nmatrix <- as.matrix(NH_N[,1:2])


# for dbscan, must define minimum number of points in a group and radius 
  # do not need to define number of groups, clusters can be any shape, can identify noise/outliers


## Produce a k-NN distance plot to determine a suitable eps for dbscan analysis
kNNdistplot(NH_Tmatrix, k=5) # knee at 5-NN distance of ~8. Use this for eps value

dbResult <- dbscan(NH_Tmatrix, eps = 8, minPts = 5) # identify clusters
dbResult # 2 clusters identified in the data with 4 noise points.



# Plot the results
pairs(NH_Tmatrix, col = dbResult$cluster+1L) # create plot of results
  # Two clusters with a few select outliers (which are very close to the groups)
  # Two groups are separated by left and right on x-axis, as expected.
  # This identifies a bimodal distribution about the x-axis


