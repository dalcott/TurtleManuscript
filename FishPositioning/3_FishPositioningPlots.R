########################            Fish Position Plots           ##############################
# Plotting the relative location of fish within the culvert from screen grabs from underwater action camera footage
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set WD to folder where this script is stored

# checking that all required packages are installed and installing any that are missing
list.of.packages <- c("ggplot2", "dplyr", "readxl", "ggpubr", "ggExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(dplyr) # for manipulating data
library(ggplot2) # for making plots
library(readxl) # for reading in excel file
library(ggExtra) # for ggMarginal function to add marginal histograms to the plots
library(ggpubr) # for ggarrange function to arrange multiple plots in one panel



####   Prep   ####
Coords = read.csv("Data/Normalized/FishAndCulvertCornerCoordinates_Normalized.csv") # this table is created in "FishPositioningSetUp_NormalizingPixelCoordinates.R"
  # this contains the coordinates of 4 corners of the culvert + center mass of fish (5 points for each image taken)

Fish = read_xlsx("../VideoObservations/VideoBehaviorsDatasheet.xlsx") # information regarding each observation including if fish was attacked, if attack was successful, etc.
Fish$FileName2 = substr(Fish$PhotFileName, 1, nchar(Fish$PhotFileName)-4) # remove file extension in order to match file names

Coords$TurtleAvailable = Fish$TurtleAvailable[match(Coords$FileName2, Fish$FileName2)] # add whether a turtle was available a the time or not to coordinates table
Coords$Species = Fish$Species[match(Coords$FileName2, Fish$FileName2)] # add fish species to coordinates table
Coords$CatchSuccess = Fish$CatchSuccess[match(Coords$FileName2, Fish$FileName2)] # add if turtle successfully captures fish to coordinates table
Coords$FishType = ifelse(Coords$Species == "RH", "Herring", "Residents") # defining fish as herring or non-herring (aka residents)


# Extracting the data to be used in the final plot
PlotData = Coords %>% 
  filter(Name == "Fish" & !is.na(Coords$TurtleAvailable)) # exclude cases where TurtleAvailable could not be determined

PlotData$CatchSuccess = Fish$CatchSuccess[match(PlotData$FileName2, Fish$FileName2)]
PlotData$CatchSuccess1 = ifelse(PlotData$CatchSuccess == "NA", "NoAttack", # if NA then no attack was attempted by turtle
                               ifelse(PlotData$CatchSuccess == 1, "FishCaptured" , "AttackMissed") # if 1 then attack successfully caught fish, if 0 then attack missed
)
PlotData$CatchSuccess1 = factor(PlotData$CatchSuccess1)
 

















################      Making Plots      ######################
# Create desired color palette
MyPal = c(AttackMissed = "#377EB8", FishCaptured = "#E41A1C", NoAttack = "#404040") # Missed = blue from RColorBrewer Set1, Captured = red from RColorBrewer Set1, No attempt = dark grey


# River herring with no turtle
p1_n = PlotData %>% 
  filter(Species == "RH" & TurtleAvailable == 0) %>%
  tally() # number of records in this group

p1_D = PlotData %>% 
  filter(Species == "RH" & TurtleAvailable == 0)


p1 = ggplot(p1_D, aes(x=X, y=Y, colour = CatchSuccess1)) + 
  geom_point(alpha=0.5, size=2)+
  scale_colour_manual(values = MyPal, drop = FALSE,
                      breaks = c("NoAttack", "AttackMissed", "FishCaptured"),
                      labels = c("No Attack", "Attack Missed", "Fish Captured"))+
  scale_x_continuous(limits = c(-20, 85), breaks = seq(-20,85,5))+
  scale_y_continuous(limits = c(-5, 55), breaks = seq(-5,55,5))+
  annotate("text", x = mean(p1_D$X), y = 54, label = paste0("n = ", p1_n$n), size = 6)+  
  theme(legend.text=element_blank(),             # font size of legend text
        legend.position = "none",
        legend.title = element_blank(),                # remove title of legend
        legend.key = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),            # major background grid lines removed
        panel.grid.minor = element_blank(),            # minor grid lines removed
        panel.background = element_blank(),            # remove grey background colour
        axis.line = element_blank(),
        axis.ticks = element_blank()
  ) 
p1 = ggMarginal(p1, type = "histogram")
p1  





# River herring with turtle available - with legend
p2_n = PlotData %>% 
  filter(Species=="RH" & TurtleAvailable== 1) %>%
  tally() # number of records in this group

p2_D = PlotData %>% 
  filter(Species=="RH" & TurtleAvailable== 1)


p2 = ggplot(subset(PlotData, Species=="RH" & TurtleAvailable== 1), aes(x=X, y=Y, colour = CatchSuccess1)) + 
  geom_point(alpha=0.5, size=2)+
  scale_colour_manual(values = MyPal, drop = FALSE,
                      breaks = c("NoAttack", "AttackMissed", "FishCaptured"),
                      labels = c("No Attack", "Attack Missed", "Fish Captured"))+
  scale_x_continuous(limits = c(-20, 85), breaks = seq(-20,85,5))+
  scale_y_continuous(limits = c(-5, 55), breaks = seq(-5,55,5))+
  annotate("text", x = mean(p2_D$X), y = 54, label = paste0("n = ", p2_n$n), size = 6)+  
  theme(legend.text=element_text(size=12),             # font size of legend text
        legend.title = element_blank(),                # remove title of legend
        legend.key = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),            # major background grid lines removed
        panel.grid.minor = element_blank(),            # minor grid lines removed
        panel.background = element_blank(),            # remove grey background colour
        axis.line = element_blank(),
        axis.ticks = element_blank()
  ) 
p2 = ggMarginal(p2, type = "histogram")
p2 # overwrite this object in next step


# River herring with turtle available - without legend for combining with other plots
p2 = ggplot(subset(PlotData, Species=="RH" & TurtleAvailable== 1), aes(x=X, y=Y, colour = CatchSuccess1)) + 
  geom_point(alpha=0.5, size=2)+
  scale_colour_manual(values = MyPal, drop = FALSE,
                      breaks = c("NoAttack", "AttackMissed", "FishCaptured"),
                      labels = c("No Attack", "Attack Missed", "Fish Captured"))+
  scale_x_continuous(limits = c(-20, 85), breaks = seq(-20,85,5))+
  scale_y_continuous(limits = c(-5, 55), breaks = seq(-5,55,5))+
  annotate("text", x = mean(p2_D$X), y = 54, label = paste0("n = ", p2_n$n), size = 6)+  
  theme(legend.text = element_blank(),             # font size of legend text
        legend.position = "none",
        legend.title = element_blank(),                # remove title of legend
        legend.key = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),            # major background grid lines removed
        panel.grid.minor = element_blank(),            # minor grid lines removed
        panel.background = element_blank(),            # remove grey background colour
        axis.line = element_blank(),
        axis.ticks = element_blank()
  ) 
p2 = ggMarginal(p2, type = "histogram")
p2



# Non-river herring with no turtle present
p3_n = PlotData %>% 
  filter(Species!="RH" & TurtleAvailable== 0) %>%
  tally() # number of records in this group

p3_D = PlotData %>% 
  filter(Species!="RH" & TurtleAvailable==0)

p3 = ggplot(subset(PlotData, Species!="RH" & TurtleAvailable==0), aes(x=X, y=Y, colour = CatchSuccess1)) + 
  geom_point(alpha=0.5, size=2)+
  scale_colour_manual(values = MyPal, drop = FALSE,
                      breaks = c("NoAttack", "AttackMissed", "FishCaptured"),
                      labels = c("No Attack", "Attack Missed", "Fish Captured"))+
  scale_x_continuous(limits = c(-20, 85), breaks = seq(-20,85,5))+
  scale_y_continuous(limits = c(-5, 55), breaks = seq(-5,55,5))+
  annotate("text", x = mean(p3_D$X), y = 54, label = paste0("n = ", p3_n$n), size = 6)+  
  theme(legend.text = element_blank(),             # font size of legend text
        legend.position = "none",
        legend.title = element_blank(),                # remove title of legend
        legend.key = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),            # major background grid lines removed
        panel.grid.minor = element_blank(),            # minor grid lines removed
        panel.background = element_blank(),            # remove grey background colour
        axis.line = element_blank(),
        axis.ticks = element_blank()
  ) 
p3 = ggMarginal(p3, type = "histogram")
p3




# Non-river herring with turtle present
p4_n = PlotData %>% 
  filter(Species!="RH" & TurtleAvailable== 1) %>%
  tally() # number of records in this group

p4_D = PlotData %>% 
  filter(Species!="RH" & TurtleAvailable== 1)


p4 = ggplot(subset(PlotData, Species!="RH" & TurtleAvailable== 1), aes(x=X, y=Y, colour = CatchSuccess1)) + 
  geom_point(alpha=0.5, size=2)+
  scale_colour_manual(values = MyPal, drop = FALSE,
                      breaks = c("NoAttack", "AttackMissed", "FishCaptured"),
                      labels = c("No Attack", "Attack Missed", "Fish Captured"))+
  scale_x_continuous(limits = c(-20, 85), breaks = seq(-20,85,5))+
  scale_y_continuous(limits = c(-5, 55), breaks = seq(-5,55,5))+
  annotate("text", x = mean(p4_D$X), y = 54, label = paste0("n = ", p4_n$n), size = 6)+  
  theme(legend.text = element_blank(),             # font size of legend text
        legend.position = "none",
        legend.title = element_blank(),                # remove title of legend
        legend.key = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),            # major background grid lines removed
        panel.grid.minor = element_blank(),            # minor grid lines removed
        panel.background = element_blank(),            # remove grey background colour
        axis.line = element_blank(),
        axis.ticks = element_blank()
  ) 
p4 = ggMarginal(p4, type = "histogram")
p4

# combine the 4 plots
p = ggarrange(p1, p2, p3, p4, nrow=2, ncol=2)
p # need to manually add labels for FishType and TurtleAvailable (ugly if done through ggarrange)
# also added lines + to divide the 4 quadrants

#ggsave("Plots/Figure1.png", width = 12, height = 10, units = "in", dpi = 500)


































#######   Vertical distribution of Non herring Upstm vs Downstream    #######
# This plot shows the vertical distribution in the water column of non-river herring fish
# with no turtle present swimming, comparing their position when swimming upstream vs downstream
# ggplot(subset(NonRH, TurtleAvailable == 0), 
#        aes(x=Y, fill=Direction)) + 
#   geom_density(alpha=.3)+ 
#   coord_flip()







###  Density cloud plot (rather than all data points)
# library(hexbin) # needed for colored density cloud plot
# HexPlot = subset(Coords, Name == "Fish" & !is.na(TurtleAvailable))
# ggplot(HexPlot, aes(x=X, y=Y))+
#   stat_binhex() +    # colored bins for density of points
#   facet_grid(FishType ~ TurtleAvailable)
