##########################                     Isotope Analyses                      #########################

# checking that all required packages are installed and installing any that are missing
list.of.packages <- c("ggplot2", "dplyr", "readxl", "rcompanion", "tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(rcompanion) # for scheirerRayHare test


####  Prep   #### 

## Stable isotope results
Isotopes = read_xlsx("StableIsotopeResults.xlsx", na = "NA") # C13 results from stable isotope analyses for each Sample ID number
SampleIDs = read.csv("SampleIDs.csv", na = "NA") # Sample ID numbers and associated biological data (species, date collected, location, etc.)


## Turtle PIT data
# PIT Tag ID numbers of turtles that were detected within culverts on the Herring River
CulvTurts = read.csv("CulvertTurtlesIDs.csv") # PIT tag ID numbers known to enter culverts


### Bringing sample information into results table
Isotopes = merge(Isotopes, SampleIDs, by = "SampleID")

Isotopes = arrange(Isotopes, Tissue, WaterbodyType, Location)

a = Isotopes %>% 
  filter(PIT %in% CulvTurts$PIT) %>% 
  group_by(PIT) %>% 
  slice(1)
# Changing the order of this factor so when plotted it will display in this order instead of alphabetical
Isotopes$Type2 <- factor(Isotopes$Type2, levels = c("LandlockedVeg", "HR-ConnectedVeg", "Herring", "CulvertTurtle", "Non-culvertTurtle", "LandLockedTurtle", "Uncertain"))

# End Prep  ####
















#####    Test 1 - Base of Food Web   ####
# Comparing mean C13 values of tissue types (i.e. floating algae, rooted aquatic plants, and river herring muscle)
# as well as pond type (landlocked vs Herring River-Connected)

## Subsetting data to only include desired samples
Test1Data = subset(Isotopes, Taxa == "Vegetation" | Taxa == "River herring") # only keep vegetation and herring samples


# Mann-Whitney-Wilcoxon Pairwise Test with Bonferroni correction
res = wilcox.test(C13 ~ Taxa, data=Test1Data, paired=FALSE, conf.int=TRUE) 
res
# p < 0.0001, reject the null that the distributions are equal.
# therefore, herring and aquatic vegetation differ (in this case by 5.68 - 7.92 with 95% CI)

pairwise.wilcox.test(Test1Data$C13, Test1Data$Type2, p.adjust.method="bonferroni")  # pair-wise comparison of means with Bonferroni p-value correction



















######    Test 2 - Comparing turtle samples     ######
Test2Data = Isotopes %>%
  filter(Taxa == "Snapping turtle" & Type2 != "Uncertain")  # keep only turtle blood samples and exclude individuals that were not clearly classified as either culvert or non-culvert
  
Test2Data = Test2Data %>% 
  mutate(BloodFraction = case_when(Type == "Plasma" ~ "Plasma/Whole", # group plasma and whole blood samples together
                                   Type == "RBC" ~ "RBC",
                                   Type == "Whole" ~ "Plasma/Whole",
                                   TRUE ~ "OOPS") # This could be done more efficiently by saying if Whole then Plasma, otherwise keep original
         )


## Comparing turtle blood sample C13 values 
scheirerRayHare(C13 ~ Type2 + BloodFraction, data = Test2Data)
    



## Wilcoxon-Mann-Whitney tests with Bonferoni correction post-hoc tests
# Must remove RBC fractions to avoid pseudo-replication
pairwise.wilcox.test(subset(Test2Data, Type != "RBC")$C13, subset(Test2Data, Type != "RBC")$Type2, p.adjust.method="bonferroni", conf.int = T, conf.level = 0.95)  # pair-wise comparison of means with Bonferroni p-value correction
  # this compares culvert vs. non-culvert vs. landlocked turtles in pairwise comparison

# Comparing plasma and RBC fractions of culvert turtles (evidence of recent diet shift)
res = wilcox.test(C13 ~ Type, data=subset(Test2Data, Type2 == "CulvertTurtle"), paired=FALSE, conf.int=TRUE) 
res # this compares the RBC vs. plasma values of culvert turtles for evidence of a diet shift in recent history








######   Creating plots    ########
library(ggpubr)
# two-panel plot, freshwater vs marine on one side; Turtle comparison on the other. Shared y-axis
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # color blind friendly palette



# Trying different approach to final figure
Isotopes$X = ifelse(Isotopes$Taxa == "Snapping turtle", as.character(Isotopes$Type2), as.character(Isotopes$Taxa)) # x-value for each group
Isotopes$Fill = ifelse(Isotopes$Taxa == "Snapping turtle", as.character(Isotopes$Type), "NA") # fill color grouping
Isotopes$Fill = ifelse(Isotopes$Fill == "Plasma", "Plasma/Whole", 
                       ifelse(Isotopes$Fill == "Whole", "Plasma/Whole", as.character(Isotopes$Fill))
                       ) # relabeling to combine Plasma and whole blood samples
Isotopes = subset(Isotopes, Type2 != "Uncertain")

levels(factor(Isotopes$X))
Isotopes$X = factor(Isotopes$X, levels = c("Vegetation", "River herring","LandLockedTurtle", "Non-culvertTurtle", "CulvertTurtle"))

# Sample sizes for plot
n1 = Test1Data %>%
  group_by(Taxa) %>%
  tally()

n2 = Test2Data %>%
  group_by(Type2, BloodFraction) %>%
  tally()

n = c(n1$n, n2$n)
n = data.frame(n)
n$Group = c("RiverHerring", "Vegetation", "Culvert-Plasma/Whole", "Culvert-RBC", "NonCulvAnad-Plasma", "NonCulvAnad-RBC", "Landlocked-Plasma/Whole", "Landlocked-RBC")
n = n[c(2,1,7,8, 5,6, 3,4),] # reorder sample size order to match the order of the plot

# NOTE: By default, ggplot2 geom_boxplot() uses the entire range of a group as the whisker values
# in a boxplot with only 2 data points, instead of excluding the whiskers. Whiskers are not
# appropriate when a data set only included 2 observations. Instead, the top and bottom of the 
# box should take the place of the two values in the data set and the median is the value halfway
# between those two points.
# This is the case for the RBC fraction of the non-culvert turtle group in our data.
# we believe visualizing this with whiskers is inappropriate. To display the data properly,
# we need to replicate the two values in the data set before plotting
z = filter(Isotopes, Fill == "RBC" & X == "Non-culvertTurtle") # extract the two records to be replicated
Isotopes1 = rbind(Isotopes, z) # add extracted records for this group to double the number of records
  # now the plot will correctly show the box without whiskers

FinalPlot2 = ggplot(data = Isotopes1, aes(y = Isotopes1$C13, x = Isotopes1$X, fill = Isotopes1$Fill)) +
  geom_boxplot(lwd = 0.05, position = position_dodge2(preserve = "single")) + # this line forces all boxes to be the same width and makes the factors that only have one box centered
  labs(y="\u03b4 C13", x = NULL) + # do not include an x-axis label (already group labels)
  scale_y_continuous(limits = c(-31,-17), breaks = seq(-17, -29, -2))+
  scale_x_discrete(breaks = c("Vegetation", "River herring","CulvertTurtle", "Non-culvertTurtle", "LandLockedTurtle"), 
                   labels = c("Vegetation", "Herring", "Culvert Turtle", "Non-culvert Turtle", "Landlocked Turtle")) + 
  scale_fill_manual(name="Blood Fraction",
                    values=c("#A4A4A4","#F0E442","#D55E00"),
                    breaks=c("NA","Plasma/Whole", "RBC"),
                    labels=c("NA","Plasma/Whole", "Red blood cell")) +
  annotate("text", x = c(1, 2, 2.8, 3.2, 3.8, 4.2, 4.8, 5.2), y = -17, label = paste0("n = ", n$n))+  
  theme(legend.text=element_text(size=14),             # font size of legend text
        axis.text = element_text(size = 12),           # font size of tick marks on axis
        axis.title=element_text(size=14,face="bold"),  # axis title font size
        title = element_text(size = 14, face="bold"),  # title font size
        panel.grid.major = element_blank(),            # major background grid lines removed
        panel.grid.minor = element_blank(),            # minor grid lines removed
        panel.background = element_blank(),            # remove grey background colour
        axis.line = element_line(colour = "black"),    # make axis lines black
        legend.key = element_blank())                  # remove background colour of legend keys
FinalPlot2

ggsave("Plots/Figure2.jpg", width = 10, height = 6, units = "in")
ggsave("Plots/Figure2.tiff", width = 10, height = 6, units = "in")
















































































### The assumptions of ANOVA were tested to determine if parametric comparisons could be performed
# These data violated assumptions of ANOVA, thus we had to use non-parametric methods.

### Testing the assumptions of ANOVA for Test 1
## Assumption #1: Experimental errors are normally distributed 
# Shapiro-Wilks test of normality
resids = residuals(lm(Result ~ WaterbodyType*Type, data=Plants)) # create a residuals object
shapiro.test(resids) # p = 0.081 > 0.05, fail to reject the null that errors are normally distributed
  # pass (just barely)

# examine qqplot
qqnorm(Plants$Result)
qqline(Plants$Result)



## Assumption #2: Equal variance between treatments
# Bartlett test for equal variances
  # must test for each treatment one at a time (i.e. pond type, then plant type)

  # Pond type equal variances test for plants
bartlett.test(Plants$Result ~ Plants$WaterbodyType)


  # Plant type equal variances test
bartlett.test(Plants$Result ~ Plants$Type)


# Fligner test for equal variances (another test)
fligner.test(Plants$Result~Plants$WaterbodyType) 
fligner.test(Plants$Result~Plants$Type) 






### Testing the assumptions of ANOVA for Test2

## Assumption #1: Experimental errors are normally distributed 
# Shapiro-Wilks test of normality
resids = residuals(lm(Result ~ Type2, data = FinalTurt)) # create a residuals object
shapiro.test(resids) 



## Assumption #2: Equal variance between treatments
# Bartlett test for equal variances
# must test for each treatment one at a time (i.e. pond/location type, then blood fraction)

# Pond/location type equal variances test for turtles (culvert turtle vs. non-culvert anadromous vs. landlocked vs. uncertain)
bartlett.test(FinalTurt$Result ~ FinalTurt$Type2)







### Testing Assumptions
# 1. Homogeneity of variances
plot(res, 1) 
# Levene's test
library(car)
leveneTest(Result ~ Type2*Type, data = subset(Isotopes, TissueType == "Turtle" & IsotopeType == "C13"))

# 2. Normality
plot(res, 2) # there are two points well off of the line. May need to remove these to avoid violating the assumption
# Shapiro-Wilk test on residuals
# Extract the residuals
aov_residuals <- residuals(object = res)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals ) # this fails the Shaprio-Wilk normality test, confirming those two points need to be removed.


### Testing assumptions of ANOVA to see if we can run ANOVA on these samples
## Assumption #1: Experimental errors are normally distributed 
# Shapiro-Wilks test of normality
resids = residuals(lm(Result ~ WaterbodyType + Type, data=Test1Data)) # create a residuals object
shapiro.test(resids) 

# checking sample size of other groups
a = Test1Data %>%
  group_by(WaterbodyType, Type) %>%
  summarise(n = n())


### Assumption #2: Equal variance between treatments
## Bartlett test for equal variances
# must test for each treatment one at a time (i.e. pond type, then plant type)

# Pond type equal variances test for plants
bartlett.test(Test1Data$Result ~ Test1Data$WaterbodyType)
# p = 0.177, fail to reject null that variances are equal between treatments 


# Plant type equal variances test
bartlett.test(Test1Data$Result ~ Test1Data$Type)

### Conclusion - we violate 2 assumptions of ANOVA, therefore cannot use ANOVA to compare means
