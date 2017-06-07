library(dplyr)
library(lubridate)
library(ggplot)
library(nlme)
library(lattice)
library(car)
library(spida2)

data <- read.csv("/Users/heatherkrause/Dropbox/R/OCAD/NCHA Jennifer.csv")

tab(data$NQ30I)
tab(data$NQ30J)
tab(data$NQ30K)
#NQ30J #considered suicide
#NQ30I #Injured self
#NQ30K #attempted suicide

#1 never; 2 not in last 12 month; all else is yes; 3 is yes in the last two weeks

data$SelfHarm <- 0
data$SelfHarm[data$NQ30I==3]<- 1
data$SelfHarm[data$NQ30I==4]<- 1
data$SelfHarm[data$NQ30I==5]<- 1


data$SuicideThought <- 0
data$SuicideThought[data$NQ30J==3]<- 1
data$SuicideThought[data$NQ30J==4]<- 1
data$SuicideThought[data$NQ30J==5]<- 1


data$SuicideAttempt <- 0
data$SuicideAttempt[data$NQ30K==3]<- 1
data$SuicideAttempt[data$NQ30K==4]<- 1
data$SuicideAttempt[data$NQ30K==5]<- 1

data$SuicideSum <- data$SuicideAttempt+data$SuicideThought+data$SelfHarm

data$SuicideBinary <- 0
data$SuicideBinary[data$SuicideSum>0]<- 1
