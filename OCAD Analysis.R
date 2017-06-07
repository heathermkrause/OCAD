library(dplyr)
library(lubridate)
library(ggplot)
library(nlme)
library(lattice)
library(car)
library(spida2)

data <- read.csv("/Users/heatherkrause/Dropbox/R/OCAD/NCHA Jennifer.csv")


#creating the suicide variable for dependent variable 
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


#Formatting predictor variables

#age
#gender
#ethnicity
#sexual orientation
#international student status
#MHC Mental Health Condition (Binary and Severity)
data$MHC1 <-0
data$MHC1[data$NQ31A1>1]<- 1
data$MHC2 <-0
data$MHC2[data$NQ31A1>1]<- 1
data$MHC3 <-0
data$MHC3[data$NQ31A1>1]<- 1
data$MHC4 <-0
data$MHC4[data$NQ31A1>1]<- 1
data$MHC5 <-0
data$MHC5[data$NQ31A2>1]<- 1
data$MHC6 <-0
data$MHC6[data$NQ31A3>1]<- 1
data$MHC7 <-0
data$MHC7[data$NQ31A4>1]<- 1
data$MHC8 <-0
data$MHC8[data$NQ31A5>1]<- 1
data$MHC9 <-0
data$MHC9[data$NQ31A6>1]<- 1
data$MHC10 <-0
data$MHC10[data$NQ31A7>1]<- 1
data$MHC11 <-0
data$MHC11[data$NQ31A8>1]<- 1
data$MHC12 <-0
data$MHC12[data$NQ31B1>1]<- 1
data$MHC13 <-0
data$MHC13[data$NQ31B2>1]<- 1
data$MHC14 <-0
data$MHC14[data$NQ31B3>1]<- 1
data$MHC15 <-0
data$MHC15[data$NQ31B4>1]<- 1
data$MHC16 <-0
data$MHC16[data$NQ31B5>1]<- 1
data$MHC17 <-0
data$MHC17[data$NQ31B6>1]<- 1
data$MHC18 <-0
data$MHC18[data$NQ31B7>1]<- 1

data$MHCSUM <- data$MHC1+data$MHC2+data$MHC3+data$MHC4+data$MHC5+data$MHC6+
  data$MHC7+data$MHC8+data$MHC9+data$MHC10+data$MHC11+data$MHC12+data$MHC13+
  data$MHC14+data$MHC15+data$MHC16+data$MHC17+data$MHC18



#PHC Physical Health Condition (Binarity and Severity)
data$PHC1<- 0
data$PHC1[data$NQ41A1>1]<-1

#SD Sleep Disturbance (Binary and Severity)



#NAF Negative Affect (Binary and Severity)
#hopeless overwhelmed lonely very sad depressed to function overhwlm anxiety wverhwelm anger

data$NAF1 <- 0
data$NAF1[data$NQ30A>2] <- 1
data$NAF2 <- 0
data$NAF2[data$NQ30B>2] <- 1
data$NAF3 <- 0
data$NAF3[data$NQ30C>2] <- 1
data$NAF4 <- 0
data$NAF4[data$NQ30D>2] <- 1
data$NAF5 <- 0
data$NAF5[data$NQ30E>2] <- 1
data$NAF6 <- 0
data$NAF6[data$NQ30F>2] <- 1
data$NAF7 <- 0
data$NAF7[data$NQ30G>2] <- 1
data$NAF8 <- 0
data$NAF8[data$NQ30H>2] <- 1

data$NAFSUM <- data$NAF1+data$NAF2+data$NAF3+data$NAF4+
  data$NAF5+data$NAF6+data$NAF7+data$NAF8

data$NAF <- 0
data$NAF[data$NAFSUM>0]<- 1