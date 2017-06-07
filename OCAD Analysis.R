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

data$MHC <- 0
data$MHC[data$MHCSUM>0]<- 1

#PHC Physical Health Condition (Binarity and Severity)
data$PHC1<- 0
data$PHC1[data$NQ41A1>1]<-1
data$PHC2<- 0
data$PHC2[data$NQ41A2>1]<-1
data$PHC3<- 0
data$PHC3[data$NQ41A3>1]<-1
data$PHC4<- 0
data$PHC4[data$NQ41A4>1]<-1
data$PHC5<- 0
data$PHC5[data$NQ41A5>1]<-1
data$PHC6<- 0
data$PHC6[data$NQ41A6>1]<-1
data$PHC7<- 0
data$PHC7[data$NQ41A7>1]<-1
data$PHC8<- 0
data$PHC8[data$NQ41A8>1]<-1
data$PHC9<- 0
data$PHC9[data$NQ41A9>1]<-1
data$PHC10<- 0
data$PHC10[data$NQ41A10>1]<-1
data$PHC11<- 0
data$PHC11[data$NQ41A11>1]<-1
data$PHC12<- 0
data$PHC12[data$NQ41A12>1]<-1
data$PHC13<- 0
data$PHC13[data$NQ41A13>1]<-1
data$PHC14<- 0
data$PHC14[data$NQ41B1>1]<-1
data$PHC15<- 0
data$PHC15[data$NQ41B2>1]<-1
data$PHC16<- 0
data$PHC16[data$NQ41B3>1]<-1
data$PHC17<- 0
data$PHC17[data$NQ41B4>1]<-1
data$PHC18<- 0
data$PHC18[data$NQ41B5>1]<-1
data$PHC19<- 0
data$PHC19[data$NQ41B6>1]<-1
data$PHC20<- 0
data$PHC20[data$NQ41B7>1]<-1
data$PHC21<- 0
data$PHC21[data$NQ41B8>1]<-1
data$PHC22<- 0
data$PHC22[data$NQ41B9>1]<-1
data$PHC23<- 0
data$PHC23[data$NQ41B10>1]<-1
data$PHC24<- 0
data$PHC24[data$NQ41B11>1]<-1
data$PHC25<- 0
data$PHC25[data$NQ41B12>1]<-1

data$PHCSUM <- data$PHC1+data$PHC2+data$PHC3+data$PHC4+data$PHC5+data$PHC6+data$PHC7+data$PHC8+data$PHC9+data$PHC10+
  data$PHC11+data$PHC12+data$PHC13+data$PHC14+data$PHC15+data$PHC16+data$PHC17+data$PHC18+data$PHC19+data$PHC20+
  data$PHC21+data$PHC22+data$PHC23+data$PHC24+data$PHC25

data$PHC <- 0
data$PHC[data$PHCSUM>0]<- 1

#SD Sleep Disturbance (Binary and Severity)
data$SD1 <-0
data$SD1[data$NQ31A7>1]<- 1
data$SD2 <-0
data$SD1[data$NQ31A8>1]<- 1

data$SDSUM <- data$SD1+data$SD2
data$SD <- 0
data$SD[data$SDSUM>0]<-1



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