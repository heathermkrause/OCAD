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

data$Age <- data$NQ46
#gender

data$Gender <- data$NQ47
#ethnicity

data$Ethnicity <- 13
data$Ethnicity[data$RNQ54A==1]<-1
data$Ethnicity[data$RNQ54B==1]<-2
data$Ethnicity[data$RNQ54C==1]<-3
data$Ethnicity[data$RNQ54D==1]<-4
data$Ethnicity[data$RNQ54E==1]<-5
data$Ethnicity[data$RNQ54F==1]<-6
data$Ethnicity[data$RNQ54G==1]<-7
data$Ethnicity[data$RNQ54H==1]<-8
data$Ethnicity[data$RNQ54I==1]<-9
data$Ethnicity[data$RNQ54J==1]<-10
data$Ethnicity[data$RNQ54K==1]<-11
data$Ethnicity[data$RNQ54L==1]<-12

#sexual orientation
#international student status

data$International[data$NQ55==1] <-0
data$International[data$NQ55==2] <-1

#PAREQ physical activity guidelines (0 is not met)

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

#mental health service use
data$MHS1 <- 0
data$MHS1[data$NQ34A==2]<- 1

data$MHS2 <- 0
data$MHS2[data$NQ34B==2]<- 1

data$MHS3 <- 0
data$MHS3[data$NQ34C==2]<- 1

data$MHS4 <- 0
data$MHS4[data$NQ34D==2]<- 1

data$MHSSUM <- data$MHS1+data$MHS2+data$MHS3+data$MHS4

data$MHS <- 0
data$MHS[data$MHSSUM>0]<-1


#Abuse
data$AB1 <- 0
data$AB1[data$NQ6A==2]<- 1
data$AB2 <- 0
data$AB2[data$NQ6B==2]<- 1
data$AB3 <- 0
data$AB3[data$NQ6C==2]<- 1

data$ABSUM <- data$AB1+data$AB2+data$AB3
data$AB <- 0
data$AB[data$ABSUM>0]<-1

write.csv(data,"/Users/heatherkrause/Dropbox/Jennifer OCAD/OCAD data recoded.csv")


#Build the models

###################
#mental health
fit0 <- glm(SuicideBinary~MHC,family="binomial",data=data)

#mental health by sociodem

fit1 <- glm(SuicideBinary~MHC*Age,family="binomial",data=data)#no
fit1b <- glm(SuicideBinary~Age,family="binomial",data=data) #no
fit2 <- glm(SuicideBinary~MHC*as.factor(Ethnicity),family="binomial",data=data) #no
fit3 <- glm(SuicideBinary~MHC*Gender,family="binomial",data=data) #no
fit4 <- glm(SuicideBinary~MHC*as.factor(NQ48),family="binomial",data=data)#no
fit4b <- glm(SuicideBinary~as.factor(NQ48),family="binomial",data=data)#yes
fit5 <- glm(SuicideBinary~MHC*Gender*as.factor(NQ48),family="binomial",data=data)#no

#mental health by by mental health services

fit6 <- glm(SuicideBinary~MHC*MHS,family="binomial",data=data)#no

fit7 <- glm(SuicideBinary~MHS,family="binomial",data=data)#yes

####################
#physical health
fit8 <- glm(SuicideBinary~PHC*Age,family="binomial",data=data)#no
fit9 <- glm(SuicideBinary~PHC*as.factor(Ethnicity),family="binomial",data=data) #no
fit10 <- glm(SuicideBinary~PHC*Gender,family="binomial",data=data) #no
fit11 <- glm(SuicideBinary~PHC*as.factor(NQ48),family="binomial",data=data)#no
fit12 <- glm(SuicideBinary~PHC*Gender*as.factor(NQ48),family="binomial",data=data)#no


#physical health by sociodem

###################
#sleep disorders
fit13 <- glm(SuicideBinary~SD*Age,family="binomial",data=data)#no
fit14 <- glm(SuicideBinary~SD*as.factor(Ethnicity),family="binomial",data=data) #no
fit15 <- glm(SuicideBinary~SD*Gender,family="binomial",data=data) #no
fit16 <- glm(SuicideBinary~SD*as.factor(NQ48),family="binomial",data=data)#no
fit17 <- glm(SuicideBinary~SD*Gender*as.factor(NQ48),family="binomial",data=data)#no
fit13b <- glm(SuicideBinary~SD*AB,family="binomial",data=data)#no
fit13c <- glm(SuicideBinary~SD,family="binomial",data=data)#no

#sleep by socio

#sleep by mental health





###################
#Negative affect

fit23 <- glm(SuicideBinary~NAF,family="binomial",data=data)#no



#negative affect by socio

fit18 <- glm(SuicideBinary~NAF*Age,family="binomial",data=data)#no
fit19 <- glm(SuicideBinary~NAF*as.factor(Ethnicity),family="binomial",data=data) #no
fit20 <- glm(SuicideBinary~NAF*Gender,family="binomial",data=data) #no
fit21 <- glm(SuicideBinary~NAF*as.factor(NQ48),family="binomial",data=data)#no
fit22 <- glm(SuicideBinary~NAF*Gender*as.factor(NQ48),family="binomial",data=data)#no



#negative affect by mental health
fit24 <- glm(SuicideBinary~NAF*MHC,family="binomial",data=data)#no


#negative affect by mental health treatment



###################
#Abuse




#abuse by socio



#abuse by mental health

fit25 <- glm(SuicideBinary~MHC,family="binomial",data=data)#yes

fit30 <- glm(SuicideBinary~MHC+NQ5D,family="binomial",data=data)#yes
fit31 <- glm(SuicideBinary~MHC+NQ6A,family="binomial",data=data)#yes
fit32 <- glm(SuicideBinary~MHC+NQ6B,family="binomial",data=data)#yes
fit33 <- glm(SuicideBinary~MHC+NQ6C,family="binomial",data=data)#yes


fit34 <- glm(SuicideBinary~NQ48*NQ5D,family="binomial",data=data) #no
fit35 <- glm(SuicideBinary~NQ48*NQ6A,family="binomial",data=data)#no
fit36 <- glm(SuicideBinary~NQ48*NQ6B,family="binomial",data=data)#no
fit37 <- glm(SuicideBinary~NQ48*NQ6C,family="binomial",data=data)#no

fit38 <- glm(SuicideBinary~AB,family="binomial",data=data)#no

fit50 <- glm(SuicideBinary~NQ48,family="binomial",data=data)#no


#abuse by mental health service
NQ6C
NQ5D

fit41 <- glm(SuicideBinary~as.factor(NQ48)+AB+MHC,family="binomial",data=data)#best model


pred <- expand.grid(NQ48=c(1,2,3,4),AB=c(0,1),MHC=c(0,1),MHS=c(0,1),SD=c(0,1))
pred$pred41 <- predict(fit41,newdata=pred, type="response")
pred$pred0 <- predict(fit0,newdata=pred, type="response")
pred$pred4b <- predict(fit4b,newdata=pred, type="response")
pred$pred7 <- predict(fit7,newdata=pred, type="response")
pred$pred13c <- predict(fit13c,newdata=pred, type="response")
pred$pred38 <- predict(fit38,newdata=pred, type="response")

write.csv(pred,"/Users/heatherkrause/Dropbox/Jennifer OCAD/OCAD Preds.csv")

