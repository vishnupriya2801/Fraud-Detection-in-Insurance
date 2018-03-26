getwd()
setwd("/Users/vishnupriyakodavatiganti/Desktop/fraud")

#cats = apply(newtea, 2, function(x) nlevels(as.factor(x)))


#install.packages('caTools')
library(caTools)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("caret")
library(caret)
#install.packages("pROC")
library(pROC)
#install.packages("ROCR")
library(ROCR)
#install.packages("DMwR")
library(DMwR)
library(corrplot)
library(dplyr)
library(RColorBrewer)
#install.packages("rattle")
library(rattle)
library(ROCR)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(rpart.plot)
library(rpart)
library(caTools)
##install.packages('e1071')
library(e1071)
#reading data
data=read.csv("claims.csv")
data=data[-16]#plocy number is identity
head(data)
#unique(data$NumberOfSuppliments)

#bar plot
histogram(training_set_b$FraudFound_P)
# Encoding the categorical variables to factors
data$Month = factor(data$Month,
                                  levels = c('Jan', 'Feb', 'Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),
                                  labels = c(1, 2, 3,4,5,6,7,8,9,10,11,12))
data$MonthClaimed = factor(data$MonthClaimed,
                               levels = c('0','Jan', 'Feb', 'Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),
                               labels = c(0,1, 2, 3,4,5,6,7,8,9,10,11,12))

data$DayOfWeek = factor(data$DayOfWeek,
                               levels = c('Monday', 'Tuesday', 'Wednesday','Thursday','Friday','Saturday','Sunday'),
                               labels = c(1, 2, 3,4,5,6,7))

data$DayOfWeekClaimed = factor(data$DayOfWeekClaimed,
                                   levels = c('0','Monday', 'Tuesday', 'Wednesday','Thursday','Friday','Saturday','Sunday'),
                                   labels = c(1, 2,3,4,5,6,7,8))


data$AccidentArea = factor(data$AccidentArea,
                                   levels = c('Urban', 'Rural'),
                                   labels = c(1, 2))


data$Sex = factor(data$Sex,
                                      levels = c('Male', 'Female'),
                                      labels = c(1, 2))

data$MaritalStatus = factor(data$MaritalStatus,
                             levels = c('Single', 'Married','Widow','Divorced'),
                             labels = c(1, 2,3,4))

data$Fault = factor(data$Fault,
                             levels = c('Policy Holder', 'Third Party'),
                             labels = c(1, 2))

data$BasePolicy = factor(data$BasePolicy,
                               levels = c('Liability', 'Collision','All Perils'),
                               labels = c(1, 2,3))

data$NumberOfCars = factor(data$NumberOfCars,
                                    levels = c('1 vehicle', '2 vehicles','3 to 4','5 to 8','more than 8'),
                                    labels = c(1, 2,3,4,5))

data$AddressChange_Claim = factor(data$AddressChange_Claim,
                                      levels = c('no change', '1 year','under 6 months','2 to 3 years','4 to 8 years'),
                                      labels = c(1, 2,3,4,5))
data$NumberOfSuppliments = as.numeric(factor(data$NumberOfSuppliments,
                                             levels = c('none', '1 to 2','3 to 5','more than 5'),
                                             labels = c(1,2,3,4)))

data$AgentType = factor(data$AgentType,
                               levels = c('External', 'Internal'),
                               labels = c(1, 2))

data$WitnessPresent =factor(data$WitnessPresent,
                                   levels = c('Yes', 'No'),
                                   labels = c(1, 2))
data$PoliceReportFiled = factor(data$PoliceReportFiled,
                                   levels = c('Yes', 'No'),
                                   labels = c(1, 2))
data$PastNumberOfClaims = factor(data$PastNumberOfClaims,
                                           levels = c('none', '1','2 to 4','more than 4'),
                                           labels = c(1, 2,3,4))

data$Days_Policy_Claim = factor(data$Days_Policy_Claim,
                                            levels = c('none','8 to 15','15 to 30','more than 30'),
                                            labels = c(1, 2,3,4))


data$Days_Policy_Accident = factor(data$Days_Policy_Accident,
                                           levels = c('none', '1 to 7','8 to 15','15 to 30','more than 30'),
                                           labels = c(1, 2,3,4,5))

data$AgeOfVehicle = factor(data$AgeOfVehicle,
                                          levels = c('new', '2 years', '3 years','4 years','5 years','6 years','7 years','more than 7'),
                                          labels = c(1, 2, 3,4,5,6,7,8))

data$AgeOfPolicyHolder = factor(data$AgeOfPolicyHolder,
                                      levels = c('16 to 17', '18 to 20', '21 to 25','26 to 30','31 to 35','36 to 40','41 to 50','51 to 65','over 65'),
                                      labels = c(1, 2, 3,4,5,6,7,8,9))
data$VehiclePrice = factor(data$VehiclePrice,
                                              levels = c('less than 20000', '20000 to 29000','30000 to 39000','40000 to 59000','60000 to 69000','more than 69000'),
                                              labels = c(1, 2,3,4,5,6))
data$PolicyType = factor(data$PolicyType,
                                           levels = c('Sport - Liability', 'Sport - Collision', 'Sedan - Liability','Utility - All Perils','Sedan - All Perils','Sedan - Collision','Utility - Collision','Utility - Liability','Sport - All Perils'),
                                           labels = c(1, 2, 3,4,5,6,7,8,9))
data$VehicleCategory = factor(data$VehicleCategory,
                                            levels = c('Sport', 'Sedan','Utility'),
                                            labels = c(1, 2,3))
data$Make = factor(data$Make,
                                    levels = c('Honda', 'Toyota', 'Ford','Mazda','Chevrolet','Pontiac','Accura','Dodge','Mercury','Jaguar','Nisson','VW','Saab','Saturn','Porche','BMW','Mecedes','Ferrari','Lexus'),
                                    labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
#unique(data$Make)
#checking for nulls induced
#drop=na.omit(data)

#checking class numeric and factor target
sapply(data,class)
data$Year=as.factor(data$Year)
data$FraudFound_P=as.factor(data$FraudFound_P)

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(data$FraudFound_P, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

# Feature Scaling
training_set[c(11,17)] = scale(training_set[c(11,17)])
test_set[c(11,17)] = scale(test_set[c(11,17)])

table(test_set$FraudFound_P)
base_accuracy=3624/(3624+231)
base_accuracy*100
