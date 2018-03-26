# Decide if a variable is important or not using Boruta
#runs randon forest in the background
install.packages("Boruta")

library(Boruta)

boruta_output <- Boruta(FraudFound_P ~ ., data=data, doTrace=2)  # perform Boruta search
boruta_output$finalDecision
boruta_output$ImpHistory
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% "Confirmed"])  # collect Confirmed and Tentative variables
boruta_signif 
plot(boruta_output,sort=TRUE, cex.axis=.5, las=2, xlab="", main="Variable Importance")  # plot variable importance
idx <- match(boruta_signif, names(training_set))
idx=c(1,2,4,7,8,10,11,12,13,14,17,21,22,23,28,29,30,31,32)
#idx=append(idx,32)
idx
#1  2  4  7  8 10 11 12 13 14 15 16 18 22 23 24 29 30 31 32 33
# 1  2  4  7  8 10 11 12 13 14 17 21 22 23 28 29 30 31 32
data2=data[,idx]
set.seed(123)
split = sample.split(data2$FraudFound_P, SplitRatio = 0.75)
training_set3 = subset(data2, split == TRUE)
test_set3 = subset(data2, split == FALSE)
# Feature Scaling
training_set3[c(7,11)] = scale(training_set3[c(7,11)])
test_set3[c(7,11)] = scale(test_set3[c(7,11)])

training_set3<- SMOTE(FraudFound_P ~ ., training_set3, perc.over = 450, perc.under=150)
#no nisson and BMW changing
