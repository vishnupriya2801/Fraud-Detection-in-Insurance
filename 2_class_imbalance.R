#class imbalance smote


#Lets balance the data
#SMOTE

# SMOTE more positive cases
training_set_b$FraudFound_P <- as.factor(training_set_b$FraudFound_P)
training_set_b<- SMOTE(FraudFound_P ~ ., training_set, perc.over = 450, perc.under=150)
training_set_b$FraudFound_P <- as.numeric(training_set_b$FraudFound_P)
training_set_b[,32]=training_set_b[,32]-1
prop.table(table(training_set_b$FraudFound_P))

# evaluate the SMOTE performance
classifier2 = glm(formula = FraudFound_P ~ .,
                 family = binomial,
                 data = training_set_b)
training_set_b$Make[1989]<- 15
prob_pred2 = predict(classifier2, type = 'response', newdata = test_set[-32])
y_pred2 = ifelse(prob_pred2 > 0.5, 1, 0)

confusionMatrix(test_set$FraudFound_P, y_pred2,dnn=c("actual","pred"))

pred2 <- prediction(as.numeric(y_pred2),as.numeric(test_set$FraudFound_P))
perf<-performance(pred2,"tpr","fpr") #tpr=TP/P fpr=FP/N
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred2),as.numeric(test_set$FraudFound_P))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve-log_balanced")
