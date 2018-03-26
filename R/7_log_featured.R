
# Fitting Logistic Regression to the Training set
classifier8 = glm(formula = FraudFound_P ~ .,
                 family = binomial,
                 data = training_set3)

unique(training_set3$Make)
training_set3$NumberOfCars[1920]<- 5
# Predicting the Test set results
prob_pred = predict(classifier8, type = 'response', newdata = test_set3[-19])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set3[, 19], y_pred > 0.5)
cm
c=confusionMatrix(test_set$FraudFound_P, y_pred,positive = "1",dnn=c("actual","pred"))
c
pred1 <- prediction(as.numeric(y_pred),as.numeric(test_set$FraudFound_P))
perf<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred),as.numeric(test_set$FraudFound_P))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve-f_b_log")

# our model has classified all the 231 fraud but incorrectly
