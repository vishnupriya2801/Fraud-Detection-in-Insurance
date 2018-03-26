# Fitting Logistic Regression to the Training set
classifier1 = glm(formula = FraudFound_P ~ .,
                 family = binomial,
                 data = training_set)
?glm
# Predicting the Test set results
prob_pred1 = predict(classifier1, type = 'response', newdata = test_set[-32])
y_pred1 = ifelse(prob_pred1 > 0.5, 1, 0)

# Making the Confusion Matrix
c=confusionMatrix(test_set$FraudFound_P, y_pred1,dnn=c("actual","pred"))
c

pred1 <- prediction(as.numeric(y_pred1),as.numeric(test_set$FraudFound_P))
perf1<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred1),as.numeric(test_set$FraudFound_P))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve-log")

# our model has classified all the 231 fraud but incorrectly and 2 items as 1 when 0
