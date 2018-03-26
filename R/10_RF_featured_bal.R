
classifier9 <- rpart(formula = FraudFound_P~.,
                     data = training_set3,method = "class",
                     parms = list(split="gini"))

# Predicting the Test set results
y_pred3 = predict(classifier9, newdata = test_set3[-19],type="class")

histogram(training_set3$FraudFound_P)

# Confusion Matrix
confusionMatrix(test_set3$FraudFound_P, y_pred3,positive = "1",dnn=c("actual","pred"))

# ROC Curve

par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred3),as.numeric(test_set$FraudFound_P))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - Decision Tree")

printcp(classifier3)

