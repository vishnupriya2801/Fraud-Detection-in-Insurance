
#scaled-balanced-feature selection data2
install.packages('e1071')
library(e1071)

#linear kernel
classifier.l = svm(formula = FraudFound_P ~ .,
                   data = training_set3,
                   type = 'C-classification',
                   kernel = 'linear')

# Predicting the Test set results
y_pred.l = predict(classifier.l, newdata = test_set3[-19])
# Making the Confusion Matrix
confusionMatrix(test_set3[, 19], y_pred.l,dnn=c("actual","pred"))

#Plotting RoC Curve 
pred1 <- prediction(as.numeric(y_pred.l),as.numeric(test_set$FraudFound_P))
perf<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred.l),as.numeric(test_set$FraudFound_P))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
ROCRperf
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
auc
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - fb_svm.l")


#radial kernel
classifier.r = svm(formula = FraudFound_P ~ .,
                   data = training_set3,
                   type = 'C-classification',
                   kernel = 'radial')

# Predicting the Test set results
y_pred.r = predict(classifier.r, newdata = test_set3[-19])
# Making the Confusion Matrix
confusionMatrix(test_set3[, 19], y_pred.r,positive = "1",dnn=c("actual","pred"))

#Plotting RoC Curve 
pred1 <- prediction(as.numeric(y_pred.r),as.numeric(test_set$FraudFound_P))
perf<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred.r),as.numeric(test_set$FraudFound_P))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - fb_svm.r")

#polynomial kernel
classifier.p = svm(formula = FraudFound_P ~ .,
                   data = training_set3,
                   type = 'C-classification',
                   kernel = 'polynomial',
                   degree=4, 
                   gamma =1 )

# Predicting the Test set results
y_pred.p = predict(classifier.p, newdata = test_set3[-19])
# Making the Confusion Matrix
confusionMatrix(test_set3[, 19], y_pred.p,positive = "1",dnn=c("actual","pred"))

#Plotting RoC Curve 
pred1 <- prediction(as.numeric(y_pred.p),as.numeric(test_set$FraudFound_P))
perf<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred.p),as.numeric(test_set$FraudFound_P))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve-fb_svm.p4 ")

#sigmoid kernel
classifier.s = svm(formula = FraudFound_P ~ .,
                   data = training_set3,
                   type = 'C-classification',
                   kernel = 'sigmoid',
                   gamma =1 )

# Predicting the Test set results
y_pred.s = predict(classifier.s, newdata = test_set3[-19])
# Making the Confusion Matrix
confusionMatrix(test_set3[, 19], y_pred.s,positive = '1',dnn=c("actual","pred"))

#Plotting RoC Curve 
pred1 <- prediction(as.numeric(y_pred.s),as.numeric(test_set$FraudFound_P))
perf<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred.s),as.numeric(test_set$FraudFound_P))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve ")


################################pair plots
install.packages("GGally")
library(GGally)
ggpairs(data2,mapping=aes(colour= FraudFound_P))

