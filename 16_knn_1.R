#install.packages('class')
library(class)
train=training_set[,-32]
test=test_set[,-32]
cl=training_set[,32]
cl2=test_set[,32]
#cross validation
y_pred_cv=knn.cv(train,cl,k=1,prob =FALSE)
cm = confusionMatrix(cl, y_pred_cv,positive ='1')
cm


#prediction on random model k=3
y_pred_r=knn(train,test,cl,k=3,prob=TRUE)
cm12 = confusionMatrix(cl2, y_pred_r,positive = '1',dnn=c("actual","pred"))
cm12

# ROC Curve KNN
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred_r),as.numeric(test_set$left))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - 3Knn")

#training loop cv
accuracy=rep(0,10)
for(i in 1:10)
{
  y_pred_cv=knn.cv(train,cl,k=i,prob=FALSE)
  t1 = table(cl, y_pred_cv)
  accuracy[i]=(t1[1]+t1[4])/(t1[1]+t1[2]+t1[3]+t1[4])*100
}
accuracy


#plot accuracy vs neigh
plot(accuracy,type="o",xlab="Number of neighbours",ylab="Accuracy")

#prediction on best model k=1
y_pred=knn(train,test,cl,k=5,prob=TRUE)
cm2 = confusionMatrix(cl2, y_pred,positive = '1',dnn=c("actual","pred"))
cm2

# ROC Curve KNN
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred),as.numeric(test_set$left))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - 1Knn")