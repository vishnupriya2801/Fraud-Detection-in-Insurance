library(class)
train1=training_set3[,-19]
test1=test_set3[,-19]
cl=training_set3[,19]
cl2=test_set3[,19]
#cross validation
y_pred_cv=knn.cv(train1,cl,k=3,prob =FALSE)
cm = confusionMatrix(cl, y_pred_cv,positive ='1')
cm

#prediction on random model k=3
y_pred_r=knn(train1,test1,cl,k=3,prob=TRUE)
cm12 = confusionMatrix(cl2, y_pred_r,positive = '1')
cm12
#training loop cv
accuracy=rep(0,10)
for(i in 1:10)
{
  y_pred_cv=knn.cv(train1,cl,k=i,prob=FALSE)
  t1 = table(cl, y_pred_cv)
  accuracy[i]=(t1[1]+t1[4])/(t1[1]+t1[2]+t1[3]+t1[4])*100
}
accuracy
plot(accuracy,type="o",xlab="Number of neighbours",ylab="Accuracy")

#prediction on best model k=1
y_pred=knn(train1,test1,cl,k=1,prob=TRUE)
cm2 = confusionMatrix(cl2, y_pred,positive = '1',dnn=c("actual","pred"))
cm2

