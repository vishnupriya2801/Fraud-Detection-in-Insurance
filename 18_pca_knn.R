library(class)
train_p=training_set_pca2[,-1]
test_p=test_set_pca[,-1]
cl_p=training_set_pca2[,1]
cl2p=test_set_pca[,1]
#cross validation
y_pred_cv=knn.cv(train_p,cl_p,k=3,prob =FALSE)
cm = confusionMatrix(cl, y_pred_cv,positive ='1')
cm

#prediction on random model k=3
y_pred_r=knn(train_p,test_p,cl,k=3,prob=TRUE)
cm12 = confusionMatrix(cl2p, y_pred_r,positive = '1')
cm12
#training loop cv
accuracy=rep(0,10)
for(i in 1:10)
{
  y_pred_cv=knn.cv(train_p,cl_p,k=i,prob=FALSE)
  t1 = table(cl_p, y_pred_cv)
  accuracy[i]=(t1[1]+t1[4])/(t1[1]+t1[2]+t1[3]+t1[4])*100
}
accuracy

#prediction on best model k=1
y_pred2=knn(train_p,test_p,cl_p,k=3,prob=TRUE)
cm2 = confusionMatrix(cl2p, y_pred2,positive = '1',dnn=c("actual","pred"))
cm2

