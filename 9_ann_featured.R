install.packages("h2o")
library(h2o)
h2o.init()


data=read.csv("claims.csv")
data2=data[,idx]

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = as.h2o(data2), 
                         ratios = c(0.7, 0.15),  #partition data into 70%, 15%, 15% chunks
                         seed = 1)  #setting a seed will guarantee reproducibility
training_set_ann<- splits[[1]]
validation_set_ann<- splits[[2]]
test_set_ann <- splits[[3]]

#grid search ANN
activation_opt <- c("Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout","Tanh","TanhWithDropout")
hidden=list(c(50,50),c(100,100),c(200,200),c(300,300))
hyper_params <- list(activation = activation_opt,
                     hidden=hidden)
grid_m <- h2o.grid("deeplearning", x = 1:19, y = 'FraudFound_P',
                   grid_id = "grid19",
                   training_frame = as.h2o(training_set_ann),
                   validation_frame=as.h2o(validation_set_ann),
                   
                   seed = 1,
                   hyper_params = hyper_params,
                   standardize=TRUE)

gridperf_n <- h2o.getGrid(grid_id = "grid19", 
                          sort_by = "rmse", 
                          decreasing = TRUE)
print(gridperf_n)

#bestmodel
best_model<- gridperf_n@model_ids[[1]]
best<- h2o.getModel(best_model)
best
#performance on new data
per= h2o.performance(model, newdata = as.h2o(test_set3))
per

validation_set3$FraudFound_P=as.factor(validation_set3$FraudFound_P)
training_set3$FraudFound_P=as.factor(training_set3$FraudFound_P)
test_set3$FraudFound_P=as.factor(test_set3$FraudFound_P)
#model with best parameters maxout hidden layers[50,50]
model = h2o.deeplearning(x=1:18,y = "FraudFound_P",
                         training_frame = as.h2o(training_set_ann),
                         validation_frame = as.h2o(validation_set_ann),
                         nfolds =10,
                         keep_cross_validation_predictions=TRUE,
                         standardize=TRUE,
                         activation = 'TanhWithDropout',
                         balance_classes = TRUE,
                         hidden = c(100,100),
                         #epochs = 100,
                         #train_samples_per_iteration = -2,
                         seed=1,
                         variable_importances = TRUE)

#variable importance
h2o.varimp(model)
h2o.varimp_plot(model)

# Predicting the Test set results
y_pred = h2o.predict(model, newdata = as.h2o(test_set_ann[-19]))
y_pred.R=as.data.frame(y_pred)
test_set.R=as.data.frame(test_set_ann)
head(y_pred.R)

cm_d=confusionMatrix(y_pred.R[,1],test_set.R[,19],positive = "1",dnn=c("pred","actual"))
cm_d

#ROC
plot(per, type = "roc")
h2o.auc(model,valid=TRUE)#on test
h2o.auc(model,valid=FALSE)#on train


??h2o.deeplearning
