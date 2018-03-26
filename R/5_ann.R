install.packages("h2o")
library(h2o)
h2o.init()
# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = as.h2o(data), 
                         ratios = c(0.7, 0.15),  #partition data into 70%, 15%, 15% chunks
                         seed = 1)  #setting a seed will guarantee reproducibility
training_set_a <- splits[[1]]
validation_set_a <- splits[[2]]
test_set_a <- splits[[3]]

#grid search ANN
activation_opt <- c("Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout","Tanh","TanhWithDropout")
hidden=list(c(50,50),c(100,100),c(200,200),c(300,300))
hyper_params <- list(activation = activation_opt,
                     hidden=hidden)
grid_m <- h2o.grid("deeplearning", x = 1:32, y = 'FraudFound_P',
                   grid_id = "grid18",
                   training_frame = as.h2o(training_set_a),
                   validation_frame=as.h2o(validation_set_a),
                  
                   seed = 1,
                   hyper_params = hyper_params,
                   standardize=TRUE)

gridperf_n <- h2o.getGrid(grid_id = "grid18", 
                          sort_by = "rmse", 
                          decreasing = TRUE)
print(gridperf_n)

#bestmodel
best_model<- gridperf_n@model_ids[[1]]
best<- h2o.getModel(best_model)
best
#performance on new data
per= h2o.performance(best, newdata = as.h2o(test_set_a))
per

test_set_a$FraudFound_P=as.factor(test_set_a$FraudFound_P)
validation_set_a$FraudFound_P=as.factor(validation_set_a$FraudFound_P)
training_set_a$FraudFound_P=as.factor(training_set_a$FraudFound_P)

#model with best parameters maxout hidden layers[50,50]
model = h2o.deeplearning(x=1:32,y = "FraudFound_P",
                         training_frame = as.h2o(training_set_a),
                         validation_frame = as.h2o(validation_set_a),
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
y_pred_h = h2o.predict(model, newdata = as.h2o(test_set_a[,-32]))
y_pred.R=as.data.frame(y_pred_h)
test_set.R=as.data.frame(test_set_a)
head(y_pred.R)

cm_d=confusionMatrix(y_pred.R[,1],test_set.R[,32],dnn=c("pred","actual"),positive = '1')
cm_d

#ROC
#performance on new data
perf = h2o.performance(model,newdata = test_set_a)
plot(perf, type = "roc")
h2o.auc(model,valid=TRUE)#on test
h2o.auc(model,valid=FALSE)#on train

