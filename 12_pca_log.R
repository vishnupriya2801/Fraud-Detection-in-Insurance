# Splitting the dataset into the Training set and Test set
histogram(training_set_pca2$FraudFound_P)
training_set_pca2<- SMOTE(FraudFound_P ~ ., training_set_pca, perc.over = 450, perc.under=150)

set.seed(123)
split = sample.split(transformed$FraudFound_P, SplitRatio = 0.75)
training_set_pca = subset(transformed, split == TRUE)
test_set_pca = subset(transformed, split == FALSE)

# Fitting Logistic Regression to the Training set
classifier11 = glm(formula = FraudFound_P ~ .,
                  family = binomial,
                  data = training_set_pca2)

# Predicting the Test set results
prob_pred1 = predict(classifier11, type = 'response', newdata = test_set_pca[-1])
y_pred1 = ifelse(prob_pred1 > 0.4, 1, 0)
unique(y_pred1)

# Making the Confusion Matrix
c=confusionMatrix(test_set_pca$FraudFound_P, y_pred1,positive = "1",dnn=c("active","pred"))
c
