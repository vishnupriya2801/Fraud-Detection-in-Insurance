data_anom=as.h2o(data2)
data_anom=data_anom[,-19]
x=colnames(data2[,-19])
x
model.anom = h2o.deeplearning(x = x,training_frame =data_anom, model_id = "autoencoders",autoencoder = TRUE,
                              activation = 'Tanh' )
?h2o.anomaly
errors <- h2o.anomaly(model.anom, data_anom,per_feature = FALSE)
print(errors)

row_outliers <- (errors > 0.0023) 
raw.R=as.data.frame(row_outliers)
#raw.R
table(raw.R$Reconstruction.MSE)

cm = table(data$FraudFound_P, raw.R$Reconstruction.MSE)
cm

data2[,19]
raw.R[1]

table(raw.R$Reconstruction.MSE)
err <- as.data.frame(errors)
quantile  = h2o.quantile(errors$Reconstruction.MSE)
quantile
threshold = quantile["99%"]
threshold
plot(err$Reconstruction.MSE,col="green")

abline(h=threshold)
?plot
######

