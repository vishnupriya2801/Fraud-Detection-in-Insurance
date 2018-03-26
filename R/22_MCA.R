install.packages("FactoMineR")
library(FactoMineR)
?MCA
cats = apply(data2, 2, function(x) nlevels(as.factor(x)))
cats
data2$WeekOfMonth=as.numeric(data2$WeekOfMonth)
data2$Year=as.numeric(data2$Year)
data2$Deductible=as.numeric(data2$Deductible)
data2$WeekOfMonthClaimed=as.numeric(data2$WeekOfMonthClaimed)
data2$Age=as.numeric(data2$Age)
sapply(data2,class)

mca=MCA(data2,graph=TRUE)
mca

mca_tra=predict.MCA(mca,data2,type="factor")


# data frame with variable coordinates
mca1_vars_df = data.frame(mca$var$coord, Variable = rep(names(cats), cats))

# data frame with observation coordinates
mca1_obs_df = data.frame(mca$ind$coord)

# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")
