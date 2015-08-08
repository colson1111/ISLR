
# boosting

library(gbm)
library(ISLR)

set.seed(1)
boost.boston <- gbm(medv ~ ., data = Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.boston)

# partial dependence plots for lstat and rm
# illustrate the marginal effect of hte selected variables on the response after integrating out the other variables
par(mfrow=c(1,2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")
par(mfrow=c(1,1))


# use boosted model to predict the test set
yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test) ^ 2)

#11.8: similar result to random forest, better than bagging

#We can change the shrinkage parameter from default of 0.001 to 0.2:
boost.boston <- gbm(medv ~ ., data = Boston[train,], distribution = "gaussian", 
                    n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test) ^ 2)

#1.51: little better with shrinkage = 0.2 instead of 0.001.
