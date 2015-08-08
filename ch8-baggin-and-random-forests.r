
library(ISLR)
library(randomForest)

# perform bagging using randomForest():  bagging is a special case of random forest where m = p
set.seed(1)
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, importance = TRUE)
bag.boston

# How good is the bagged model?
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2)

# 13.50:  almost half the test MSE we got from the optimally pruned single regression tree


# using only 25 trees
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, ntree = 25, importance = TRUE)
bag.boston
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
mean((yhat.bag - boston.test) ^ 2)

# performing random forest: same thing, except mtry value changes
set.seed(1)
rf.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf - boston.test) ^ 2)

# 11.48:  MSE is lower for random forest than bagging in this case.

importance(rf.boston)

# %IncMSE:  mean decrease of accuracy in predictions on the out of bag samples when the variable is excluded
# IncNodePurity:  atotal decrease in node impurity from splitting on the variable, average over all tree

varImpPlot(rf.boston)

# wealth level and house size are the two most important variables in predicting median house value
