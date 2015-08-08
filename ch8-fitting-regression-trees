
# fitting regression trees

library(ISLR)
library(MASS)

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv~., Boston, subset = train)
summary(tree.boston)

par(mfrow = c(1,1))

plot(tree.boston)
text(tree.boston, pretty = 0)


# will pruning the tree improve performance? using cross validation
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')


# prune the tree?
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat - boston.test)^2)

# the test set MSE is about 25.05.  The RMSE is around 5.005.  This means this model leads to test predictions
# that are within around $5,005 of the true median home value for the suburbs.

