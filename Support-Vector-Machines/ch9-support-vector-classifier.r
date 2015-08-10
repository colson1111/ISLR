# SUPPORT VECTOR CLASSIFIER


install.packages("e1071")

# e1071 contains implementations for support vector machines

library(e1071)
library(ISLR)

set.seed(1)
x = matrix(rnorm(20 * 2), ncol = 2)
y = c(rep(-1, 10), rep(1, 10))
x[y==1,]=x[y==1,] + 1

plot(x, col = (3-y))

# in order for svm() to perform classification, we encode to response as a factor
dat <- data.frame(x = x, y = as.factor(y))

svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)

# scale = FALSE:  does not scale the features to mean = 0, sd = 1

plot(svmfit, dat)

# which observations are the support vectors?
svmfit$index

summary(svmfit)


svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
svmft$index
plot(svmfit, dat)

# use tune to perform cross validation
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10,100)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)

# create a test set and predict classes
xtest <- matrix(rnorm(20*2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)

# what if we used cost = 0.01 instead of cost = 0.1?
svmfit <- svm(y ~. , data = dat, kernel = "linear", cost = 0.01, scale = FALSE)
ypred = predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)

# what if they are linearly separable?

x[y==1,]=x[y==1,] + 0.5
plot(x, col = (y + 5)/2, pch = 19)

dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost =1e5)
summary(svmfit)
plot(svmfit, dat)

svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)

# using a smaller value of cost, we misclassify a training observationn, but we also obtain a
# much winder margin and make use of seven support vectors.  This model is probably better
# than the cost = 1e5 model.
