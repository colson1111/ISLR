# SUPPORT VECTOR MACHINE

install.packages("e1071")

# e1071 contains implementations for support vector machines

library(e1071)
library(ISLR)

# for SVM we use a different value of kernal.  We use "polynomial" or "radial"
set.seed(1)
x = matrix(rnorm(200 * 2), ncol = 2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat = data.frame(x = x, y = as.factor(y))

plot(x, col = y)

train <- sample(200, 100)
svmfit <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train,])

summary(svmfit)

# Trying to reduce the number of training errors by increasing the value of cost
svmfit <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train,])

# using cross-validation to select the best choice of gamma and cost
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat[train,], kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
# choose the combation that minimizes error:  cost = 1, gamma = 2
summary(tune.out)

table(true = dat[-train, "y"], pred = predict(tune.out$best.model, newx = dat[-train,]))
(18 + 21)/(18 + 21 + 56 + 5)
# 39% of the test observations are misclassified


# ROC CURVES
library(ROCR)

rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

svmfit.opt <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 2, cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit.opt, dat[train,], decision.values=TRUE))$decision.values

par(mfrow=c(1,2))
rocplot(fitted, dat[train, "y"], main = "Training Data")
svmfit.flex <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 50, cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit.flex, dat[train,], decision.values = T))$decision.values
rocplot(fitted,dat[train,"y"], add = T, col = "red")

fitted = attribute(predict(svmfit.opt, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], main = "Test Data")
fitted <- attributes(predict(svmfit.flex, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], add = T, col = "red")


#  SUPPORT VECTOR MACHINE WITH MULTIPLE CLASSES
set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y = c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat = data.frame(x=x, y=as.factor(y))
par(mfrow = c(1,1))
plot(x, col = (y + 1))

svmfit <- svm(y ~ ., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)




