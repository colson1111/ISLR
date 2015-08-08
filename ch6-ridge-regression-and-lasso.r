# ridge regression and the lasso

library(glmnet)
library(ISLR)

Hitters <- na.omit(Hitters)

# create x matrix of inputs and y vector of responses
x <- model.matrix(Salary ~ ., Hitters)[,-1]
y <- Hitters$Salary

# RIDGE REGRESSION

# In glmnet(), use alpha = 0 for ridge regression and alpha = 1 for lasso.

grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

# creates a matrix storing all ridge regression coeffecients for each value of lamba (20 x 100 matrix here)
dim(coef(ridge.mod))

# example:  when lambda = 11,498
ridge.mod$lambda[50]
coef(ridge.mod)[,50]

# l2 norm function
l2_norm <- function(x, n){
  sqrt(sum(coef(x)[-1, n]^2))
}

l2_norm_50 <- l2_norm(ridge.mod, 50)


# example: when lambda = 705.  Note that much larger l2_norm is associate with the smaller value of lambda
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
l2_norm_60 <- l2_norm(ridge.mod, 60)

# use predict() to obtain the ridge regression coefficients for a new value of lambda (50):
predict(ridge.mod, s = 50, type = "coefficients")[1:20,]

# split into training and test sets to estimate the test error of ridge regression and lasso
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

# use predict() get predictions for a test set
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test,])
mean((ridge.pred - y.test)^2)

# only an intercept
mean((mean(y[train]) - y.test) ^ 2)

# ridge regression with very large lambda
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test) ^ 2)

# least squares is ridge regression with lambda = 0
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,], exact = T)
mean((ridge.pred - y.test) ^ 2)

lm(y~x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients")[1:20,]


# Use built-in glmnet() cross validation to choose the optimal lambda
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam # best lambda is 212.  what is the test MSE for this value?
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2)

# fit on full data to get coefficient estimates
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]

# Note:  none of the coefficient are zero; ridge regression does not do variable selection


# THE LASSO
# Can we get a better model than ridge regression?
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test) ^ 2)

# check out the coefficients
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.coef

# The lasso model with lambda chosen by cross validation only contains seven variables.





