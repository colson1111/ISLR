# VALIDATION SET APPROACH

set.seed(1)

train <- sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test <- (!train)

#train model
regfit.best <- regsubsets(Salary ~ ., data = Hitters[train,], nvmax = 19)

#make model matrix from test data
test.mat <- model.matrix(Salary ~ ., data = Hitters[test,])

# Loop through each model and calculate the test MSE
val.errors <- rep(NA, 19)
for (i in 1:19){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[,names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}

#find the model that minimizes test MSE
which.min(val.errors)

# check out the model
coef(regfit.best, 10)

# Write predict function for the regsubsets() function
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars] %*% coefi
}

# compare the 10 variable model found above to the 10 variable model found using the full data set
regfit.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(regfit.best, 10)


# CROSS VALIDATION APPROACH

#1.  create a vector to allocates each observation to a fold and create a matrix to store results
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

#2.  For loop: use predict function defined above to perform predictions within each fold
for (j in 1:k){
  best.fit <- regsubsets(Salary ~ ., data = Hitters[folds != j,], nvmax = 19)
  for (i in 1:19){
    pred <- predict(best.fit, Hitters[folds==j,], id = i)
    cv.errors[j,i] <- mean((Hitters$Salary[folds == j] - pred) ^ 2)
  }
}

#3.  Use apply to average of the columns to obtain vector for which the jth element is the cross validation error for the j variable model.
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

#4.  Choose minimum CV error model.  Rerun model on full data to build the model.
which.min(mean.cv.errors)
reg.best <- regsubsets(Salary ~. , data = Hitters, nvmax = 19)
coef(reg.best, 11)





