
# PRINCIPLE COMPONENTS REGRESSION
 library(pls)
 library(ISLR)
 
 Hitters <- na.omit(Hitters)
 
 set.seed(2)
 
 pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "CV")
 
 summary(pcr.fit)
 
 #note:  pcr() reports the root mse, so we need to square it to get the mse.
 
 validationplot(pcr.fit, val.type = "MSEP")
 
 
 #Use training and test set to build model
 
 set.seed(1)
 x <- model.matrix(Salary~.,Hitters)[,-1]
 y <- Hitters$Salary
 train <- sample(1:nrow(x), nrow(x)/2)
 test <- (-train)
 y.test <- y[test]
 
 pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
 validationplot(pcr.fit, val.type = "MSEP")
 
 # compute test MSE
 pcr.pred <- predict(pcr.fit, x[test,], ncomp=7)
 mean((pcr.pred - y.test)^2)
 
 # finally, fit PCR on the full data set using M = 7:  the number of components identified by cross validation
 pcr.fit <- pcr(y~x, scale = TRUE, ncomp = 7)
 summary(pcr.fit)
 
 
 # PARTIAL LEAST SQUARES REGRESSION
 
 set.seed(1)
 
 pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
 summary(pls.fit)
 validationplot(pls.fit, val.type = "MSEP")
 
 # calculate the test set mse for the lowest MSE from the validation plot (M = 2)
 pls.pred <- predict(pls.fit, x[test,], ncomp = 2)
 mean((pls.pred - y.test) ^ 2)
 
 # build partial least squares model on the full data set
 pls.fit <- plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 2)
 summary(pls.fit)
 
 
 
 
