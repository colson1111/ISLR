#create function


#estimating the accuracy of a statistic of interest
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100,100,replace=T))

boot(Portfolio, alpha.fn, R=1000)

#estimating the accuracy of a linear regression model
boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))

boot.fn(Auto, 1:392)

boot.fn(Auto, sample(392,392,replace=T))
boot.fn(Auto, sample(392,392,replace=T))

boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower, data=Auto))$coef


boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))

set.seed(1)
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef
