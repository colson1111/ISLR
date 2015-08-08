library(ISLR)

################################
#  The Validation Set Approach


set.seed(1)

train=sample(392,196)

lm.fit<-lm(mpg~horsepower, data=Auto, subset=train)


attach(Auto)

mean((mpg-predict(lm.fit,Auto))[-train]^2)

# The estimated test MSE for the linear regression fit is 26.14.

lm.fit2<-lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

# The estimated test MSE for the quadratic fit is 19.82259

lm.fit3<-lm(mpg~poly(horsepower,3),data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# The estimated test MSE for the cubic fit is 19.78252





#try it with a different training set, we will obtain slightly different errors on the validation set.
set.seed(2)
train=sample(392,192)

lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

# The estimated test MSE for the linear regression fit is 23.25404.

lm.fit2<-lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

# The estimated test MSE for the quadratic fit is 18.91531

lm.fit3<-lm(mpg~poly(horsepower,3),data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# The estimated test MSE for the cubic fit is 19.22212

###########################
#  Leave one out cross validation (LOOCV)

glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)

lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta


cv.error=rep(0,5)
for(i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}

cv.error


###########################
#  k-fold Cross Validation
set.seed(17)
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}

cv.error.10

