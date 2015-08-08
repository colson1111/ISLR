install.packages("ISLR")

require(ISLR)
names(Smarket)
summary(Smarket)

cor(Smarket)
cor(Smarket[,-9])

attach(Smarket)
plot(Volume)

pairs(Smarket, col=Smarket$Direction)

#Logistc Regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)


glm.probs=predict(glm.fit, type="response")

glm.probs[1:5]

glm.pred=ifelse(glm.probs>0.5,"Up","Down")

attach(Smarket)
table(glm.pred, Direction)
mean(glm.pred==Direction)


#Make your training and test set
train=Year<2005
Smarket.2005<-Smarket[!train,]

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)

glm.probs=predict(glm.fit, newdata=Smarket[!train,], type="response")

glm.pred=ifelse(glm.probs>0.5, "Up", "Down")

Direction.2005=Smarket$Direction[!train]

table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)


#Fit smaller model
glm.fit=glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)

glm.probs=predict(glm.fit, newdata=Smarket[!train,], type="response")

glm.pred=ifelse(glm.probs>0.5, "Up", "Down")

Direction.2005=Smarket$Direction[!train]

table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

summary(glm.fit)


coef(glm.fit)

contrasts(Direction)
