require(ISLR)

dim(Caravan)
str(Caravan)
attach(Caravan)

summary(Purchase)

#exclude column 86 (qualitative response variable, Purchase)
#standardize the data
standardized.X=scale(Caravan[,-86])

var(Caravan[,1])
var(Caravan[,2])

var(standardized.X[,1])
var(standardized.X[,2])

test<-1:1000
train.X<-standardized.X[-test,]
test.X<-standardized.X[test,]

train.Y<-Purchase[-test]
test.Y<-Purchase[test]

set.seed(1)
knn.pred=knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")

table(knn.pred, test.Y)
#1 nearest neighbor success rate on Yes response:
9/(68+9)

knn.pred=knn(train.X, test.X, train.Y, k=3)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred, test.Y)
# 3 nearest neighbor success rate on Yes response:
5/(21+5)


knn.pred=knn(train.X, test.X, train.Y, k=5)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred, test.Y)
# 5 nearest neighbor success rate on Yes response:
4/(11+4)


#logistic regression comparison

glm.fit=glm(Purchase~., data=Caravan, family=binomial, subset=-test)

glm.probs=predict(glm.fit, Caravan[test,], type="response")
glm.pred<-rep("No", 1000)

glm.pred[glm.probs>.5]<-"Yes"
table(glm.pred, test.Y)

glm.pred[glm.probs>.25]<-"Yes"
table(glm.pred, test.Y)
11/(22+11)
