# FITTING CLASSIFICATION TREES

library(tree)
library(ISLR)

attach(Carseats)

High <- ifelse(Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

tree.carseats <- tree(High ~. -Sales, Carseats)

summary(tree.carseats)

par(mfrow=c(1,1))

plot(tree.carseats)
text(tree.carseats, pretty = 0)

tree.carseats

#validate using test and training sets
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]

tree.carseats <- tree(High~.-Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)

(88 + 56)/200

# use cv.tree for cross validation to determine the optimal level of tree complexity
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)

cv.carseats
# the $dev corresponds to the cross-validation error rate, we see this is minimized with 9 terminal nodes
# plot error rate as a function of size and alpha (k)
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# test pruned classification tree on test data
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)

(94 + 60) / 200

# trying to increase the value of best
prune.carseats <- prune.misclass(tree.carseats, best = 15)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)

(86 + 62) / 200
