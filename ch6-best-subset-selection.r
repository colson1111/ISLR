library(ISLR)

fix(Hitters)
names(Hitters)

dim(Hitters)

sum(is.na(Hitters$Salary))

#salary is missing for 59 players

Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# The leaps library performs best subset selection using regsubsets() function
library(leaps)
regfit.full <- regsubsets(Salary ~ . , Hitters)
summary(regfit.full)

#an asterisk indicates that a given variable is included in the corresponding model


regfit.full <- regsubsets(Salary ~ ., Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)

names(reg.summary)

reg.summary$rsq

# plot RSS, adj-R^2, CP, and BIC to decide which model to select
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)


plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)


# Using the regsubsets() function built in plot to display selection variables

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(rgefit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

coef(regfit.full, 6)





