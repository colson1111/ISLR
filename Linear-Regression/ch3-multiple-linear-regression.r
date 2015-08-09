# MULTIPLE LINEAR REGRESSION

# two predictors
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

# all predictors
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)

# can access components of the summary object by name:
summary(lm.fit)$r.sq
summary(lm.fit)$sigma

# Check out variance inflation factors using the vif() function in the car package
library(car)
vif(lm.fit) # a few to keep an eye out for:  nox, dis, rad, tax

# leaving out one variable
lm.fit1 <- lm(medv ~ . -age, data = Boston)
summary(lm.fit1)

# same thing, but updating the first lm.fit object
lm.fit1 <- update(lm.fit, ~ . - age)


# ----------------------------------------------------------

# INTERACTION TERMS

# lstat:black -> include interaction term between lstat and black
# lstat*black -> include lstat, black, and the interaction term between the two
summary(lm(medv ~ lstat * age, data = Boston))
summary(lm(medv ~ lstat:age, data = Boston))


# ---------------------------------------------------------

# NON-LINEAR TRANSFORMATIONS OF THE PREDICTORS

# Use I() to allow the standard usage of ^ in the formula (it has a special meaning in lm())
lm.fit2 <- lm(medv ~ lstat + I(lstat ^ 2))
summary(lm.fit2)

# the p value suggests the model is improved

# use anova() to compare the two models
lm.fit <- lm(medv ~ lstat)
anova(lm.fit, lm.fit2)
# p value is significant, so we reject the null hypothesis that the models perform equally well

par(mfrow = c(2,2))
plot(lm.fit2)
# the residual plots look a lot better now

# to create a higher order polynomial function, we can use the poly() function
lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)

# try a log transformation
summary(lm(medv ~ log(rm), data = Boston))

# ---------------------------------------------------------

# QUALITATIVE PREDICTORS

fix(Carseats)
names(Carseats)

lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

# we can use contrasts() to see the dummy variable coding used by R
contrasts(Carseats$ShelveLoc)

# In the output, we see that a positive coefficient for ShelveLocGood 
# suggests better shelving location is associated with higher sales


