# Simple Linear Regression

library(MASS)

fix(Boston)
names(Boston)

# predict median home value based on % of households with low socioeconomic status
lm.fit <- lm(medv ~ lstat, data = Boston)

lm.fit

summary(lm.fit)

# see what we can get out of the lm.fit object
names(lm.fit)

# use $ to get coefficients
lm.fit$coefficients

# use extractor functions to get coefficients
coef(lm.fit)

# get confidence interval for the coefficients
confint(lm.fit)

# use predict fnction to produce intervals for a prediction of medv based on a single lstat value
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "confidence")
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "prediction")

# plot with the least squares line
attach(Boston)
plot(lstat, medv)
abline(lm.fit)

# examine diagnostic plots
par(mfrow = c(2, 2))
plot(lm.fit)

# plot fit vs. residuals
plot(predict(lm.fit), residuals(lm.fit))

# plot fit vs. studentized residuals
plot(predict(lm.fit), rstudent(lm.fit))

# the residual plots suggest non-linearity.

# calculate leverage statistics to identify leverage points
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# observation #375 has the highest hat value and might be a leverage point
