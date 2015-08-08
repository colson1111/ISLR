# Generalized Additive Models

#gam using natural splines
gam1 <- lm(wage~ns(year, 4) + ns(age, 5) + education, data = Wage)

#gam using smoothing splines
library(gam)

gam.m3 <- gam(wage~s(year, 4) + s(age, 5) + education, data = Wage)

par(mfrow=c(1,3))
plot(gam.m3, se = TRUE, col = "blue")

plot.gam(gam1, se = TRUE, col = "red")


gam.m1 <- gam(wage~s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage~year + s(age, 5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")

summary(gam.m3)

preds <- predict(gam.m2, newdata = Wage)

#use lo to include a local regression term
gam.lo <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage)
plot.gam(gam.lo, se = TRUE, col = "green")

# use localized regression to create interactions
gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)
library(akima)
plot(gam.lo.i)

# logistic regression GAM
gam.lr <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)
par(mfrow = c(1, 3))
plot(gam.lr, se = TRUE, col = "green")

table(education, I(wage>250)) # there are no high earners in the <HS Grad group, so we want to leave this category out

gam.lr.s <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, 
                data = Wage, subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se = T, col = "green")





