
# Regression Splines, Smoothing Splines, and Local Regression

# REGRESSION SPLINES
library(splines)

#fitting a cubic spline
fit <- lm(wage~bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se = TRUE)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2*pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2*pred$se, lty = "dashed")

#determining knots at uniform quantiles of data
dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots")

#fitting a natural spline
fit2 <- lm(wage~ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

# SMOOTHING SPLINES
plot(age, wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Smoothing Splines")
fit <- smooth.spline(age, wage, df = 16)  # uses the value of lambda that yields 16 df
fit2 <- smooth.spline(age, wage, cv = TRUE) # uses cross validation to determine lambda
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)



# LOCAL REGRESSION
plot(age, wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage~age, span = 0.2, data = Wage)
fit2 <- loess(wage~age, span = 0.5, data = Wage)
fit3 <- loess(wage~age, span = 0.9, data = Wage)

lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "blue", lwd = 2)
lines(age.grid, predict(fit3, data.frame(age = age.grid)), col = "green", lwd = 2)

legend("topright", legend = c("Span = 0.2", "Span = 0.5", "Span = 0.9"), 
       col = c("red", "blue", "green"), lty = 1, lwd = 2, cex = 0.8)







