## Testing log likelihood and prediction plots
## Created: 29/4/15
## Lasted edited: 29/4/15
## Isabel Fenton


## libraries ---------------------------------------------------------------
source("../../../Code/lr_calculations.R") # code for calculating likelihood ratios
library(lmtest) # for likelihood tests

## 1. Set up an example model ----------------------------------------------

exa <- data.frame(Lat = rep(1:10, 10), Long = rep(1:10, each = 10))
# three evs. One with a linear relationship, one a polynomial, one no relationship
exa$EV1 <- rep(1:10, 10)
exa$EV2 <- rep(10:1, each = 10)
exa$EV3 <- runif(100, 1, 10)

exa$RV <- exa$EV1*10 + exa$EV2^2 + rnorm(100, sd = 0.1)

# look at this model
with(exa, plot(Lat, Long, col = rainbow(max(EV1))[EV1], pch = 16))
with(exa, plot(Lat, Long, col = rainbow(max(EV2))[EV2], pch = 16))
with(exa, plot(Lat, Long, col = rainbow(max(EV3))[EV3], pch = 16))
with(exa, plot(Lat, Long, col = rainbow(max(round(RV,0)))[round(RV, 0)], pch = 16))

with(exa, plot(EV1, RV))
with(exa, plot(EV2, RV))
with(exa, plot(EV3, RV))

with(exa, pairs(cbind(EV1, EV2, EV3)))

mod.1 <- lm(RV ~ EV1 + EV2 + EV3, data = exa)
summary(mod.1)

mod.2 <- lm(RV ~ EV1 + poly(EV2, 2) + EV3, data = exa)
summary(mod.2)

## 2. Test log-likelihood plots --------------------------------------------
# full model is mod.2. Try without each of the EVs
mod.ev1 <- lm(RV ~ poly(EV2,2) + EV3, data = exa)
mod.ev2 <- lm(RV ~ EV1 + EV3, data = exa)
mod.ev3 <- lm(RV ~ EV1 + poly(EV2,2), data = exa)

# can set the ylim to be the value of the RV with no EVs in the model
barplot(c(lrtest(mod.2, mod.ev1)$Chisq[2], lrtest(mod.2, mod.ev2)$Chisq[2], lrtest(mod.2, mod.ev3)$Chisq[2]), names.arg = c("EV1", "EV2", "EV3"), ylim = c(0, round(lrtest(mod.2)$Chisq[2], -2)))

## 3. Test interaction plots -----------------------------------------------
exa$p <- predict(mod.2)
exa$p.ev1 <- predict(mod.ev1)
exa$p.ev2 <- predict(mod.ev2)
exa$p.ev3 <- predict(mod.ev3)

with(exa, plot(p - p.ev1 ~ EV1))
points(1:10, 1:10*10 - 50, type = "l")
with(exa, plot(RV ~ EV1))
points(1:10, 1:10*10, type = "l")

with(exa, plot(p - p.ev2 ~ EV2))
points(1:10, (1:10)^2 - 50, type = "l")
with(exa, plot(RV ~ EV2))
points(1:10, (1:10)^2, type = "l")

with(exa, plot(p - p.ev3 ~ EV3))
with(exa, plot(RV ~ EV3))

## 4. What if there are interactions ---------------------------------------
exa2 <- data.frame(Lat = rep(1:10, 10), Long = rep(1:10, each = 10))
# four evs. Two with a linear relationship, one of which interacts with the polynomial, one no relationship
exa2$EV1 <- rep(1:10, 10)
exa2$EV2 <- rep(10:1, each = 10)
exa2$EV3 <- runif(100, 1, 10)
exa2$EV4 <- rep(c(1:5, 5:1), 10) + rep(c(1:5, 5:1), each = 10)

exa2$RV <- with(exa2, EV1 + EV2^2 - EV1*EV2 + 10*EV4 + rnorm(100, sd = 0.1))

# look at this model
with(exa2, plot(Lat, Long, col = rainbow(max(EV1))[EV1], pch = 16))
with(exa2, plot(Lat, Long, col = rainbow(max(EV2))[EV2], pch = 16))
with(exa2, plot(Lat, Long, col = rainbow(max(EV3))[EV3], pch = 16))
with(exa2, plot(Lat, Long, col = rainbow(max(EV4))[EV4], pch = 16))
with(exa2, plot(Lat, Long, col = rainbow(max(round(RV,0)))[round(RV, 0)], pch = 16))

with(exa2, plot(EV1, RV))
with(exa2, plot(EV2, RV))
with(exa2, plot(EV3, RV))
with(exa2, plot(EV4, RV))

with(exa2, pairs(cbind(EV1, EV2, EV3, EV4)))

mod2.1 <- lm(RV ~ EV1 + EV2 + EV3 + EV4, data = exa2)
summary(mod2.1)

mod2.2 <- lm(RV ~ (EV1 + poly(EV2, 2) + EV3 + EV4)^2, data = exa2)
summary(mod2.2)

# full model is mod2.2. Try without each of the EVs
mod2.ev1 <- lm(RV ~ (poly(EV2, 2) + EV3 + EV4)^2, data = exa2)
mod2.ev2 <- lm(RV ~ (EV1 + EV3 + EV4)^2, data = exa2)
mod2.ev3 <- lm(RV ~ (EV1 + poly(EV2, 2) + EV4)^2, data = exa2)
mod2.ev4 <- lm(RV ~ (EV1 + poly(EV2, 2) + EV3)^2, data = exa2)

# can set the ylim to be the value of the RV with no EVs in the model
barplot(c(lrtest(mod2.2, mod2.ev1)$Chisq[2], lrtest(mod2.2, mod2.ev2)$Chisq[2], lrtest(mod2.2, mod2.ev3)$Chisq[2], lrtest(mod2.2, mod2.ev4)$Chisq[2]), names.arg = c("EV1", "EV2", "EV3", "EV4"), ylim = c(0, round(lrtest(mod2.2)$Chisq[2], -2)))

# Test interaction plots
exa2$p2 <- predict(mod2.2)
exa2$p2.ev1 <- predict(mod2.ev1)
exa2$p2.ev2 <- predict(mod2.ev2)
exa2$p2.ev3 <- predict(mod2.ev3)
exa2$p2.ev4 <- predict(mod2.ev4)

# check predictions
with(exa2, plot(Lat, Long, col = rainbow(max(round(RV,0)))[round(RV, 0)], pch = 16))
with(exa2, plot(Lat, Long, col = rainbow(max(round(RV,0)))[round(p2, 0)], pch = 16))
with(exa2, plot(Lat, Long, col = rainbow(max(round(RV,0)))[round(p2.ev1, 0)], pch = 16))
with(exa2, plot(Lat, Long, col = rainbow(max(round(RV,0)))[round(p2.ev2, 0)], pch = 16))
with(exa2, plot(Lat, Long, col = rainbow(max(round(RV,0)))[round(p2.ev3, 0)], pch = 16))
with(exa2, plot(Lat, Long, col = rainbow(max(round(RV,0)))[round(p2.ev4, 0)], pch = 16))

EV1 + EV2^2 - EV1*EV2 + EV4*10

with(exa2, plot(p2 - p2.ev1 ~ EV1))
points(1:10, -1:-10*5 + 25, type = "l")
with(exa2, plot(EV1, EV1 - EV1*EV2, pch = 16))
points(1:10, -1:-10*5, type = "l")
with(exa2, plot(RV ~ EV1))

with(exa2, plot(p2 - p2.ev2 ~ EV2))
points(1:10, -1:-10*5 + (1:10)^2 - 12.5, type = "l")
with(exa2, plot(EV2, EV2^2 - EV1*EV2, pch = 16))
points(1:10, -1:-10*5 + (1:10)^2, type = "l")
with(exa2, plot(RV ~ EV2))

with(exa2, plot(p2 - p2.ev3 ~ EV3))
with(exa2, plot(RV ~ EV3))

with(exa2, plot(p2 - p2.ev4 ~ EV4))
points(1:10, 1:10*10 - 62.5, type = "l")
with(exa2, plot(EV4, EV4*10, pch = 16))
points(1:10, 1:10*10, type = "l")
with(exa2, plot(RV ~ EV4))
