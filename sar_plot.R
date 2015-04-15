## 09 / 06 / 2014
## Isabel Fenton
## Model plots for sarlm's
##
## Code to run model plots for the sarlm models
##
## Functions: 
## sar.plot - function to run model plots

sar.plot <- function (model, ask = prod(par("mfcol")) < 2 && dev.interactive(), ...) {
  
  # graphics parameters
  # n.b. this is taken from plot.lm
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
    
  # plot fitted against residuals
  plot(fitted(model), residuals(model))
  abline(h = 0)
  
  # plot a qq plot
  qqnorm(residuals(model))
  qqline(residuals(model))
}