## Adding plot parameters to plot.spline.correlog
## 19 / 12 / 2013
## plot.spline.correlog currently doesn't allow the user to change the axis labels or to add a title plot
## therefore I have edited the function to permit this

## did this by moving the default plotting arguments to the root of the function, and adding '...' to the end 
## of the plotting default

plot.spline.correlog.n <- function (x, xmax = 0, text = TRUE, xlim = c(0, xmax), ylim = c(-1, 1), type = "l", 
                                    xlab = "Distance", ylab = "Correlation", ...) 
  {
    obj <- x
    xmax <- ifelse(xmax == 0, obj$max.distance, xmax)
    x <- round(obj$real$x, 1)
    y <- round(obj$real$y, 2)
    xul <- round(quantile(obj$boot$boot.summary$x.intercept, probs = c(0.025, 0.975), na.rm = TRUE), 1)
    yul <- round(quantile(obj$boot$boot.summary$y.intercept, probs = c(0.025, 0.975), na.rm = TRUE), 2)
    plot(obj$real$predicted$x, obj$real$predicted$y, xlim = xlim, ylim = ylim, type = type, 
         xlab = xlab, ylab = ylab, ...)
    lines(obj$real$predicted$x, obj$real$predicted$y)
    lines(c(0, max(obj$real$predicted$x)), c(0, 0))
    if (!is.null(obj$boot$boot.summary$predicted$x)) {
      lines(obj$boot$boot.summary$predicted$x, obj$boot$boot.summary$predicted$y["0.025", ])
      lines(obj$boot$boot.summary$predicted$x, obj$boot$boot.summary$predicted$y["0.975", ])
    }
  }