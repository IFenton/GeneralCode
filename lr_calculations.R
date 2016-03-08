## File created: 30 / 5 / 2014
## Last updated: 15 / 7 / 2015
## Isabel Fenton
## Likelihood ratio plots
##
## Code to calculate likelihood ratios for a set of variables
##
## Functions: 
## model.evs - a function to get the list of variables from a model, including poly
## lr.calc - a function that calculates likelihood ratios for a set of variables
## lr.plot - a function to plot the likelihood ratios / comparisons on barplots
##
## To do: Initially set to run with sarlm models
##        suspect this only works with one poly variable
##        n.b. can't handle log()

library(lmtest)
library(scales)

model.evs <- function(model) {
  # function to get the list of variables from a model, including poly
  # input  - model: the model to work from
  # output - a list of the variables
  EVs <- all.vars(as.formula(model))[-1] # use the -1 to remove the RV
  # if poly is present
  if (length(grep("poly", names(model$coefficients), fixed = TRUE)) > 0) {
    # identify where poly is present and it's not an interaction
    warning("Only tested with one poly variable")
    poly.ev <- names(model$coefficients)[setdiff(grep("poly", names(model$coefficients), fixed = TRUE), grep(":", names(model$coefficients), fixed = TRUE))]
    
    tmp.poly <- NULL
    full.poly <- NULL
    # identify the unique EVs from that list (uni.poly)
    for (i in poly.ev){
      tmp.poly <- c(tmp.poly, names(unlist(sapply(EVs, grep, i, fixed = TRUE))))
    }
    uni.poly <- unique(tmp.poly)
    # for each unique poly generate a sequence from the max order to 1
    for (i in uni.poly) {
      # identify the max order of poly
      tmp <- poly.ev[grep(i, poly.ev, fixed = TRUE)][1] # given these aren't interactions, these must have the highest order of poly, so arbitraily take the first one
      poly.seq <- seq(as.numeric(substr(tmp, nchar(tmp) - 2, nchar(tmp) - 2)), 1)
      for (j in poly.seq) {
        substr(tmp, nchar(tmp) - 2, nchar(tmp) - 2) <- as.character(j)
        full.poly <- c(full.poly, substr(tmp, 1, nchar(tmp) - 1))
      }
    }
    # remove those EVs which are poly from the total EV list
    EVs <- setdiff(EVs, uni.poly)
    # add the poly values
    EVs <- c(EVs, full.poly)
  }
  
  # repeat for those with I()
  if (length(grep("I(", names(model$coefficients), fixed = TRUE)) > 0) {
    # identify where I( is present and it's not an interaction
    Ib.ev <- names(model$coefficients)[setdiff(grep("I(", names(model$coefficients), fixed = TRUE), grep(":", names(model$coefficients), fixed = TRUE))]
    
    tmp.Ib <- NULL
    # identify EVs without the I() from that list (uni.Ib)
    for (i in Ib.ev){
      tmp.Ib <- c(tmp.Ib, names(unlist(sapply(EVs, grep, i, fixed = TRUE))))
    }
    uni.Ib <- unique(tmp.Ib)
    # remove those EVs which are Ib from the total EV list
    EVs <- setdiff(EVs, uni.Ib)
    # add the Ib values
    EVs <- c(EVs, Ib.ev)
  }
  return(EVs)
}


lr.calc <- function(model, EVs = NULL, plots = FALSE, pred.data = NULL, mod.data = NULL, file.nm = as.character(model), multiple.oce = TRUE) {
  # function to calculate likelihood ratios
  # input  - model: the model to work from
  #        - EVs: dataframe of variables, and the groups they are in (optional, otherwise calculated from model)
  #        - plots: if TRUE then produce plots by ocean of the predicted values
  #        - pred.data: the dataframe for predicting
  #        - mod.data: the dataframe on which the original model was run
  # output - dataframe containing variables, p values and significance stars
  
  # obtain a list of variables
  if (is.null(EVs)) {
    # calculate the list of EVs
    EVs <- model.evs(model)
    # create a dataframe of these, so it matches the input of groups
    EVs <- data.frame(name = EVs, group = EVs)
  } else {
    # create a standard format
    if(!is.data.frame(EVs)) stop("Expecting a dataframe")
    if(ncol(EVs) != 2) stop ("Expecting 2 columns")
    if(length(unique(EVs[, 1])) < length(unique(EVs[, 2]))) stop ("Order should be name, then group")
    colnames(EVs) <- c("name", "group")
  }
  
  # if plots, then get predicted values from the main model
  if (plots) {
    mod.p <- sar.predict(model, olddata = mod.data, newdata = pred.data)
    m <- 1
  }
  
  # generate a dataframe to store these values
  LRs <- data.frame(names = unique(EVs$group), lr = NA, p = NA, stars = NA)
  
  # for each value in group
  for (i in unique(EVs$group)) {
    # identify the variables to remove
    m.EVs <- EVs$name[EVs$group == i]
    # if it is poly need to add higher orders
    if (length(grep("poly", i, fixed = TRUE)) > 0){
      # identify the order
      poly.order <- substr(i, regexpr(")", i) - 1, regexpr(")", i) - 1)
      # if there are higher orders, then add those to m.EVs
      # what polys are there
      all.poly <- unique(EVs$name)[grep("poly", unique(EVs$name), fixed = TRUE)]
      max.poly <- max(unlist(lapply(all.poly, function(x) substr(x, regexpr(")", x) - 1, regexpr(")", x) - 1))))
      # if the maximum is greater than the one we have currently, add these higher ones in
      if (poly.order < max.poly)
        m.EVs <- c(as.character(m.EVs), as.character(all.poly[substr(all.poly, regexpr(")", all.poly) - 1, regexpr(")", all.poly) - 1) > substr(i, regexpr(")", i) - 1, regexpr(")", i) - 1)]))
    }
    # create all possible interaction variables
    m.all.int <- unlist(lapply(m.EVs, function(x) names(model$coefficients)[grep(x, names(model$coefficients), fixed = TRUE)]))
    # make sure these are the same as found in EVs$name
    m.all.EVs <- NULL
    for (j in 1:length(m.all.int)) {
      tmp.split <- strsplit(m.all.int[j], ":")
      # for each part, what is the EVs$name that contains that part
      tmp.1 <- unlist(lapply(EVs$name, function(x) if (length(grep(x, tmp.split[[1]][1], fixed = TRUE)) > 0) as.character(x)))
      if(length(tmp.split[[1]]) == 2) {
        tmp.2 <- unlist(lapply(EVs$name, function(x) if (length(grep(x, tmp.split[[1]][2], fixed = TRUE)) > 0) as.character(x)))
        tmp.1 <- paste(tmp.1, ":", tmp.2, sep= "")
      }
      m.all.EVs <- c(m.all.EVs, tmp.1)
    }
    m.all.EVs <- unique(m.all.EVs)
    # if it is poly, need to add some in as well
    # if the main variable is poly
    if (length(grep("poly", i, fixed = TRUE)) > 0 ) {
      # reduce the order by one for all poly
      tmp.order <- as.numeric(min(unlist(lapply(m.EVs, function(x) substr(x, regexpr(")", x) - 1, regexpr(")", x) - 1)))))
      if (tmp.order > 1) {
        poly.add <- NULL
        for (k in m.all.EVs) {
          tmp.add <- as.numeric(substr(k, regexpr(")", k) - 1, regexpr(")", k) - 1))
          # subtract one if the value is less than 1
          if (tmp.add > 1) 
            substr(k, regexpr(")", k) - 1, regexpr(")", k) - 1) <- as.character(tmp.order - 1)
          poly.add <- c(poly.add, k)
        }
        tmp.mod <- eval(parse(text = paste("update(model, ~. -", paste(m.all.EVs, collapse = " - "), " + ", paste(poly.add, collapse = " + "), ")", sep = "")))
      } else {
        tmp.mod <- eval(parse(text = paste("update(model, ~. -", paste(m.all.EVs, collapse = " - "), ")", sep = "")))
      }
    } else {
      # create a model with that variable(s) removed
      tmp.mod <- eval(parse(text = paste("update(model, ~. -", paste(m.all.EVs, collapse = " - "), ")", sep = "")))
    }
    # obtain the LR and P-value
    LRs$lr[LRs$names == i] <- lrtest(model, tmp.mod)$Chisq[2]
    LRs$p[LRs$names == i] <- lrtest(model, tmp.mod)$Pr[2]
    
    if (plots) {
      # set the choice of colors
      color <- c("gray50", "springgreen3", "orchid3")
      ifelse(i == "prop2.oxy", span <- 1, span <- 0.75)
      
      tmp.mod.p <- sar.predict(tmp.mod, olddata = mod.data, newdata = pred.data)
      # if groups
      if (sum(EVs$name != EVs$group) == 0) {
        # extract variable name if it is a poly variable
        if (length(grep("poly", i)) > 0) {
          var <- gsub(",.*", "", gsub(".*\\(", "", i))
          m <- m + 1
        } else if (length(grep("I(", i, fixed = TRUE)) > 0) {
          var <- gsub("/.*", "", gsub(".*\\(", "", i))
        } else {
          var <- i
        }
        if (!is.factor(pred.data[, var]) & multiple.oce){
          if (min(pred.data[, var]) != max(pred.data[, var])) {
            # create a vector containing values for each ocean
            p.data <- data.frame(p.x = rep(seq(min(pred.data[, var]), max(pred.data[, var]), length.out = 100), 3), p.oce = rep(levels(pred.data[, grep("Ocean", names(pred.data))]), each = 100), p.y = NA)
            # for each of the oceans, run a loess and predict the values
            for (l in 1:3) {
              tmp.val <- which(pred.data[, grep("Ocean", names(pred.data))] == levels(pred.data[, grep("Ocean", names(pred.data))])[l])
              # need to set the span for data where there are limited x values
              lo <- loess((mod.p - tmp.mod.p)[tmp.val] ~ pred.data[tmp.val, var], span = span)
              p.data$p.y[(1:100) + 100 * (l - 1)] <- predict(lo, newdata = p.data$p.x[(1:100) + 100 * (l - 1)])
            }
          }
        }
        # randomise to plot better
        samp <- sample(1:nrow(pred.data), nrow(pred.data))
        # plot these results
        png(paste("Figures/",file.nm, "_", var, "_", m, ".png", sep = ""))
        with(pred.data[samp, ], plot(pred.data[samp, var], mod.p[samp] - tmp.mod.p[samp], col = alpha(color[pred.data[samp, grep("Ocean", names(pred.data))]], 0.5), xlab = var, ylab = paste("Relative", eval(model$call[2][[1]])[2][[1]]), bty = "l", las = 1, cex.lab = 1.2, cex.axis = 1.2))
        if (!is.factor(pred.data[, var]) & multiple.oce){
          if (min(pred.data[, var]) != max(pred.data[, var])) {
            with(p.data[1:100, ], lines(p.x, p.y, col = "black", lwd = 2))
            with(p.data[101:200, ], lines(p.x, p.y, col = "green4", lwd = 2))
            with(p.data[201:300, ], lines(p.x, p.y, col = "magenta4", lwd = 2))
          }
        }
        dev.off()
      } else {
        k <- 1
        for (j in EVs$name[EVs$group == i]) {
          if (length(grep("poly", j) > 0)) {
            var <- gsub(",.*", "", gsub(".*\\(", "", j))
            m <- m + 1
          } else {
            var <- j
          }
          if (!is.factor(pred.data[, var]) & multiple.oce){
            if (min(pred.data[, var]) != max(pred.data[, var])) {
              # create a vector containing values for each ocean
              p.data <- data.frame(p.x = rep(seq(min(pred.data[, var]), max(pred.data[, var]), length.out = 100), 3), p.oce = rep(levels(pred.data[, grep("Ocean", names(pred.data))]), each = 100), p.y = NA)
              # for each of the oceans, run a loess and predict the values
              for (l in 1:3) {
                tmp.val <- which(pred.data[, grep("Ocean", names(pred.data))] == levels(pred.data[, grep("Ocean", names(pred.data))])[l])
                # need to set the span for data where there are limited x values
                lo <- loess((mod.p - tmp.mod.p)[tmp.val] ~ pred.data[tmp.val, var], span = span)
                p.data$p.y[(1:100) + 100 * (l - 1)] <- predict(lo, newdata = p.data$p.x[(1:100) + 100 * (l - 1)])
              }
            }
          }
          # randomise to plot better
          samp <- sample(1:nrow(pred.data), nrow(pred.data))
          # plot these results
          png(paste("Figures/",file.nm, "_", var, "_", m, ".png", sep = ""))
          with(pred.data[samp, ], plot(pred.data[samp, var], mod.p[samp] - tmp.mod.p[samp], col = alpha(color[pred.data[samp, grep("Ocean", names(pred.data))]], 0.5), xlab = var, ylab = paste("Relative", eval(model$call[2][[1]])[2][[1]]), bty = "l", las = 1, cex.lab = 1.2, cex.axis = 1.2, main = paste(i, k, sep = ": ")))
          if (!is.factor(pred.data[, var]) & multiple.oce){
            if (min(pred.data[, var]) != max(pred.data[, var])) {
              with(p.data[1:100, ], lines(p.x, p.y, col = "black", lwd = 2))
              with(p.data[101:200, ], lines(p.x, p.y, col = "green4", lwd = 2))
              with(p.data[201:300, ], lines(p.x, p.y, col = "magenta4", lwd = 2))
            }
          }
          dev.off()
          k <- k + 1
        }
      }
    }
    
  }
  # calculate significance stars
  stars <- c(0.001, 0.01, 0.05, 0.1)
  names(stars) <- c("***", "**", "*", ".")
  for (i in 1:length(stars)) {
    LRs$stars[which(LRs$p <= stars[i] & is.na(LRs$stars))] <- names(stars)[i]
  }
  return(LRs)
}

lr.plot <- function(lr.mod1, lr.mod2 = NULL, lr.mod3 = NULL, lr.mod4 = NULL, order = NULL, plt = 0.4, leg.txt = NULL, leg.x = "topright", leg.y = NULL, leg.cex = 1, star.pos = 10, legend = TRUE, ylim = NULL, cex.pts = 1, srt = NULL, cex.axis = 1, se.val = NULL, ...) {
  # function to plot the likelihood ratios for up to four models
  # input - likelihood ratios from lr.calc (up to four)
  #       - order: a list of numbers for the order of the code
  #       - plt: the plot parameter for the x-axis 
  #       - leg.text: the words for the legend, otherwise defaults to the model names
  # output - the x-coordinates of the bars
  # calculate number of comparisons
  # create a list of the models
  lr.mods <- c("lr.mod1", "lr.mod2", "lr.mod3", "lr.mod4")
  num.mod <- 0
  # create the dataframe for analysis
  for (i in 1:length(lr.mods)) {
    # get each model
    tmp <- eval(parse(text = lr.mods[i]))
    # if there is a model
    if (!is.null(tmp)) {
      num.mod <- num.mod + 1
      # rename the variables, so they are unique
      names(tmp)[2:4] <- paste(names(tmp)[2:4], lr.mods[i], sep = ".")
      # if it is the first one
      if (i == 1) {
        all.lr.mods <- tmp
      } else {
        all.lr.mods <- merge(all.lr.mods, tmp, all = TRUE)
      }
    }
  }
  # make sure order of se and of lr values are the same when multiple models are present
  if (!is.null(se.val)) {
    if(!is.null(dim(se.val))) {
      se.val <- se.val[, as.character(all.lr.mods$names)]
    }
  }
  
  # order the points, if order is present
  if (!is.null(order)) {
    if (length(order) != nrow(all.lr.mods)) stop("Different lengths in order and lr dataframe")
    all.lr.mods <- all.lr.mods[order,]
    if(!is.null(se.val)) {
      if (is.null(dim(se.val))) {
        se.val <- se.val[order]
      } else {
        se.val <- se.val[, order]
      }
    }
     
  }
  
  # generate the data for the barplot
  bar.lr.mods <- t(as.matrix(all.lr.mods[grep("^lr", names(all.lr.mods))]))
  
  # plot the absolute coefficients
  plt.def <- par("plt")
  on.exit(par(plt.def))
  par(plt = c(plt.def[1:2], plt, plt.def[4]))
  x.axis <- "s"
  if (!is.null(srt)) x.axis <- "n"
  if (is.null(ylim)) {
    if (is.null(se.val)) {
      ylim <- c(0, max(bar.lr.mods, na.rm = T) + star.pos + 5)
    } else {
      ylim.max <- which(bar.lr.mods == max(bar.lr.mods, na.rm = T))
      ylim <- c(0, bar.lr.mods[ylim.max] + star.pos + 5 + se.val[ylim.max])  
    }    
  }
  pts.x <- barplot(bar.lr.mods, names = all.lr.mods$names, beside = T, las = 2, ylim = ylim, xaxt = x.axis, ...)
  if (!is.null(srt)) {
    text(c(pts.x[2,]), par("usr")[3] - 10, srt = srt, labels = all.lr.mods$names, adj = 1, xpd = TRUE, cex = cex.axis)
  }
  star.se <- 0
  if (!is.null(se.val)) {
    # error bar function
    error.bars <- function(xv, yv, z) {
      g <- (pts.x[2] - pts.x[1]) * 0.2
      for (i in 1:length(xv)) {
        lines(c(xv[i], xv[i]), c(yv[i] + z[i], yv[i] - z[i]))
        lines(c(xv[i] - g, xv[i] + g), c(yv[i] + z[i], yv[i] + z[i]))
        lines(c(xv[i] - g, xv[i] + g), c(yv[i] - z[i], yv[i] - z[i]))
      }
    }
    # add them to the plot
    error.bars(pts.x, bar.lr.mods, se.val)
    
  }
  for (i in 1:num.mod) {
    if(!is.null(lr.mods[i])) {
      if (!is.null(dim(se.val))) {
        star.se <- se.val[i, ]
      } else {
        star.se <- se.val[i]
      }
      text(pts.x[i, ], all.lr.mods[, grep("^lr", names(all.lr.mods))[i]] + star.pos + star.se, all.lr.mods[, grep("^stars", names(all.lr.mods))[i]], cex = cex.pts)
    }
  }
  if (legend) {
    if (is.null(leg.txt)) {
      leg.txt <- lr.mods[1:num.mod]
    }
    legend(leg.x, leg.y, leg.txt, fill = gray.colors(length(lr.mods))[1:length(lr.mods)], cex = leg.cex)
  }
  
  return(pts.x)
}


# lr.calc(mod.sar.opf)
# ms.lr
# tmp <- data.frame(names = model.evs(mod.sar.opf), group = c("Temp", "Strat", "Chla", "Sal", "Sal", "Ocean", "Dissol", "Temp", "Temp", "Temp"))
# lr.calc(mod.sar.opf, tmp)
# ms.lr.group