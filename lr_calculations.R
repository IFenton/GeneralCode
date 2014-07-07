## 30 / 5 / 2014
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
  return(EVs)
}


lr.calc <- function(model, EVs = NULL) {
  # function to calculate likelihood ratios
  # input  - model: the model to work from
  #        - EVs: dataframe of variables, and the groups they are in (optional, otherwise calculated from model)
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
  }
  # calculate significance stars
  stars <- c(0.001, 0.01, 0.05, 0.1)
  names(stars) <- c("***", "**", "*", ".")
  for (i in 1:length(stars)) {
    LRs$stars[which(LRs$p <= stars[i] & is.na(LRs$stars))] <- names(stars)[i]
  }
  return(LRs)
}

lr.plot <- function(lr.mod1, lr.mod2 = NULL, lr.mod3 = NULL, lr.mod4 = NULL, order = NULL, plt = 0.4, leg.txt = NULL, leg.x = "topright", leg.y = NULL, star.pos = 10, ...) {
  # function to plot the likelihood ratios for up to four models
  # input - likelihood ratios from lr.calc (up to four)
  #       - order: a list of numbers for the order of the code
  #       - plt: the plot parameter for the x-axis 
  #       - leg.text: the words for the legend, otherwise defaults to the model names
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
  # order the points
  if (!is.null(order)) {
    if (length(order) != nrow(all.lr.mods)) stop("Different lengths in order and lr dataframe")
    all.lr.mods <- all.lr.mods[order,]
  }
  
  # generate the data for the barplot
  bar.lr.mods <- t(as.matrix(all.lr.mods[grep("^lr", names(all.lr.mods))]))
  
  # plot the absolute coefficients
  plt.def <- par("plt")
  on.exit(par(plt.def))
  par(plt = c(plt.def[1:2], plt, plt.def[4]))
  pts.x <- barplot(bar.lr.mods, names = all.lr.mods$names, beside = T, las = 2, ylim = c(0, max(bar.lr.mods, na.rm = T) + 15), ...)
  for (i in 1:num.mod) {
    if(!is.null(lr.mods[i])) {
      text(pts.x[i, ], all.lr.mods[, grep("^lr", names(all.lr.mods))[i]] + star.pos, all.lr.mods[, grep("^stars", names(all.lr.mods))[i]])
    }
  }
  if (is.null(leg.txt)) {
    leg.txt <- lr.mods[1:num.mod]
  }
  legend(leg.x, leg.y, leg.txt, fill = gray.colors(length(lr.mods))[1:length(lr.mods)])
}


# lr.calc(mod.sar.opf)
# ms.lr
# tmp <- data.frame(names = model.evs(mod.sar.opf), group = c("Temp", "Strat", "Chla", "Sal", "Sal", "Ocean", "Dissol", "Temp", "Temp", "Temp"))
# lr.calc(mod.sar.opf, tmp)
# ms.lr.group