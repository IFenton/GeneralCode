## calculating vifs for SAR error models
## 30 / 07 / 2014
## Isabel Fenton

source("C:/Documents/Science/PhD/Code/lr_calculations.R")

vif.sar <- function(model, data) {
  evs <- model.evs(model)
  evs <- gsub("poly\\(", "", evs)
  evs <- gsub("\\,.*", "", evs)
  evs <- unique(evs)
  
  data2 <- data[, evs]
  
  if (length(as.character(model$call)) > 5) warning("Only tested with 4 inputs")
  
  mod.call <- as.character(model$call)
  
  # identify the tolerance from the model
  tmp <- grep("[0-9]", mod.call)
  if (length(tmp) == 1) {
    tol <- as.numeric(mod.call[tmp])
  } else if (length(tmp) == 0) {
    tol <- 1e-10
  } else {
    tol <- as.numeric(mod.call[tmp[1]])
    print("Taking tol.solve as ", tol.solve, ". Is this correct?")
  }
  
  # identify the listw
  listw.num <- which(unlist(lapply(mod.call, function(x) class(eval(parse(text = x))))) == "listw")
  listw.val <- mod.call[listw.num]

  # generate the output
  out <- rep(NA, length(evs))
  names(out) <- evs
  
  for (i in evs) {
    if (is.factor(data2[, i])) {
      out[i] <- NA
    } else {
      # calculate the vif for each ev
      tmp.mod <- errorsarlm(data2[, i] ~ ., eval(parse(text = listw.val)), tol.solve = tol, zero.policy = model$zero.policy, data = data2[, -which(names(data2) == i)])
      out[i] <- 1 / (1 - summary(tmp.mod, Nagelkerke = TRUE)$NK)
    }
  }
  return(out)
}


vif.lmt <- function(model, data) {
  evs <- model.evs(model)
  evs <- gsub("poly\\(", "", evs)
  evs <- gsub("\\,.*", "", evs)
  evs <- unique(evs)
  
  data2 <- data[, evs]
  
  #if (length(as.character(model$call)) > 5) warning("Only tested with 4 inputs")
  
  #mod.call <- as.character(model$call)
  
  # identify the tolerance from the model
#   tmp <- grep("[0-9]", mod.call)
#   if (length(tmp) == 1) {
#     tol <- as.numeric(mod.call[tmp])
#   } else if (length(tmp) == 0) {
#     tol <- 1e-10
#   } else {
#     tol <- as.numeric(mod.call[tmp[1]])
#     print("Taking tol.solve as ", tol.solve, ". Is this correct?")
#   }
#   
#   # identify the listw
#   listw.num <- which(unlist(lapply(mod.call, function(x) class(eval(parse(text = x))))) == "listw")
#   listw.val <- mod.call[listw.num]
#   
#   # generate the output
   out <- rep(NA, length(evs))
  names(out) <- evs
  
  for (i in evs) {
    if (is.factor(data2[, i])) {
      out[i] <- NA
    } else {
      # calculate the vif for each ev
      tmp.mod <- glm(data2[, i] ~ ., data = data2[, -which(names(data2) == i)])
      out[i] <- 1 / (1 - NagelkerkeR2(tmp.mod)$R2)
    }
  }
  return(out)
}
