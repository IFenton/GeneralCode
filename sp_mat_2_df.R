## function for converting from a spatial matrix into a dataframe of Long, Lat and value
## created 10 / 01 / 2014
## Originally to convert chlorophyll a data into a usable format
## the code starts at the top left corner of the matrix

## input: x.coords, y.coords (n.b. this must be in the correct order,e.g. positive to negative), spatial matrix
## output: data.frame with 3 columns

## example: sp.mat.2.df(-180:180, 89.5:-89.5, chl.mod.dat)

sp.mat.2.df <- function(x, y, sp.mat) {
  # create the initial data frame
  dat <- as.data.frame(cbind(rep(x, each = nrow(sp.mat)), rep(y, ncol(sp.mat))))
  names(dat) <- c("Long", "Lat")
  # populate the new column
  dat$val <- as.vector(sp.mat)
  return(dat)
}

# # worked example
data <- rbind(c(1,2,3,4,5,6,7),c(2,4,6,8,10,12,14), c(3,6,9,12,15,18,21))
sp.mat.2.df(1:7, 1:3, data)

