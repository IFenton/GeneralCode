## function for converting from a spatial matrix into a dataframe of Long, Lat and value
## created 10 / 01 / 2014
## Originally to convert chlorophyll a data into a usable format
## the code starts at the top left corner of the matrix

## input: x.coords, y.coords (n.b. this must be in the correct order,e.g. positive to negative), spatial matrix
## output: data.frame with 3 columns

## example: sp.mat.2.df(-180:180, 89.5:-89.5, chl.mod.dat)

sp.mat.2.df <- function(Long, Lat, sp.mat) {
  # create the initial data frame
  dat <- as.data.frame(cbind(rep(Long, each = nrow(sp.mat)), rep(Lat, ncol(sp.mat))))
  names(dat) <- c("Long", "Lat")
  # populate the new column
  dat$val <- as.vector(sp.mat)
  return(dat)
}

# worked example
data <- rbind(c(1,2,3,4,5,6,7), c(2,4,6,8,10,12,14), c(3,6,9,12,15,18,21))
sp.mat.2.df(1:7, 1:3, data)

# worked example
data <- cbind(c(1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0), c(2, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 1), c(3, 3, 0, 0, 3, 3, 3, 3, 0, 0, 0, 0, 3), c(4, 4, 0, 4, 4, 4, 0, 0, 4, 0, 0, 0, 4), c(3, 3, 3, 0, 0, 3, 0, 0, 3, 3, 0, 3, 3), c(2, 2, 2, 0, 0, 2, 1, 0, 3, 2, 2, 2, 2), c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1))
data2 <- sp.mat.2.df(seq(90, -90, by = -30), seq(-180, 180, by = 30), data)
with(data2, distrib.map(Lat, Long, val, pch = 15))

