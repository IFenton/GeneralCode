# Code for plotting bathymetric maps
# Created: 15 / 05 / 2015
# Last edited: 15 / 05 / 2015
# Authors: Tom Ezard, Isabel Fenton

library(marmap)


## 1. Create a more useful error function ----------------------------------
# The getNOAA.bathy function gives a generic response to a failed attempt to download data from the website irrespective of the reason. Therefore create a function that gives more useful info.

getNOAA.error <- function (lon1, lon2, lat1, lat2, resolution = 4, keep = FALSE, 
                           antimeridian = FALSE) {
  # this is based on the getNOAA.bathy, however it is edited to interpret the errors.
  x1 = x2 = y1 = y2 = NULL
  if (lon1 < lon2) {
    x1 <- lon1
    x2 <- lon2
  }
  else {
    x2 <- lon1
    x1 <- lon2
  }
  if (lat1 < lat2) {
    y1 <- lat1
    y2 <- lat2
  }
  else {
    y2 <- lat1
    y1 <- lat2
  }
  res = resolution * 0.0166666666666667
  fetch <- function(x1, y1, x2, y2, res) {
    WEB.REQUEST <- paste("http://mapserver.ngdc.noaa.gov/cgi-bin/public/wcs/etopo1.xyz?filename=etopo1.xyz&request=getcoverage&version=1.0.0&service=wcs&coverage=etopo1&CRS=EPSG:4326&format=xyz&resx=", 
                         res, "&resy=", res, "&bbox=", x1, ",", y1, ",", 
                         x2, ",", y2, sep = "")
    dat <- suppressWarnings(try(read.table(WEB.REQUEST), 
                                silent = TRUE))
    return(dat)
  }
  
  # create fetch2, for when there are errors
  fetch2 <- function(x1, y1, x2, y2, res) {
    WEB.REQUEST <- paste("http://mapserver.ngdc.noaa.gov/cgi-bin/public/wcs/etopo1.xyz?filename=etopo1.xyz&request=getcoverage&version=1.0.0&service=wcs&coverage=etopo1&CRS=EPSG:4326&format=xyz&resx=", 
                         res, "&resy=", res, "&bbox=", x1, ",", y1, ",", 
                         x2, ",", y2, sep = "")
    dat <- scan(WEB.REQUEST, what = "character", sep = "\n")
    return(dat)
  }
  # add fetch two before every version of stop("The NOAA server cannot be reached\n")
  
  if (antimeridian) {
    FILE <- paste("marmap_coord_", x1, ";", y1, ";", x2, 
                  ";", y2, "_res_", resolution, "_anti", ".csv", sep = "")
  }
  else {
    FILE <- paste("marmap_coord_", x1, ";", y1, ";", x2, 
                  ";", y2, "_res_", resolution, ".csv", sep = "")
  }
  if (FILE %in% list.files()) {
    cat("File already exists ; loading '", FILE, "'", sep = "")
    exisiting.bathy <- read.bathy(FILE, header = T)
    return(exisiting.bathy)
  }
  else {
    if (antimeridian) {
      l1 <- x2
      l2 <- 180
      l3 <- -180
      l4 <- x1
      cat("Querying NOAA database ...\n")
      cat("This may take seconds to minutes, depending on grid size\n")
      left <- fetch(l1, y1, l2, y2, res)
      right <- fetch(l3, y1, l4, y2, res)
      if (is(left, "try-error") | is(right, "try-error")) {
        msg1 <- fetch2(l1, y1, l2, y2, res)
        msg2 <- fetch2(l3, y1, l4, y2, res)
        cat(msg1)
        cat(msg2)
        stop("The NOAA server cannot be reached\n")
      }
      else {
        cat("Building bathy matrix ...\n")
        left <- as.bathy(left)
        left <- left[-nrow(left), ]
        right <- as.bathy(right)
        rownames(right) <- as.numeric(rownames(right)) + 
          360
        bath2 <- rbind(left, right)
        class(bath2) <- "bathy"
        bath <- as.xyz(bath2)
      }
    }
    else {
      cat("Querying NOAA database ...\n")
      cat("This may take seconds to minutes, depending on grid size\n")
      bath <- fetch(x1, y1, x2, y2, res)
      if (is(bath, "try-error")) {
        msg <- fetch2(x1, y1, x2, y2, res)
        cat(msg)
        stop("The NOAA server cannot be reached\n")
      }
      else {
        cat("Building bathy matrix ...\n")
        bath2 <- as.bathy(bath)
      }
    }
    if (keep) {
      write.table(bath, file = FILE, sep = ",", quote = FALSE, 
                  row.names = FALSE)
    }
    return(bath2)
  }
}

## 2. Code for plotting the maps -------------------------------------------
glo <- getNOAA.bathy(lon1 = -180, lon2 = 180, lat1 = -80, lat2 = 90)
# currently isn't running, so run getNOAA.error
getNOAA.error(lon1 = -180, lon2 = 180, lat1 = -80, lat2 = 90)
# reason is "Raster size out of range, width and height of resulting coverage must be no more than MAXSIZE=5000"
# This is implying that the resolution is too fine. So check / change
360/5000 * 60 # 4.32
# i.e. minimum possible resolution for this scale is 4.32 minutes, so use 5 minutes instead
glo <- getNOAA.bathy(lon1 = -180, lon2 = 180, lat1 = -80, lat2 = 90, resolution = 5)
bb <- rev(colorRampPalette(c("midnightblue", "darkblue", "slateblue1", "cadetblue1", "aliceblue"))(250))

rms <- as.numeric(rownames(glo))
cms <- as.numeric(colnames(glo))

image(rms, cms, sqrt(0 - glo), col = bb, asp = TRUE, xlab = "Longitude", ylab = "Latitude", ylim = range(-60, 60), xlim = range(-175, 175), bty = 'l')
contour(rms, cms, glo, add = TRUE, levels = 0, drawlabels = FALSE)

#add points etc.
