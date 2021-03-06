## Created 21 / 3 / 2014
## Isabel Fenton
##
## The code for producing world maps, from various BFD files, mainly colours.R
##
## functions:
## world.oceancol - a map with coloured oceans
## world.oceancol.contour - function for contour maps
## world.shiftcol - colour for the shifted world map
## world.shiftcol.contour - colour function for the shifted map with contours
## world.map - edit of the world.map to include the colours
## world.points - add points to a world map
## distrib.map - plot a map with points
## distrib.filled - plot a map with contours

library(maps)
library(fields)
library(akima)
data(world.dat)

# world.oceancol ----------------------------------------------------------
## world ocean, with the ability to colour the ocean
world.oceancol <- function (obj, xlim = c(-180, 180), ylim = c(-90, 90), col.water = "white",
                            col.land = "darkgrey", zones = FALSE, ...) 
{ library(fields)
  data(world.dat)
  
  land <- TRUE
  lakes <- TRUE
  ind <- (1:length(obj$x))[is.na(obj$x)]
  ind <- c(1, ind)
  N <- length(ind) - 1
  lakes.id <- c(46, 53, 25, 26, 28, 27, 4, 47, 48, 51, 49, 50)
  land.id <- (1:N)[-lakes.id]
  # draw a rectangle of the ocean
  rect(-180, -90, 180, 90, col = col.water, border = col.water)
  if (zones) {
    polygon(-183 + tropical$X, -tropical$Y + 90)
    polygon(-181 + subtropical$X, -subtropical$Y + 92)
    polygon(-183 + transitional$X * 2, -transitional$Y * 2 + 90)
    polygon(-183 + subpolar$X * 2, -subpolar$Y * 2 + 90)
  }
  if (land) {
    for (k in land.id) {
      tempi <- ind[k]:ind[k + 1]
      polygon(list(x = obj$x[tempi], y = obj$y[tempi]),
              col = col.land, border = col.land, ...)
    }
  }
  ytemp <- obj$y[ind[21] + 1]
  rect(-180, -90, 180, ytemp, col = col.land, border = col.land,
       ...)
  if (lakes) {
    for (k in lakes.id) {
      tempi <- ind[k]:ind[k + 1]
      polygon(list(x = obj$x[tempi], y = obj$y[tempi]),
              col = col.water, border = col.water, ...)
    }
  }
}

# world.oceancol.contour --------------------------------------------------
world.oceancol.contour <- function (obj, xlim = c(-180, 180), ylim = c(-90, 90), col.water = "white",
                                    col.land = "darkgrey", ...)
{ library(fields)
  data(world.dat)
  
  land <- TRUE
  lakes <- TRUE
  ind <- (1:length(obj$x))[is.na(obj$x)]
  ind <- c(1, ind)
  N <- length(ind) - 1
  lakes.id <- c(46, 53, 25, 26, 28, 27, 4, 47, 48, 51, 49,
                50)
  land.id <- (1:N)[-lakes.id]
  if (land) {
    for (k in land.id) {
      tempi <- ind[k]:ind[k + 1]
      polygon(list(x = obj$x[tempi], y = obj$y[tempi]),
              col = col.land, border = col.land, ...)
    }
  }
  ytemp <- obj$y[ind[21] + 1]
  rect(-180, -90, 180, ytemp, col = col.land, border = col.land,
       ...)
  if (lakes) {
    for (k in lakes.id) {
      tempi <- ind[k]:ind[k + 1]
      polygon(list(x = obj$x[tempi], y = obj$y[tempi]),
              col = col.water, border = col.water, ...)
    }
  }
}

# world.shiftcol ----------------------------------------------------------
# create an colour function for the shifted world map
world.shiftcol <- function (xlim = c(0, 360), ylim = c(-90, 90), col.water = "white",
                            col.land = "darkgrey", zones = FALSE, ...)
{ library(fields)
  data(world.dat)
  
  # add 360 to those points to the west of the meridian
  worldshift.x <- world.dat$x
  worldshift.y <- world.dat$y
  worldshift.x <- ifelse(worldshift.x < 0, worldshift.x + 360, worldshift.x)
  # add new eurasia data
  eurasia.x <- worldshift.x[1344:3951]
  eurasia.y <- worldshift.y[1344:3951]
  eurasia.x1 <- c(NA, 360.0, eurasia.x[470:583], 360.0, 360.0, eurasia.x[586:587], 360.0, NA, eurasia.x[588:1019], 0.0, NA, 360.0, eurasia.x[1020:1176], 360.0, NA, eurasia.x[1177:2607], eurasia.x[2:469], 0.0, NA)
  eurasia.y1 <- c(NA, 49.3, eurasia.y[470:583], 38.6, 38.9, eurasia.y[586:587], 39.9, NA, eurasia.y[588:1019], 36.0, NA, 36.0, eurasia.y[1020:1176], 5.5, NA, eurasia.y[1177:2607], eurasia.y[2:469], 49.3, NA)
  # add new uk data
  uk.x <- worldshift.x[5413:5576]
  uk.y <- worldshift.y[5413:5576]
  uk.x1 <- c(NA, uk.x[119:139], NA, 360.0, uk.x[140:163], uk.x[2:118], 360.0, NA)
  uk.y1 <- c(NA, uk.y[119:139], NA, 53.8, uk.y[140:163], uk.y[2:118], 50.8, NA)
  # add new antarctica data
  antarctica.x <- worldshift.x[5636:5862]
  antarctica.y <- worldshift.y[5636:5862]
  antarctica.x1 <- c(NA, antarctica.x[109:226], antarctica.x[2:108], 360.0, 360.0, 0.0, 0.0, NA)
  antarctica.y1 <- c(NA, antarctica.y[109:226], antarctica.y[2:108], -68.9, -90, -90, -68.9, NA)
  # concatenate this data together
  worldshift.x1 <- c(worldshift.x[1:1343], eurasia.x1, worldshift.x[3952:5412], uk.x1, worldshift.x[5577:5635], antarctica.x1, worldshift.x[5863:6973])
  worldshift.y1 <- c(worldshift.y[1:1343], eurasia.y1, worldshift.y[3952:5412], uk.y1, worldshift.y[5577:5635], antarctica.y1, worldshift.y[5863:6973])
  
  land <- TRUE
  lakes <- TRUE
  ind <- (1:length(worldshift.x1))[is.na(worldshift.x1)]
  ind <- c(1, ind)
  N <- length(ind) - 1
  # edited lakes to colour the correct ones
  lakes.id <- c(3, 7, 29, 30, 31, 32, 50, 51, 52, 53, 54, 55, 57)
  land.id <- (1:N)[-lakes.id]
  # draw a rectangle of the ocean
  rect(0, -90, 360, 90, col = col.water, border = col.water)
  if (zones) {
    polygon(-183 + tropical$X, -tropical$Y + 90)
    polygon(-181 + subtropical$X, -subtropical$Y + 92)
    polygon(-183 + transitional$X * 2, -transitional$Y * 2 + 90)
    polygon(-183 + subpolar$X * 2, -subpolar$Y * 2 + 90)
  }
  if (land) {
    for (k in land.id) {
      tempi <- ind[k]:ind[k + 1]
      polygon(list(x = worldshift.x1[tempi], y = worldshift.y1[tempi]),
              col = col.land, border = col.land, ...)
    }
  }
  
  if (lakes) {
    for (k in lakes.id) {
      tempi <- ind[k]:ind[k + 1]
      polygon(list(x = worldshift.x1[tempi], y = worldshift.y1[tempi]),
              col = col.water, border = col.water, ...)
    }
  }
}

# world.shiftcol.contour --------------------------------------------------
# create an colour function for the shifted world map with a contour
world.shiftcol.contour <- function (xlim = c(0, 360), ylim = c(-90, 90), col.water = "white",
                                    col.land = "darkgrey", zones = FALSE, ...)
{ library(fields)
  data(world.dat)
  
  # add 360 to those points to the west of the meridian
  worldshift.x <- world.dat$x
  worldshift.y <- world.dat$y
  worldshift.x <- ifelse(worldshift.x < 0, worldshift.x + 360, worldshift.x)
  # add new eurasia data
  eurasia.x <- worldshift.x[1344:3951]
  eurasia.y <- worldshift.y[1344:3951]
  eurasia.x1 <- c(NA, 360.0, eurasia.x[470:583], 360.0, 360.0, eurasia.x[586:587], 360.0, NA, eurasia.x[588:1019], 0.0, NA, 360.0, eurasia.x[1020:1176], 360.0, NA, eurasia.x[1177:2607], eurasia.x[2:469], 0.0, NA)
  eurasia.y1 <- c(NA, 49.3, eurasia.y[470:583], 38.6, 38.9, eurasia.y[586:587], 39.9, NA, eurasia.y[588:1019], 36.0, NA, 36.0, eurasia.y[1020:1176], 5.5, NA, eurasia.y[1177:2607], eurasia.y[2:469], 49.3, NA)
  # add new uk data
  uk.x <- worldshift.x[5413:5576]
  uk.y <- worldshift.y[5413:5576]
  uk.x1 <- c(NA, uk.x[119:139], NA, 360.0, uk.x[140:163], uk.x[2:118], 360.0, NA)
  uk.y1 <- c(NA, uk.y[119:139], NA, 53.8, uk.y[140:163], uk.y[2:118], 50.8, NA)
  # add new antarctica data
  antarctica.x <- worldshift.x[5636:5862]
  antarctica.y <- worldshift.y[5636:5862]
  antarctica.x1 <- c(NA, antarctica.x[109:226], antarctica.x[2:108], 360.0, 360.0, 0.0, 0.0, NA)
  antarctica.y1 <- c(NA, antarctica.y[109:226], antarctica.y[2:108], -68.9, -90, -90, -68.9, NA)
  # concatenate this data together
  worldshift.x1 <- c(worldshift.x[1:1343], eurasia.x1, worldshift.x[3952:5412], uk.x1, worldshift.x[5577:5635], antarctica.x1, worldshift.x[5863:6973])
  worldshift.y1 <- c(worldshift.y[1:1343], eurasia.y1, worldshift.y[3952:5412], uk.y1, worldshift.y[5577:5635], antarctica.y1, worldshift.y[5863:6973])
  
  land <- TRUE
  lakes <- TRUE
  ind <- (1:length(worldshift.x1))[is.na(worldshift.x1)]
  ind <- c(1, ind)
  N <- length(ind) - 1
  # edited lakes to colour the correct ones
  lakes.id <- c(3, 7, 29, 30, 31, 32, 50, 51, 52, 53, 54, 55, 57)
  land.id <- (1:N)[-lakes.id]
  if (land) {
    for (k in land.id) {
      tempi <- ind[k]:ind[k + 1]
      polygon(list(x = worldshift.x1[tempi], y = worldshift.y1[tempi]),
              col = col.land, border = col.land, ...)
    }
  }
  
  if (lakes) {
    for (k in lakes.id) {
      tempi <- ind[k]:ind[k + 1]
      polygon(list(x = worldshift.x1[tempi], y = worldshift.y1[tempi]),
              col = col.water, border = col.water, ...)
    }
  }
}

# world.map ---------------------------------------------------------------
# edit the world.map function so it can colour the shifted world map and it colours the oceans
world.map <-  function (ylim = c(-90, 90), xlim = NULL, add = FALSE, asp = 1, zones = FALSE,
                        xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty="n", eps = 0.1,
                        col = 1, shift = FALSE, fill = FALSE, col.water = "white",
                        col.land = "darkgrey", alpha = NA, subtitle = "", sub.italics = FALSE, ...)
{
  data(world.dat)
  par(mai = c(0.2, 0.3, 0.5, 0.3))
  
  # to create a shifted map
  if (shift) {
    ind1 <- !is.na(world.dat$x)
    ind2 <- (world.dat$x < 0)
    world.dat$x[ind2 & ind1] <- world.dat$x[ind2 & ind1] +
      360
    world.dat$x[(world.dat$x <= eps | world.dat$x >= (360 -
                                                        eps)) & ind1] <- NA
  }
  # if xlim not specified, plot the whole world, shifted or not
  if (is.null(xlim)) {
    if (shift) {
      xlim <- c(0, 360)
    }
    else {
      xlim <- c(-180, 180)
    }
  }
  # create a new plot
  if (!add) {
    plot(world.dat, ylim = ylim, xlim = xlim, type = "n",
         xaxt = xaxt, yaxt = yaxt, xlab = xlab, ylab = ylab,
         bty = "n", asp = asp, ...)
  }
  if (!fill) {
    lines(world.dat, err = -1, col = col, ...)
    # add a rectangular border
    if (shift) {
      rect(0, -90, 360, 90, border = col, ...)
    }
    else {
      rect(-180, -90, 180, 90, border = col, ...)
    }
  }
  else {
    if (shift) {
      world.shiftcol(world.dat, col.water = col.water, col.land = col.land,
                     zones = zones, ...)
    }
    else {
      world.oceancol(world.dat, col.water = col.water, col.land = col.land,
                     zones = zones, ...)
    }
  }
  if (!sub.italics) {
    title(main = subtitle, line = -0.3, cex.main = 1, font.main = 1)
  } else {
    title(main = subtitle, line = -0.3, cex.main = 0.9, font.main = 3)
  }
  par(mai = c(1.02, 0.82, 0.82, 0.42))
  invisible()
}

# world.points ------------------------------------------------------------
# create a world.points function that adds points to a world map
world.points <- function(x, y, color, palette = "log.heat", pch = 20, ...) 
{
  par(mai=c(0.2,0.3,0.5,0.3))
  if (grepl("^#(\\d|[a-f]){6,8}$", color[!is.na(color)][1], ignore.case = TRUE)) {# if it is a colour
    points(x, y, pch = pch, col = color, ...)
  } else {
    # to allow colors for non-integers
    if (sum(as.integer(color) != (color), na.rm = T) > 0 && !is.factor(color)) {
      a <- nchar(as.integer(max(color, na.rm = T)))
      plot.color <- as.integer(color * 10^(4 - a) + 0.5)
    } else if (length(grep("#", palette)) != 0) {
      plot.color <- palette[color]
    } else {
      plot.color <- color
    }
    # change scaling so that it can handle zeros and negatives and makes the most effective use of the range
    if (length(grep("#", palette)) == 0) {
      plot.color <- plot.color - min(plot.color, na.rm = TRUE) + 1
    }
    if (palette == "none") {
      points(x, y, pch = pch, col = as.integer(plot.color), ...)
    } else if (length(grep("#", palette)) != 0) {
      points(x, y, pch = pch, col = plot.color, ...)
    } else {
      points(x, y, pch = pch, col = do.call(palette, list(max(plot.color, na.rm = T)))[plot.color], ...) 
    }
  }
  par(mai = c(1.02, 0.82, 0.82, 0.42))
  par(mar = c(5.1, 4.1, 4.1, 2.1))
}

# distrib.map -------------------------------------------------------------
# create a function that plots distribution maps with or without keys
distrib.map <- function (x, y, color, key = TRUE, palette = "log.heat", shift = FALSE, maintitle = "", 
                         subtitle = "", keytitle = "", key.cex = 0.9, sub.italics = FALSE, pch = 20, 
                         ylim = c(-90, 90), xlim = NULL, add = FALSE, asp = 1, xlab = "", ylab = "", 
                         xaxt = "n", yaxt = "n", bty = "n", eps = 0.1, col = 1, fill = TRUE, 
                         col.water = "steelblue2", col.land = "green4", alpha = NA, zones = FALSE, 
                         min.col = min(pretty(color), na.rm = T), max.col = max(pretty(color), na.rm = T), cex.key.names = 0.8, cex = 1, ...)
{ 
  if (is.null(color)) stop("Check color: no data")
  if(!is.factor(color)) {
    if(max(color, na.rm = TRUE) > max.col) warning("Largest color value is greater than scale max")
    if(min(color, na.rm = TRUE) < min.col) warning("Smallest color value is less than scale min")
  }
  
  if (shift) {
    x <- ifelse(x < 0, x + 360, x)
  }
  
  if (palette == "none" && !is.factor(color)) {
    if (diff(range(color)) > 8) stop("Too many colours: Need to specify a palette")
    if (min(color, na.rm = TRUE) < 1) stop("Must be greater than zero: Need to specify a palette")
    if (!all(as.integer(color) == color)) stop("Requires integers: Need to specify a palette")
  }
  
  # set up parameters so if an error occurs, still get correct parameters for next plot
  opar <- par("mfrow", "mar", "mai")
  on.exit(par(opar))
  
  col.values <- color
  
  if (is.factor(color)) { # if the colours are factors
    map.colors <- as.numeric(color)
    
    if (length(grep("^[0-9]", col.values)) > 0) stop("Factors shouldn't start with numbers")
    
    if (key) { # if there is a key
      if (palette == "none") {
        key.colors <- as.numeric(factor(levels(color)))
      } else if (length(grep("#", palette)) != 0) {
        key.colors <- palette[as.numeric(factor(levels(color)))]
      } else {
        key.colors <- do.call(palette, list(length(levels(color))))
      }
      
      key.names <- levels(color)
      key.length <- length(key.names)
    }
  } else { # if not factors
    
    if (min.col != min(color, na.rm = TRUE)) color <- c(min.col, color) # include the minimum and maximum pretty values 
    if (max.col != max(color, na.rm = TRUE)) color <- c(color, max.col)
    color <- color - min.col + 1 # rescale so that the values are positive
    
    if (all(as.integer(color) == color) && max(color, na.rm = TRUE) < 50) { # if using integers, n.b. this is rescaled
      
      if (palette == "none") { # if palette is none
        
        key.colors <- min(color, na.rm = TRUE):max(color, na.rm = TRUE) # of the rescaled colours
        names(key.colors) <- min.col:max.col
        
        map.colors <- key.colors[match(col.values, names(key.colors))] # match colors for the map
        
      } else if (length(grep("#", palette)) != 0) {
        #browser()
        if (length(palette) == length(unique(color))) { # if all the colours are prespecified
          key.colors <- palette
          names(key.colors) <- unique(col.values)
          
          map.colors <- key.colors[match(col.values, names(key.colors))] # match colors for the map
        } else {
        
          key.colors <- min(color, na.rm = TRUE):max(color, na.rm = TRUE) # of the rescaled colours
          names(key.colors) <- min.col:max.col
          
          map.colors <- key.colors[match(col.values, names(key.colors))] # match colors for the map
      }
              
      } else { # if integers but palette isn't none
        
        key.colors <- do.call(palette, list(max(color))) # calculate those colours
        names(key.colors) <- min.col:max.col
        
        map.colors <- key.colors[match(col.values, names(key.colors))] # match colors for the map      
      } 
      
      key.length <- length(key.colors)
      
    } else { # if not integers
      
      # generate a color scale for the key 
      # should be a sequence from min to max of color by regular steps
      # number of steps for key sequence
      diff.col <- max.col - min.col
      key.length <- (diff.col) * 10 ^ -floor(log10(diff.col)) * 1000 + 1
      key.colors <- do.call(palette, list(key.length)) # calculate those colours
      # add names that link these colours to their values
      # get the colors for the plot from this scale
      col.step <- (max.col - min.col) / (key.length - 1)
      num.dec <- -log10(col.step)
      names(key.colors) <- round(seq(min.col, max.col, length.out = as.integer(key.length)), num.dec)
      
      map.colors <- key.colors[match(round(col.values, num.dec), names(key.colors))] # match colors
    }
    
    if (key) { # names for the key
      
      # calculate the numbers for the key; use those from pretty, but need to be able to change the range
      key.values <- pretty(as.numeric(names(key.colors)))
      key.values <- key.values[which(key.values >= min.col & key.values <= max.col)]
      key.names <- rep(NA, key.length)
      key.names[match(key.values, names(key.colors))] <- key.values
    }
    
  }
  
  if (key) { # plot key
    par(mfrow = c(2, 1))
    
    # set up the layout for a key
    layout(matrix(c(1, 2), 1, 2, byrow = TRUE), c(5, 1), 3, TRUE)
    
    # plot the world.map outline and add the points
    world.map(main = maintitle, subtitle = subtitle, sub.italics = sub.italics, 
              fill = fill, shift = shift, col.water = col.water, col.land = col.land,
              ylim = ylim, xlim = xlim, add = add, asp = asp, xlab = xlab, ylab = ylab, 
              xaxt = xaxt, yaxt = yaxt, bty = bty, eps = eps, col = col, alpha = alpha, zones = zones, ...)
    world.points(x = x, y = y, color = map.colors, palette = palette, pch = pch, cex = cex, ...)
    
    par(mai = c(1, 0.25, 1, 0.85))
    
    axis.spacing <- c(0, 1, 0)
    
    key <- rep(1, key.length)
    barplot(key, names.arg = key.names, main = keytitle, horiz = TRUE, space = 0, border = NA, 
            col = key.colors, fg = "white", las = 1, mgp = axis.spacing, xaxt = "n", cex.names = cex.key.names, 
            cex.main = key.cex, font.main = 1)
    
    par(mai = c(1.02, 0.82, 0.82, 0.42))
    par(mfrow = c(1, 1))
    
  } else {
    world.map(main = maintitle, subtitle = subtitle, sub.italics = sub.italics, 
              fill = fill, shift = shift, col.water = col.water, col.land = col.land,
              ylim = ylim, xlim = xlim, add = add, asp = asp, xlab = xlab, ylab = ylab, 
              xaxt = xaxt, yaxt = yaxt, bty = bty, eps = eps, col = col, alpha = alpha, zones = zones, ...)
    world.points(x = x, y = y, color = map.colors, palette = palette, pch = pch, cex = cex, ...)
  }
}

# distrib.filled ----------------------------------------------------------
# distrib.filled
distrib.filled <- function (x, y = NULL, color = NULL, key = TRUE, palette = "log.heat", shift = FALSE, maintitle = "", 
                            subtitle = "", keytitle = "", key.cex = 0.9, sub.italics = FALSE, pch = 20, 
                            ylim = c(-90, 90), xlim = NULL, add = FALSE, asp = 1, xlab = "", ylab = "", 
                            xaxt = "n", yaxt = "n", bty = "n", eps = 0.1, col = 1, fill = TRUE, 
                            col.water = "steelblue2", col.land = "green4", alpha = NA, nlevels = 20, ...)
{ library(akima) # needed for the interp function
  
  # set up parameters so if an error occurs, still get correct parameters for next plot
  opar <- par("mfrow", "mar", "mai")
  on.exit(par(opar))
  
  if (is.list(x)) {
    color <- x$z
    y <- x$y
    x <- x$x
  }
  else {
    x <- interp(x, y, color, duplicate = "mean")
    color <- x$z
    y <- x$y
    x <- x$x
  }
  
  # rescale color so that it is a more useful scale
  if (!is.factor(color)) {
    min.col <- min(pretty(color), na.rm = T) # use pretty to get round numbers
    color <- color - min(pretty(color), na.rm = T)
  }
  
  zlim <- range(color, finite = TRUE)
  levels <- pretty(zlim, nlevels)
  fill.col <- do.call(palette,as.list(length(levels) - 1))
  
  if (shift) {
    x <- ifelse(x < 0, x + 360, x)
  }
  
  if (key) {
    par(mfrow=c(2, 1))
    # set up the layout for a key
    layout(matrix(c(1, 2), 1, 2, byrow=TRUE), c(5, 1), 3, TRUE)
    
    # if factor
    if (is.factor(color)) {
      world.map(main = maintitle, subtitle = subtitle, sub.italics = sub.italics, 
                fill = fill, shift = shift, col.water = col.water, col.land = col.land,
                ylim = ylim, xlim = xlim, add = add, asp = asp, xlab = xlab, ylab = ylab, 
                xaxt = xaxt, yaxt = yaxt, bty = bty, eps = eps, col = col, alpha = alpha, ...)
      world.points(x = x, y = y, color = as.numeric(color), palette = palette, pch = pch, ...)
      
      par(mai = c(1, 0.25, 1, 0.85))
      
      plot.col <- length(levels(color))
      nam.hist <- levels(color)
      
      axis.spacing <- c(0, 0.5, 0)
      
    } else {
      # add the key
      if (sum(as.integer(color) != (color), na.rm = T) > 0) {
        # plot the world.map outline and add the points
        
        world.map(main = maintitle, subtitle = subtitle, sub.italics = sub.italics, 
                  fill = fill, shift = shift, col.water = col.water, col.land = col.land,
                  ylim = ylim, xlim = xlim, add = add, asp = asp, xlab = xlab, ylab = ylab, 
                  xaxt = xaxt, yaxt = yaxt, bty = bty, eps = eps, col = col, alpha = alpha, ...)
        .filled.contour(as.double(x), as.double(y), color, as.double(levels), 
                        col = fill.col)
        
        world.oceancol.contour(world.dat, col.land = "green4", col.water = "steelblue2")
        
        #world.points(x = x, y = y, color = color, palette = palette, pch = pch, ...)
        
        par(mai = c(1, 0.25, 1, 0.85))
        
        a <- nchar(as.integer(max(color, na.rm = T)))
        
        plot.col <- as.integer(max(color, na.rm = T) * 10^(4 - a)+ 0.5)
        
        nam <- seq(max(color, na.rm = T) / plot.col, max(color, na.rm = T), length.out = plot.col)
        plot.nam <- 1:plot.col
        nam.hist <- rep(NA, length(nam))
        
        # difference between less than 5 and more than 5
        if (round(plot.col, -nchar(plot.col)) != 0) {
          spacing <- round(plot.col, -nchar(plot.col)) / 10
        } else {
          spacing <- 10^(nchar(plot.col)) / 25
        }
        nam.hist[which(plot.nam %% spacing == 0)] <- round(nam[which(plot.nam %% spacing == 0)], 3 - nchar(as.integer(max(nam, na.rm = T))))
        nam.hist <- c(0, nam.hist) + min.col        
        
      } else {
        # plot the world.map outline and add the points
        world.map(main = maintitle, subtitle = subtitle, sub.italics = sub.italics, 
                  fill = fill, shift = shift, col.water = col.water, col.land = col.land,
                  ylim = ylim, xlim = xlim, add = add, asp = asp, xlab = xlab, ylab = ylab, 
                  xaxt = xaxt, yaxt = yaxt, bty = bty, eps = eps, col = col, alpha = alpha, ...)
        .filled.contour(as.double(x), as.double(y), color, as.double(levels), 
                        col = noquote(palette)(length(levels) - 1))
        world.points(x = x, y = y, color = color, palette = palette, pch = pch, ...)
        
        par(mai = c(1, 0.25, 1, 0.85))      
        plot.col <- max(color, na.rm = T)
        
        nam <- 1:plot.col
        nam.hist <- rep(NA, length(nam))
        
        
        if (round(plot.col,-nchar(plot.col)) != 0) {
          spacing <- round(plot.col, -nchar(plot.col)) / 10
        } else {
          spacing <- 10^(nchar(plot.col)) / 20
        }
        nam.hist[which(nam %% spacing == 0)] <- nam[which(nam %% spacing == 0)]
        if(min(color, na.rm = T) == 0) nam.hist <- c(0,nam.hist) + min.col
      }
      axis.spacing <- c(0, 1, 0)
    }
    key <- rep(1, plot.col)
    if(!is.factor(color) && (min(color, na.rm = T) == 0 || sum(as.integer(color) != (color), na.rm = T) > 0)) key <- c(1, key)
    
    if (palette == "log.heat") {
      bar.col <- log.heat(plot.col)
      if(!is.factor(color) && (min(color, na.rm = T) == 0 || sum(as.integer(color) != (color), na.rm = T) > 0)) bar.col <- c(hsv(1, 0, 1), bar.col)
    }
    if (palette == "rev.log.heat") {
      bar.col <- rev.log.heat(plot.col)
      if(!is.factor(color) && (min(color, na.rm = T) == 0 || sum(as.integer(color) != (color), na.rm = T) > 0)) bar.col <- c(hsv(1/6, 1, 1), bar.col)
    }
    if (palette == "heat.colors") {
      bar.col <- heat.colors(plot.col)
      if(!is.factor(color) && (min(color, na.rm = T) == 0 || sum(as.integer(color) != (color), na.rm = T) > 0)) bar.col <- c(hsv(1/6, 1, 1), bar.col)
    }
    
    if (palette == "water.colors") {
      bar.col <- water.colors(plot.col)
      if(!is.factor(color) && (min(color, na.rm = T) == 0 || sum(as.integer(color) != (color), na.rm = T) > 0)) bar.col <- c(hsv(1, 0, 1), bar.col)
    }
    if (palette == "rainbow") bar.col <- rainbow(plot.col)
    if (palette == "none") bar.col <- 1:length(levels(color))
    
    barplot(key, names.arg = nam.hist, main = keytitle, horiz = TRUE, space = 0, border = NA, col = bar.col,
            fg = "white", las = 1, mgp = axis.spacing, xaxt = "n", cex.names = 0.8, cex.main = key.cex, font.main = 1)
    
    par(mai = c(1.02, 0.82, 0.82, 0.42))
    par(mfrow = c(1, 1))
    
  } else {
    if (is.factor(color)) {
      world.map(main = maintitle, subtitle = subtitle, sub.italics = sub.italics, 
                fill = fill, shift = shift, col.water = col.water, col.land = col.land,
                ylim = ylim, xlim = xlim, add = add, asp = asp, xlab = xlab, ylab = ylab, 
                xaxt = xaxt, yaxt = yaxt, bty = bty, eps = eps, col = col, alpha = alpha, ...)
      world.points(x = x, y = y, color = as.numeric(color), palette = palette, pch = pch, ...)
    } else {
      world.map(main = maintitle, subtitle = subtitle, sub.italics = sub.italics, 
                fill = fill, shift = shift, col.water = col.water, col.land = col.land,
                ylim = ylim, xlim = xlim, add = add, asp = asp, xlab = xlab, ylab = ylab, 
                xaxt = xaxt, yaxt = yaxt, bty = bty, eps = eps, col = col, alpha = alpha, ...)
      .filled.contour(as.double(x), as.double(y), color, as.double(levels), 
                      col = fill.col)
      
      world.oceancol.contour(world.dat, col.land = "green4", col.water = "steelblue2")
    }
  }
}