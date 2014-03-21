## Created 09 / 01 / 2014
## Isabel Fenton (work for Andy)
##
## adding points to the distrib.filled maps
##
## function:
## filled.points 
## distrib.filled.pts - distrib.filled with the capability to add points

source("C:\\Documents\\Science\\PhD\\Code\\maps.R")

# distrib.filled.pts ------------------------------------------------------
distrib.filled.pts <- function (xc, yc, colc, x1, y1, palette = "log.heat", pal.cutoff = 0.1, 
                            pal.col.main = "grey", pal.col.back = "white", key = TRUE, shift = FALSE,
                            maintitle = "", subtitle = "", keytitle = "", key.cex = 0.9, 
                            sub.italics = FALSE, pch = 20, 
                            ylim = c(-90, 90), xlim = NULL, add = FALSE, asp = 1, xlab = "", ylab = "", 
                            xaxt = "n", yaxt = "n", bty="n", eps = 0.1, col = 1, fill = TRUE, 
                            col.water = "steelblue2", col.land = "green4", alpha = NA,nlevels = 20, 
                            pts.col = "black", pts.pch = 16, ...)
{ 
  ## to plot the graph centered on the Pacific
  if (shift) {
    xc <- ifelse(xc < 0, xc + 360, xc)
    x1 <- ifelse(x1 < 0, x1 + 360, x1)
  }
    
  x <- interp(xc, yc, colc, duplicate = "mean")
  ## needs to contour to be in the interp format
  if (is.list(x)) {
    color <- x$z
    y <- x$y
    x <- x$x
  }
  else stop("no 'z' matrix specified")
  
  
  zlim <- range(color, finite = TRUE)
  levels <- pretty(zlim, nlevels)
  if (palette == "percent.col"){
    fill.col <- percent.col(length(levels), main.col = pal.col.main, back.col = pal.col.back,
                            cutoff = pal.cutoff)
  } else {
    fill.col <- do.call(palette, as.list(length(levels) - 1))
  }
  
  if (key) {
    par(mfrow=c(2,1))
    # set up the layout for a key
    layout(matrix(c(1,2),1,2,byrow=TRUE),c(5,1),3,TRUE)
    
    # if factor
    if (is.factor(color)) {
      world.map(main = maintitle, subtitle = subtitle, sub.italics = sub.italics, 
                fill = fill, shift = shift, col.water = col.water, col.land = col.land,
                ylim = ylim, xlim = xlim, add = add, asp = asp, xlab = xlab, ylab = ylab, 
                xaxt = xaxt, yaxt = yaxt, bty = bty, eps = eps, col = col, alpha = alpha, ...)
      #world.points(x = x, y = y, color = as.numeric(color), palette = palette, pch = pch, ...)
      
      points(x1, y1, col = pts.col, pch = pts.pch)
      
      par(mai = c(1,0.25,1,0.85))
      
      plot.col <- length(levels(color))
      nam.hist <- levels(color)
      
      axis.spacing <- c(0,0.5,0)
      
    } else {
      # add the key
      if (sum(as.integer(color) != (color),na.rm = T) > 0) {
        # plot the world.map outline and add the points
        
        world.map(main = maintitle, subtitle = subtitle, sub.italics = sub.italics, 
                  fill = fill, shift = shift, col.water = col.water, col.land = col.land,
                  ylim = ylim, xlim = xlim, add = add, asp = asp, xlab = xlab, ylab = ylab, 
                  xaxt = xaxt, yaxt = yaxt, bty = bty, eps = eps, col = col, alpha = alpha, ...)
        .filled.contour(as.double(x), as.double(y), color, as.double(levels), 
                        col = fill.col)
        
        if(shift) {
          world.shiftcol.contour(world.dat,col.land = col.land,col.water = col.water, xlim = xlim, ylim = ylim)
        } else {          
          world.oceancol.contour(world.dat,col.land = col.land,col.water = col.water, xlim = xlim, ylim = ylim)
        }
        
        #world.points(x = x, y = y, color = color, palette = palette, pch = pch, ...)
        
        points(x1, y1, col = pts.col, pch = pts.pch)
        
        par(mai = c(1,0.25,1,0.85))
        
        a <- nchar(as.integer(max(color, na.rm = T)))
        
        plot.col <- as.integer(max(color, na.rm = T) * 10^(4 - a)+ 0.5)
        
        nam <- seq(max(color, na.rm = T)/plot.col,max(color, na.rm = T),length.out=plot.col)
        plot.nam <- 1:plot.col
        nam.hist<-rep(NA,length(nam))
        
        # difference between less than 5 and more than 5
        if (round(plot.col,-nchar(plot.col)) != 0) {
          spacing <- round(plot.col, -nchar(plot.col))/10
        } else {
          spacing <- 10^(nchar(plot.col))/25
        }
        nam.hist[which(plot.nam %% spacing == 0)] <- round(nam[which(plot.nam %% spacing == 0)],3-nchar(as.integer(max(nam))))
        nam.hist<-c(0,nam.hist)        
        
      } else {
        # plot the world.map outline and add the points
        world.map(main = maintitle, subtitle = subtitle, sub.italics = sub.italics, 
                  fill = fill, shift = shift, col.water = col.water, col.land = col.land,
                  ylim = ylim, xlim = xlim, add = add, asp = asp, xlab = xlab, ylab = ylab, 
                  xaxt = xaxt, yaxt = yaxt, bty = bty, eps = eps, col = col, alpha = alpha, ...)
        .filled.contour(as.double(x), as.double(y), color, as.double(levels), 
                        col = fill.col)
        #world.points(x = x, y = y, color = color, palette = palette, pch = pch, ...)
        
        points(x1, y1, col = pts.col, pch = pts.pch)
        
        par(mai = c(1,0.25,1,0.85))      
        plot.col <- max(color, na.rm = T)
        
        nam <- 1:plot.col
        nam.hist<-rep(NA,length(nam))
        
        
        if (round(plot.col,-nchar(plot.col)) != 0) {
          spacing <- round(plot.col, -nchar(plot.col))/10
        } else {
          spacing <- 10^(nchar(plot.col))/20
        }
        nam.hist[which(nam %% spacing == 0)] <- nam[which(nam %% spacing == 0)]
        if(min(color, na.rm = T) == 0) nam.hist<-c(0,nam.hist)
      }
      axis.spacing <- c(0,1,0)
    }
    key<-rep(1,plot.col)
    if(!is.factor(color) && (min(color, na.rm = T) == 0 || sum(as.integer(color) != (color), na.rm = T) > 0)) key <- c(1,key)
    
    if (palette == "log.heat") {
      bar.col<-log.heat(plot.col)
      if(!is.factor(color) && (min(color, na.rm = T) == 0 || sum(as.integer(color) != (color), na.rm = T) > 0)) bar.col <- c(hsv(1,0,1),bar.col)
    }
    if (palette == "rev.log.heat") {
      bar.col<-rev.log.heat(plot.col)
      if(!is.factor(color) && (min(color, na.rm = T) == 0 || sum(as.integer(color) != (color), na.rm = T) > 0)) bar.col <- c(hsv(1/6,1,1),bar.col)
    }
    if (palette == "heat.colors") {
      bar.col<-heat.colors(plot.col)
      if(!is.factor(color) && (min(color, na.rm = T) == 0 || sum(as.integer(color) != (color), na.rm = T) > 0)) bar.col <- c(hsv(1/6,1,1),bar.col)
    }
    
    if (palette == "water.colors") {
      bar.col<-water.colors(plot.col)
      if(!is.factor(color) && (min(color, na.rm = T) == 0 || sum(as.integer(color) != (color), na.rm = T) > 0)) bar.col <- c(hsv(1,0,1),bar.col)
    }
    if (palette == "rainbow") bar.col<-rainbow(plot.col)
    if (palette == "none") bar.col <- 1:length(levels(color))
    
    barplot(key, names.arg = nam.hist, main = keytitle, horiz=TRUE, space=0, border=NA, col=bar.col,
            fg="white",las=1,mgp=axis.spacing,xaxt = "n", cex.names = 0.8, cex.main = key.cex, font.main = 1)
    
    par(mai=c(1.02,0.82,0.82,0.42))
    par(mfrow = c(1,1))
    
  } else {
    ## if no key
    if (is.factor(color)) {
      world.map(main = maintitle, subtitle = subtitle, sub.italics = sub.italics, 
                fill = fill, shift = shift, col.water = col.water, col.land = col.land,
                ylim = ylim, xlim = xlim, add = add, asp = asp, xlab = xlab, ylab = ylab, 
                xaxt = xaxt, yaxt = yaxt, bty = bty, eps = eps, col = col, alpha = alpha, ...)
      #world.points(x = x, y = y, color = as.numeric(color), palette = palette, pch = pch, ...)
      
      points(x1, y1, col = pts.col, pch = pts.pch)
      
    } else {
      world.map(main = maintitle, subtitle = subtitle, sub.italics = sub.italics, 
                fill = fill, shift = shift, col.water = col.water, col.land = col.land,
                ylim = ylim, xlim = xlim, add = add, asp = asp, xlab = xlab, ylab = ylab, 
                xaxt = xaxt, yaxt = yaxt, bty = bty, eps = eps, col = col, alpha = alpha, ...)
      .filled.contour(as.double(x), as.double(y), color, as.double(levels), 
                      col = fill.col)
      
      if(shift) {
        world.shiftcol.contour(world.dat,col.land = col.land,col.water = col.water, xlim = xlim, ylim = ylim)
      } else {          
        world.oceancol.contour(world.dat,col.land = col.land,col.water = col.water, xlim = xlim, ylim = ylim)
      }
      
      #world.points(x = x, y = y, color = color, palette = palette, pch = pch, ...)
      
      points(x1, y1, col = pts.col, pch = pts.pch)
      
      
    }
  }
  par(mar = c(5.1,4.1,4.1,2.1))
}

## filled.points -----------------------------------------------------------
# filled contour plus points
filled.points <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, length.out = ncol(z)), z,
                           x.points, y.points, y.points.lab = "", set.layout = T,
                           xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE),
                           levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
                           col = color.palette(length(levels) - 1), plot.title, plot.axes, 
                           key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
                           axes = TRUE, frame.plot = axes, ...) 
{
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  on.exit(par(par.orig))
  if(set.layout)
  {
    w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
  }
  par(las = las)
  mar <- mar.orig
  mar[4L] <- mar[2L]
  mar[2L] <- 1
  par(mar = mar)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
              yaxs = "i")
  
  # remove the boxes along the key
  rect(0, levels[-length(levels)], 1, levels[-1L], col = col, border = NA)
  
  if (missing(key.axes)) {
    if (axes) 
      axis(4)
  }
  else key.axes
  box()
  if (!missing(key.title)) 
    key.title
  mar <- mar.orig
  mar[4L] <- mar[4L] + 1
  par(mar = mar)
  plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L) 
    stop("no proper 'z' matrix specified")
  if (!is.double(z)) 
    storage.mode(z) <- "double"
  .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                  col = col)
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) 
    box()
  if (missing(plot.title)) 
    title(...)
  else plot.title
  # add more points
  par(new = T)
  plot(x.points, y.points, bty = "n", axes = F, ylab = "", xlab = "", pch = 20)
  axis(4, las = 1)
  mtext(side = 4, line = 2.5, y.points.lab, las = 0)
  invisible()
}

