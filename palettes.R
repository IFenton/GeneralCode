## Created 10 / 01 / 2014
## Isabel Fenton
##
## Contains a range of methods for plotting colours
## a lot of this is based on colours.R
##
## functions:
## percent.col - fill a certain percentage with a given colour
## heat.sat - palette
## heat.v - palette
## log.heat - palette
## rev.log.heat - palette
## water.colors - palette


## basic colors for colorblind ---------------------------------------------
coloring <- c("black","red","blue")

## percent.col; fill a certain percentage with a given colour -------------
## create a function that fills a certain percentage of levels with a given color. 
## created for Andy to colour gyres
percent.col <- function(n.levels, main.col = "white", back.col = "grey", cutoff = 0.3){
  cut <- round(n.levels * cutoff, 0)
  c(rep(main.col, cut), rep(back.col, n.levels - cut - 1))
}

## heat saturation colour --------------------------------------------------
heat.sat<-function (n, h = 1/6, s = s, v = 1, alpha = 1) {
  if ((n <- as.integer(n[1L])) > 0) {
    n
    if (n > 0) hsv(h, s = seq.int(from = 1/(2 * n), to = 1 - 1/(2 * n), length.out = n),
                   v, alpha)
  }
  else character()
}

## heat v colour -----------------------------------------------------------
heat.v<-function (n, h = 1/6, s = 1, v = v, alpha = 1) {
  if ((n <- as.integer(n[1L])) > 0) {
    n
    if (n > 0) hsv(h, s, v = seq.int(from = 1- 1/(2 * n), to = 1/(2 * n), length.out = n)
                   , alpha)
  }
  else character()
}

## log heat colour ---------------------------------------------------------
log.heat<- function (n, alpha = 1) {
  if (n < 2) {
    n <- as.integer (n * 1000 +0.5)
  }
  if ((n <- as.integer(n[1L])) > 0) {
    
    j <- n%/%4
    i <- n - j
    q<-round(log(seq(10,1,length.out=i),10)/6,3)
    c(heat.sat(j, alpha = alpha),hsv(q,1,v = 1, alpha = alpha) )
  }
  else character()
}

## reverse log heat colour -------------------------------------------------
rev.log.heat<- function (n, alpha = 1) {
  if (n < 2) {
    n <- as.integer (n * 1000 +0.5)
  }
  if ((n <- as.integer(n[1L])) > 0) {
    q<-round((1-log(seq(1,10,length.out=n),10))/6,4)
    hsv(q,1,v = 1, alpha = alpha)
  }
  else character()
}

## water colours -----------------------------------------------------------
water.colors<-function(n, alpha = 1) {
  if ((n <- as.integer(n[1L])) > 0) {
    j <- n%/%4
    i <- n - j*2
    q <- round(seq(0.5,0.7,length.out = i),4)
    c(heat.sat(j, h = 0.5, alpha = alpha),hsv(q,1, v = 1, alpha = alpha),heat.v(j, h = 0.7) )
  }
  else character()
}