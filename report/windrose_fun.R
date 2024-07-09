
## Windrose functions adapted from oce R-package

windrose.calc=function(x,y,data, dtheta = 15){
  # from oce::as.windrose
  ## simplified function
  # x=wnd.dat.xtab$u
  # y=wnd.dat.xtab$v
  if(!missing(data)){
    x=data[,deparse(substitute(x))]
    y=data[,deparse(substitute(y))]
  }
  
  na_rm_val = !is.na(x)&!is.na(y)
  x = x[na_rm_val]
  y = y[na_rm_val]
  xlen <- length(x)
  pi <- atan2(1, 1) * 4
  dt <- dtheta * pi / 180
  dt2 <- dt / 2
  R <- sqrt(x^2 + y^2)
  angle <- atan2(y, x)
  nt <- round(2 * pi / dt)
  count <- mean <- vector("numeric", nt)
  fives <- matrix(0, nt, 5)
  theta <- seq(-pi + dt2, pi - dt2, length.out = nt)
  
  ## binning
  ai <- 1 + floor((angle + pi) / dt)
  ai <- (ai - 1) %% nt + 1 
  if (min(ai) < 1) {stop("problem setting up bins (ai<1)")}
  if (max(ai) > nt) {stop("problem setting up bins (ai>xlen)")}
  for(i in 1:nt){
    inside <- ai == i
    count[i] <- sum(inside)
    mean[i] <- mean(R[inside], na.rm = TRUE)
    fives[i, ] <- boxplot.stats(R[inside])$stats# fivenum(R[inside])
  }
  
  res=list(
    n = length(x), 
    x.mean = mean(x, na.rm = TRUE), y.mean = mean(y, na.rm = TRUE),
    theta = theta * 180 / pi, # in degrees
    count = count, 
    mean = mean, 
    fives = fives
  )
  return(res)
}


windrose_plot = function(
    x,
    type = c("count", "mean", "median", "fivenum"),
    convention = c("meteorological", "oceanographic"),
    # col <- c("red", "pink", "blue", "darkgray"),
    fill.col = "red",
    bx.border.col = "blue",
    border.col = "darkgrey",
    box.cols = c("red","pink"),
    lab.col = NULL,
    lab.val = c("S","W","N","E"),
    lab.cex = 1,
    lab.offset= 0.25,
    max.val = NULL,
    med.lwd = 2,
    med.col = "black",
    ...){
  # paired down customizable plot
  
  if(is.null(lab.col)==T){lab.col=border.col}
  
  # from oce::plot.windrose
  nt <- length(x$theta)
  pi <- 4.0 * atan2(1.0, 1.0)
  if (convention == "meteorological") {
    t <- x$theta * pi / 180 # in radians
  } else {
    t <- pi + x$theta * pi / 180 # in radians
  }
  dt <- t[2] - t[1]
  dt2 <- dt / 2
  
  pin <- par("pin")
  xlim.val <- c(-1.0, 1.0)
  ylim.val <- c(-1.0, 1.0)
  if (pin[1] > pin[2]) {
    xlim.val <- (pin[1] / pin[2]) * xlim.val
  } else {
    ylim.val <- (pin[2] / pin[1]) * ylim.val
  }
  
  plot(xlim.val,ylim.val,type="n",ann=F,axes=F,...)
  # Draw circle and radii
  tt <- seq(0, 2 * pi, length.out = 100)
  px <- cos(tt)
  py <- sin(tt)
  lines(px, py, col = border.col)
  for (i in 1:nt) {
    lines(c(0, cos(t[i] - dt2)), c(0, sin(t[i] - dt2)), lwd = 0.5, col = border.col)
  }
  text( 0, -1, lab.val[1], pos = 1,cex=lab.cex,col=lab.col,offset = lab.offset,...)
  text(-1,  0, lab.val[2], pos = 2,cex=lab.cex,col=lab.col,offset = lab.offset,...)
  text( 0,  1, lab.val[3], pos = 3,cex=lab.cex,col=lab.col,offset = lab.offset,...)
  text( 1,  0, lab.val[4], pos = 4,cex=lab.cex,col=lab.col,offset = lab.offset,...)
  
  if (type == "count") {
    max <- if(is.null(max.val)==F){max.val}else{max(x$count, na.rm = TRUE)}
    for (i in 1:nt) {
      r <- x$count[i] / max
      xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
      ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
      polygon(xlist, ylist, col = fill.col, border = bx.border.col)
    }
  }
  
  if (type == "mean") {
    max <- if(is.null(max.val)==F){max.val}else{max(x$mean, na.rm = TRUE)}
    for (i in 1:nt) {
      r <- x$mean[i] / max
      # cat("t=", t[i], " r=", r, "\n")
      xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
      ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
      polygon(xlist, ylist, col = fill.col, border = bx.border.col)
    }
  }
  
  if (type == "median") {
    max <- if(is.null(max.val)==F){max.val}else{max(x$fives[, 5], na.rm = TRUE)}
    for (i in 1:nt) {
      r <- x$fives[i, 3] / max
      xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
      ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
      polygon(xlist, ylist, col = fill.col, border = bx.border.col)
    }
  }
  
  if (type == "fivenum") {
    max <- if(is.null(max.val)==F){max.val}else{max(x$fives[, 5], na.rm = TRUE)}
    for (i in 1:nt) {
      tm <- t[i] - dt2
      tp <- t[i] + dt2
      for (j in 2:5) {
        r0 <- x$fives[i, j - 1] / max
        r <- x$fives[i, j] / max
        xlist <- c(r0 * cos(tm), r * cos(tm), r * cos(tp), r0 * cos(tp))
        ylist <- c(r0 * sin(tm), r * sin(tm), r * sin(tp), r0 * sin(tp))
        thiscol <- if(length(box.cols)==2){box.cols[c(2, 1, 1, 2)][j - 1]}else{box.cols[j - 1]}
        polygon(xlist, ylist, col = thiscol, border = border.col)
      }
      # Median in black
      r <- x$fives[i, 3] / max
      lines(c(r * cos(tm), r * cos(tp)),
            c(r * sin(tm), r * sin(tp)),
            lwd = med.lwd,col=med.col
      )
    }
  }
  
}
