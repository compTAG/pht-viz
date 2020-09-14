
use_package <- function(p) {
  if (!is.element(as.character(p), installed.packages()[, 1]))
    install.packages(p, dep = TRUE)
  library(p, character.only = TRUE)
}
  

use_package('TDA')
use_package('plotrix')
use_package('animation')
#use_package('extra_font')

plot.diagram <-
  function(x, diagLim = NULL, dimension = NULL, col = NULL, rotated = FALSE,
           barcode = FALSE, band = NULL, lab.line = 2.2, colorBand = "pink",
           colorBorder = NA, add = FALSE, trailer=FALSE, cex = 1, ...) {
    # 2019-11-30
    # temporary fix for _R_CHECK_LENGTH_1_LOGIC2_ ( 'length(x) = 2 > 1' in coercion to 'logical(1)' ) error
    # if (((class(x) != "diagram" && class(x) != "matrix" && !is.data.frame(x)) ||
    # if (((any(class(x) != "diagram") && any(class(x) != "matrix") && !is.data.frame(x)) ||
    #     NCOL(x) != 3) && (!is.numeric(x) || length(x) != 3)) {
    #   stop("x should be a diagram, or a P by 3 matrix")
    # }
    if (!is.null(diagLim) && (!is.numeric(diagLim) || length(diagLim) != 2)) {
      stop("diagLim should be a vector of length 2")
    }
    if (!is.null(dimension) && (!is.numeric(dimension) ||
                                length(dimension) != 1 || any(dimension < 0))) {
      stop("dimension should be a nonnegative integer")
    }
    if (!is.logical(rotated)) {
      stop("rotated should be logical")
    }
    if (!is.logical(barcode)) {
      stop("barcode should be logical")
    }
    if (!is.null(band) && (!is.numeric(band) || length(band) != 1)) {
      stop("band should be a number")
    }
    if (!is.logical(add)) {
      stop("add should be logical")
    }
    # 2019-11-30
    # temporary fix for _R_CHECK_LENGTH_1_LOGIC2_ ( 'length(x) = 2 > 1' in coercion to 'logical(1)' ) error
    # if (class(x) != "diagram" && is.numeric(x)) {
    if (any(class(x) != "diagram") && is.numeric(x)) {
      x <- matrix(x, ncol = 3, dimnames = list(NULL, colnames(x)))
    }
    # diagLim should be finite
    if (is.null(diagLim) || any(diagLim == -Inf) || any(diagLim == Inf)) {
      # 2019-11-30
      # temporary fix for _R_CHECK_LENGTH_1_LOGIC2_ ( 'length(x) = 2 > 1' in coercion to 'logical(1)' ) error
      # if (class(x) == "diagram") {
      if (any(class(x) == "diagram")) {
        diagLim <- attributes(x)[["scale"]]
      } else {
        nonInf <- which(
          x[, 2] != Inf & x[, 2] != -Inf & x[, 3] != Inf & x[, 3] != -Inf)
        if (length(nonInf) > 0) {
          diagLim <- c(min(x[nonInf, 2:3]), max(x[nonInf, 2:3]))
        } else { # when diagram is empty or all the points are Inf
          diagLim <- c(0,0)
        }
      }
    }
    # all the points outside diagLim are trimmed to diagLim
    #x[x[, 2] < diagLim[1], 2] <- diagLim[1]
    #x[x[, 3] < diagLim[1], 3] <- diagLim[1]
    #x[x[, 2] > diagLim[2], 2] <- diagLim[2]
    #x[x[, 3] > diagLim[2], 3] <- diagLim[2]
    sublevel <- TRUE
    # use any() function to deal with when colnames(x) is NULL
    if (any(colnames(x)[3] == "Birth")) {
      sublevel <- FALSE
    }
    if (!is.null(dimension)) {
      x <- x[which(x[, 1] == dimension), , drop = FALSE]
    }
    if (is.null(match.call()[["pch"]])) {
      symb <- x[, 1]
      for (i in seq(along = symb)) {
        if (symb[i] == 0) {
          symb[i] <- 16
        } else if (symb[i] == 1) {
          symb[i] <- 2
        } else if (symb[i] == 2) {
          symb[i] <- 5
        } else if (symb[i] == 5) {
          symb[i] <- 1
        }
      }
    } else {
      symb <- match.call()[["pch"]]
    }
    if (is.null(col)){
      if(trailer == FALSE){
        col <- x[, 1] + 1  # betti0 black, betti1 red
      } else {
        col <- x[, 1] + 3  # betti0 black, betti1 red
      }
      
      for (i in seq(along = x[, 1])) {
        if (x[i, 1] == 2) {
          col[i] <- 4    # betti2 blue
        }
        if (x[i, 1] == 3) {
          col[i] <- 3    # betti3 green
        }
      }
    }
    ### barcode plot
    if (barcode) {
      if (length(col) == 1) {
        col <- rep(col, nrow(x))
      }
      ## first we sort the bars
      maxD <- max(x[, 1])
      minD <- min(x[, 1])
      if (maxD > 0) {
        sortedDiag <- x
        sortedCol <- col
        posD <- which(x[, 1] == minD)
        lD <- 0
        for (dd in (minD):maxD) {
          oldlD <- lD
          posD <- which(x[,1] == dd)
          if (length(posD) != 0) {
            lD <- oldlD + length(posD)        
            sortedDiag[(oldlD + 1):(lD), ] <- x[posD, ]
            sortedCol[(oldlD + 1):(lD)] <- col[posD]
          }
        }
        x <- sortedDiag
        col <- sortedCol
      } 
      ## now we plot the bars
      left <- x[, 2]
      right <- x[, 3]
      n <- length(left)
      Bmax <- max(right)
      Bmin <- min(left)
      graphics::plot(c(Bmin, Bmax), c(1, n + 1), type = "n", xlab = "",
                     ylab = "", xlim = c(Bmin, Bmax), ylim = c(0, n + 1), xaxt = "n",
                     yaxt = "n", ...)
      graphics::axis(1)
      graphics::title(xlab = "time", line = lab.line)
      lwid <- rep(2,n)
      ltype <- rep(1,n)
      if (!is.null(band)){
        for(i in seq_len(n)) {
          if ((x[i, 3] - x[i, 2]) <= band) {
            ltype[i] <- 3
            lwid[i] <- 1.5
          }
        }
      }
      graphics::segments(left, 1:n, right, 1:n, lwd = lwid, lty = ltype,
                         col = col)
    } else{  ### diagram plot
      if (rotated == TRUE) {
        if (add == FALSE) {
          graphics::plot(0, 0,type = "n", axes = FALSE, xlim = diagLim,
                         ylim = diagLim, xlab = " ", ylab = " ", ...)
        }
        if (!is.null(band)) {
          graphics::polygon(c(0, diagLim[2] + 1, diagLim[2] + 1, 0),
                            c(0, 0, band, band), col = colorBand, lwd = 1.5,
                            border = colorBorder)
        }
        graphics::points((x[, 2] + x[, 3]) / 2, (x[, 3]-x[, 2]) / 2, col = col,
                         pch = symb, lwd = 2, cex = cex)
      } else{
        if (add == FALSE) {
          graphics::plot(0, 0, type = "n", axes = FALSE, xlim = diagLim,
                         ylim = diagLim, xlab = " ", ylab = " ", ...)
        }
        if (!is.null(band)) {
          graphics::polygon(
            c(diagLim[1] - 1, diagLim[2] + 1, diagLim[2] + 1, diagLim[1] - 1),
            c(diagLim[1] - 1,diagLim[2] + 1, diagLim[2] + 1 + band,
              diagLim[1] - 1 + band),
            col = colorBand, lwd = 1.5, border = colorBorder)
        }
        graphics::points(x[, 2], x[, 3], pch = symb, lwd = 2, cex = cex, col = col)
        graphics::abline(0, 1)
      }
      if (add==FALSE){
        graphics::axis(1)
        graphics::axis(2)
        if (sublevel) {
          if (!rotated) {
            graphics::title(main = "", xlab = "Birth", ylab = "Death",
                            line = lab.line)
          } else {
            graphics::title(main = "", ylab = "(Death-Birth)/2",
                            xlab = "(Death+Birth)/2", line = lab.line)
          }
        } 
        if (!sublevel) {
          if (!rotated) {
            graphics::title(main = "", xlab = "Death", ylab = "Birth",
                            line = lab.line)
          } else {
            graphics::title(main = "", ylab = "(Birth-Death)/2",
                            xlab = "(Death+Birth)/2", line = lab.line)
          }
        } 
      }
    }
  }





# helper method for making edges
make_edge <- function(u, v) { return(sort(c(u,v))) }

# define a few operations
order_on_a_vector <- function(verts, theta = pi/2){
  X <- matrix(rbind(verts[,1], verts[,2]), 2, length(verts[,1]))
  rot_mat <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), 2, 2, byrow = TRUE)
  rotated <- solve(rot_mat) %*% X
  labels <- as.character(verts[,3][order(rotated[1,])])
  order <- order(rotated[1,])
  height <- rotated[1, order(rotated[1,])]
  return(list(order=order, height = height, labels = labels))
}

edge_equal <- function(e1, e2) {
  return(all(e1 == e2))
}

edge_in_set <- function(e, edges) {
  pred = FALSE
  for (ei in edges) {
    pred <- edge_equal(e, ei) || pred
  }
  return(pred);
}


make_filtration <- function(verts, edges, theta) {
  order_on_vec_list <- order_on_a_vector(verts = verts, theta = theta)
  order_vec <- order_on_vec_list$order
  height_vec <- order_on_vec_list$height
  cmplx = list(order_vec[1])
  values = height_vec[1]
  for (i in 2:length(order_vec)) {
    vi <- order_vec[i]
    added <- list()
    for (j in 1:(i-1)) {
      vj <- order_vec[j]
      e <- make_edge(verts$vert_lab[vi], verts$vert_lab[vj])
      if (edge_in_set(e, edges)) {
        added[length(added)+1] <- list(c(vi, vj))
      }
    }
    
    # add the vertex
    cmplx[length(cmplx)+1] <- vi
    values = c(values, height_vec[i])
    
    # add the edges
    dists <- matrix(dist(height_vec))
    ordered_dists <- dists[order(dists)]
    min_ordered_dists <- ordered_dists[ ordered_dists >= 1/(10^10)][1]
    delta <- min_ordered_dists / (length(added)+1)
    weight <- height_vec[i]
    for (e in added) {
      weight <- weight + delta
      cmplx[length(cmplx)+1] <- list(e)
      values = c(values, weight)
    }
  }
  
  return(list("cmplx" = cmplx, "values" = values, "increasing" = TRUE))
}

fix_diagram <- function(dgm, thresh){
  for(i in 1:length(dgm$diagram[,3])){
    if(dgm$diagram[i,3] - dgm$diagram[i,2] <= thresh){
      dgm$diagram[i,3] <- dgm$diagram[i,2]
      dgm$diagram[i,2] <- 5000
      dgm$diagram[i,3] <- 5000
    }
  }
  
  x <- dgm
  #for(i in 1:nrow(x$diagram)){
    #if (x$diagram[i, 2] == x$diagram[i, 3]){
      #print(x$diagram[i, 2])
      #print(x$diagram[i, 3])
      #dgm$diagram <- dgm$diagram[-c(i),]
      #print(dgm$diagram)
      
   # }
 #}
  return(dgm)
}


#use_package(extrafont)
# font_import() # import system fonts if needed


# Build filtration from direction theta
plot_diagram <- function(verts, theta1, col=NULL, ...){
  order_on_vec_list <- order_on_a_vector(verts = verts, theta = theta1)
  order_vec <- order_on_vec_list$order
  height_vec <- order_on_vec_list$height
  thresh <- min(dist(height_vec))
  filtration <- make_filtration(verts = verts, edges=edges, theta = theta1)
  diag <- filtrationDiag(
    filtration = filtration,
    maxdim = 1,
    location = TRUE,
    diagLimit = 5,
    library = "Dionysus"
  )
  diag2 <- fix_diagram(diag, thresh)
  plot(diag2$diagram, add = TRUE, col=col, ...)
}



plot_complex <- function(verts, edges, ...){
  plot(verts$vert_x, verts$vert_y, ...)
  for(e in edges){
    e_vert <- verts$vert_lab %in% e
    segments(verts$vert_x[e_vert][1], verts$vert_y[e_vert][1],
             verts$vert_x[e_vert][2], verts$vert_y[e_vert][2], lwd = 3)
  }
}


vert_x <- c(0, 1, 2, 3, 4)
vert_y <- c(2, 0, 2, 0, 2)
vert_lab <- c("v1", "v2", "v3", "v4", "v5")
verts <- data.frame(vert_x, vert_y, vert_lab, stringsAsFactors = F)
edges <- list(
  make_edge("v1", "v2"),
  make_edge("v2", "v3"),
  make_edge("v3", "v4"),
  make_edge("v4", "v5")
)

piseq <- seq(0, 2 * pi, length.out = 100)

createGIF <- function(i){
  order_on_vec_list <- order_on_a_vector(verts = verts, theta = i)
  order_vec <- order_on_vec_list$order
  height_vec <- order_on_vec_list$height
  par(family = "sans")
  layout(matrix(c(1,2, 2, 3, 4, 4, 5, 5), 2, 4, byrow = TRUE),
         widths=c(1,1,1,1), heights=c(1,1))
  par(family = "sans")
  plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
  par(family = "sans")
  plot_complex(verts, edges, xaxt = 'n', yaxt = 'n', pch = 20, cex = 1.8, lwd = 1.5, xlab = "", ylab = "")
  for(j in 1:nrow(verts)){
    p1 <- c(cos(i) * height_vec[order_vec == j], sin(i) * height_vec[order_vec == j])
    p2 <- verts[j, 1:2]
    p1x <- as.numeric(p1[1])
    p1y <- as.numeric(p1[2])
    p2x <- as.numeric(p2[1])
    p2y <- as.numeric(p2[2])
    m <- (p1y - p2y) / (p1x - p2x)
    b <- - m * p2x + p2y
    if ((b != Inf) && (m != Inf)) {
      abline(a = b, b = m)
    }
  }
  par(family = "sans")
  plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
  par(family = "sans")
  plot(NULL, xaxt = 'n', yaxt = 'n', xlim=c(-2,2), ylim=c(-2,2), ylab="", xlab="")
  draw.circle(0, 0, 1)
  arrows(0, 0,
         x1 = cos(i), y1 = sin(i), length = 0.25, angle = 30,
         code = 2, col = par("fg"), lty = par("lty"),
         lwd = 4
  )
  par(family = "sans")
  #plot(NULL, xlim=c(-350,350), ylim=c(-350,350), ylab="Death", xlab="Birth", family = "sans")
  plot(NULL, xaxt = 'n', yaxt = 'n', xlim=c(-5,5), ylim=c(-5,5), ylab="", xlab="")
  plot_diagram(verts, i, cex = 3.5)
}

saveGIF(
  {
    for (i in 2:length(piseq)) {
      createGIF(piseq[i])
      trailers <- 8
      for (j in 1:trailers){
        if(i-j >0){
          color <- rgb(.5,.5,.5, alpha=1-(.1*j))
          plot_diagram(verts, piseq[i-j], col=color, cex=3.5)
        }
        
      }
      #color <- rgb(.5, .5, .5, alpha=.9)
      
     # plot_diagram(verts, piseq[i-1], col=color, cex = 3.5)
      #plot_diagram(verts, piseq[i-2], col="gray", cex = 3.5)
      #plot_diagram(verts, piseq[i-3], col="gray", cex = 3.5)
    }
  },
  movie.name = "animation.gif",
  img.name = "Rplot",
  convert = "magick",
  cmd.fun,
  clean = TRUE,
  interval = 0.1,
  ani.width = 960,
  ani.height = 960
)

print(piseq[1])
print(piseq[2])
print(piseq[3])
par()
plot()
X11Font("-*-courier-%s-%s-*-*-%d-*-*-*-*-*-*-*")

