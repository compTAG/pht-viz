#Imports
library('TDA')
library('plotrix')
library('animation')
library('stringr')

#Overriding plot function for diagrams, will plot persistence diagrams
plot.diagram <- function(x, diagLim=NULL, dimension=NULL, col=NULL, rotated=FALSE,
                         band=NULL, lab.line=2.2, colorBand="pink", colorBorder=NA,
                         add=FALSE, trailer=FALSE, cex=1, ...){
  #If a diagLim is passed in and its not numeric/not length two, stops program
  if(!is.null(diagLim) && (!is.numeric(diagLim) || length(diagLim) != 2)){
    stop("diagLim should be a vector of length 2")
  }
  #If a dimension is passed in and its not numeric/not length one/ less than 0, stops program
  if(!is.null(dimension) && (!is.numeric(dimension) || length(dimension != 1)
                             || any(dimension < 0))){
    stop("dimension shold be a nonnegative integer")
  }
  if(is.null(col)){
    col="black"
  }
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
  
  #If not adding to another graph, plot a box to contain new diagram in
  if(add == FALSE){
    graphics::plot(0, 0,type = "n", axes = FALSE, xlim = diagLim,
                   ylim = diagLim, xlab = " ", ylab = " ", ...)
  }
  
  #Plot each birth/death pair of the persistence diagram
  symb <- match.call()[["pch"]]
  graphics::points(x[, 2], x[, 3], pch = 19, lwd = 1, cex = 1.5, col = col)
  #Plots a diagonal line through the graph
  graphics::abline(0, 1)
  
  #If creating new plot, plots axes and titles with this plot
  if(add == FALSE){
    graphics::axis(1)
    graphics::axis(2)
    graphics::title(main="", xlab="Birth", ylab="Death", line=lab.line)
  }
  
}

# helper method for making edges
make_edge <- function(u, v) { return(sort(c(u,v))) }

#Orders points on a vector
order_on_a_vector <- function(verts, theta=pi/2){
  X <- matrix(rbind(verts[,1], verts[,2]), 2, length(verts[,1]))
  rot_mat <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), 2, 2, byrow = TRUE)
  rotated <- solve(rot_mat) %*% X
  labels <- as.character(verts[,3][order(rotated[1,])])
  order <- order(rotated[1,])
  height <- rotated[1, order(rotated[1,])]
  
  return(list(order=order, height = height, labels = labels))
}



#Checks if all of the edges passed in are equal
edge_equal <- function(e1, e2) {
  return(all(e1 == e2))
}

#Checks whether edge e is in the list of edges
edge_in_set <- function(e, edges) {
  pred = FALSE
  for (ei in edges) {
    pred <- edge_equal(e, ei) || pred
  }
  return(pred);
}

#Using the vertex list and the R TDA Package, this returns a filtration
#from a direction, theta, of the simplicial complex represented by
#the verts and edges
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

#Using the diagram and a given threshold, if two points have similar
#birth and death values (fall into the threshold), then it sets their values
#to outside the scope of the plot (5000) for now
#TODO: Find some less hacky way to implement removing points with equal
#birth and death values
fix_diagram <- function(dgm, thresh, diagonals=FALSE){
  for(i in 1:length(dgm$diagram[,3])){
    if(dgm$diagram[i,3] - dgm$diagram[i,2] <= thresh && diagonals == FALSE){
      dgm$diagram[i,3] <- dgm$diagram[i,2]
      dgm$diagram[i,2] <- 5000
      dgm$diagram[i,3] <- 5000
    }
  }
  
  return(dgm)
}

#This function makes a filtration, uses that to create a PD, fixes the diagram, then
#plots the PD
plot_diagram <- function(verts, theta1, col=NULL, edges, diagonals,  ...){
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
  diag2 <- fix_diagram(diag, thresh, diagonals=diagonals)
  print("DIAGRAM: ")
  print(diag2$diagram)
  plot(diag2$diagram, add = FALSE, col=col, ...)
}

#This diagram will plot the inputted simplicial complex
plot_complex <- function(verts, edges, ...){
  plot(verts$vert_x, verts$vert_y, ...)
  for(e in edges){
    e_vert <- verts$vert_lab %in% e
    segments(verts$vert_x[e_vert][1], verts$vert_y[e_vert][1],
             verts$vert_x[e_vert][2], verts$vert_y[e_vert][2], lwd = 3)
  }
}

#This function reads in file input to construct graph nodes and edges
read_input <- function(graph){
  conn <- file(graph, open="r")
  lines <- readLines(conn)
  close(conn)
  vert_x <- numeric()
  vert_y <- numeric()
  vert_lab <- character()
  edges <- list(20)
  edge_num <- 1
  for (i in 1:length(lines)){
    line <- lines[i]
    if(grepl( "pos", line, fixed = TRUE)){
      vert_labels <- strsplit(line, " ")[[1]]
      
      vert_lab <- c(vert_lab, vert_labels[1])
      pos <- str_extract(line, "[0-9]+,[0-9]+")[[1]]
      positions <- strsplit(pos, ",")[[1]]
      vert_x <- c(vert_x, as.numeric(positions[1]))
      vert_y <- c(vert_y, as.numeric(positions[2]))
    
    } else if(grepl("--", line, fixed=TRUE)){
      edge <- strsplit(line, "--")[[1]]
      edge <- make_edge(trimws(edge[1]), trimws(edge[2]))
      edges[[edge_num]] <-  edge
      
      edge_num <- edge_num + 1
      
      
    }
  }
  verts <- data.frame(vert_x, vert_y, vert_lab, stringsAsFactors = F)
  diagLim <- max(c(vert_x, vert_y)) + 1
  diagLim <- c(-diagLim, diagLim)
  return(list(verts, edges, diagLim))
}

#This function acts on previous functions to create the entire diagram
createPlot <- function(i, diagonals, filtlines, graph){
  if(is.null(graph)){
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
    
    diagLim <- max(c(vert_x, vert_y)) + 1
    diagLim <- c(-diagLim, diagLim)
  } else {
    graph_path <- graph$datapath[1]
    graph_info <- read_input(graph_path)
    verts <- graph_info[[1]]
    edges <- graph_info[[2]]
    diagLim <- graph_info[[3]]
  }
  order_on_vec_list <- order_on_a_vector(verts = verts, theta = i)
  order_vec <- order_on_vec_list$order
  height_vec <- order_on_vec_list$height
  
  layout(matrix(c(1, 2, 2, 3, 4, 4, 5, 5), nrow = 2, ncol = 4, byrow = TRUE), heights=
           c(1, 1))
  plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
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
    if (!is.na(b) && (!is.na(m)) && filtlines){
      if ((b != Inf) && (m != Inf)) {
        abline(a = b, b = m)
      }
    }
  }
  plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '') 
  plot(NULL, xaxt = 'n', yaxt = 'n', xlim=c(-2,2), ylim=c(-2,2), ylab="", xlab="")
  draw.circle(0, 0, 1)
  arrows(0, 0,
         x1 = cos(i), y1 = 2*sin(i), length = 0.25, angle = 30,
         code = 2, col = par("fg"), lty = par("lty"),
         lwd = 2
  )
  plot_diagram(verts, i, cex = 3.5, edges=edges, diagonals=diagonals, diagLim=diagLim)
  
  
}




