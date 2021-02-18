library("rgl")

# helper method for making edges
make_edge3d <- function(u, v) { return(sort(c(u,v))) }
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

get_direction_vector <- function(theta, phi){
  #Helper function that converts theta and phi (in degrees) into a direction vector
  theta <- deg2rad(theta)
  phi <- deg2rad(phi)
  Z <- sin(phi) * cos(theta)
  X <- sin(phi) * sin(theta)
  Y <- cos(phi)
  
  return( c(X, Y, Z))
}

#Orders points in direction of filtration
order_on_a_vector3d <- function(verts, theta = pi/2, phi=pi){
  
  direction <- get_direction_vector(theta, phi)
  heights <- numeric(nrow(verts))
  for (i in 1:nrow(verts)){
    vert_x <- verts[i, 1]
    vert_y <- verts[i, 2]
    vert_z <- verts[i, 3]
    point = c(vert_x, vert_y, vert_z)
    left = (direction[1])^2 + (direction[2])^2 + (direction[3])^2
    right = direction[1] * point[1] + direction[2] * point[2] + direction[3] * point[3]
    heights[i] <- solve(left, right)
  }
  order <- order(heights)
  labels <- as.character(verts[,4][order(heights)])
  height <- sort(heights)
  return(list(order=order, height = height, labels = labels))
}

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
    graphics::plot(0, 0,type = "n", axes = TRUE, xlim = diagLim,
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

#plots the PD
plot_diagram3d <- function(verts, theta1, phi, col=NULL, edges, diagonals,  ...){
  order_on_vec_list <- order_on_a_vector3d(verts = verts, theta = theta1, phi=phi)
  order_vec <- order_on_vec_list$order
  height_vec <- order_on_vec_list$height
  
  thresh <- min(dist(height_vec))
  filtration <- make_filtration3d(verts = verts, edges=edges, theta1, phi)
  diag <- filtrationDiag(
    filtration = filtration,
    maxdim = 1,
    location = TRUE,
    diagLimit = 5,
    library = "Dionysus"
  )
  diag2 <- fix_diagram(diag, thresh, diagonals=diagonals)
  plot(diag2$diagram, add = FALSE, col=col, ...)
}

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

make_filtration3d <- function(verts, edges, theta, phi) {
  order_on_vec_list <- order_on_a_vector3d(verts = verts, theta = theta, phi = phi)
  order_vec <- order_on_vec_list$order
  height_vec <- order_on_vec_list$height
  cmplx = list(order_vec[1])
  values = height_vec[1]
  for (i in 2:length(order_vec)) {
    vi <- order_vec[i]
    added <- list()
    for (j in 1:(i-1)) {
      vj <- order_vec[j]
      e <- make_edge3d(verts$vert_lab[vi], verts$vert_lab[vj])
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

plotPD3D <- function(theta, phi, graph3d, diagonals){
  #Plots the persistence diagram 
  #Default 'W' shape
  if(is.null(graph3d)){
    vert_x <- c(0, 1, 2, 3, 4)
    vert_y <- c(2, 0, 2, 0, 2)
    vert_z <- c(0, 0, 0, 0, 0)
    vert_lab <- c("v1", "v2", "v3", "v4", "v5")
    verts <- data.frame(vert_x, vert_y,vert_z, vert_lab, stringsAsFactors = F)
    edges <- list(
      make_edge3d("v1", "v2"),
      make_edge3d("v2", "v3"),
      make_edge3d("v3", "v4"),
      make_edge3d("v4", "v5")
    )
    diagLim <- max(c(vert_x, vert_y)) + 1
    diagLim <- c(-diagLim, diagLim)
  } else {
    #rgl.quit()
    graph_path <- graph3d$datapath[1]
    graph_info <- read_input3d(graph_path)
    verts <- graph_info[[1]]
    edges <- graph_info[[2]]
    diagLim <- graph_info[[3]]
    vert_x <- verts[,1]
    vert_y <- verts[,2]
    vert_z <- verts[,3]
  }
  
  plot_diagram3d(verts, theta,phi, cex = 3.5, edges=edges, diagonals=diagonals, diagLim=diagLim)
  
}
#This function reads in file input to construct graph nodes and edges
read_input3d <- function(graph){
  conn <- file(graph, open="r")
  lines <- readLines(conn)
  close(conn)
  vert_x <- numeric()
  vert_y <- numeric()
  vert_z <- numeric()
  vert_lab <- character()
  edges <- list(20)
  edge_num <- 1
  for (i in 1:length(lines)){
    line <- lines[i]
    if(grepl( "pos", line, fixed = TRUE)){
      vert_labels <- strsplit(line, " ")[[1]]
      
      vert_lab <- c(vert_lab, vert_labels[1])
      pos <- str_extract(line, "[0-9]+,[0-9]+,[0-9]+")[[1]]
      positions <- strsplit(pos, ",")[[1]]
      vert_x <- c(vert_x, as.numeric(positions[1]))
      vert_y <- c(vert_y, as.numeric(positions[2]))
      vert_z <- c(vert_z, as.numeric(positions[3]))
      
    } else if(grepl("--", line, fixed=TRUE)){
      edge <- strsplit(line, "--")[[1]]
      edge <- make_edge(trimws(edge[1]), trimws(edge[2]))
      edges[[edge_num]] <-  edge
      
      edge_num <- edge_num + 1
      
      
    }
  }
  verts <- data.frame(vert_x, vert_y, vert_z, vert_lab, stringsAsFactors = F)
  diagLim <- max(c(vert_x, vert_y)) + 1
  diagLim <- c(-diagLim, diagLim)
  return(list(verts, edges, diagLim))
}

#This graphs filtration planes orthogonal to viewpoint vector going through each point
plot_filtration_planes <- function(verts, edges, direction){
  x_lim <- max(verts[,1]) + 1
  y_lim <- max(verts[,2]) + 1
  z_lim <- max(verts[,3]) + 1
  cube3d(color="black", alpha=0) %>% scale3d(x_lim,y_lim,z_lim) %>% wire3d(alpha=0)
  
  for (i in 1:nrow(verts)){
    vert = verts[i,]
    a = direction[1]
    b = direction[2]
    c = direction[3]
    d = -vert[1] * a - vert[2] * b - vert[3] * c
    planes3d(a,b,c,d,col='black', alpha=0.2)
  }
  
}


#This diagram will plot the inputted simplicial complex
plot_shape3d <- function(verts, edges, ...){
  vert_x <- verts[,1]
  vert_y <- verts[,2]
  vert_z <- verts[,3]
  
  #Draws the Points
  for (i in 1:length(vert_x)){
    points3d(vert_x[i], vert_y[i], vert_z[i])
  }
  #Constructs the edges
  for(e in edges){
    e_vert <- verts$vert_lab %in% e
    p1x = verts$vert_x[e_vert][1]
    p2x = verts$vert_x[e_vert][2]
    p1y = verts$vert_y[e_vert][1]
    p2y = verts$vert_y[e_vert][2]
    p1z = verts$vert_z[e_vert][1]
    p2z = verts$vert_z[e_vert][2]
    lines3d(c(p1x, p2x), c(p1y, p2y), c(p1z, p2z))
  }
}


#Main function that plots out the persistence diagram of the 3d simplicial complex
plot3d <- function(phi=0, theta=0, graph3d, filtlines){

  #Default 'W' shape
  if(is.null(graph3d)){
    vert_x <- c(0, 1, 2, 3, 4)
    vert_y <- c(2, 0, 2, 0, 2)
    vert_z <- c(0, 0, 0, 0, 0)
    vert_lab <- c("v1", "v2", "v3", "v4", "v5")
    verts <- data.frame(vert_x, vert_y,vert_z, vert_lab, stringsAsFactors = F)
    edges <- list(
      make_edge3d("v1", "v2"),
      make_edge3d("v2", "v3"),
      make_edge3d("v3", "v4"),
      make_edge3d("v4", "v5")
    )
    
  } else {
    #rgl.quit()
    graph_path <- graph3d$datapath[1]
    graph_info <- read_input3d(graph_path)
    verts <- graph_info[[1]]
    edges <- graph_info[[2]]
    diagLim <- graph_info[[3]]
    
  }
  
  direction <- get_direction_vector(theta, phi)
  
  #Opens up the rgl plot
  open3d(useNULL = TRUE)
  axes3d()
  plot_shape3d(verts, edges)
  if(filtlines){
    plot_filtration_planes(verts, edges, direction)
  }
 
  

  highlevel(integer())
  rgl.viewpoint(theta, phi - 90, zoom=.4)
  rglwidget()
}