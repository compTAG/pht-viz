library("rgl")

# helper method for making edges
make_edge3d <- function(u, v) { return(sort(c(u,v))) }

#Orders points in direction of filtration
order_on_a_vector3d <- function(verts){
  X <- matrix(rbind(verts[,1], verts[,2], verts[,3]), 3, length(verts[,1]))
  print(X)
  return(verts)
}

#Main function that plots out the persistence diagram of the 3d simplicial complex
plot3d <- function(phi=0, theta=0){

  #Opens up the rgl plot
  open3d(useNULL = TRUE)
  axes3d()
  #Default 'W' shape
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
  #print(type)
  verts <- order_on_a_vector3d(verts)
  #print(verts)
  for (i in 1:length(vert_x)){
    points3d(vert_x[i], vert_y[i], vert_z[i])
    # segments3d(vert_x[1:2], vert_y[1:2], vert_z[1:2])
    if(i != length(vert_x)){
      start <- i
      end <- i+1
      segments3d(vert_x[start:end], vert_y[start:end], vert_z[start:end])
    }
  }

  
  
  highlevel(integer())
  rgl.viewpoint(theta, phi, zoom=.4)
  rglwidget()
}