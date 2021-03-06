make_edge <- function(u, v) { return(sort(c(u,v))) }

make_simple_graph <- function() {
  vert_x <- c(0, 1, 2, 3, 4)
  vert_y <- c(2, 0, 2, 0, 2)
  vert_lab <- c("v1", "v2", "v3", "v4", "v5")
  verts <- data.frame(vert_x, vert_y, vert_lab, stringsAsFactors = F)
  edges <- list(
    c("v1", "v2"),
    c("v2", "v3"),
    c("v4", "v5")
  )
  graph <- list("verts" = verts, "edges" = edges)
  return(graph)
}

to_complex <- function(verts, edges) {
  num_verts <- nrow(verts)
  num_edges <- length(edges)

  # create a map of vertex labels to vertex indicies
  vert_idx <- new.env()
  for (i in 1:num_verts) { vert_idx[[ verts[i,3] ]] <- i }

  # preallocate for the complex
  simps <- vector("list", num_verts+num_edges)

  # convert the verts to 0-simps
  for (i in 1:nrow(verts)) { simps[i] = c(i) }

  # convert the edges to 1-simps
  for (i in 1:num_edges) {
    edge_as_labels <- unlist(edges[i])
    edge <- make_edge(vert_idx[[ edge_as_labels[1] ]], vert_idx[[ edge_as_labels[2] ]] )
    simps[num_verts+i] <- list(edge)
  }

  return(simps)
}
