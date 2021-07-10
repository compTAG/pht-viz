really_make_filtration <- function(verts, edges, theta) {
  # convert the graph to a complex
  cmplx <- to_complex(verts, edges)

  # get the function value of each vertex
  dot <- function(v) { v[1]*cos(theta) + v[2]*sin(theta) }
  fv <- apply(verts[,1:2], 1, dot)

  # get the function value for each simplex
  filter <- function(v) {
    fbase <- function(e) { fv[e] }
    vert_vals <- lapply(unlist(v), fbase)
    max(unlist(vert_vals))
  }
  values <- unlist(lapply(cmplx, filter))

  # get the dimension of each simplex
  dims <- unlist(lapply(cmplx, length))

  # crate the filtration
  filter_order <- order(values, dims)
  return(list("cmplx" = cmplx[filter_order], "values" = values[filter_order], "increasing" = TRUE))
}


is_vertex <- function(simplex) {
  return(length(simplex) == 1)
}


create_ecc <- function(filtration) {
  num_verts <- 0
  num_edges <- 0

  cur_step <- c(-Inf, 0)
  last_ecc <- 0
  ecc <- list()
  for (i in 1:length(filtration$values)) {
    simplex <- filtration$cmplx[[i]]
    value <- filtration$values[i]

    # update euler charicteristic
    if (is_vertex(simplex)) {
      num_verts = num_verts + 1
    } else {
      num_edges = num_edges + 1
    }
    euler_charicteristic <- num_verts - num_edges

    # push new step if curve changes
    if (cur_step[1] < value) {
      if (length(ecc) == 0 || ecc[[ length(ecc) ]][2] != cur_step[2]) {
        ecc <- append(ecc, list(cur_step))
      }
      cur_step <- c(value, euler_charicteristic)
    } else {
      stopifnot(cur_step[1] == value)
      cur_step[2] = euler_charicteristic
    }
  }
  return(ecc)
}

example <- function() {
  g <- make_simple_graph()
  f <- really_make_filtration(g$verts, g$edges, 0)
  ecc <- create_ecc(f)
  print(ecc)
}

get_interval <- function(ecc, i) {
  begin <- ecc[[i]][1]
  if (i+1 > length(ecc)) {
    end <- Inf
  } else {
    end <- ecc[[i+1]][1]
  }
  value <- ecc[[i]][2]
  return(list("begin"=begin, "end"=end, "value"=value))
}

