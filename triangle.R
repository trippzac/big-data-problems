triangle <- function(edges){
  K <- dim(edges)[1]
  N <- max(edges) # number of edges in graph
  
  adjacency_matrix <- matrix(rep(0, N^2), N, N)
  for (i in 1:K) {
    adjacency_matrix[edges[i, 1], edges[i, 2]] <- adjacency_matrix[edges[i, 1], edges[i, 2]] + 1
    adjacency_matrix[edges[i, 2], edges[i, 1]] <- adjacency_matrix[edges[i, 2], edges[i, 1]] + 1
  }
  
  return (sum(diag(adjacency_matrix %*% adjacency_matrix %*% adjacency_matrix)) > 0)
}