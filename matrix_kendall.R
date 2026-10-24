matrix_kendall <- function(X){
  n <- dim(X)[2]
  return (max(cor(t(X), matrix(1:n,n), method = "kendall")))
}