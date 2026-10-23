gen_logit <- function(beta, X){
  prod <- beta%*%X
  array_to_exponentiate <- c(0,prod)
  # we may shift the array of elements to be exponentiated because we will eventually
  # divide by the sum. This will mean we are in fact multiplying by the same exponential
  # shift in the numerator and denominator, which will cancel out
  shifted_array <- array_to_exponentiate - max(array_to_exponentiate) 
  exp_shifted_array <- exp(shifted_array)
  return(exp_shifted_array/sum(exp_shifted_array))
}