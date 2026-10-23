match_vec <- function(x,y) {
  total_matches = 0
  
  # resolve the case where we might have issues with 
  if (length(x) > length(y))
  {
    return(0)
  }
  
  # to get possible candidates, let's narrow down the search by first finding
  # where our possible starting points are (this is quick due to vectorization)
  possible_start_indices = which(x[1] == y)
  
  # now, we only have to use a for loop more on the scale of length(y)/length(x)
  for (start_index in possible_start_indices)
  {
    end_index <- start_index + length(x) - 1
    # check if it is possible to fit a copy of x in the rest of y
    if (end_index <= length(y))
    {
      total_matches <- total_matches + all(x == y[start_index:end_index])
    }
  }
  return(total_matches)
}
