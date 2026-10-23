quadratic <- function(a,b,c) {
  if (a == 0) # make sure to not use log(0) later
  {
    if (b == 0)
    {
      return (NaN)
    }
    return (-c/b)
  }
  
  # to make our code simpler, we will assume a > 0 by returning quadratic(-a,-b,-c)
  # if it is not
  if (a < 0)
  {
    return (quadratic(-a,-b,-c))
  }
  
  disc <- b^2 - 4*a*c
  if (disc < 0)
  {
    return (NaN)
  }
  sqrt_disc <- exp(log(disc)/2)
  
  # we now divide this into two cases to avoid precision loss
  if (b >= 0)
  {
    # in this case, -b - sqrt_disc won't have precision loss, so we use the 
    # usual quadratic formula
    numerator <- (-b-sqrt_disc) #since a > 0, this gives us the smaller root
    if (numerator == 0) # make sure not to take log(0) again
    {
      return (0)
    }
    
    return (sign(numerator)*exp(log(abs(numerator)) - log(2)-log(abs(a))))
  }
  if (b < 0)
  {
    # in this case, -b - sqrt_disc may have precision loss since they are opposite signs
    # thus, we instead use the alternative formula 2c/(-b+sqrt_disc) for the smallest root
    denom <- -b + sqrt_disc
    return (sign(denom*c)*exp(log(2) + log(abs(c)) - log(abs(denom))))
  }
}
