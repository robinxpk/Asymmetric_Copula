# Functions I used for the paper


# p1158
m4_copula = function(u1, u2, t){
  # t denotes theta
 return(
   (u1^(-t) + u2^(-t) - 1)^(-1/t)
 )
}

# Gumbel Copula -----------------------------------------------------------
# On p1158, this is Copula M6

# Gumbel Copula generator function
gumbel_gen = function(u, theta){
  return(
    (-log(u))^theta
  )
}
# Gumbel Copula inverse generator function
gumbel_igen = function(gumbel_value, theta){
  return(
    exp(-gumbel_value^(1/theta))
  )
}
# Gumbel bivariate copula
gumbel_bicopula = function(u1, u2, theta = 2){
  return(
    gumbel_igen(
      gumbel_gen(u1, theta) + gumbel_gen(u2, theta),
      theta
    )
  )
}

nested_gumbel = function(u, theta){
  # u is a vector of uniform RV
  lenu = length(u)
  if (lenu == 2) return(gumbel_bicopula(u[1], u[2], theta))
  return(
    gumbel_bicopula(u[1], nested_gumbel(u[2:lenu], theta), theta)
  )
}
