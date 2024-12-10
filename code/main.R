source("func.R")
library(copula)
library(ggplot2)
# Parameters --------------------------------------------------------------
n = 100
theta1 = 2
theta2 = 2

# Test plots --------------------------------------------------------------
# Check if my Gumbel generator function looks somewhat like it is supposed to look
df = data.frame(
  x = seq(0, 2, length.out = 200),
  y = unlist(lapply(x, FUN = gumbel_gen, theta = 2))
)
ggplot(df, aes(x = x, y = y)) + 
  geom_line() + 
  geom_hline(yintercept = 0, color = "red") + 
  theme_minimal()
# Check if my nested Gumbel makes sense
u1_seq = seq(1/n, 1, length.out = n)
u2_seq = seq(1/n, 1, length.out = n)
u3_seq = seq(1/n, 1, length.out = n)
grid = tibble::as_tibble(expand.grid(u1 = u1_seq, u2 = u2_seq, u3 = u3_seq))
grid$z = apply(grid, MARGIN = 1, FUN = nested_gumbel, theta = theta1)
# GPT suggestion:
hist(grid$z, breaks = 50, main = "Distribution of z", xlab = "z")
# What does this hist tell me?
     
contour_axis_values= seq(0, 1, 0.1)
ggplot(grid, aes(x = u1, y = u2, z = z)) + 
  geom_contour() 
ggplot(grid, aes(x = u1, y = u2, color = z)) + 
  geom_point()

# Test --------------------------------------------------------------------
test = tibble::tibble(
  u1 = runif(n),
  u2 = runif(n),
  u3 = runif(n),
  c2 = m4_copula(u1, u2, theta2),
  c1 = m4_copula(u3, c2, theta1) 
)
ggplot(test, aes(x = u1, y = u2, color = c2)) + 
  geom_point()


# Note: Start after 0 to avoid 0-division
u1_seq = seq(1/n, 1, length.out = n)
u2_seq = seq(1/n, 1, length.out = n)
grid = expand.grid(u1 = u1_seq, u2 = u2_seq)
grid$z = with(grid, m4_copula(u1, u2, theta2))
contour_axis_values= seq(0, 1, 0.1)
# Drop non-finite values
grid = grid[is.finite(grid$z), ]
ggplot(grid, aes(x = u1, y = u2, z = z)) + 
  geom_contour() + 
  labs(
    title = paste(
      "Contour Plot of m4_copula p. 1158 with theta2 =", theta2
    )
  ) 


  
