###########################################
###   1. Lorenz differential equation   ###

# s, r, b can be any real number.
# The 10, 28, 8/3 are the numbers with which Lorenz studied the Equation.
para <- c(s = 10, r = 28, b = 8/3)
point <- c(x = 1, y = 1, z = 1)  #Initial point
t <- seq(0, 100, by = 0.001)

lorenz <- function(t, point, para) {
  with(as.list(c(point, para)), {
  matr1 <- rbind(c(-s, s, 0), c(r, -1, 0), c(0, 0, -b))
  matr2 <- c(x,y,z)
  matr3 <- c(0, -x*z, x*y)
  res <- (matr1 %*% matr2 + matr3)
  list(c(res[1], res[2], res[3]))
  })
}



###################
###   2. Plot   ###

#install.packages("deSolve")
library(deSolve)
points <- ode(y = point, times = t, func = lorenz, parms = para)

plot(points[, "y"], points[, "x"],
     pch = ".",
     main = "Lorenz Attractor - Y, X",
     xlab = "Y",
     ylab = "X")

plot(points[, "y"], points[, "z"],
     pch = ".",
     main = "Lorenz Attractor - Y, Z",
     xlab = "Y",
     ylab = "Z")

# And the most famous one:
plot(points[, "x"], points[, "z"],
     pch = ".",
     main = "Lorenz Attractor - X,Z",
     xlab = "X",
     ylab = "Z")



#####################
###   3. Export   ###

# To txt
write.table(points, file = "lorenz.txt", sep = "\t",
            row.names = TRUE, col.names = TRUE)