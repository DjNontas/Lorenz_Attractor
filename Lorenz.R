###########################################
###   1. Lorenz differential equation   ###
###########################################

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
###################

#install.packages("plotly")
#install.packages("deSolve")
library(plotly)
library(deSolve)


#########################
## a) Lorenz Attractor ##

points <- ode(y = point, times = t, func = lorenz, parms = para)
df <- as.data.frame(points)

plot_ly(data = df, x = ~x, y = ~y, z = ~z) %>%
  add_paths(color = ~time) %>% 
  layout(scene = list(camera = list(eye = list(x = -1, y = 1, z = 0.25))))


########################
## b) Phase Portraits ##


plot(points[, "y"], points[, "x"],
     pch = ".",
     main = "Phase Portrait - Y, X",
     xlab = "Y",
     ylab = "X")

plot(points[, "y"], points[, "z"],
     pch = ".",
     main = "Phase Portrait - Y, Z",
     xlab = "Y",
     ylab = "Z")

# And the most famous one:
plot(points[, "x"], points[, "z"],
     pch = ".",
     main = "Phase Portrait - X,Z",
     xlab = "X",
     ylab = "Z")



#####################
###   3. Export   ###
#####################

# To txt
write.table(points, file = "lorenz.txt", sep = "\t",
            row.names = TRUE, col.names = TRUE)