
# source: https://selbydavid.com/2018/01/09/neural-network/

# functions ---------------------------------------------------------------

# for updating parameter values by gradient descent:

update_parameters <- function(x, y, y_hat, coefficients_1, coefficients_2, hidden_layer, learn_rate) {
  dcoefficients_2 <- t(cbind(1, hidden_layer)) %*% (y_hat - y)
  dhidden_layer  <- (y_hat - y) %*% t(coefficients_2[-1, , drop = FALSE])
  dcoefficients_1 <- t(cbind(1, x)) %*% (hidden_layer * (1 - hidden_layer) * dhidden_layer)
  
  coefficients_1 <- coefficients_1 - learn_rate * dcoefficients_1
  coefficients_2 <- coefficients_2 - learn_rate * dcoefficients_2
  
  list(coefficients_1 = coefficients_1, coefficients_2 = coefficients_2)
}

# for data simulation:

two_spirals <- function(N = 200,
                        radians = 3*pi,
                        theta0 = pi/2,
                        labels = 0:1) {
  N1 <- floor(N / 2)
  N2 <- N - N1
  
  theta <- theta0 + runif(N1) * radians
  spiral1 <- cbind(-theta * cos(theta) + runif(N1),
                   theta * sin(theta) + runif(N1))
  spiral2 <- cbind(theta * cos(theta) + runif(N2),
                   -theta * sin(theta) + runif(N2))
  
  points <- rbind(spiral1, spiral2)
  classes <- c(rep(0, N1), rep(1, N2))
  
  data.frame(x1 = points[, 1],
             x2 = points[, 2],
             class = factor(classes, labels = labels))
}

# simulate data -----------------------------------------------------------

data <- two_spirals(labels = c("nondetect", "detection"))
