
# This week we're building a simple neural network to illustrate the concept. We'll need the two 
# functions contained in "neural-network-inputs.R"---one to update the parameters and one to 
# simulate the data. This script draws heavily from the excellent resource Tea & Stats 
# (https://selbydavid.com/2018/01/09/neural-network/)

# setup -------------------------------------------------------------------

library("tidyverse")
library("withr") # for reproducible results
with_seed(1, source("R/2023-12-08-neural-network-inputs.R"))

theme_set(theme_bw())

# plots -------------------------------------------------------------------

# this sigmoid function, or logistic curve, is the building block of our neural network:

tibble(x = seq(-10, 10, by = .25), y = plogis(x)) %>% 
  ggplot(aes(x, y)) + 
  geom_line()

# we can approximate any curve as a linear combination of sigmoid functions:

tibble(x = runif(200, 0, 100), y = sqrt(x) + rnorm(200)) %>% 
  ggplot(aes(x, y)) + 
  geom_point() + 
  # guess the "neural network" by trail and error:
  geom_line(aes(y = 4 * plogis(0.1 * x) + 21 * plogis(0.012 * x) - 10), col = "red")

# here is the data we'll be working with. the goal is to predict whether we'll get a detection or
# a nondetect based on the inputs, x1 and x2. we'll use the same strategy that we used for the previous plot:
# 1) generate a series of linear combinations of our inputs, 2) apply a sigmoid transformation to each, and 
# 3) approximate a curve (or decision boundary for classification) using a linear combination of the sigmoids.

p1 <- data %>% 
  ggplot(aes(x1, x2, col = class)) +
  geom_point()

p1

# first iteration ---------------------------------------------------------

# for the first iteration, we'll initialize our parameters randomly:

x <- data.matrix(data[, c("x1", "x2")]) # predictor matrix
n_hidden_predictors <- 5 # (called "nodes")
n_real_predictors <- ncol(x) + 1
coefficients_1 <- matrix(rnorm(n_hidden_predictors * n_real_predictors), n_real_predictors, n_hidden_predictors)
coefficients_2 <- as.matrix(rnorm(n_hidden_predictors + 1))
step_1 <- cbind(1, x) %*% coefficients_1 # generate linear combinations of the inputs (the 1 column is the intercept)
step_2 <- plogis(step_1) # apply the sigmoid transformation
step_3 <- cbind(1, step_2) %*% coefficients_2 # generate linear combinations of the sigmoids
step_4 <- plogis(step_3) # apply the sigmoid transformation again, so that the predictions are on the (0,1) scale
# closer to 0 -> nondetect; closer to 1 -> detection

# functions ---------------------------------------------------------------

# we can put lines 47 to 50 in a function so that we can repeat it, improving the parameter estimates each time:

make_predictions <- function(x, coefficients_1, coefficients_2) {
  step_1 <- cbind(1, x) %*% coefficients_1
  step_2 <- plogis(step_1)
  step_3 <- cbind(1, step_2) %*% coefficients_2
  step_4 <- plogis(step_3)
  list(output = step_4, hidden_layer = step_2)
}

# putting everything together, the following function initializes our parameter estimates, and then 
# repeatedly predicts the output and fine-tunes the parameter estimates by comparing with the response:

train_model <- function(x, y, n_hidden_predictors = 5, learn_rate = 1e-2, n_iterations = 1e5) {
  # initialize:
  n_real_predictors <- ncol(x) + 1
  coefficients_1 <- matrix(rnorm(n_hidden_predictors * n_real_predictors), n_real_predictors, n_hidden_predictors)
  coefficients_2 <- as.matrix(rnorm(n_hidden_predictors + 1))
  # iterate and update:
  for (iteration in 1:n_iterations) {
    if (iteration %% 1e3 == 0) print(paste(iteration, "iterations")) # print progress
    predictions <- make_predictions(x, coefficients_1, coefficients_2)
    updated_parameters <- update_parameters(
      x, y, y_hat = predictions$output,
      coefficients_1, coefficients_2,
      hidden_layer = predictions$hidden_layer, 
      learn_rate
    )
    coefficients_1 <- updated_parameters$coefficients_1
    coefficients_2 <- updated_parameters$coefficients_2
  }
  list(output = predictions$output, coefficients_1 = coefficients_1, coefficients_2 = coefficients_2)
}

# train model -------------------------------------------------------------

# let's train the model with 5 hidden predictors:

y <- data$class == "detection"
nnet <- with_seed(1, train_model(x, y))
mean((nnet$output > 0.5) == y) # calculate prediction accuracy

# plot decision boundary --------------------------------------------------

# we can plot the decision boundary by predicting over a fine grid:

grid <- crossing(
  x1 = seq(min(data$x1) - 1, max(data$x1) + 1, by = 0.25),
  x2 = seq(min(data$x2) - 1, max(data$x2) + 1, by = 0.25),
)

# predict over the grid:

grid_predictions <- make_predictions(
  x = data.matrix(grid[, c("x1", "x2")]), nnet$coefficients_1, nnet$coefficients_2
)

grid$class <- if_else(grid_predictions$output > 0.5, "detection", "nondetect") %>% 
  factor(levels = c("nondetect", "detection"))

# visualize:

p1 + 
  geom_raster(data = grid, aes(fill = class), alpha = 0.2)

# not bad, but the model hasn't really learned the spiral shape of the data

# train bigger model ------------------------------------------------------

# let's try 30 hidden predictors:

nnet_large <- with_seed(1, train_model(x, y, n_hidden_predictors = 30))
mean((nnet_large$output > 0.5) == y) # 100% prediction accuracy

# predict over the grid:

grid_predictions <- make_predictions(
  x = data.matrix(grid[, c("x1", "x2")]), nnet_large$coefficients_1, nnet_large$coefficients_2
)

grid$class <- if_else(grid_predictions$output > 0.5, "detection", "nondetect") %>% 
  factor(levels = c("nondetect", "detection"))

# visualize:

p1 + 
  geom_raster(data = grid, aes(fill = class), alpha = 0.2)

# now the model is learning the spiral shape!
