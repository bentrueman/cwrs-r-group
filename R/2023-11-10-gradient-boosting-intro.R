
# This script introduces gradient boosting, one of the most popular machine learning algorithms 
# for tabular data. It is implemented in R via the "xgboost" package, but before we use that 
# library, it will be helpful understand it conceptually. We'll build a gradient-boosted 
# regression tree model from scratch, using only tidyverse functions and base R. It won't be 
# nearly as performant as xgboost, but it should help in understanding what xgboost actually does.

# setup -------------------------------------------------------------------

library("tidyverse")

theme_set(theme_bw())

# simulate data -----------------------------------------------------------

# first, we'll simulate some data that we can use for a univariate, nonlinear regression:

data <- tibble(
  x = runif(100, 0, 3),
  y = -x ^ 2 + rnorm(100)
)

# plot --------------------------------------------------------------------

p1 <- data %>% 
  ggplot(aes(x, y)) + 
  geom_point()
p1

# zero iterations ---------------------------------------------------------

# perhaps the simplest model of y would be the mean of y:

p1 + 
  geom_line(
    aes(y = mean(y)),
    col = "red"
  )

# one iteration, fixed split ----------------------------------------------

# we can improve on that by splitting y into two regions: one where x = 0-1.5 and one 
# where x = 1.5-3. if we calculate a mean in each region we get a better (but still crude) 
# approximation of the true function:

split <- 1.5
low <- mean(data$y[data$x < split])
high <- mean(data$y[data$x >= split])

p1 + 
  geom_step(
    aes(y = if_else(x < split, low, high)),
    col = "red"
  )

# one iteration, learned split --------------------------------------------

# in the previous step, we picked a value to split the data in half. here' we'll learn the
# optimal split point from the data by choosing the split that minimizes the total (squared) 
# error of the resulting model.

# first, we'll need a function to calculate a mean for each region, based on an arbitraty split of 
# the x coordinate:

get_mu <- function(x, y, split) {
  low <- mean(y[x < split])
  high <- mean(y[x >= split])
  mu <- if_else(x < split, low, high)
  list("low" = low, "high" = high, "mu" = mu)
}

# then we need some candidate split points. we'll use a sequence of possibilities
# that spans the range of x:

splits <- seq(min(data$x), max(data$x), length.out = 100)

# for each candidate, calculate the total squared error of the resulting model:

sum_squared_error <- map_dbl(splits, ~ sum((data$y - get_mu(data$x, data$y, split = .x)$mu) ^ 2))

# then pick the split that yields the lowest error:

this_index <- which.min(sum_squared_error)

this_split <- splits[this_index]

# use that split to build our model:

yhat <- get_mu(data$x, data$y, split = this_split)$mu

p1 + 
  geom_step(
    aes(y = yhat),
    col = "red"
  )

# two iterations ----------------------------------------------------------

# this still isn't a very good model, so let's do the same thing again. but this time,
# we'll use the residuals of previous model to estimate the split point for the next one.
# that way we can focus on the points that our previous model predicted poorly.

# other than that, we'll do the same thing we did last iteration:

residuals <- data$y - yhat

sum_squared_error <- map_dbl(splits, ~ sum((residuals - get_mu(data$x, residuals, split = .x)$mu) ^ 2))

this_index <- which.min(sum_squared_error)

this_split <- splits[this_index]

yhat_current <- get_mu(data$x, residuals, split = this_split)$mu

# here is the model from iteration two superimposed on the residuals from the model
# built in iteration one:

data %>% 
  ggplot(aes(x, residuals)) + 
  geom_point() + 
  geom_step(
    aes(y = yhat_current),
    col = "red"
  )

# the new model is the sum of the model from iteration one (fitted to the data) and the 
# model from iteration two (fitted to the residuals from iteration one):

yhat <- yhat + yhat_current

p1 + 
  geom_step(
    aes(y = yhat),
    col = "red"
  )

# n iterations ------------------------------------------------------------

# we'll need many more iterations to get a good model. the following code 
# performs n iterations. to avoid overfitting, the contribution of each
# one is shrunk by a constant, the learning rate. to understand what the 
# learning rate does, play around with different settings of n_iterations and learning_rate

n_iterations <- 100
learning_rate <- 0.1
yhat <- mean(data$y)

for(iteration in 1:n_iterations) {
  if(iteration %% 10 == 0) print(paste(iteration, "iterations")) # print progress
  residuals <- data$y - yhat
  sum_squared_error <- map_dbl(splits, ~ sum((residuals - get_mu(data$x, residuals, split = .x)$mu) ^ 2))
  this_index <- which.min(sum_squared_error)
  this_split <- splits[this_index]
  yhat_current <- get_mu(data$x, residuals, split = this_split)$mu * learning_rate
  yhat <- yhat + yhat_current
}

# and here is our final model!

p1 + 
  geom_step(
    aes(y = yhat, col = "estimated relationship"),
  ) + 
  geom_line(aes(y = -x ^ 2, col = "true relationship")) +
  theme(legend.position = "bottom") + 
  labs(col = NULL)

# this algorithm doesn't offer much improvement over other options when we only have one predictor,
# but it really shines when there are a lot of predictors. we'll extend this script to that 
# situation below.

# more predictors ---------------------------------------------------------

# let's simulate data so that y is a function of two predictors. we'll also add some "noise" variables 
# that don't have any relationship to y, just to see how the boosting algorithm handles them.

n <- 200

data2 <- tibble(
  x1 = runif(n, 0, 3),
  x2 = runif(n, 0, 3),
  # add noise variables:
  x3 = rnorm(n),
  x4 = rnorm(n),
  x5 = rnorm(n),
  y = x1 ^ 2 + x2 ^ 2 + rnorm(n)
)

# train/test split --------------------------------------------------------

# with many predictors, we can very easily overfit, modeling the noise in the data instead of the
# signal. to protect against this, we'll evaluate predictive performance on a test dataset that 
# was not used to fit (train) the model.

train <- sample(seq(n), size = n / 2) # indices of training observations
data_train <- data2[train, ] # training data used to fit the model
data_test <- data2[-train, ] # test data, used to evaluate the model

# plot --------------------------------------------------------------------

data_train %>% 
  ggplot(aes(x1, x2, col = y)) +
  scale_color_viridis_c() + 
  geom_point(size = 2)

# fit model ---------------------------------------------------------------

# this is similar to the one predictor case, but now we have to choose which variable to
# split on at each iteration:

n_iterations <- 200
learning_rate <- 0.1
yhat <- mean(data_train$y)

# build a list of candidate split points:

splits <- data_train %>%
  select(-y) %>% 
  as.list() %>% 
  map_dfr(~ seq(min(.x), max(.x), length.out = 100)) %>% 
  pivot_longer(everything(), names_to = "variable")

# set up tibble to record the choices made during fitting (which variable/split point at each iteration):

model <- tibble(variable = NA, value = NA, mu_low = NA, mu_high = NA, rmse = NA, .rows = n_iterations)

for(iteration in 1:n_iterations) {
  if(iteration %% 10 == 0) print(paste(iteration, "iterations")) # print progress
  residuals <- data_train$y - yhat # calculate residuals
  # select best split:
  sum_squared_error <- map2_dbl(
    splits$variable, splits$value,
    ~ sum((residuals - get_mu(data_train[[.x]], residuals, split = .y)$mu) ^ 2)
  )
  this_index <- which.min(sum_squared_error)
  this_variable <- splits$variable[this_index]
  this_value <- splits$value[this_index]
  # calculate means above/below split point:
  mu <- get_mu(data_train[[this_variable]], residuals, split = this_value)
  # add current model to overall model:
  yhat_current <- mu$mu * learning_rate
  yhat <- yhat + yhat_current
  # record model for this iteration:
  model$variable[iteration] <- this_variable
  model$value[iteration] <- this_value
  model$mu_low[iteration] <- mu$low
  model$mu_high[iteration] <- mu$high
  model$rmse[iteration] <- sqrt(mean((data_train$y - yhat) ^ 2))
}

# predictions (training data) ---------------------------------------------

data_train %>% 
  ggplot(aes(y, yhat)) + 
  geom_abline() + 
  geom_point(aes(col = "estimated relationship")) +
  geom_point(aes(y = x1 ^ 2 + x2 ^ 2, col = "true relationship + error")) + 
  theme(legend.position = "bottom") +
  labs(col = NULL)

# calculate root mean squared error (rmse):

sqrt(mean((data_train$y - yhat) ^ 2)) # model rmse
sqrt(mean((data_train$y - data_train$x1 ^ 2 - data_train$x2 ^ 2) ^ 2)) # rmse for true relationship

# variable importance -----------------------------------------------------

# the tibble "model" tells us which variables we split on and at which points. it also tells us how much
# of a reduction in rmse we saw at each iteration. if we sum up the reduction 
# in rmse attributable to each variable we can use it as a measure of variable importance:

rmse_null <- sqrt(mean((data_train$y - mean(data_train$y)) ^ 2)) # error at zero iterations

model %>% 
  mutate(
    # calculate the change in rmse at each iteration:
    delta_rmse = lag(rmse),
    delta_rmse = replace_na(delta_rmse, rmse_null),
    delta_rmse = delta_rmse - rmse
  ) %>% 
  group_by(variable) %>% 
  summarize(delta_rmse = sum(delta_rmse)) %>% 
  ggplot(aes(variable, delta_rmse)) +
  geom_bar(stat = "identity")

# predictions (test data) -------------------------------------------------

# we can use the instructions contained in the tibble "model" to make predictions of y for new observations:

n_iterations <- 200
yhat_test <- mean(data_train$y)

for(iteration in 1:n_iterations) {
  this_variable <- model$variable[iteration]
  this_value <- model$value[iteration]
  mu_low <- model$mu_low[iteration]
  mu_high <- model$mu_high[iteration]
  yhat_current <- if_else(data_test[[this_variable]] < this_value, mu_low, mu_high) * learning_rate
  yhat_test <- yhat_test + yhat_current
}

data_test %>% 
  ggplot(aes(y, yhat_test)) + 
  geom_abline() + 
  geom_point(aes(col = "estimated relationship")) + 
  geom_point(aes(y = x1 ^ 2 + x2 ^ 2, col = "true relationship + error")) +
  theme(legend.position = "bottom") +
  labs(col = NULL)

sqrt(mean((data_test$y - yhat_test) ^ 2)) # test rmse
sqrt(mean((with(data_test, y - x1 ^ 2 - x2 ^ 2)) ^ 2)) # true minimum rmse
