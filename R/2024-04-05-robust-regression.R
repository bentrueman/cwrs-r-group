
# This week we're covering robust linear regression, meaning linear regression where 
# the data, after accounting for one or more covariate, are not normally distributed. We'll 
# introduce the Student-t distribution as an alternative and build a model based on it.

# setup -------------------------------------------------------------------

library("tidyverse")
library("withr")

theme_set(theme_bw())

# the student t distribution ----------------------------------------------

# the student t distribution is a generalization of the normal distribution, with an extra parameter,
# nu, that determines how "fat" the tails are:

tibble(
  x = seq(-10, 10, length.out = 200),
  `normal distribution` = dnorm(x),
  # `t distribution (1000 degrees of freedom)` = dt(x, df = 1000), # almost the same as the normal
  `t distribution (30 degrees of freedom)` = dt(x, df = 30),
  `t distribution (10 degrees of freedom)` = dt(x, df = 10),
  `t distribution (3 degrees of freedom)` = dt(x, df = 3)
) |> 
  pivot_longer(-x, names_to = "distribution") |> 
  ggplot(aes(x, value, col = distribution)) + 
  geom_line() + 
  labs(y = "Probability density")

# simulate data -----------------------------------------------------------

# model:
# y ~ T(mu, sigma, nu)
# mu = beta0 + beta1 * x

n <- 15
# estimate theses from the data:
beta0 <- 25
beta1 <- 0.25
# assume these are known:
sigma <- 1
nu <- 1

data <- with_seed(1, {
  tibble(
    x = seq(n),
    y = beta0 + beta1 * x + rt(n, df = nu)
  )
})

# plot data ---------------------------------------------------------------

p1 <- data |> 
  ggplot(aes(x, y)) + 
  geom_point() + 
  # add a linear regression line (same as lm(y ~ x)):
  geom_smooth(aes(col = "linear regression"), method = "lm", se = FALSE) + 
  labs(col = NULL)

p1

# fit a robust regression -------------------------------------------------

# model:
# y ~ T(mu, sigma = 1, nu = 1)
# mu = beta0

# guess values for our parameters:

beta0_guess <- 15
beta1_guess <- 0.25

# check to see if they make sense:

tibble(
  x = seq(-12, 35, length.out = 500),
  y = dt(x, df = nu)
) |> 
  ggplot(aes(x, y)) + 
  geom_vline(
    data = data,
    aes(xintercept = y - beta0_guess - beta1_guess * x, col = "data"),
    linetype = 3
  ) +
  geom_line(aes(col = "model")) + 
  labs(y = "Probability density")

# calculate the joint probabiliy, on the log scale, that all of the data came from the model:

sum(dt(data$y - beta0_guess - beta1_guess * data$x, df = nu, log = TRUE))

# estimate the previous line for a whole grid of guesses, then choose the best one:

model <- crossing(
  beta0_estimate = seq(-50, 50, length.out = 200),
  beta1_estimate = seq(-2, 2, length.out = 200)
) |> 
  rowwise() |> 
  mutate(
    log_likelihood = sum(dt(data$y - beta0_estimate - beta1_estimate * data$x, df = nu, log = TRUE))
    # next line is equivalent to linear regression:
    # log_likelihood = sum(dnorm(data$y - beta0_estimate - beta1_estimate * data$x, log = TRUE))
  ) |> 
  ungroup() |> 
  slice_max(log_likelihood)

# plot results ------------------------------------------------------------

# the robust regression line estimates the true relationship much better:

p2 <- p1 +
  geom_line(aes(y = beta0 + beta1 * x, col = "true relationship")) +
  geom_line(aes(y = model$beta0_estimate + model$beta1_estimate * x, col = "robust regression"))

p2

# using brms --------------------------------------------------------------

# the grid approximation above is cumbersome and slow, so this is how one might actually 
# do this analysis:

# n.b., installing brms requires a C++ compiler, so it's slightly more complicated than
# a typical R package. see https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started 

# library("brms")
# 
# model_brms <- brm(y ~ x, data = data, family = "student")
# 
# # add brms line to plot ---------------------------------------------------
# 
# p2 + 
#   geom_line(aes(y = fitted(model_brms)[, "Estimate"], col = "brms robust regression"))
