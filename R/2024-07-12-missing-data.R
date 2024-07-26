
# This script introduces a particular strategy for handling missing values: treating them
# as parameters in the model to be estimated, just like an intercept or regression slope.
# We'll use a dataset of mercury in fish to illustrate.

# setup -------------------------------------------------------------------

library("tidyverse")
library("NADA2")

theme_set(theme_bw())

# get the data ------------------------------------------------------------

fish <- as_tibble(NADA2::Brumbaugh) |> 
  select(WatMeHg, Hg) |> 
  # remove all but one of the missing values:
  slice(-c(9:13))

# store the index of the remaining missing value:

missing_value <- with(fish, which(is.na(WatMeHg)))

# plot the data -----------------------------------------------------------

p0 <- fish |> 
  ggplot(aes(log(WatMeHg), log(Hg))) +
  geom_point()

p1 <- p0 +
  geom_hline(
    data = function(x) filter(x, is.na(WatMeHg)),
    aes(yintercept = log(Hg), col = "Missing value"),
  ) +
  labs(col = NULL)

p1

# scaling a variable ------------------------------------------------------

# subtract the mean of a variable and then divide by the standard deviation to get a 
# transformed variable with zero mean and standard deviation of one

sd((rnorm(10000, mean = 10, sd = 15) - 10) / 15)

# fit a linear model (method 1) -------------------------------------------

# our linear regression model:

# log(Hg) ~ N(mu, sigma)
# mu = beta0 + beta1 * log(WatMeHg)

# if we guess the correct linear relationship (i.e., the mean,
# conditional on the WatMeHg value), and the correct residual standard deviation,
# then scale Hg by them, we'll end up with a histogram that looks like a standard normal
# distribution---i.e., N(0,1).

beta0_guess <- 0
beta1_guess <- 0.5
sigma_guess <- 0.7
mu_guess <- beta0_guess + beta1_guess * log(fish$WatMeHg[-missing_value])

fish |> 
  drop_na() |> 
  ggplot(aes((log(Hg) - mu_guess) / sigma_guess)) +
  geom_histogram()

# what is the joint probability---on the log scale---of the data we have (Hg),
# give the linear model we guessed?

sum(dnorm(
  log(fish$Hg[-missing_value]), mean = mu_guess, sd = sigma_guess, log = TRUE
))

# we'll use this single number metric to score a grid of guesses in the next section.

# fit a linear model (method 2) -------------------------------------------

grid_size <- 35

beta0 <- seq(-0.5, 0.5, length.out = grid_size)
beta1 <- seq(0, 1, length.out = grid_size)
sigma <- seq(0, 1.5, length.out = grid_size)

grid_output <- crossing(beta0, beta1, sigma) |> 
  rowwise() |> 
  mutate(
    log_likelihood = sum(dnorm(
      log(fish$Hg[-missing_value]), 
      mean = beta0 + beta1 * log(fish$WatMeHg[-missing_value]), 
      sd = sigma, 
      log = TRUE
    ))
  ) |> 
  ungroup()

# what is the best guess from the grid, based on our probability metric (i.e., the likelihood)?

grid_output |> 
  slice_max(log_likelihood)

# get parameter samples ---------------------------------------------------

# we can use the likelihood to generate a bunch of samples from the grid,
# drawn according to how plausible they are:

posterior_sample <- grid_output |> 
  mutate(
    probability = exp(log_likelihood),
    probability = probability / sum(probability)
  ) |>
  slice_sample(n = 1e4, weight_by = probability, replace = TRUE)

posterior_sample |>   
  pivot_longer(c(beta0, beta1, sigma)) |> 
  ggplot(aes(value)) + 
  facet_wrap(vars(name), scales = "free_x") +
  geom_histogram()

# estimate the regression line --------------------------------------------

# for each plausible linear model, we can generate a line:

posterior_mean <- posterior_sample |> 
  rowwise() |> 
  mutate(predictions = map2(beta0, beta1, ~ .x + .y * log(fish$WatMeHg))) |> 
  ungroup() |> 
  # add a row id so that we can map predictions back to the data:
  mutate(predictions = map(predictions, ~ tibble(rowid = seq_along(.x), prediction = .x))) |> 
  unnest(predictions) |> 
  # summarize the predictions:
  group_by(rowid) |> 
  summarize(
    estimate = median(prediction),
    lower = quantile(prediction, 0.025, na.rm = TRUE),
    upper = quantile(prediction, 0.975, na.rm = TRUE)
  ) |> 
  bind_cols(fish)

# plot the results --------------------------------------------------------

p1 + 
  geom_ribbon(data = posterior_mean, aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(data = posterior_mean, aes(y = estimate)) + 
  # verify that this gives comparable results to lm():
  geom_smooth(method = "lm", linewidth = 0.5)

# add a missing value parameter -------------------------------------------

# now we add a fourth parameter to estimate the missing WatMeHg value:

grid_size <- 15

beta0 <- seq(-0.5, 0.5, length.out = grid_size)
beta1 <- seq(0.2, 0.8, length.out = grid_size)
sigma <- seq(0.5, 1, length.out = grid_size)
x_missing <- seq(-2, -9, length.out = grid_size)

grid_output <- crossing(beta0, beta1, sigma, x_missing) |> 
  rowwise() |> 
  mutate(
    log_likelihood = sum(dnorm(
      log(fish$Hg), 
      mean = beta0 + beta1 * replace_na(log(fish$WatMeHg), x_missing), 
      sd = sigma, 
      log = TRUE
    ))
  ) |> 
  ungroup()

grid_output |> 
  slice_max(log_likelihood)

# get parameter values for missing value model ----------------------------

posterior_sample <- grid_output |> 
  mutate(
    probability = exp(log_likelihood),
    probability = probability / sum(probability)
  ) |>
  slice_sample(n = 1e4, weight_by = probability, replace = TRUE)

posterior_sample |>   
  pivot_longer(c(beta0, beta1, sigma, x_missing)) |> 
  ggplot(aes(value)) + 
  facet_wrap(vars(name), scales = "free") +
  geom_histogram()

# estimate the regression line (missing value model) ----------------------

posterior_mean <- posterior_sample |> 
  rowwise() |> 
  mutate(
    predictions = pmap(list(beta0, beta1, x_missing), \(x, y, z) x + y * replace_na(log(fish$WatMeHg), z))
  ) |> 
  ungroup() |> 
  mutate(predictions = map(predictions, ~ tibble(rowid = seq_along(.x), prediction = .x))) |> 
  unnest(predictions) |> 
  group_by(rowid) |> 
  summarize(
    estimate = median(prediction),
    lower = quantile(prediction, 0.025, na.rm = TRUE),
    upper = quantile(prediction, 0.975, na.rm = TRUE)
  ) |> 
  bind_cols(fish)

# plot the results of the missing value model -----------------------------

missing_value_summary <- posterior_sample |> 
  summarize(
  xmin = quantile(x_missing, 0.25), 
  xmax = quantile(x_missing, 0.75), 
  WatMeHg = exp(median(x_missing)), 
  Hg = fish$Hg[missing_value]
)

p0 + 
  geom_ribbon(data = posterior_mean, aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(data = posterior_mean, aes(y = estimate)) + 
  geom_errorbarh(
    data = missing_value_summary,
    aes(xmin = xmin, xmax = xmax, col = "Missing value"),
    height = 0.1
  ) + 
  geom_point(
    data = missing_value_summary,
    aes(col = "Missing value")
  ) + 
  labs(col = NULL)
  