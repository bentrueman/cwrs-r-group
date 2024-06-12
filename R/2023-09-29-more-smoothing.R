
# This week, we're continuing our discussion of smoothing by extending 
# the moving average to get one of the most popular smoothers: the default 
# option to geom_smooth(), which used stats::loess() under the hood.

# setup -------------------------------------------------------------------

library("tidyverse")
library("testthat")

theme_set(theme_bw())

# function ----------------------------------------------------------------

# first let's build our own function to calculate moving averages.
# unlike stats::filter(), our version will smooth right up to the end points:

moving_average <- function(x, n_pts = 3) {
  n_vals <- length(x)
  if (n_pts > n_vals) stop("Number of points can't be larger than length(x)")
  # the moving window in which to calculate the average:
  window <- seq(n_pts)
  window <- window - ceiling(median(window))
  y <- rep(NA, n_vals)
  for(i in seq(n_vals)) {
    index <- i + window
    index <- index[index > 0 & index <= n_vals]
    y[i] <- mean(x[index], na.rm = TRUE)
  }
  y
}

# if we modify moving_average() by replacing mean() with lm(), we get the 
# basic function used by default in geom_smooth().

local_linear_regression <- function(x, n_pts = 3) {
  n_vals <- length(x)
  if (n_pts > n_vals) stop("Number of points can't be larger than length(x)")
  # the moving window in which to calculate the average:
  window <- seq(n_pts)
  window <- window - ceiling(median(window))
  y <- rep(NA, n_vals)
  for(i in seq(n_vals)) {
    # weights for linear regression. these ensure we get a smooth curve,
    # since the endpoints are down-weighted:
    tricube_weights <- (1 - abs(window / max(abs(window))) ^ 3) ^ 3
    # indices for subsetting x:
    index <- i + window
    # these are the valid indices:
    keep_these <- index > 0 & index <= n_vals
    # retain only valid indices:
    index <- index[keep_these]
    # retain only the weights corresponding to valid indices:
    tricube_weights <- tricube_weights[keep_these]
    # retain values of x in the "window":
    response <- x[index]
    # fit a line to the points in the window, and use it to predict the
    # value of the smooth curve at position i:
    model <- lm(response ~ index, weights = tricube_weights)
    y[i] <- predict(model, newdata = tibble(index = i))
  }
  y
}

# plot --------------------------------------------------------------------

# let's compare four methods of smoothing data:

# geom_smooth() and local_linear_regression() are almost the same 
# (as expected, since they are essentially the same procedure).
# moving_average() is biased when y changes rapidly near the boundaries,
# and stats::filter() truncates when the window is incomplete 
# (tradeoff between smoothness and coverage). 

set.seed(1256)

tibble(
  x = 1:100,
  y = 20 * cos(x / 100) + rnorm(100)
) %>% 
  mutate(
    ma = moving_average(y, n_pts = 75),
    ma_stats = stats::filter(y, rep(1/75, 75)),
    llr = local_linear_regression(y, n_pts = 75),
    geom_smooth = NA
  ) %>% 
  pivot_longer(c(ma, ma_stats, llr, geom_smooth)) %>% 
  ggplot(aes(x, y)) + 
  facet_wrap(
    vars(name),
    labeller = as_labeller(c(
      ma = "moving_average()",
      ma_stats = "stats::filter()",
      llr = "local_linear_regression()",
      geom_smooth = "geom_smooth()"
    ))
  ) +
  geom_point() + 
  geom_line(aes(y = value), col = "red") + 
  geom_smooth(
    data = . %>% 
      filter(name == "geom_smooth"),
    se = FALSE
  )

# tests -------------------------------------------------------------------

# tests of our functions. test allow us to be explicit about the expected behaviour of 
# our functions. When we make changes to them, we can be confident that if the tests still 
# pass, we haven't broken anything---as long as we have good test coverage.

# moving_average():

test_that("moving_average() returns the expected result for a vector on ones", {
  ones <- rep(1, 30)
  out <- moving_average(ones)
  expect_equal(out, ones)
})

test_that("moving_average() returns an error for n_pts > n_vals", {
  ones <- rep(1, 30)
  expect_error(moving_average(ones, 31))
})

test_that("moving_average() returns the same result as stats::filter()", {
  x <- rnorm(100)
  out1 <- moving_average(x)
  out2 <- stats::filter(x, rep(1/3, 3))
  expect_equal(out1[-c(1, 100)], out2[-c(1, 100)])
})

# local_linear_regression():

test_that("local_linear_regression() returns expected result", {
  x <- 1:100
  expect_equal(local_linear_regression(x), x)
  expect_equal(local_linear_regression(x, 50), x)
  expect_equal(local_linear_regression(x, 100), x)
})
