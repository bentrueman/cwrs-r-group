
# Linear regression with a censored response:

# this is a very common problem in environmental science/engineering,
# and we can use the grid approximation strategy from last time to solve it
# without imputing values or deleting nondetects

# setup -------------------------------------------------------------------

library("tidyverse")
library("withr")

theme_set(theme_bw())

# simulate a dataset ------------------------------------------------------

# parameters for simulated data:

n <- 25 # number of observations
sd <- .5 # residual standard deviation
beta0 <- 3 # intercept
beta1 <- .15 # slope
lcl <- 4.5 # left-censoring limit (detection limit)

# simulate a linear relationship between y and x:

with_seed(1, {
  data <- tibble(
    x = seq_len(n),
    y = beta0 + beta1 * x + rnorm(n, 0, sd)
  )
})

data$ystar <- pmax(data$y, lcl) # censor y values
data$cens <- data$y < lcl # create censoring indicator

# plot the data -----------------------------------------------------------

# plot data, using geom_segment() to represent censored values:

p1 <- data %>%
  ggplot(aes(x, y)) + 
  geom_point(
    data = . %>% 
      filter(!cens)
  ) +
  geom_segment(
    data = . %>% 
      filter(cens),
    aes(xend = x, y = -Inf, yend = ystar)
  ) +
  geom_point(
    data = . %>% 
      filter(cens),
    alpha = .3
  ) 

p1

# grid approximation ------------------------------------------------------

# fit a model by grid approximation:

# 1. Guess the model 
# 2. What is the probability that the data came from that model?
# 3. Repeat

# let's try the following linear regression model:

# y = beta0 + beta1 * x + e
# where e ~ N(0, sd)

# with:

# sd = 1
# beta0 = 1
# beta1 = 0.5

# this is equivalent to:

# y ~ N(mu, sd)
# mu = beta0 + beta1 * x

# what is the probability of the first data point, given our model?

mu_i <- 1 + 0.5 * data$x[4]

dnorm(data$ystar[4], mean = mu_i)

tibble(x = seq(0, 8, by = .1), y = dnorm(x, mean = mu_i)) %>% 
  ggplot(aes(x, y)) + 
  geom_line() + 
  geom_vline(xintercept = data$ystar[4], col = "red")

# what about the censored value?:
# use the cumulative distribution function instead of the probability distribution function for censored data

mu_i <- 1 + 0.5 * data$x[1]

pnorm(data$ystar[1], mean = mu_i)

tibble(x = seq(-3, 6, by = .1), y = pnorm(x, mean = mu_i)) %>% 
  ggplot(aes(x, y)) + 
  geom_line() + 
  geom_vline(xintercept = data$ystar[1], col = "red")

# put this in a function:

calc_lh <- function(x, cens, ...) {
  if_else(cens, pnorm(x, ...), dnorm(x, ...))
}

# calculate the probability of each data point, using our guess of beta0 = 1, beta1 = 0.5, sd = 1 for the model:

data %>% 
  mutate(lh = calc_lh(ystar, cens, mean = 1 + 0.5 * x))

# multiply those probabilities together to get a probability for the dataset, given any guess of mu and sigma:

tibble(
  beta0 = 1, 
  beta1 = 0.5,
  sigma = 1, 
  lh = prod(calc_lh(data$ystar, data$cens, mean = beta0 + beta1 * data$x, sd = sigma))
)

# fit a model -------------------------------------------------------------

# then do this for a whole bunch of guesses, on a grid:

len <- 25

post <- crossing(
  beta0 = seq(1, 5, length.out = len),
  beta1 = seq(0, .25, length.out = len),
  sigma = seq(0.1, 1.2, length.out = len)
) %>% 
  rowwise() %>% 
  mutate(
    # do this on the log scale to minimize rounding errors with very small probabilities:
    ll = sum(calc_lh(data$ystar, data$cens, mean = beta0 + beta1 * data$x, sd = sigma, log = TRUE))
  ) %>% 
  ungroup() %>% 
  mutate(
    prob = exp(ll), # back-transform
    prob = prob / sum(prob) # scale so that probabilities sum to 1
  )

# which mu and sigma are most probable?

post %>% 
  pivot_longer(c(beta0, beta1, sigma)) %>% 
  group_by(name) %>% 
  slice_max(prob)

# sample values of mu and sigma in proportion to their probabilities:

post_sample <- post %>% 
  slice_sample(n = 1e4, replace = TRUE, weight_by = prob) %>%
  select(beta0, beta1, sigma)

# plot the results --------------------------------------------------------

# predictions:

post_sample %>%
  pivot_longer(everything()) %>% 
  ggplot(aes(value)) + 
  facet_wrap(vars(name), scales = "free_x") +
  geom_histogram(bins = 40) +
  geom_vline(
    data = tibble(name = c("beta0", "beta1", "sigma"), param = c(beta0, beta1, sd)),
    aes(xintercept = param), col = "red"
  )

# plot the model:

# matrix of betas:

beta <- post_sample %>% 
  select(beta0, beta1) %>% 
  as.matrix()

# design matrix for the model:

X <- data %>% 
  mutate(x0 = 1) %>% 
  select(x0, x1 = x) %>% 
  as.matrix()

# multiple them together (beta0 + beta1 * x for each beta0 and beta1 pair):

post_mat <- X %*% t(beta)

# the resulting matrix is 25 by 10000, representing 1e4 plausible lines that might go through 
# the data. we can't show all of them, so we need a summary function to compute a point estimate and 
# interval for each of the 25 observations:

post_summary <- function(x) {
  tibble(est = mean(x), min = quantile(x, .025), max = quantile(x, .975))
}

# summarize the 10000 plausible lines:

post_summ <- map_dfr(seq_len(nrow(post_mat)), ~ post_summary(post_mat[.x, ])) %>% 
  mutate(x = data$x)

# add the summary to the original plot:

p1 + 
  geom_abline(aes(intercept = beta0, slope = beta1, col = "true linear model")) +
  geom_ribbon(
    data = post_summ,
    aes(x, ymin = min, ymax = max),
    inherit.aes = FALSE, alpha = .3
  ) +
  geom_line(
    data = post_summ,
    aes(y = est, col = "grid approximation")
  ) + 
  # next, compare our results with two of the most common "ad-hoc" methods for dealing 
  # with linear regression with a censored response: deleting nondetects and substituting 1/2 the 
  # detection limit.
  # sub 1/2 the DL:
  geom_smooth(
    data = . %>% 
      mutate(ystar = if_else(cens, .5 * ystar, ystar)),
    aes(x, ystar, col = "sub 1/2 detection limit"), method = "lm", se = FALSE
  ) + 
  # delete non-detects:
  geom_smooth(
    data = . %>% 
      filter(!cens),
    aes(x, ystar, col = "delete non-detects"), method = "lm", se = FALSE
  ) + 
  labs(col = NULL)
