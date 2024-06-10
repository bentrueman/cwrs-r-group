
# This week's script introduces gamma regression, which is a natural choice when we want to model the mean of a strictly 
# positive real-valued measurement as a function of covariates---without transforming the data. The most common data 
# transformation in this scenario is the log, but the backtransformed predictions estimate the geometric mean, which is 
# an estimate of the median for lognormal data. In the case of a contaminant study, like the one shown here,
# the geometric mean doesn't have the natural interpretation as a long-term average exposure that the arithmetic mean---the output of the 
# gamma regression---has.

# setup -------------------------------------------------------------------

library("tidyverse")
library("NADA2") # NADA2 contains the data we'll need

theme_set(theme_bw())

# clean data --------------------------------------------------------------

# the data are measurements of mercury in fish across the USA:

hg_fish <- as_tibble(NADA2::Brumbaugh) |> 
  mutate(Weight = Weight / 100)

# plot the data -----------------------------------------------------------

# mercury concentrations are censored at three different detection limits:

p1 <- hg_fish |> 
  ggplot(aes(Weight, Hg)) + 
  scale_y_log10() +
  geom_point(data = \(x) filter(x, !HgCen)) + 
  geom_segment(
    data = \(x) filter(x, HgCen),
    aes(xend = Weight, yend = 0),
    col = "grey"
  )

p1

# the geometric mean ------------------------------------------------------

# a common option when modeling strictly positive real-valued data is to take a log transformation. 
# that way, we don't have to worry about our model predicting negative values---when we backtransform,
# they will end up positive. it does mean, though, that when we backtransform we're predicting the geometric mean,
# not the arithmetic (i.e., conventional) mean. The geometric mean is an estimate of the median when the data are lognormal.
# it doesn't estimate long-term average exposure like the arithmetic mean does.

prod(1:10) ^ (1/10) # geometric mean
exp(mean(log(1:10))) # geometric mean via a log transformation
mean(1:10) # arithmetic mean

# the gamma distribution --------------------------------------------------

# the gamma distribution is parameterized by a mean and positive shape parameter

shape <- 1.5
mu <- 10

# dgamma() doesn't accept the mean as an argument so we pass scale = mu / shape (a different parameterization)

tibble(
  x = seq(0, 100, length.out = 200),
  y = dgamma(x, shape = shape, scale = mu / shape)
) |> 
  ggplot(aes(x, y)) + 
  geom_line()

# option 1: linear regression after log transformation --------------------

# this is the typical model we'd fit to these data using a log transformation.

# log(Hg) ~ Normal(mu, sigma)
# mu = beta0 + beta1 * Weight

# option 2: gamma regression ----------------------------------------------

# Hg ~ gamma(mu, shape)
# log(mu) = beta0 + beta1 * Weight
# mu = exp(beta0) * exp(beta1 * Weight)

grid_approximation <- crossing(
  beta0 = seq(-3, -1, length.out = 25),
  beta1 = seq(0.05, 0.45, length.out = 25),
  shape = seq(0.1, 5, length.out = 25) # this doubles as sigma for the gaussian regression (option 1)
) |> 
  rowwise() |> 
  mutate(
    # option 1:
    log_likelihood_censored = with(filter(hg_fish, HgCen), sum(pnorm(log(Hg), sd = shape, mean = beta0 + beta1 * Weight, log = TRUE))),
    log_likelihood_observed = with(filter(hg_fish, !HgCen), sum(dnorm(log(Hg), sd = shape, mean = beta0 + beta1 * Weight, log = TRUE))),
    # combine censored and observed:
    log_likelihood_gaussian = log_likelihood_censored + log_likelihood_observed,
    # option 2:
    log_likelihood_censored = with(filter(hg_fish, HgCen), sum(pgamma(Hg, shape, scale = exp(beta0 + beta1 * Weight) / shape, log = TRUE))),
    log_likelihood_observed = with(filter(hg_fish, !HgCen), sum(dgamma(Hg, shape, scale = exp(beta0 + beta1 * Weight) / shape, log = TRUE))),
    # combine censored and observed:
    log_likelihood_gamma = log_likelihood_censored + log_likelihood_observed
  ) |> 
  ungroup()

model_gamma <- grid_approximation |> 
  slice_max(log_likelihood_gamma)

model_gaussian <- grid_approximation |> 
  slice_max(log_likelihood_gaussian)

# plot model results ------------------------------------------------------

# note that predictions from the gaussian regression model are lower than those from the 
# gamma regression model. this is expected, since the geometric mean is at most the arithmetic mean
# (and is typicall smaller).

p1 + 
  geom_line(aes(y = with(model_gamma, exp(beta0 + beta1 * Weight)), col = "gamma regression model")) +
  geom_line(aes(y = with(model_gaussian, exp(beta0 + beta1 * Weight)), col = "gaussian regression model")) + 
  theme(legend.position = "bottom") +
  labs(col = NULL)

# make a compatibility interval for beta1 ---------------------------------

# if we convert the log likelihoods for each model choice to probabilities, we can 
# get a sample of probable slopes and use them to calculate an interval containing the most probable
# values

grid_sample <- grid_approximation |> 
  mutate(
    likelihood = exp(log_likelihood_gamma),
    probability = likelihood / sum(likelihood)
  ) |> 
  slice_sample(n = 1e4, weight_by = probability, replace = TRUE)

# here is the distribution of our sample:

grid_sample |> 
  ggplot(aes(beta1)) + 
  geom_histogram()

# the middle 95% occur between these values, which have been converted to express percent increases in 
# mercury concentration due to a 100 g increase in weight:

with(grid_sample, 100 * (quantile(exp(beta1), c(.025, .5, .975)) - 1))
