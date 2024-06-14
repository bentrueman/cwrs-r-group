
# Analysis of partially censored data (e.g., data with non-detects):

# setup -------------------------------------------------------------------

library("tidyverse")
library("withr")

theme_set(theme_bw())

# the rank-sum test as a special case -------------------------------------

# 1. First, a special case. If all we're interested in is comparing two independent groups and getting a p-value, 
# we can use the Wilcoxon rank-sum test to do that. There are other rank-based procedures for other types of censored 
# data (e.g., matched pairs), but these procedures are mostly qualitative (i.e., they don't provide a straightforward way 
# to estimate model parameters and their uncertainties). So this approach is limited.

# as a demo, a simulation-based approximation of the rank-sum test:

# parameters for simulated data:

n <- 6
sd <- 1
mu <- 3

# simulate null distribution (g1 == g2)

h0 <- replicate(1e5, {
  g1 <- rnorm(n, mu, sd)
  g2 <- rnorm(n, mu, sd)
  r <- rank(c(g1, g2))
  sum(r[1:n]) # this is the test statistic, the sum of the ranks of g1
})

# plot the null distribution:

p1 <- tibble(h0) %>% 
  ggplot(aes(x = h0)) + 
  geom_histogram(bins = 100)

p1

# add .025 and .975 quantiles to get a rough graphical version of the rank-sum test:

p2 <- p1 + 
  geom_vline(xintercept = quantile(h0, c(.025, .975)), col = "blue")

p2

# for simulated data with a true difference, 
# compute a test statistic and compare with the null

with_seed(1, {
  g1 <- rnorm(n, mu, sd)
  g2 <- rnorm(n, mu + 1, sd)
})

r <- rank(c(g1, g2))

test_stat <- sum(r[1:n])

# what fraction of rank-sums from the null distribution are at least as extreme as the one we got?
# if the fraction is small, we reject the H0

2 * mean(h0 <= 25) # approximate p-value

w1 <- wilcox.test(g1, g2) # exact version

w1

p2 +
  geom_vline(xintercept = test_stat, col = "red")

# do the same thing, but with censored data:

# pretend that the smallest value of g1 is censored, and make an indicator vector:

cens_g1 <- g1 == min(g1) 

# on the scale of measurement, we don't know the value of censored observations, but on the rank scale we do. 
# So there is no problem doing a rank-sum test on censored data:

tibble(x = c(g1, g2), cens = c(cens_g1, rep(FALSE, length(g2)))) %>% 
  mutate(r = rank(x)) %>% 
  arrange(r)

# fit a model using grid approximation ------------------------------------

# Next, a more general strategy that allows us to build a complete model of the data.
# It also makes it easy incorporate censoring into any model.

# The basic idea:

# 1. Guess the model 
# 2. Use the data to determine if the guess is reasonable

# let's try N(0, 1) as a model for group g1:

# what is the probability of the first data point, given our model?

dnorm(g1[1])

tibble(x = seq(-3, 3, by = .1), y = dnorm(x)) %>% 
  ggplot(aes(x, y)) + 
  geom_line() + 
  geom_vline(xintercept = g1[1], col = "red")

# what about the censored value?:
# use the cumulative distribution function instead of the probability distribution function for censored data

pnorm(g1[cens_g1])

tibble(x = seq(-3, 3, by = .1), y = pnorm(x)) %>% 
  ggplot(aes(x, y)) + 
  geom_line() + 
  geom_vline(xintercept = g1[1], col = "red")

# put this in a function:

calc_lh <- function(x, cens, ...) {
  if_else(cens, pnorm(x, ...), dnorm(x, ...))
}

# calculate the probability of each data point, using our guess of mu = 0, sd = 1 for the model:

tibble(g1, cens_g1, lh = calc_lh(g1, cens_g1))

# multiply those probabilities together to get a probability for the dataset, given any guess of mu and sigma:

tibble(
  mu = 0, 
  sigma = 1, 
  lh = prod(calc_lh(g1, cens_g1, mean = mu, sd = sigma))
)

# then do this for a whole bunch of guesses, on a grid:

post <- crossing(
  mu = seq(0, 5, length.out = 200),
  sigma = seq(.1, 4, length.out = 200)
) %>% 
  rowwise() %>% 
  mutate(
    # do this on the log scale to minimize rounding errors with very small probabilities:
    ll = sum(calc_lh(g1, cens_g1, mean = mu, sd = sigma, log = TRUE))
  ) %>% 
  ungroup() %>% 
  mutate(
    prob = exp(ll), # back-transform
    prob = prob / sum(prob) # scale so that probabilities sum to 1
  )

# which mu and sigma are most probable?

post$mu[which.max(post$prob)]
post$sigma[which.max(post$prob)]

# sample values of mu and sigma in proportion to their probabilities:

post_sample <- post %>% 
  slice_sample(n = 1e4, replace = TRUE, weight_by = prob) %>%
  select(mu, sigma)

# plot the results --------------------------------------------------------

post %>% 
  ggplot(aes(mu, sigma)) + 
  scale_fill_viridis_c() +
  geom_raster(aes(fill = prob), show.legend = FALSE) +
  geom_contour(aes(z = prob), col = "white")

post_sample %>%
  pivot_longer(everything()) %>% 
  ggplot(aes(value)) + 
  facet_wrap(vars(name), scales = "free_x") +
  geom_histogram(binwidth = .1) +
  geom_vline(
    data = tibble(name = c("mu", "sigma"), param = c(mu, sd)),
    aes(xintercept = param), col = "red"
  )
