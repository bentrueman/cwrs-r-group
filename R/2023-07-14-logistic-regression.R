
# this week we're covering logistic regression---predicting a binary (0/1) 
# response variable such as presence/absence of a toxin like MCLR. we model 
# this kind of variable using a Bernoulli distribution 
# (https://en.wikipedia.org/wiki/Bernoulli_distribution), with a single
# parameter, p, the probability of a 1 instead of a zero (i.e., success instead
# failure or detect instead of non-detect).

# we'll simulate data and build up the logistic regression model "from scratch".

# setup -------------------------------------------------------------------

library("tidyverse")
library("withr")

theme_set(theme_bw())

n <- 200

# model 1 -----------------------------------------------------------------

# intercept-only model:

# y ~ Bernoulli(p)

p <- 0.7 # the true value of our parameter

# generate random draws from the Bernoulli distribution:
y <- with_seed(1, rbinom(n, size = 1, prob = p))

# if we guess p = 0.9, what is the (log) probability of observing y?
sum(dbinom(y, size = 1, prob = 0.9, log = TRUE))

# what about for other guesses? we can do a grid search:

post <- tibble(p = seq(0, 1, length.out = 1e4)) %>% 
  # for each row (guess for p),
  rowwise() %>% 
  # calculate the relative probability of the data, given p:
  mutate(ll = sum(dbinom(y, size = 1, prob = .data$p, log = TRUE))) %>% 
  ungroup() %>% 
  # convert back to a probability on [0,1]:
  mutate(
    prob = exp(ll),
    prob = prob / max(prob)
  ) %>% 
  # to get a probability distribution for p, given our data y,
  # sample the values of p in the dataframe according to their probabilities:
  slice_sample(n = 1e4, weight_by = prob, replace = TRUE) %>% 
  select(-c(prob, ll))

# here is the probability distribution of p, given the data y:
post %>% 
  ggplot(aes(p)) + 
  geom_histogram() + 
  geom_vline(xintercept = p, col = "white")

# model 2 -----------------------------------------------------------------

# linear model with a single predictor x, truncated to [0,1]:

# y ~ Bernoulli(mu)
# mu = cut01(beta0 + beta1 * x)

# the function cut01() ensures that our predictions for p 
# have a minimum of 0 and a maximum of 1 (since they are probabilities).

cut01 <- function(x) pmax(pmin(x, 1), 0)

beta0 <- 1 # true intercept
beta1 <- 4 # true slope
beta <- c("beta0" = beta0, "beta1" = beta1) # put these in vector for plotting

# generate random draws form the Bernoulli distribution, with p determined by 
# our linear model:
data <- with_seed(1, {
  tibble(
    # our predictor:
    x = rnorm(n), 
    # the 0/1 response variable:
    y = rbinom(n, size = 1, prob = cut01(beta0 + beta1 * x))
  )
})

# do another grid search, this time over possible combinations of beta0 and beta1:
post <- crossing(
  beta0 = seq(0.5, 2, length.out = 100),
  beta1 = seq(1.5, 9, length.out = 100)
) %>% 
  rowwise() %>% 
  mutate(ll = sum(dbinom(data$y, size = 1, prob = cut01(beta0 + beta1 * data$x), log = TRUE))) %>% 
  ungroup() %>% 
  mutate(
    prob = exp(ll),
    prob = prob / max(prob)
  ) %>% 
  slice_sample(n = 1e4, weight_by = prob, replace = TRUE) %>% 
  select(-c(prob, ll))

# plot the probability distributions of the model parameters, 
# given the data x and y:
post %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(value)) + 
  facet_wrap(vars(name), scales = "free") +
  geom_histogram() + 
  geom_vline(
    data = as_tibble(beta, rownames = "name"),
    aes(xintercept = value),
    col = "white"
  )

# each combo of bet0 and beta1 yields a model, which we plot as a curve:
curves <- post[1:50,] %>%
  rowid_to_column() %>% 
  rowwise() %>% 
  mutate(x = list(seq(min(data$x), max(data$x), length.out = n))) %>% 
  ungroup() %>% 
  unnest(x) %>% 
  mutate(yhat = cut01(beta0 + beta1 * x))

# plot the data, plausible models for it, and the true model:

data %>% 
  mutate(yhat = cut01(beta0 + beta1 * x)) %>% 
  ggplot(aes(x, y)) + 
  # plausible models:
  geom_line(
    data = curves,
    aes(x = x, y = yhat, group = rowid),
    col = "black", linewidth = 0.2
  ) +
  geom_line(aes(y = yhat), col = "red") + # true model
  geom_point() # the data

# model 3 -----------------------------------------------------------------

# logistic regression model (just replace the function "cut01" with the 
# logistic function):

# y ~ Bernoulli(mu)
# mu = logistic(beta0 + beta1 * x)

logistic <- function(x) 1 / (1 + exp(-x))
# (see https://en.wikipedia.org/wiki/Logistic_function)

# true values of our parameters:
beta0 <- 2
beta1 <- 3
beta <- c("beta0" = beta0, "beta1" = beta1)

# simulated data:
data <- with_seed(1, {
  tibble(
    x = rnorm(n),
    y = rbinom(n, size = 1, prob = logistic(beta0 + beta1 * x))
  )
})

# grid search:
post <- crossing(
  beta0 = seq(1, 3.5, length.out = 100),
  beta1 = seq(1.5, 7, length.out = 100)
) %>% 
  rowwise() %>% 
  mutate(ll = sum(dbinom(data$y, size = 1, prob = logistic(beta0 + beta1 * data$x), log = TRUE))) %>% 
  ungroup() %>% 
  mutate(
    prob = exp(ll),
    prob = prob / max(prob)
  ) %>% 
  slice_sample(n = 1e4, weight_by = prob, replace = TRUE) %>% 
  select(-c(prob, ll))

# probability distributions of the parameters, given the data:
post %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(value)) + 
  facet_wrap(vars(name), scales = "free") +
  geom_histogram() + 
  geom_vline(
    data = as_tibble(beta, rownames = "name"),
    aes(xintercept = value),
    col = "white"
  )

# generate predictions from plausible models:
curves <- post[1:50,] %>%
  rowid_to_column() %>% 
  rowwise() %>% 
  mutate(x = list(seq(min(data$x), max(data$x), length.out = n))) %>% 
  ungroup() %>% 
  unnest(x) %>% 
  mutate(yhat = logistic(beta0 + beta1 * x))

# plot the data, plausible models, and the true model:
data %>% 
  mutate(yhat = logistic(beta0 + beta1 * x)) %>% 
  ggplot(aes(x, y)) + 
  geom_line(
    data = curves,
    aes(x = x, y = yhat, group = rowid),
    col = "black", linewidth = 0.1
  ) +
  geom_line(aes(y = yhat), col = "red") +
  geom_point()


# compare with stats::glm() -----------------------------------------------

# let's compare with logistic regression as implemented in the "stats" package:
glm(y ~ x, data = data, family = binomial) # logistic regression

apply(post, 2, mean) # our grid search method is roughly comparable for this simulation
