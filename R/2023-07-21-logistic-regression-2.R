
# this script is a continuation of last week's intro to logistic 
# regression. we expand the model to include two predictors, calculate 
# a decision boundary, and evaluate the model using the ROC curve. 

# setup -------------------------------------------------------------------

library("tidyverse")
library("withr")
library("pROC")

theme_set(theme_bw())

n <- 200 # number of simulated observations

# model 4 -----------------------------------------------------------------

# y ~ Bernoulli(mu)
# mu = logistic(beta0 + beta1 * x1 + beta2 * x2)

logistic <- function(x) 1 / (1 + exp(-x))
# (see https://en.wikipedia.org/wiki/Logistic_function)

# true values of our parameters:
beta0 <- 2
beta1 <- 3
beta2 <- 4
beta <- c("beta0" = beta0, "beta1" = beta1, "beta2" = beta2)

# simulated data:
data <- with_seed(1, {
  tibble(
    x1 = rnorm(n),
    x2 = rnorm(n),
    y = as.logical(rbinom(
      n, size = 1, 
      prob = logistic(beta0 + beta1 * x1 + beta2 * x2)
    ))
  )
})

# do a grid search over possible parameter values, then sample from the 
# grid according to the probability that any combo generated the data we 
# have:
post <- crossing(
  beta0 = seq(1, 3.5, length.out = 25),
  beta1 = seq(1.5, 7, length.out = 25),
  beta2 = seq(0, 6, length.out = 25)
) %>% 
  rowwise() %>% 
  mutate(
    ll = sum(dbinom(
      data$y, size = 1, 
      prob = logistic(beta0 + beta1 * data$x1 + beta2 * data$x2),
      log = TRUE
    ))
  ) %>% 
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
    col = "red"
  )

# generate decision boundaries from the plausible models:

# if mu = logistic(beta0 + beta1 * x1 + beta2 * x2), we can
# use the inverse of the logistic to make this a linear model in x1 and x2
# first, rearrange the equation above to get
# log(mu / (1 - mu)) = beta0 + beta1 * x1 + beta2 * x2
# then set mu = 0.5
# which gives
#  x2 = - beta0 / beta2 - beta1 / beta2 * x1

boundaries <- post[1:50,] %>%
  mutate(
    slope = -beta1 / beta2,
    intercept = -beta0 / beta2,
  )

# plot the data, plausible decision boundaries, and the true
# decision boundary:
data %>% 
  ggplot(aes(x1, x2, col = y)) + 
  geom_point() + 
  geom_abline(
    data = boundaries,
    aes(slope = slope, intercept = intercept),
    linewidth = 0.1,
    col = "grey55"
  ) +
  geom_abline(
    slope = -beta1 / beta2, intercept = -beta0 / beta2,
    col = "red"
  )

# compare with stats::glm() -----------------------------------------------

# let's compare with logistic regression as implemented in the "stats" package:
glm(y ~ x1 + x2, data = data, family = binomial) # logistic regression

apply(post, 2, mean) # our grid search method is roughly comparable for this simulation

# make roc curve ----------------------------------------------------------

# let's evaluate our classifier using an ROC curve 
# (https://en.wikipedia.org/wiki/Receiver_operating_characteristic)

# this is a tool for comparing the true and false positive rates as we vary 
# mu in the decision boundary computation above. we have many plausible models,
# so we'll calculate a set of 100 ROC curves.

roc_in <- post[1:100, ] %>%
  rowwise() %>% 
  mutate(data = list(data)) %>% 
  ungroup() %>% 
  rowid_to_column("draw") %>% 
  unnest(data) %>% 
  # generate model predictions for each set of betas:
  mutate(yhat = logistic(beta0 + beta1 * x1 + beta2 * x2)) %>% 
  # for each set of betas,
  group_by(draw) %>% 
  # and each possible decision boundary from the highest score to the lowest,
  arrange(desc(yhat)) %>% 
  mutate(
    # calculate the true positive rate,
    tpr = cumsum(y) / sum(y),
    # and the false positive rate.
    fpr = cumsum(!y) / sum(!y)
  ) %>% 
  ungroup()

# plot results:
roc_in %>% 
  ggplot(aes(fpr, tpr, group = draw)) + 
  geom_abline() +
  geom_line(linewidth = 0.1)

# the following simulation illustrates the meaning of the area under the ROC curve:

# what is the probability that a randomly-selected 1 has a 
# higher model score (estimated probability of success) than a randomly selected 0? 
iter <- 2e4
p_corr <- rep(NA, iter)
p_corr_in <- roc_in %>% 
  filter(draw == 1)
for (i in 1:iter) {
  detect <- sample(p_corr_in$yhat[p_corr_in$y], size = 1)
  nondetect <- sample(p_corr_in$yhat[!p_corr_in$y], size = 1)
  p_corr[i] <- detect > nondetect
}

# plot results:

p1 <- tibble(
  iter = seq_len(length(p_corr)),
  p = cumsum(p_corr) / iter
) %>% 
  ggplot(aes(iter, p)) + 
  geom_line()

p1

# this is the same thing as the area under the ROC curve:
auc <- roc_in %>% 
  group_by(draw) %>% 
  mutate(
    delta_x = fpr - lag(fpr), # change in x
    area = tpr * delta_x # y * (change in x)
  ) %>% 
  filter(!is.na(delta_x)) %>% 
  summarize(auc = sum(area) / sum(delta_x)) %>% 
  ungroup()

# sampling strategy converges to the calculated AUC:
p1 + 
  geom_hline(yintercept = auc$auc[1], col = "red")

# compare with pROC package -----------------------------------------------

roc_in %>% 
  filter(draw == 1) %>% 
  with(roc(y, yhat)) %>% 
  plot(print.auc = TRUE)

auc$auc[1]
