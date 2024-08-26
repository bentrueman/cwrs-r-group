
# notes -------------------------------------------------------------------

# this week we're covering interval censoring, which can crop up when we analyze
# paired data with measurements below the limit of detection---when we have data that are only
# know to be below some detection limit, their pairwise differences are interval censored: above a 
# lower bound and below an upper bound. As we did with censored linear regression, we can use 
# the cumulative distribution function to solve the problem.

# setup -------------------------------------------------------------------

library("tidyverse")
library("ggtext")
library("scales")

theme_set(
  theme_bw() + 
    theme(
      legend.position = "bottom",
      axis.title = element_markdown()
    )
)

# read the data -----------------------------------------------------------

# the data represent samples collected at a set of residences before and after a change
# in the drinking water treatment process at the plant supplying the sites:

pb_paired <- read_csv("data/paired-lead-concentrations.csv")

pb_wide <- pb_paired |> 
  pivot_wider(names_from = period, values_from = c(pb_ppb, bdl))

# plot the data -----------------------------------------------------------

p1 <- pb_wide |> 
  ggplot(aes(x = pb_ppb_before, y = pb_ppb_after)) + 
  scale_color_manual(values = c("grey", "black")) +
  scale_fill_manual(values = "grey") +
  geom_abline(aes(slope = 1, intercept = 0, col = "y=x")) +
  geom_segment(
    data = \(x) filter(x, bdl_before & !bdl_after),
    aes(xend = 0, yend = pb_ppb_after, col = "Half-censored pair")
  ) +
  geom_segment(
    data = \(x) filter(x, !bdl_before & bdl_after),
    aes(yend = 0, xend = pb_ppb_before, col = "Half-censored pair")
  ) +
  geom_rect(
    data = \(x) filter(x, bdl_before & bdl_after),
    aes(
      xmin = 0, xmax = pb_ppb_before, ymin = 0, ymax = pb_ppb_after, 
      fill = "Fully censored pair"
    ),
    alpha = 0.3
  ) +
  geom_point(
    data = function(x) filter(x, !bdl_before & !bdl_after), 
    size = 3, shape = 16, alpha = 0.7
  ) + 
  labs(
    x = "[Pb]<sub>before</sub> (ppb)",
    y = "[Pb]<sub>after</sub> (ppb)",
    col = NULL, fill = NULL
  )

p1

# calculate before/after differences --------------------------------------

# interval censored differences arise when one or both elements of a matched pair is a nondetect.

pairwise_differences <- pb_wide |> 
  transmute(
    site,
    censoring = paste(bdl_before, bdl_after),
    lower_bound = case_when(
      censoring %in% c("FALSE FALSE", "TRUE FALSE") ~ pb_ppb_after - pb_ppb_before,
      censoring %in% c("TRUE TRUE", "FALSE TRUE") ~ -pb_ppb_before
    ),
    upper_bound = case_when(
      censoring %in% c("FALSE FALSE", "FALSE TRUE") ~ pb_ppb_after - pb_ppb_before,
      censoring %in% c("TRUE TRUE", "TRUE FALSE") ~ pb_ppb_after
    ),
    censoring = censoring != "FALSE FALSE"
  )

# model the pairwise differences graphically ------------------------------

# our model:
# pairwise_differences ~ N(mu, sigma)

# guess the parameters:

mu <- 1
sigma <- 1

# plot the data and the guess:

tibble(
  x = seq(from = mu - 4 * sigma, to = mu + 4 * sigma, length.out = 200),
  y = dnorm(x = x, mean = mu, sd = sigma)
) |> 
  ggplot() +
  geom_line(aes(x = x, y = y)) + 
  geom_point(data = filter(pairwise_differences, !censoring), aes(x = lower_bound, y = 0)) +
  geom_linerange(
    data = filter(pairwise_differences, censoring),
    aes(xmin = lower_bound, xmax = upper_bound, y = -0.025, col = site)
  ) + 
  labs(
    x = "Pairwise difference",
    y = "Normal density",
    col = "Site"
  )

# score the guess against the data (i.e., calculate the likelihood)

log_likelihood_observed <- with(pairwise_differences, dnorm(x = lower_bound[!censoring], mean = mu, sd = sigma)) |> 
  log() |> 
  sum()

log_likelihood_censored <- with(
  pairwise_differences, pnorm(q = upper_bound[censoring], mean = mu, sd = sigma) - 
    pnorm(q = lower_bound[censoring], mean = mu, sd = sigma)
) |> 
  log() |>
  sum()

log_likelihood_observed + log_likelihood_censored

# write a likelihood function ---------------------------------------------

calculate_likelihood <- function(lower_bound, upper_bound, censoring, mu, sigma) {
  log_likelihood_observed <- sum(log(dnorm(x = lower_bound[!censoring], mean = mu, sd = sigma)))
  log_likelihood_censored <- sum(log(pnorm(q = upper_bound[censoring], mean = mu, sd = sigma) - 
                                       pnorm(q = lower_bound[censoring], mean = mu, sd = sigma)))
  log_likelihood_observed + log_likelihood_censored
}

# estimate the model by grid approximation --------------------------------

model_grid <- crossing(
  mu = seq(-0.8, 4, length.out = 200),
  sigma = seq(0.5, 5, length.out = 200)
) |> 
  rowwise() |> 
  mutate(log_likelihood = with(pairwise_differences, calculate_likelihood(
    lower_bound, upper_bound, censoring, mu, sigma
  ))) |> 
  ungroup()

# sample from the grid of guesses, weighted by the likelihood:

posterior <- model_grid |> 
  slice_sample(n = 1e4, weight_by = rescale(exp(log_likelihood), 0:1), replace = TRUE)

# plot the samples:

posterior |> 
  pivot_longer(-log_likelihood) |> 
  ggplot(aes(value)) +
  facet_wrap(vars(name), scales = "free") +
  geom_histogram()

# summarize the samples:

model_summary <- posterior |> 
  pivot_longer(-log_likelihood) |>
  group_by(name) |> 
  summarize(
    estimate = median(value),
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975)
  )

# add the difference estimate to the plot:

p1 + 
  scale_color_manual(values = c("salmon", "grey", "black")) +
  scale_fill_manual(values = c("salmon", "grey")) +
  geom_ribbon(
    aes(
      ymin = pb_ppb_before + model_summary$lower[1], 
      ymax = pb_ppb_before + model_summary$upper[1],
      fill = "Difference estimate"
    ), 
    alpha = 0.3
  ) +
  geom_line(aes(y = pb_ppb_before + model_summary$estimate[1], col = "Difference estimate"))
