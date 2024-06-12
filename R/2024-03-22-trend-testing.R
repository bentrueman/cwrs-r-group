
# this week we're covering simple trend testing methods for time series. the upshot: models that estimate 
# autocorrelation---the dependence of one observation on the previous one---identify 
# fewer spurious trends.

# setup -------------------------------------------------------------------

library("tidyverse")
library("broom")
library("zyp") # one option for the Theil-Sen line (the Mann-Kendall test's accompanying trend estimate)

# simulate from a first order autoregressive model ------------------------

# this is a foundational time series model. it expresses the observation at time t as a 
# function of the previous observation and some random noise, e at time t. the parameter phi is the 
# autocorrelation coefficient---it quantifies how strongly an observation depends on the previous observation.

# x_{t} = phi * x_{t-1} + e_{t}
# e_{t} = N(0, sigma)

n <- 100
total_length <- 5 * n
phi <- 0.9
x <- accumulate(rnorm(total_length), \(acc, nxt) phi * acc + nxt)
x <- x[(total_length - n + 1):total_length] # discard the first 80% of the observations, since they may
# be affected by the initial condition

# plot the results --------------------------------------------------------

# the time series. notice how autocorrelation causes runs in the time series that look like trends

tibble(x) |> 
  ggplot(aes(time(x), x)) +
  geom_line()

# the time series plotted against itself lagged by 1--16 observations. notice how
# the autocorrelation---correlation of a time series with its past self---decays with time separation.

map(1:16, ~ lag(x, .x)) |> 
  set_names(1:16) |> 
  as_tibble() |> 
  mutate(x) |> 
  pivot_longer(-x, names_to = "lag") |> 
  mutate(lag = factor(lag, levels = 1:16)) |> 
  ggplot(aes(x, value)) + 
  facet_wrap(vars(lag)) +
  geom_point()

# testing framework -------------------------------------------------------

# a trend testing method should be able to tell the difference between autocorrelation 
# (due to the parameter phi on line 10) and trend. here, we're simulating a time series with no trend
# and then estimating the trend using various models. a good model won't find a trend most of the time.

x <- seq(n)

simulation_ouput <- map(
  seq(100),
  \(id) {
    y <- accumulate(rnorm(total_length), \(acc, nxt) phi * acc + nxt)[(total_length - n + 1):total_length]
    model_1 <- lm(y ~ x)
    model_2 <- arima(y, order = c(1, 0, 0), xreg = x)
    model_3 <- zyp.sen(y ~ x, data = tibble(x, y))
    model_3_ci <- confint(model_3)
    bind_rows(
      "linear regression" = tidy(model_1, conf.int = TRUE)[2,],
      "linear regression with first-order\nautoregressive errors" = tidy(model_2, conf.int = TRUE)[3,],
      "theil-sen line\n(mann-kendall test)" = tibble(estimate = coef(model_3)[2], conf.low = model_3_ci[2, 1], conf.high = model_3_ci[2, 2]),
      .id = "model"
    ) |> 
      mutate(simulation_id = id)
  }
) |> 
  list_rbind()

# plot the output: 

simulation_ouput |> 
  mutate(includes_zero = sign(conf.low) != sign(conf.high)) |> 
  ggplot(aes(reorder(simulation_id, estimate), estimate, col = includes_zero)) + 
  facet_wrap(vars(model), scales = "free_y") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_point() + 
  geom_text(
    data = \(x) x |> 
      group_by(model) |> 
      summarize(
        x = -Inf, y = Inf, 
        label = paste0("Spurious trends: ",  100 * (1 - mean(includes_zero)), " %")
      ),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE, hjust = "inward", vjust = "inward"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom"
  ) + 
  labs(x = NULL, col = "confidence interval on slope includes zero:")

