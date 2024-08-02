
# This week we're exploring a different strategy for handling missing values: fitting a model that 
# omits them and then imputing them with predictions from the model. We'll look at a linear model,
# note it's shortcomings, and then see if we can do better.

# setup -------------------------------------------------------------------

library("tidyverse")
library("scales")
library("withr")
library("ggtext")

theme_set(
  theme_bw() + 
    theme(
      axis.title = element_markdown(),
      legend.position = "bottom"
    )
)

# read the data -----------------------------------------------------------

global_temps <- read_csv("data/climate-data.csv", skip = 4) |> 
  filter(Year > 1900) |> 
  transmute(
    year = Year,
    year_scaled = year - min(year),
    anomaly = Anomaly
  )

# plot the data -----------------------------------------------------------

p1 <- global_temps |> 
  ggplot(aes(x = year, y = anomaly)) + 
  geom_line() + 
  labs(
    x = NULL,
    y = "Global Land and Ocean June<br>Average Temperature Anomalies (&deg;C)",
    col = NULL
  )

p1

# fit a linear model ------------------------------------------------------

model_lm <- lm(anomaly ~ year_scaled, data = global_temps)

# add the linear model predictions to the plot ----------------------------

p1 + 
  geom_line(aes(y = fitted(model_lm), col = "Linear model"))

# simulate missing values -------------------------------------------------

global_temps <- global_temps |> 
  mutate(
    missing = with_seed(1, sample(c(FALSE, TRUE), size = length(year), replace = TRUE, prob = c(0.7, 0.3))),
    anomaly_observed = if_else(missing, NA_real_, anomaly)
  )

# plot the incomplete dataset ---------------------------------------------

p2 <- global_temps |> 
  ggplot(aes(x = year, y = anomaly_observed)) + 
  geom_line(aes(y = anomaly), linewidth = 0.2, col = "grey") +
  geom_line() + 
  labs(
    x = NULL,
    y = "Global Land and Ocean June<br>Average Temperature Anomalies (&deg;C)",
    col = NULL
  )

p2

# refit the linear model to the incomplete dataset ------------------------

model_lm_missing <- lm(anomaly_observed ~ year_scaled, data = global_temps)

# add predictions to the plot ---------------------------------------------

global_temps <- global_temps |> 
  mutate(
    predictions_lm = predict(model_lm_missing, newdata = tibble(year_scaled)),
    predictions_lm = if_else(missing, predictions_lm, NA_real_)
  )

p3 <- p2 + 
  geom_point(data = global_temps, aes(y = predictions_lm, col = "Linear model"))

p3

# can we do better? continuous time autoregression ------------------------

# CAR1

# anomaly_{t} = N(mu_{t}, sigma)
# mu_{t} = linear_predictor_{t} + deviation_{t}
# linear_predictor_{t} = beta0 + beta1 * year_scaled_{t}
# deviation_{t} = residual_{t - timelag} * memory ^ timelag
# residual_{t - timelag} = anomaly_{t - timelag} - linear_predictor_{t - timelag}
# memory is in [0, 1]

# make a CAR1 function ----------------------------------------------------

continuous_time_autoregression <- function(beta0, beta1, memory, year_scaled, anomaly, timelag) {
  linear_predictor <- beta0 + beta1 * year_scaled
  previous_residual <- lag(anomaly - linear_predictor)
  previous_residual[1] <- 0
  deviation <- previous_residual * memory ^ timelag
  linear_predictor + deviation
}

# estimate the CAR1 parameters --------------------------------------------

global_temps_subset <- global_temps |> 
  drop_na(anomaly_observed) |> 
  mutate(timelag = year - lag(year))

global_temps_subset$timelag[1] <- 0
  
grid_size <- 15

beta0 <- seq(-0.6, -0.1, length.out = grid_size)
beta1 <- seq(6e-3, 13e-3, length.out = grid_size)
sigma <- seq(0.1, 0.18, length.out = grid_size)
memory <- seq(0, 1, length.out = grid_size)

grid_output <- crossing(beta0, beta1, sigma, memory) |> 
  rowwise() |> 
  mutate(
    log_likelihood = sum(dnorm(
      global_temps_subset$anomaly, 
      mean = with(global_temps_subset, continuous_time_autoregression(
        beta0, beta1, memory, year_scaled, anomaly, timelag
      )), 
      sd = sigma, 
      log = TRUE
    ))
  ) |> 
  ungroup()

model_parameters <- grid_output |> 
  slice_max(log_likelihood)

model_parameters

