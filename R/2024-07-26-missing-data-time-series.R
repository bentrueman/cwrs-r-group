
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

global_temps |> 
  mutate(
    missing = with_seed(1, sample(c(FALSE, TRUE), size = length(year), replace = TRUE, prob = c(0.7, 0.3))),
    anomaly_observed = if_else(missing, NA_real_, anomaly)
  )

# to be continued ...
