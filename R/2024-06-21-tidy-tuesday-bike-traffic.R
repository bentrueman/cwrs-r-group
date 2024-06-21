
# setup -------------------------------------------------------------------

library("tidyverse")
library("tidytuesdayR")
library("lubridate")
library("furrr")

theme_set(theme_bw())

plan("multisession")

# download the data -------------------------------------------------------

tt_data <- tt_load(2019, 14)

# clean the data ----------------------------------------------------------

bike_traffic <- tt_data$bike_traffic |> 
  mutate(
    date_time = mdy_hms(date),
    hour = hour(date_time),
    week_day = wday(date_time, label = TRUE),
    doy = yday(date_time),
    location = paste(crossing, direction),
    pattern = if_else(week_day %in% c("Sat", "Sun"), "Weekend", "Weekday")
  ) |> 
  group_by(location) |> 
  mutate(mean_bike_count = mean(bike_count, na.rm = TRUE)) |> 
  ungroup() |> 
  # take the top 5 locations by mean bike count:
  filter(mean_bike_count %in% sort(unique(mean_bike_count), decreasing = TRUE)[1:5])

# plot the data -----------------------------------------------------------

bike_traffic |> 
  mutate(bike_count = log10(bike_count + 1)) |>
  ggplot(aes(hour, doy, fill = bike_count)) + 
  scale_fill_viridis_c(option = "mako", breaks = 0:3, labels = c(1, 10, 100, 1000)) +
  facet_grid(
    rows = vars(location), cols = vars(pattern), 
    labeller = label_wrap_gen(width = 12)
  ) + 
  geom_tile() +
  labs(
    x = "Hour of the day",
    y = "Day of the year",
    fill = "Bikes counted"
  )

# plot the weekday pattern ------------------------------------------------

bike_traffic_subset <- bike_traffic |> 
  filter(pattern == "Weekday", location == "MTS Trail East") |> 
  arrange(hour)

p1 <- bike_traffic_subset |> 
  ggplot(aes(hour, bike_count)) +
  geom_point() +
  labs(
    x = "Hour of the day",
    y = "Bikes counted"
  )

p1

# smoothing the weekday pattern -------------------------------------------

# we'll borrow the moving average function we wrote last time

window_size <- round(0.2 * nrow(bike_traffic_subset)) # n = 20% of the data
window_uncentered <- seq(window_size) # create an index from 1 to n
window <- window_uncentered - round(median(window_uncentered)) # center our window at 0, so we go back n/2 and forward n/2 to calculate our average

# create the input to moving average function:

input_adaptive_window <- bike_traffic_subset |> 
  select(hour, bike_count)

# our function is almost the same as the one we used last time - we just need to change the argument to mean():

moving_average_adaptive_window <- function(x) {
  keep_these_rows <- x + window
  # make sure keep_these_rows is positive (we can't include points before the first point) and at most 
  # equal to the number of rows in the input (the moving average estimate for the last point will only 
  # include time points in the past)
  valid_rows <- keep_these_rows > 0 & keep_these_rows <= nrow(input_adaptive_window)
  keep_these_rows <- keep_these_rows[valid_rows]
  input_adaptive_window |> 
    slice(keep_these_rows) |> 
    with(mean(bike_count, na.rm = TRUE))
}

# future_map() applies our function to each row of the input dataframe, processing in parallel for speed:

output_adaptive_window <- future_map_dbl(
  seq(nrow(input_adaptive_window)), moving_average_adaptive_window, .progress = TRUE
)

# add the output to the input:

input_adaptive_window <- input_adaptive_window |> 
  mutate(output_adaptive_window)

# add the results to our plot ---------------------------------------------

p2 <- p1 +
  geom_line(
    data = input_adaptive_window,
    aes(y = output_adaptive_window, col = "Moving average")
  ) +
  labs(col = NULL)

p2

# local linear regression -------------------------------------------------

# the moving average above is a step function because we have a discrete x variable (hour).

# but if we use linear regression instead of just averaging, we can smoothly interpolate between the
# discrete hour-long intervals by making predictions from a different linear regression model fitted
# at each time point to data within the window (i.e., "local linear regression"). this will give us a 
# more realistic summary of the weekday pattern in bike traffic.

# create the input to the local linear regression. this time, we're going to be making predictions over
# a grid of possible time points - not just averaging on the hour:

input_local_linear_regression <- bike_traffic_subset |> 
  select(hour, bike_count) |> 
  # create a regular grid of times over which to make predictions:
  group_by(hour) |> 
  mutate(hour_interpolated = seq(min(hour), min(hour) + 1, length.out = length(hour))) |> 
  ungroup() |> 
  arrange(hour)

# create a vector of weights for our data. this ensures that the output is smooth, since as the window moves,
# new points that enter it have only a small influence on the prediction until they migrate to the center of 
# the window. call plot(tricube_weights) to see why.

tricube_weights <- (1 - abs(window / max(abs(window))) ^ 3) ^ 3 # one of many possible weight functions

# our new function uses lm() and predict() instead of mean():

local_linear_regression <- function(x) {
  keep_these_rows <- x + window
  # make sure keep_these_rows is positive (we can't include points before the first point) and at most 
  # equal to the number of rows in the input (the moving average estimate for the last point will only 
  # include time points in the past)
  valid_rows <- keep_these_rows > 0 & keep_these_rows <= nrow(input_local_linear_regression)
  keep_these_rows <- keep_these_rows[valid_rows]
  tricube_weights <- tricube_weights[valid_rows]
  input_local_linear_regression |> 
    slice(keep_these_rows) |> 
    with(predict(
      lm(bike_count ~ hour, weights = tricube_weights), 
      newdata = tibble(hour = input_local_linear_regression$hour_interpolated[x])
    ))
}

# future_map() applies our function to each row of the input dataframe, processing in parallel for speed:

output_local_linear_regression <- future_map_dbl(
  seq(nrow(input_local_linear_regression)), local_linear_regression, .progress = TRUE
)

# add the output to the input:

input_local_linear_regression <- input_local_linear_regression |> 
  mutate(output_local_linear_regression)

# add results to the plot -------------------------------------------------

p2 + 
  geom_line(
    data = input_local_linear_regression,
    aes(hour_interpolated, output_local_linear_regression, col = "Local linear regression")
  )

