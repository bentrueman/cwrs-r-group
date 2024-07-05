
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
    aes(y = output_adaptive_window, col = "Moving average (adaptive window)")
  ) +
  labs(col = NULL)

p2

# this estimate is a step function because we're assigning the moving average output
# to an integer hour. if we instead assign to output to an interpolated fractional hour,
# we get a smooth function, although the mapping of moving average output to fractional hour
# is arbitrary

# interpolated moving average ---------------------------------------------

# create the input to moving average function:

input_interpolated <- bike_traffic_subset |> 
  select(hour, bike_count) |> 
  group_by(hour) |> 
  mutate(hour_interpolated = seq(unique(hour), unique(hour) + 1, length.out = length(hour))) |> 
  ungroup()

# future_map() applies our function to each row of the input dataframe, processing in parallel for speed:

output_interpolated <- future_map_dbl(
  seq(nrow(input_interpolated)), moving_average_adaptive_window, .progress = TRUE
)

# add the output to the input:

input_interpolated <- input_interpolated |> 
  mutate(output_interpolated)

# add the results to our plot ---------------------------------------------

p3 <- p2 +
  geom_line(
    data = input_interpolated,
    aes(hour_interpolated, output_interpolated, col = "Moving average (interpolated)")
  ) +
  labs(col = NULL)

p3

# this is still not completely smooth, because when data enters the window,
# they can have a discontinuous effect on the moving average. we can solve this by 
# downweighting the edges of the window, using tricube weights.

# weighted moving average -------------------------------------------------

tricube_weights <- (1 - abs(window / max(abs(window))) ^ 3) ^ 3

plot(tricube_weights, type = "l")

# create the input to moving average function:

input_weighted <- input_interpolated

# redefine our function:

moving_average_weighted <- function(x) {
  keep_these_rows <- x + window
  # make sure keep_these_rows is positive (we can't include points before the first point) and at most 
  # equal to the number of rows in the input (the moving average estimate for the last point will only 
  # include time points in the past)
  valid_rows <- keep_these_rows > 0 & keep_these_rows <= nrow(input_weighted)
  keep_these_rows <- keep_these_rows[valid_rows]
  tricube_weights <- tricube_weights[valid_rows]
  input_weighted |> 
    slice(keep_these_rows) |> 
    with(mean(bike_count * tricube_weights, na.rm = TRUE))
}

# future_map() applies our function to each row of the input dataframe, processing in parallel for speed:

output_weighted <- future_map_dbl(
  seq(nrow(input_weighted)), moving_average_weighted, .progress = TRUE
)

# add the output to the input:

input_weighted <- input_weighted |> 
  mutate(output_weighted)

# add the results to our plot ---------------------------------------------

p4 <- p3 +
  geom_line(
    data = input_weighted,
    aes(hour_interpolated, output_weighted, col = "Moving average (weighted)")
  ) +
  labs(col = NULL)

p4

# we now have a smooth function, but it is damped significantly by the weights, which haven't been 
# normalized. we can solve this, and make a much more natural connection between the moving average and 
# the interpolated fractional hours, by using linear regression instead of a simple average. that way, 
# we can actually make a prediction from a linear model at each fractional hour.

# local linear regression ("loess") ---------------------------------------

# create the input to moving average function:

input_local_linear <- input_interpolated

# redefine our function:

moving_average_local_linear <- function(x) {
  keep_these_rows <- x + window
  # make sure keep_these_rows is positive (we can't include points before the first point) and at most 
  # equal to the number of rows in the input (the moving average estimate for the last point will only 
  # include time points in the past)
  valid_rows <- keep_these_rows > 0 & keep_these_rows <= nrow(input_local_linear)
  keep_these_rows <- keep_these_rows[valid_rows]
  tricube_weights <- tricube_weights[valid_rows]
  input_local_linear |> 
    slice(keep_these_rows) |> 
    with(predict(
      # here we are doing quadratic regression, which captures nonlinear variation 
      # better than linear regression:
      lm(bike_count ~ hour + I(hour ^ 2), weights = tricube_weights), 
      newdata = tibble(hour = input_local_linear$hour_interpolated[x])
    ))
}

# future_map() applies our function to each row of the input dataframe, processing in parallel for speed:

output_local_linear <- future_map_dbl(
  seq(nrow(input_local_linear)), moving_average_local_linear, .progress = TRUE
)

# add the output to the input:

input_local_linear <- input_local_linear |> 
  mutate(output_local_linear)

# the result is almost the same as the output of geom_smooth(method = "loess"),
# which uses a similar algorithm:

# add the results to our plot ---------------------------------------------

p5 <- p4 +
  geom_line(
    data = input_local_linear,
    aes(hour_interpolated, output_local_linear, col = "Moving average (local linear)")
  ) +
  labs(col = NULL)

p5 + 
  geom_smooth(
    aes(col = "geom_smooth()"), 
    method = "loess", 
    span = 0.2,
    se = FALSE, 
    linewidth = 0.6
  )
  
