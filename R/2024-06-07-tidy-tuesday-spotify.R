
# This week we're attempting an answer to the question, "Did the introduction of Spotify
# encourage shorter song lengths?". We'll be using a few methods to estimate how song length 
# changed over time, before and after the founding of Spotify:

# 1) a linear model
# 2) a fixed window moving average
# 3) an adaptive window moving average

# setup -------------------------------------------------------------------

library("tidyverse")
library("tidytuesdayR")

spotify_founded <- as.Date("2006-04-23") # from https://en.wikipedia.org/wiki/Spotify

theme_set(theme_bw())

# read data ---------------------------------------------------------------

data <- tt_load(2020, 4)

# clean data --------------------------------------------------------------

spotify_songs <- data$spotify_songs |>
  mutate(
    track_album_release_date = as.Date(track_album_release_date),
    duration_min = duration_ms / 6e4, # convert song length to minutes
    spotify_era = track_album_release_date >= spotify_founded,
    # the next line sets day 0 as the date spotify was founded; since the intercept in the model we fit
    # below represents the song length when date is zero, this gives us a more interpretable intercept
    track_album_release_date_model = track_album_release_date - spotify_founded
  ) |>
  # we lose about 5% of the data here, representing incomplete dates
  drop_na(track_album_release_date)

# plot the data -----------------------------------------------------------

p1 <- spotify_songs |> 
  ggplot(aes(track_album_release_date, duration_min)) +
  geom_point() +
  geom_vline(aes(xintercept = spotify_founded, col = "Spotify founded")) +
  labs(col = NULL)

p1

# method 1: fit a linear model --------------------------------------------

# our linear model can be summarized in the following two lines, which say that song length 
# is normally distributed with a fixed standard deviation and a mean that varies linearly over time

# duration_min ~ Normal(mu, sigma)
# mu = beta0 + beta1 * track_album_release_date

model_1 <- lm(duration_min ~ track_album_release_date, data = spotify_songs)

# We want to fit a more complex model, where the linear slope is different depending on whether 
# Spotify exists yet. This is how we do that:

# mu = beta0 + beta1 * track_album_release_date + beta2 * track_album_release_date * spotify_era

# if spotify_era == 0:
# mu = beta0 + beta1 * track_album_release_date

# if spotify_era == 1:
# mu = beta0 + (beta1 + beta2) * track_album_release_date

model_2 <- lm(
  duration_min ~ track_album_release_date_model + track_album_release_date_model:spotify_era, 
  data = spotify_songs
)

# plot the model results --------------------------------------------------

p1 + 
  geom_line(aes(y = fitted(model_1), col = "model 1")) +
  geom_line(aes(y = fitted(model_2), col = "model 2"))

# moving average with a fixed window size ---------------------------------

# next, let's look at a more flexible approach - the moving average. for each date in our dataset,
# we'll estimate the expected song length as the average song length in a two-year window from one year
# before to one year after that date.

window <- 365 # in days

# define a function to select only songs within our time window, then take the average length:

moving_average_fixed_window <- function(x) {
  input_fixed_window |> 
    filter(
      track_album_release_date > track_album_release_date[x] - window,
      track_album_release_date < track_album_release_date[x] + window,
    ) |> 
    with(mean(duration_min))
}

# to speed things up, select only the columns we need:

input_fixed_window <- spotify_songs |> 
  select(track_album_release_date, duration_min)

# map() applies our function to each row of the input dataframe:

output_fixed_window <- map_dbl(
  seq(nrow(input_fixed_window)), moving_average_fixed_window, .progress = TRUE
)

# plot the results --------------------------------------------------------

p1 <- p1 +
  geom_line(aes(y = output_fixed_window, col = "Moving average, fixed window"))

# moving average with an adaptive window ----------------------------------

# the fixed window moving average undersmooths when there aren't many data points.
# but if we increase the window size we end up oversmoothing when there are lots of data - 
# try it and see. 

# we'll get better results with an adaptive window, that varies in width according to how much 
# data we have:

# let's keep 10% of the data the data in our window at all times. we'll 
# specify this using indices - that is, we'll take an average of the n data points 
# going back and forward in time

window <- round(0.1 * nrow(spotify_songs)) # n = 10% of the data
window <- seq(window) # create an index from 1 to n
window <- window - round(median(window)) # center our window at 0, so we go back n/2 and forward n/2 to calculate our average

# this time, we'll need to put our data in time-order, since we're using positions, not dates, to subset:

input_adaptive_window <- spotify_songs |> 
  arrange(track_album_release_date) |> 
  select(track_album_release_date, duration_min)

# our function is similar to moving_average_fixed_window()

moving_average_adaptive_window <- function(x) {
  keep_these_rows <- x + window
  # make sure keep_these_rows is positive (we can't include points before the first point) and at most 
  # equal to the number of rows in the input (the moving average estimate for the last point will only 
  # include time points in the past)
  keep_these_rows <- keep_these_rows[keep_these_rows > 0 & keep_these_rows <= nrow(input_adaptive_window)]
  input_adaptive_window |> 
    slice(keep_these_rows) |> 
    with(mean(duration_min))
}

# again, map() applies our function to each row of the input dataframe:

output_adaptive_window <- map_dbl(
  seq(nrow(input_adaptive_window)), moving_average_adaptive_window, .progress = TRUE
)

# add the output to the input:

input_adaptive_window <- input_adaptive_window |> 
  mutate(output_adaptive_window)

# plot the results --------------------------------------------------------

p1 +
  geom_line(
    data = input_adaptive_window,
    aes(track_album_release_date, output_adaptive_window, col = "Moving average, adaptive window")
  ) +
  geom_line(aes(y = fitted(model_2), col = "Linear model")) + 
  labs(
    x = "Track release date", 
    y = "Song length (min.)"
  )
