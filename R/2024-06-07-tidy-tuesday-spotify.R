
# This week we're attempting an answer to the question, "Did the introduction of Spotify
# encourage shorter song lengths?". We'll be using a few methods to estimate how song length 
# changed over time, before and after the founding of Spotify.

# setup -------------------------------------------------------------------

library("tidyverse")
library("tidytuesdayR")

spotify_founded <- as.Date("2006-04-23") # from https://en.wikipedia.org/wiki/Spotify

theme_set(
  theme_bw() + 
    theme(legend.position = "bottom")
)

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
  facet_wrap(vars(playlist_genre)) +
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
