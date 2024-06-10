
# 2024-05-10

# We're starting off the summer term with a Tidy Tuesday dataset. This is an online event that takes
# place every Tuesday where participants use a common dataset to practice their data communication skills
# in R---and share their results with others.

# Our data this week concerns the birthdays of Canadian NHL players vs. the general population. More 
# details are available in the README (see below).

# setup -------------------------------------------------------------------

library("tidyverse") # for plotting and data wrangling functions
library("tidytuesdayR") # use this package to download the data

# read the data -----------------------------------------------------------

week_2 <- tt_load("2024", week = 2)

readme(week_2)

# data summaries ----------------------------------------------------------

# tally births by month:

nhl_player_births_counted <- week_2$nhl_player_births |> 
  filter(birth_country == "CAN") |> 
  group_by(month = birth_month) |> 
  count() |> 
  ungroup()

# do the opposite (return a row for each canadian birth in the dataset):

canada_births_1991_2022_uncounted <- week_2$canada_births_1991_2022 |> 
  uncount(births)

# basic plots of the data -------------------------------------------------

# distribution of births over the year (Canadian NHL players)

nhl_player_births_counted |> 
  ggplot(aes(x = factor(month), y = n)) + 
  geom_col()

# distribution of births over the year (Canadians)

week_2$canada_births_1991_2022 |> 
  group_by(month) |> 
  summarize(n = sum(births)) |> 
  ggplot(aes(x = factor(month), y = n)) + 
  geom_col()

# develop a test statistic ------------------------------------------------

# use the ratio of players born in the first quarter to players born in the last fourth quarter
# as a measure of asymmetry in the birth distribution:

nhl_stats <- nhl_player_births_counted |> 
  summarize(
    test_statistic = sum(n[month %in% 1:3]) / sum(n[month %in% 10:12]),
    total_players = sum(n)
  )

# calculate the same test statistic for 1000 random samples of Canadian babies:

null_distribution <- replicate(1e3, {
  canada_births_1991_2022_uncounted |> 
    slice_sample(n = nhl_stats$total_players) |> 
    group_by(month) |> 
    count() |> 
    ungroup() |> 
    summarize(test_statistic = sum(n[month %in% 1:3]) / sum(n[month %in% 10:12])) |> 
    pull(test_statistic)
})

# Compare the ratio for NHL players to a sample of 1000 possible ratios for Canadian babies. 
# If NHL players are just as likely to be born at the beginning as the end of the year, we might expect
# their birth months to be a random sample from the Canadian population. Under that assumption---the null
# hypothesis---how unusual is the ratio we actually observed?

tibble(null_distribution) |> 
  ggplot(aes(null_distribution)) +
  geom_histogram() + 
  geom_vline(aes(xintercept = nhl_stats$test_statistic, col = "NHL players"))

# Very unusual.

# compute the test statistic by decade ------------------------------------

# let's see how this test statistic evolves over time.

# first, tally births by month within each decade:

nhl_player_births_counted_by_decade <- week_2$nhl_player_births |> 
  filter(birth_country == "CAN") |> 
  group_by(decade = birth_year - birth_year %% 10, month = birth_month) |> 
  count() |> 
  ungroup()

# then calculate the same test statistic by decade:

nhl_stats_by_decade <- nhl_player_births_counted_by_decade |>
  group_by(decade) |> 
  summarize(
    test_statistic = sum(n[month %in% 1:3]) / sum(n[month %in% 10:12]),
    total_players = sum(n)
  ) |> 
  filter(total_players > 1)

# we'll need to re-calculate the null distribution from above by decade too. that means
# changing the sample size from total Canadian NHL players to Canadian NHL players per decade.
# the following function takes one argument, "sample_size", that accomplishes this.

calculate_null_distribution <- function(sample_size) {
  replicate(1e3, {
    canada_births_1991_2022_uncounted |> 
      slice_sample(n = sample_size) |> 
      group_by(month) |> 
      count() |> 
      ungroup() |> 
      summarize(test_statistic = sum(n[month %in% 1:3]) / sum(n[month %in% 10:12])) |> 
      pull(test_statistic)
  }) 
}

# now we can calculate the null distribution for the number of players in each decade:

nhl_stats_by_decade <- nhl_stats_by_decade |> 
  group_by(total_players) |> 
  nest() |> 
  ungroup() |> 
  mutate(
    null_distribution = map(total_players, calculate_null_distribution),
    # summarize the null distribution with its 2.5th and 97.5th percentiles 
    # (i.e, the middle 95% of the null distribution)
    ymin = map(null_distribution, ~ quantile(.x, .025)),
    ymax = map(null_distribution, ~ quantile(.x, .975))
  ) |> 
  unnest(c(data, ymin, ymax))

# plot the results --------------------------------------------------------

# plot:

nhl_stats_by_decade |> 
  # we subtract 1 from the test statistic so that columns representing statistics less than one
  # (i.e., more births in the last quarter of the year) point down rather than up:
  ggplot(aes(decade, test_statistic - 1, fill = total_players)) + 
  geom_col() + 
  geom_ribbon(
    aes(
      x = decade, ymin = ymin - 1, ymax = ymax - 1, 
      col = "Expected variation based on 1000 same-sized\nsamples from the Canadian population"
    ), 
    fill = "grey", alpha = 0.4, inherit.aes = FALSE,
    linewidth = 0
  ) +
  annotate(
    "label", x = 1855, y = 0, label = "No difference", 
    hjust = 0, size = 3, label.size = 0
  ) +
  annotate(
    "label", x = 1855, y = 1, label = "Twice as many", 
    hjust = 0, size = 3, label.size = 0
  ) +
  annotate(
    "label", x = 1855, y = 2, label = "Three times\nas many", 
    hjust = 0, size = 3, label.size = 0
  ) +
  annotate(
    "label", x = 1855, y = 3, label = "Four times\nas many", 
    hjust = 0, size = 3, label.size = 0
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(limits = c(1855, 2010), breaks = c(1880, 1920, 1960, 2000)) +
  labs(
    x = NULL, y = NULL, fill = "Canadian NHL\nplayers per decade", col = NULL,
    title = "How many more Canadian NHL players are born in the first quarter\nof the year (compared to the last quarter)?"
  )
