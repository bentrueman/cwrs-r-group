
# This week we're tackling another Tidy Tuesday dataset: Bechdel test scores for films over time. We'll use it to track the 
# proportion of films passing the test by decade, and we'll generate a distribution of plausible proportions to accompany
# each point estimate.

# setup -------------------------------------------------------------------

library("tidyverse")
library("tidytuesdayR") # to download the data
library("ggdist") # to visualize the distributions of proportions

theme_set(theme_bw())

# read data ---------------------------------------------------------------

film_data <- tt_load(2021, week = 11)

readme(film_data) # read about the Bechdel test

# clean data --------------------------------------------------------------

# tally the films passing the Bechdel test (rating == 3) and the total films by decade:

passes_bechdel <- film_data$raw_bechdel |> 
  group_by(decade = year - year %% 10) |> 
  summarize(
    passes_test = sum(rating == 3), 
    n_films = n()
  ) |> 
  mutate(proportion_passed = passes_test / n_films)

# binomial distribution ---------------------------------------------------

# We'll assume that the films in the dataset for each decade represent a random sample of the films made in that decade.
# That way, the proportion of films passing the Bechdel test can be modeled as a binomial distribution. The binomial is a
# natural choice for anything that can be analogized to a coin toss with probability of heads p. Here, passing the Bechdel 
# is like coming up heads.

# The code below simulates 10 experiments, each with 13 coin flips and a 7.7% probability of heads. 
# The integer output is the number of heads in each experiment.

rbinom(n = 10, size = 13, prob = 0.0769) 

# The code quantifies the probability of getting heads twice in 13 coin flips, when the probability of heads is 9%

dbinom(x = 2, size = 13, prob = .09) 

# binomial model ----------------------------------------------------------

# For each decade, let's evaluate the plausibility each proportion in a long sequence from 0 to 1, using a binomial 
# model to describe the number of Bechdel test passes in N films.

these_proportions <- seq(0, 1, length.out = 1e3) # 1000 possible proportions between 0 and 1

# evaluate each of them by calculating the probability that they generated the data:

binomial_model <- passes_bechdel |> 
  crossing(proportion = these_proportions) |> 
  group_by(decade) |> 
  mutate(
    # probability that a binomial distribution with each proportion in the sequence generated our data:
    compatibility = dbinom(x = passes_test, size = n_films, prob = proportion)
  ) |> 
  nest() |> 
  ungroup() |> 
  # sample from the possible proportions for each decade, weighting by their compatibility with the data:
  mutate(
    proportions_sampled = 
      map(data, ~ slice_sample(.x, n = 1e4, weight_by = compatibility, replace = TRUE))
  ) |> 
  unnest(proportions_sampled)

# plot the results --------------------------------------------------------

binomial_model |> 
  filter(decade > 1920) |> # films before (roughly) 1930 may not be comparable with later films, so we remove them
  mutate(decade = factor(decade)) |> 
  ggplot(aes(proportion, decade, fill = n_films)) + 
  stat_slab(density = "histogram") + 
  geom_point(
    # the dot pronoun below represents the data we passed to ggplot():
    data = . %>% distinct(decade, proportion_passed, n_films),
    aes(x = proportion_passed, col = "Point\nestimate")
  ) +
  labs(
    x = "Proportion of films passing the Bechdel test",
    y = NULL,
    fill = "Number of\nfilms",
    col = NULL,
    title = "Is gender balance in films improving over time?"
  )
