
# This week we're constructing reference distributions to test the impact of an intervention on a 
# parameter measured over time. Specifically, we're comparing the 10-day average pH after an intervention
# to the distribution of all possible 10-day averages before the intervention. This is conceptually 
# similar to comparing a mean to a theoretical distribution (like a t-test). But since our reference 
# distribution is constructed from our data, it will be a better model than a simple theoretical
# distribution would be.

# setup -------------------------------------------------------------------

library("readr")
library("dplyr")
library("ggplot2")
library("purrr")

# read --------------------------------------------------------------------

# data source: 

# Berthouex, Paul Mac, and Linfield C. Brown. 
# Statistics for Environmental Engineers. 2nd ed. Boca Raton 
# London New York: CRC Press, 2002.

ph_data <- read_csv("data/ph-time-series.csv")

# plot --------------------------------------------------------------------

ph_data %>% 
  ggplot(aes(x = day, y = value, col = period)) +
  geom_line()

# construct a reference distribution --------------------------------------

# we can compare the average pH before and after, the intervention, 
# but this doesn't really tell us much:

average_ph <- ph_data %>% 
  summarize(
    before = mean(value[period == "before"]),
    after = mean(value[period == "after"])
  )

# we'll get more information by comparing the average pH after the intervention with a 
# reference distrubution representing all possible averages representing the same length of time:

n_before <- sum(ph_data$period == "before") # number of measurements before the intervention

# set up an index representing the start of the moving average window:

moving_average_start <- seq(n_before - 9) 

# calculate the 10-day moving average:

moving_average <- map_dbl(moving_average_start, ~ mean(ph_data$value[.x:(.x + 9)]))

# plot the resulting distribution, and compare the new average to it:

tibble(moving_average) %>% 
  ggplot(aes(moving_average)) + 
  geom_histogram() + 
  geom_vline(xintercept = average_ph$after, col = "red")

# what fraction of 10-pt averages is lower than the one calculated from the "after" period?

mean(moving_average < average_ph$after)

# construct a reference distribution, method 2 ----------------------------

# we can also compare the difference between average pH in consecutive 10-day periods. this will
# give us more insight into how short-term fluctuations in pH vary over time:

# set up an index representing the start of the moving difference window:

moving_difference_start <- seq(n_before - 19)

# calculate the moving difference:

moving_difference <- map_dbl(
  moving_difference_start, 
  ~ mean(ph_data$value[(.x + 10):(.x + 19)]) - mean(ph_data$value[.x:(.x + 9)])
)

# compare the final difference, which includes the post-intervention data, to all other 
# differences of consecutive 10-pt averages:

tibble(moving_difference) %>% 
  ggplot(aes(moving_difference)) + 
  geom_histogram() + 
  geom_vline(xintercept = average_ph$after - mean(ph_data$value[121:130]), col = "red")

mean(moving_difference < average_ph$after - mean(ph_data$value[121:130]))

# this reinforces the conclusion that pH in the post-intervention period would be unusual 
# if the intervention had no effect.
