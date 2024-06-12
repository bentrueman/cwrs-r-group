
# This week we're looking at Newark, New Jersey's Lead and Copper Rule compliance data. The regulation
# for lead in drinking water in the US is based on the 90th percentile lead concentration representing 
# drinking water collected from a sample of homes---in this case, 60. In the first half of 2019, Newark 
# reported a 90th percentile concentration exceeding the regulatory limit of 15 ppb. Later that year,
# they reported a much lower value that was in compliance. We'd like to know whether that decrease was 
# due to sampling variability or a real change in the propensity of Newark's drinking water to carry lead.
# But while there is a well-known, easy-to-implement test for the difference in means between two groups 
# (the two sample t-test), there is nothing analogous for the 90th percentile. We're going to make the comparison 
# using two general purpose tools: bootstrapping and permutation tests.

# setup -------------------------------------------------------------------

library("tidyverse")
theme_set(theme_bw())

# read --------------------------------------------------------------------

lcrdata <- read_csv("data/newark-lcr-data-cleaned.csv")

# plot --------------------------------------------------------------------

lcrdata %>% 
  ggplot(aes(as.character(period_start), pb_mg_l)) + 
  geom_boxplot() + 
  labs(x = "reporting period")

# calculate 90th percentile -----------------------------------------------

# we want the difference between the 90th percentiles in the two reporting periods:

delta_p90 <- lcrdata %>% 
  group_by(period_start) %>% 
  arrange(pb_mg_l) %>% 
  summarize(percentile_90 = pb_mg_l[54]) %>% 
  summarize(delta_percentile_90 = diff(percentile_90))

# bootstrap 90th percentiles ----------------------------------------------

# first, we'll estimate a 95% confidence interval on the difference in 90th percentiles across the two 
# reporting periods. we'll be calculating the difference in the 90th percentiles between the two reporting 
# periods repeatedly, each time resampling the data with replacement to create different versions. then,
# we'll use the distribution of difference estimates to create our interval.

bootstrap_delta_p90 <- replicate(1e4, {
  ids <- sample(seq(60), 60, replace = TRUE)
  before <- lcrdata$pb_mg_l[lcrdata$period_start == "2019-01-01"][ids]
  ids <- sample(seq(60), 60, replace = TRUE)
  after <- lcrdata$pb_mg_l[lcrdata$period_start == "2019-07-01"][ids]
  p90_before <- sort(before)[54]
  p90_after <- sort(after)[54]
  p90_after - p90_before
})

mean(bootstrap_delta_p90) # mean of the 1e4 bootstrapped replicates
median(bootstrap_delta_p90)
quantile(bootstrap_delta_p90, c(.025, .975)) # a 95% confidence interval calculated using the replicates

# plot the distribution of the replicates:

tibble(bootstrap_delta_p90) %>% 
  ggplot(aes(bootstrap_delta_p90)) + 
  geom_histogram()

# permutation test --------------------------------------------------------

# now we need to estimate the probability of seeing a difference at least as big as the one we did,
# under the assumption that there is no difference between the reporting periods (that's what a two sample  
# t-test does for the difference in means). First, we'll reshuffle the lead measurements between the 
# reporting periods so that high and low values are equally likely in each. Then' we'll calculate the
# difference in the 90th percentiles under this null hypothesis. Do this 1e4 times and we have a null 
# distribution.

lcrdata_permuted <- lcrdata

permuted_null_distribution <- replicate(1e4, {
  ids <- sample(seq(120), 120, replace = FALSE)
  lcrdata_permuted$pb_mg_l <- lcrdata_permuted$pb_mg_l[ids]
  before <- lcrdata_permuted$pb_mg_l[lcrdata_permuted$period_start == "2019-01-01"]
  after <- lcrdata_permuted$pb_mg_l[lcrdata_permuted$period_start == "2019-07-01"]
  p90_before <- sort(before)[54]
  p90_after <- sort(after)[54]
  p90_after - p90_before
})

# finally, we calculate the proportion of the null distribution that exceeds the observed difference in 
# absolute value---that's our p-value. The upshot: it would be very unlikely to see the difference we did if there hadn't been
# a meaningful change in the propensity of Newark's drinking water to carry lead (assuming of course, that data from each period
# are a random sample).

mean(abs(permuted_null_distribution) >= abs(delta_p90$delta_percentile_90))

tibble(permuted_null_distribution) %>% 
  mutate(null_exceeded = abs(permuted_null_distribution) >= abs(delta_p90$delta_percentile_90)) %>% 
  ggplot(aes(permuted_null_distribution, fill = null_exceeded)) +
  geom_histogram()

# t-test analogue ---------------------------------------------------------

# for differences in means, we have the t-test:

t.test(pb_mg_l ~ period_start, data = lcrdata)
