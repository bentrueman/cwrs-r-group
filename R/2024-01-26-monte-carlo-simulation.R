
# This week, we're doing a very cursory introduction to Monte Carlo simulation, which is used to 
# estimate the uncertainty in predictions from a model. The basic idea is to define
# a probability distribution for each parameter, sample from it repeatedly, and generate model
# predictions for all of the samples. We'll be using the logistic growth model as an example.

# setup -------------------------------------------------------------------

library("tidyverse")
theme_set(theme_bw())

# model parameters --------------------------------------------------------

time_steps <- 100
growth_rate <- 0.1 # 1000s of individuals per unit time
population_initial <- 0.1 # 1000 individuals
carrying_capacity <- 10 # 1000 individuals

# simulate from our model -------------------------------------------------

population_time_series <- accumulate(
  rep(population_initial, time_steps),
  \(population_current, ignore_this_argument) {
    population_current + 
      growth_rate * population_current * (1 - population_current / carrying_capacity)
  }
)

p1 <- tibble(time = seq(time_steps), population = population_time_series) %>% 
  ggplot(aes(time, population)) + 
  geom_line(aes(col = "original prediction")) + 
  labs(col = NULL)
p1

# monte carlo simulation --------------------------------------------------

simulation_size <- 1e3
# define probability distributions for the parameters, then sample from them:
growth_rate_monte_carlo <- rlnorm(simulation_size, log(0.1), 0.25) # 1000s of individuals per unit time
population_initial_monte_carlo <- rlnorm(simulation_size, log(0.1), 0.25) # 1000 individuals
carrying_capacity_monte_carlo <- rnorm(simulation_size, 10, 1) # 1000 individuals

# we'll need a function to calculate the model predictions for each sample:

logistic_growth <- function(growth_rate, population_initial, carrying_capacity) {
  accumulate(
    rep(population_initial, time_steps),
    \(population_current, ignore_this_argument) {
      population_current + 
        growth_rate * population_current * (1 - population_current / carrying_capacity)
    }
  )
}

# predict from the model for each random sample from the parameter space:

population_monte_carlo <- pmap(
  list(growth_rate_monte_carlo, population_initial_monte_carlo, carrying_capacity_monte_carlo),
  \(x, y, z) {
    population <- logistic_growth(x, y, z)
    tibble(time = seq(time_steps), population)
  }
) %>% 
  list_rbind(names_to = "iteration")

# plot all of the growth curves from the simulation

p2 <- p1 + 
  geom_line(
    data = population_monte_carlo,
    aes(group = iteration),
    linewidth = .05
  )
p2$layers <- p2$layers[c(2, 1)]
p2

# calculate a quantile interval to represent uncertainty in our model predictions:

population_monte_carlo_summary <- population_monte_carlo %>% 
  group_by(time) %>% 
  summarize(
    lower = quantile(population, .025),
    upper = quantile(population, .975)
  )

p1 + 
  geom_ribbon(
    data = population_monte_carlo_summary,
    aes(x = time, ymin = lower, ymax = upper),
    fill = "deepskyblue",
    alpha = 0.3,
    inherit.aes = FALSE
  )
