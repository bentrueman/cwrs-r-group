
# this week, we're covering tidyphreeqc, which is an excellent 
# package for creating PHREEQC input files that are then run by 
# the phreeqc package. I've chosen two examples: calcite solubility 
# in water, and aluminum hydroxide solubility as a function of pH and 
# temperature.

# setup -------------------------------------------------------------------

library("tidyverse")
library("tidyphreeqc")
library("chemr")
library("scales")

theme_set(theme_bw())

# calcite solubility ------------------------------------------------------

phr_input(
  phr_solution(pH = 7, temperature = 25),
  phr_equilibrium_phases(Calcite = 1),
  phr_selected_output(alkalinity = TRUE, molalities = c("HCO3-", "CO3-2"))
) %>% 
  phr_run() %>% 
  phr_print_output()

# Al(OH)3 solubility ------------------------------------------------------

# note the interaction between pH and temperature:
# the effect of each depends on the other

out <- phr_input(
  phr_solution_list(
    pH = seq(4, 10, by = .1),
    temperature = 1:25,
    Al = "1 Gibbsite 0"
  ),
  phr_selected_output(totals = "Al", temperature = TRUE),
  phr_use_db(phreeqc::minteq.v4.dat)
) %>% 
  phr_run() %>% 
  as_tibble() %>% 
  mutate(al_ppb = `Al(mol/kgw)` * mass("Al") * 1e6) %>% 
  rename(temp = `temp(C)`)

# plot the output:

out %>% 
  ggplot(aes(pH, al_ppb, col = temp, group = temp)) + 
  scale_y_log10() +
  geom_line()
