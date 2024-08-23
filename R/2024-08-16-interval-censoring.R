
# setup -------------------------------------------------------------------

library("tidyverse")
library("scales")

theme_set(theme_bw() + theme(legend.position = "bottom"))

# read the data -----------------------------------------------------------

pb_paired <- read_csv("data/paired-lead-concentrations.csv")

pb_wide <- pb_paired |> 
  pivot_wider(names_from = period, values_from = c(pb_ppb, bdl))

# plot the data -----------------------------------------------------------

pb_wide |> 
  ggplot(aes(x = pb_ppb_before, y = pb_ppb_after)) + 
  scale_color_manual(values = c("grey", "black")) +
  scale_fill_manual(values = "grey") +
  geom_abline(aes(slope = 1, intercept = 0, col = "y=x")) +
  geom_point(data = function(x) filter(x, !bdl_before & !bdl_after)) + 
  geom_segment(
    data = \(x) filter(x, bdl_before & !bdl_after),
    aes(xend = 0, yend = pb_ppb_after, col = "half-censored pair")
  ) +
  geom_segment(
    data = \(x) filter(x, !bdl_before & bdl_after),
    aes(yend = 0, xend = pb_ppb_before, col = "half-censored pair")
  ) +
  geom_rect(
    data = \(x) filter(x, bdl_before & bdl_after),
    aes(xmin = 0, xmax = pb_ppb_before, ymin = 0, ymax = pb_ppb_after, fill = "fully censored pair"),
    alpha = 0.3
  ) +
  labs(
    x = "[Pb] (ppb) - before",
    y = "[Pb] (ppb) - after",
    col = NULL, fill = NULL
  )
  
# ... to be continued 