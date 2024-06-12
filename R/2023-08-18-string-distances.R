
# this script explores the "stringdist" package, in this case for matching 
# sample IDs with typos to their true identities

# setup -------------------------------------------------------------------

library("tidyverse")
library("stringdist")

theme_set(theme_bw())

# simulate ----------------------------------------------------------------

# simulate IDs of the form abc...-123

n <- 50 # possible sample IDs
len_str <- 3 # length of letter string in an ID (the "word")

# choose a random start for the number component of the ID
start <- sample(100:(1e3 - n), size = 1)

sample_ids <- sample(letters, size = n * len_str, replace = TRUE) %>% 
  matrix(n, len_str) %>% 
  apply(MARGIN = 1, \(x) paste(x, collapse = "")) %>% 
  paste(start:(start + n - 1), ., sep = "-") %>% 
  paste("BFT", ., sep = "-") # add human-readable prefix

p <- 2 # number of typos

# positions of typos:
positions <- sample(c(1:3, 5:(5 + len_str - 1)), size = p)

# choose an ID to corrupt:
error <- str_split(sample_ids[1], pattern = "")[[1]]

# add typos:
error[positions] <- "%"

error <- paste(error, collapse = "")

# calculate the distance between the corrupted ID and the set of candidates:
distance <- stringdist(error, sample_ids, method = "dl")

# if the ID is an appropriate length, the smallest string distance should 
# correspond to the correct match:

which.min(distance) == 1 # correct match?

sum(distance == min(distance)) == 1 # unique match?

# turn into a function ----------------------------------------------------

n <- 50 # possible sample IDs
len_str <- 5 # number of letters in "word"

# function arguments specify no. of iterations, number typos, and letter typos, 
# respectively:

simulate_typos <- function(iter, p_n, p_l) {
  replicate(iter, {
    
    start <- sample(100:(1e3 - n), size = 1) # random start
    
    sample_ids <- sample(letters, size = n * len_str, replace = TRUE) %>% 
      matrix(n, len_str) %>% 
      apply(MARGIN = 1, \(x) paste(x, collapse = "")) %>% 
      paste(start:(start + n - 1), ., sep = "-")
    
    positions_n <- sample(1:3, size = p_n)
    
    positions_l <- sample(5:(5 + len_str - 1), size = p_l)
    
    error <- str_split(sample_ids[1], pattern = "")[[1]]
    
    error[positions_n] <- sample(0:9, size = 1)
    
    error[positions_l] <- sample(letters, size = 1)
    
    error <- paste(error, collapse = "")
    
    distance <- stringdist(error, sample_ids, method = "dl")
    
    correct_match <- which.min(distance) == 1 # correct match?
    
    unique_match <- sum(distance == min(distance)) == 1 # unique match?
    
    correct_match & unique_match
    
  })
}

simulate_typos(1e2, 1, 1)

# evaluate for a range of typos in the letter and number portion of the ID:

out <- crossing(
  p_n = 0:3,
  p_l = 0:len_str
) %>% 
  mutate(p_sum = p_n + p_l) %>% 
  rowwise() %>% 
  mutate(out = list(simulate_typos(1e2, p_n, p_l))) %>% 
  ungroup() %>% 
  mutate(out_sum = map(out, mean))

# plot error rate as a function of total errors:

out %>% 
  unnest(out_sum) %>% 
  ggplot(aes(p_sum, out_sum)) + 
  geom_line(
    data = . %>% 
      group_by(p_sum) %>% 
      summarize(out_sum = mean(out_sum))
  ) +
  geom_point() + 
  labs(
    x = "Number of errors",
    y = "Probability of a unique correct match"
  )

