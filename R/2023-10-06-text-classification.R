
# This week we're exploring text classification. An obvious use case is 
# filtering spam from email, where the frequencies of words in a large 
# collection of emails (the training data) can be used to classify a new email 
# as "spam" or "not spam". The basic principle is that certain words are much 
# more common in spam emails.

# The use case we're considering today is classify sequences into groups---for 
# example, assigning rRNA sequences to a particular genus. The same principles 
# apply: for any sequence, the "words" or subsequences are predictive of the 
# genus. To make this simpler, we'll simulate data and build a classifier from 
# scratch using tidyverse verbs.

# setup -------------------------------------------------------------------

library("tidyverse")
library("testthat")

# functions ---------------------------------------------------------------

# this function introduces variation in sequences by changing bases at random:

change_bases <- function(x, n_changes, bases) {
  sequence_length <- length(x)
  these_positions <- sample(seq(sequence_length), n_changes)
  these_bases <- sample(bases, n_changes, replace = TRUE)
  x[these_positions] <- these_bases
  x
}

# this functions extracts subsequences:

get_words <- function(x, word_length) {
  regular_expression <- paste0("(?=(.{", word_length, "}))")
  str_match_all(x, regular_expression)[[1]][,2]
}

# tests -------------------------------------------------------------------

test_that("change_bases() changes the expected number of bases", {
  input_length <- 101
  output_changes <- 55
  input <- rep("A", input_length)
  output <- change_bases(input, output_changes, c("C", "G", "T"))
  expect_equal(sum(output %in% input), input_length - output_changes)
  expect_error(change_bases(input, input_length + 1))
})

test_that("get_words() returns the alphabet", {
  input <- paste(LETTERS, collapse = "")
  expect_equal(get_words(input, 1), LETTERS)
})

# parameters --------------------------------------------------------------

bases <- c("A", "C", "G", "T") # the elements of our sequences
sequence_length <- 25
n_genera <- 15
n_replicates <- 5 # replicate sequences from each genus
n_changes <- 5 # the maximum number of bases changed by change_bases()
genera <- LETTERS[seq(n_genera)]
word_length <- 5

# generate sequences ------------------------------------------------------

# first, generate prototype sequences for each genus:

sequences_master <- map(
  seq(n_genera), 
  ~ sample(bases, sequence_length, replace = TRUE)
) %>% 
  set_names(genera)

# then, replicate these prototypes and introduce variation with change_bases():

sequences <- sequences_master %>% 
  rep(each = n_replicates) %>% 
  map(~ change_bases(.x, n_changes, bases)) %>% 
  map(~ paste(.x, collapse = "")) %>% 
  unlist()

# extract vocabulary ------------------------------------------------------

# we'll use the frequency of each "word", or sub-sequence, in sequences from 
# each genus to classify new sequences.

# first we need to extract the words from each sequence; 
# all sub-sequences of length n:

str_match_all(sequences[1], "(?=(.{5}))")[[1]][,2]

# turn this into a function and apply it to all of the sequences:

words <- map(sequences, ~ get_words(.x, word_length)) %>% 
  unlist() %>% 
  unique()

# train model -------------------------------------------------------------

words_detected <- map(sequences, \(s) map_lgl(words, \(w) str_detect(s, w))) 

word_probabilities <- words_detected %>% 
  map(~ set_names(.x, words)) %>% 
  bind_rows() %>% 
  mutate(genus = names(words_detected), .before = 0) %>% 
  pivot_longer(-genus, names_to = "word") %>% 
  group_by(genus, word) %>% 
  # calculate the smoothed frequency of matches for each genus and word:
  summarize(prob = (sum(value) + 0.5) / (n() + 1)) %>% 
  ungroup()

# plot --------------------------------------------------------------------

word_probabilities %>% 
  mutate(word = factor(word, levels = words)) %>% 
  ggplot(aes(genus, word, fill = prob)) + 
  geom_tile() + 
  scale_fill_viridis_c() + 
  scale_y_discrete(guide = guide_axis(n.dodge = 9))

# predictions -------------------------------------------------------------

# make a new set of sequences from our master list:

sequences_new <- sequences_master %>% 
  map(~ change_bases(.x, n_changes, bases)) %>% 
  map_chr(~ paste(.x, collapse = "")) %>% 
  tibble(genus = names(.), sequence = .)

# check that we haven't seen them before:
all(!sequences %in% sequences_new$sequence)

# extract the words in each one:
words_new <- sequences_new$sequence %>% 
  map(~ get_words(.x, word_length))

# for each new sequence, look up the probability that each of its words
# match each genus. then for each genus, take the product of 
# those probabilities. the genus yielding the highest score is the predicted 
# genus for the new sequence.

words_new %>%
  map( ~ filter(word_probabilities, word %in% .x)) %>% 
  list_rbind(names_to = "genus_true") %>% 
  group_by(genus_true, genus_predicted = genus) %>% 
  summarize(score = sum(log(prob))) %>% 
  slice_max(score) %>% 
  ungroup() %>% 
  mutate(correct = genus_true == genus_predicted)
