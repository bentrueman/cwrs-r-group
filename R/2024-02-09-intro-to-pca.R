
# This week, we're covering principal components analysis, which is a technique used to
# summarize a multivariate dataset using a smaller set of new variables---the principal components---each 
# of which is a linear combination of the original variables. The first principal component should capture as much 
# of the information (variance) in the dataset as possible, and each subsequent principal component should be
# orthogonal to all the others and capture as much of the residual variance as possible. We'll start with the 
# simplest example: summarizing a bivariate dataset using one principal component.

# setup -------------------------------------------------------------------

library("tidyverse")

# read --------------------------------------------------------------------

data <- read_csv("data/pca-example.csv")

# plot --------------------------------------------------------------------

p1 <- data %>% 
  ggplot(aes(x, y)) + 
  geom_point(aes(col = "original"))

p1

# we want to rotate the data so that, after rotation, as much variance as possible is captured by the 
# x coordinate. We'll call that the first principal component.

# how to rotate a point (i.e., vector) about the origin -------------------

# we want to rotate a single point (1.5, 1.5) by an angle theta 

theta <- pi / 4 

p2 <- tibble(x = 1.5, y = 1.5) %>% 
  ggplot(aes(x, y)) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  geom_point(aes(col = "original")) + 
  coord_fixed(xlim = c(-2, 2), ylim = c(-2, 2))

p2

# we can rotate the x and y coordinates separately and then add them together:

tibble(x = 1.5, y = 0) %>% 
  mutate(
    x_rotated = x * cos(theta), # the x coordinate of the rotated point (1.5, 0)
    y_rotated = .$x * sin(theta) # the y coordinate of the rotated point (1.5, 0)
  ) %>% 
  ggplot(aes(x, y)) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  geom_point(aes(col = "original")) + 
  geom_point(aes(x_rotated, y_rotated, col = "rotated")) +
  # draw the right triangle the rotated point makes with the origin:
  geom_segment(
    data = . %>% 
      with(tibble(x = c(0, x_rotated), y = 0, xend = x_rotated, yend = y_rotated)),
    aes(x, y, xend = xend, yend = yend, col = "rotated"),
    linetype = 3, show.legend = FALSE
  ) +
  coord_fixed(xlim = c(-2, 2), ylim = c(-2, 2))

tibble(x = 0, y = 1.5) %>% 
  mutate(
    x_rotated = -y * sin(theta), # the x coordinate of the rotated point (0, 1.5)
    y_rotated = y * cos(theta) # the y coordinate of the rotated point (0, 1.5)
  ) %>%  
  ggplot(aes(x, y)) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  geom_point(aes(col = "original")) + 
  geom_point(aes(x_rotated, y_rotated, col = "rotated")) +
  # draw the right triangle the rotated point makes with the origin:
  geom_segment(
    data = . %>% 
      with(tibble(x = 0, y = c(0, y_rotated), xend = x_rotated, yend = y_rotated)),
    aes(x, y, xend = xend, yend = yend, col = "rotated"),
    linetype = 3, show.legend = FALSE
  ) +
  coord_fixed(xlim = c(-2, 2), ylim = c(-2, 2))

# add the two rotated coordinates together to get the coordinates of the original rotated point (1.5, 1.5):

p2 +
  geom_point(
    data = . %>%
      mutate(
        x = x * cos(theta) - y * sin(theta),
        y = .$x * sin(theta) + y * cos(theta)
      ),
    aes(col = "rotated")
  ) + 
  # use Pythagoras to check that it's in the right spot:
  geom_hline(yintercept = sqrt(2 * 1.5 ^ 2), linetype = 3)

# rotate our data ---------------------------------------------------------

# we can use the same algorithm to rotate all of our data by theta:

p1 +
  geom_point(
    data = . %>%
      mutate(
        x = x * cos(theta) - y * sin(theta),
        y = .$x * sin(theta) + y * cos(theta)
      ),
    aes(col = "rotated")
  )

# find the optimum theta --------------------------------------------------

# remember, we're looking for a synthetic variable to summarize our dataset, and we want it to 
# capture the most variance possible. Principal components analysis finds the angle of rotation
# that maximizes the variance of the x coordinate after rotation. since we have a formula for the
# x coordinate given theta, that's easy to do:

theta_max <- tibble(theta = seq(0, -pi/2, length.out = 1e4)) %>% # start with 1e4 possible thetas
  rowwise() %>% 
  # use the formula for the x coordinate after rotation to find and the variance for each theta:
  mutate(variance_x = with(data, var(x * cos(theta) - y * sin(theta)))) %>% 
  ungroup() %>% 
  # then choose theta that yields the biggest variance:
  slice_max(variance_x)

# try going back and rotating the data by this angle:

theta <- theta_max$theta

# using prcomp() ----------------------------------------------------------

# let's compare this with the prcomp() function:

pca <- prcomp(data)

# the linear combination of x and y that gives us the first principal component is the same, up to a sign,
# as the linear combination that gave us our optimal rotation:
pca$rotation[, "PC1"]
cos(theta)
-sin(theta)

# the first principal component captures this fraction of the total variance in the data:

pca$sdev[1] ^ 2 / sum(pca$sdev ^ 2)

# which is the same as our calculation:

theta_max$variance_x / with(data, var(x) + var(y))

# an example using the iris dataset ---------------------------------------

# the iris dataset includes four variables measured across samples from three species of iris:

head(iris)

# let's do the principal components analysis. here, we scale the data so that all the variables have the same 
# variance:

pca_iris <- prcomp(iris[,-5], scale = TRUE)

# here are the linear combinations of the original variables that make up the principal components:

pca_iris$rotation

# the first two principal components account for this fraction of the total variance in the data:

sum(pca_iris$sdev[1:2] ^ 2) / sum(pca_iris$sdev ^ 2)

# here are the data projected onto the first two principal components:

pca_iris$x %>% 
  as_tibble() %>% 
  mutate(species = iris$Species) %>% 
  ggplot(aes(PC1, PC2, col = species)) + 
  geom_point()

# setosa is particularly negative on the first principal component, PC1. Since PC1's only negative coefficient 
# is Sepal.Width, that means it has unusually wide sepals.

# PC2 contrasts petals and sepals: flowers with the biggest sepals have the most negative PC2 values
