
# this week, we're touching on splines. the basic idea is that 
# we can build a matrix of synthetic predictors to do curve fitting
# in the context of a linear model. as we'll see, it's relatively straightforward to 
# penalize overfitting, so that we don't have to worry much about how many synthetic 
# predictors we're adding. the code here owes a lot to Michael Clark 
# (https://m-clark.github.io/models-by-example/cubic-spline.html)

# setup -------------------------------------------------------------------

library("tidyverse")
library("mgcv")

theme_set(theme_bw())

# data --------------------------------------------------------------------

# our dataset represents acceleration of the head over 
# time in a simulated motorcycle crash:

data(mcycle, package = "MASS")

mcycle <- as_tibble(mcycle)

mcycle

# plot --------------------------------------------------------------------

# our goal is to fit a smooth curve to the data

p1 <- mcycle %>% 
  ggplot(aes(times, accel)) + 
  geom_point()

p1

# linear fit is inadequate:

p1 + geom_smooth(method = "lm") 

# cubic regression spline is much better---but how does it work?

p1 + geom_smooth(method = "gam", se = FALSE) 

# polynomial regression ---------------------------------------------------

# add synthetic predictors to the linear model. each one is a function of x = times:

# f(x) = x ^ i, where i is the ith synthetic predictor

# quadratic regression:

m1 <- mcycle %>% 
  mutate(times2 = times ^ 2) %>% 
  lm(accel ~ times + times2, data = .)

p1 + geom_line(aes(y = fitted(m1)))

# plot the basis, or the full set of predictors:

plot_basis <- function(m) {
  m %>% 
    model.matrix() %>% 
    bind_cols(x = mcycle$times) %>% 
    pivot_longer(-x) %>% 
    ggplot(aes(x, value)) + 
    facet_wrap(vars(name), scales = "free_y") + 
    geom_line()
}

plot_basis(m1)

# higher degree polynomial fits:

m2 <- lm(accel ~ poly(times, degree = 20, raw = TRUE), data = mcycle)

p1 + geom_line(aes(y = fitted(m2)))

plot_basis(m2)

# but when do we stop adding predictors? we can easily overfit the data, 
# and there is no clear stopping rule.

# cubic splines -----------------------------------------------------------

# penalized splines do a much better job; while we add 
# synthetic predictors in a similar way to polynomial regression, it's 
# relatively straightforward to come up with a clear stopping rule

# first, we need a function to make our synthetic predictors:

source("R/2023-05-12-reproducing-kernel.R")

# the function works on x that has been normalized to span [0,1]

x <- mcycle$times - min(mcycle$times)
x <- x / max(x)
range(x)

# we make one new predictor for each "knot", and the knots 
# define, roughly speaking, the position of the new predictor
# (the curve these predictors will fit is piecewise cubic---made up of sections of 
# different cubic polynomials. The knots represent the boundaries of the 
# sections, and the cubic polynomials are continuous at the boundaries up to a 
# 2nd deriivative. See 

# Wood, S.N. (2017) Generalized Additive Models: An Introduction with R (2nd
# edition). Chapman and Hall/CRC.

knots <- 1:8 / 9 # knot locations
n <- length(x) # number of observations
p <- length(knots) + 2 # total number of predictors, including intercept
X <- matrix(1, nrow = n, ncol = p) # initialize matrix of predictors
X[,2] <- x # add "times" predictor
X[,3:p] <- outer(x, knots, FUN = rk) # make the synthetic predictors

# fit the cubic spline

m3 <- lm(mcycle$accel ~ X - 1)

p1 + geom_line(aes(y = fitted(m3)))

plot_basis(m3)

# again, though, it's not obvious how many predictors to add. 
# but it turns out that it's relatively easy to penalize overfitting.
# while the previous model has coefficients that minimize squared error,
# we can discourage overfitting by instead minimizing squared error PLUS the 
# (integrated) sum of the squared second derivative of our curve

# set up a matrix that defines the squared second derivative penalty 
# (see Wood 2017 for details):

S <- matrix(0, p, p)
S[3:p, 3:p] <- outer(knots, knots, FUN = rk)

# the linear model coefficients that minimize squared error are found by:
# (although lm() uses a different method that is more numerically stable)

beta <- solve(t(X) %*% X) %*% t(X) %*% mcycle$accel
all(near(beta[,1], coef(m3), tol = 1e-4))

# add in the penalty matrix S and a multiplier lambda to get the penalized version:

lam <- 1e-4 # lambda, the multiplier on the penalty matrix

beta <- solve(t(X) %*% X + lam * S) %*% t(X) %*% mcycle$accel # penalized linear model

p1 + geom_line(aes(y = X %*% beta))

# then, all we need to do is choose an appropriate value for lambda (there are a number of
# methods to do this). we don't have to worry about having too many additional predictors, 
# since overfitting is penalized.

# fitting splines using mgcv ----------------------------------------------

m4 <- gam(accel ~ s(times, bs = "cr", k = 10), data = mcycle)

p1 + geom_line(aes(y = fitted(m4)))

plot_basis(m4) # new predictors look slightly different, but the idea is the same

# this is how the basis functions (new predictors) come together to 
# make the fitted curve:

model.matrix(m4) %*% diag(coef(m4)) %>% 
  bind_cols(x = mcycle$times) %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(x, rowid)) %>% 
  ggplot(aes(x, value, col = name)) + 
  # plot the predictors weighted by the coefficients:
  geom_line() + 
  # add the data:
  geom_point(
    data = mcycle,
    aes(times, accel),
    col = "black"
  ) + 
  # to get the fitted curve,
  # add up the component curves, 
  # weighted by the model coefficients:
  geom_line(
    data = . %>% 
      group_by(rowid, x) %>% 
      summarize(value = sum(value)) %>% 
      ungroup(),
    aes(x, value),
    col = "red", 
    linewidth = 2
  )

