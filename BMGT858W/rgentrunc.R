
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Sampling from Truncated Distributions 
###
### Purpose
###  * Provide a general function to sample from truncated forms of
###    standard R distribution functions, e.g. rnorm() 
###
### Primary Creator(s): Joe Tidwell (jtidwell@umd.edu)
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For plotting
library(ggplot2)
# Sets a 'prettier' default plot format
theme_set(theme_classic())


rgentrunc <- function(dist.name, n, a = -Inf, b = Inf, ...) {
# Draw random samples from truncated distribution function
#
# n          # of samples
# dist.name  base distribution name for R functions, e.g. "norm"
# a          lower bound for truncation
# b          upper bound for truncation
# ...        optional parameters to pass to distribution function

  # Assign CDF of dist.name to F
  # Note: F is a function, not a variable
  # E.g. if dist.name = 'norm', then F() is the same as pnorm()
  F  <- eval(parse(text = paste0("p",dist.name)))

  # Assign Inverse CDF of dist.name to Q
  # Note: Q is a function, not a variable
  Q  <- eval(parse(text = paste0("q",dist.name)))

  # Draw n samples from truncated distribution
  Q(runif(n, min=F(a, ...), max=F(b, ...)), ...)
}


# Example with Gaussian distribution
sample <- rgentrunc("norm", n=1e5, a=1, b=6, mean=5, sd=1)

# Don't have to specify parameter names if you know the order
# E.g.: sample <- rgentrunc("norm",1e5,1,6,5,1)
# Note: This is equivalent to 
#       qnorm(runif(1e5,pnorm(1,5,1),pnorm(6,5,1)))

qplot(x=sample, geom='histogram') +
  labs(x="Truncated Normal(5,1) [1,6]")

# Example with Exponential distribution
sample <- rgentrunc("exp", n=1e5, 1, 20, 1/6)
qplot(x=sample, geom='histogram') +
  labs(x="Truncated Exponential(1/6) [1,20]")

# Example with Beta distribution
sample <- rgentrunc("beta", n=1e5, 0, .7, 2,5)
qplot(x=sample, geom='histogram') +
  labs(x="Truncated Beta(2,5) [0,.7]")

# Example with Gamma distribution
sample <- rgentrunc("gamma", n=1e5, 1, 6, 1,1/2)
qplot(x=sample, geom='histogram') +
  labs(x="Truncated Gamma(1,2) [1,6]")

