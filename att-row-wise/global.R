library(shiny); library(bslib)
library(ggplot2)
library(keras3)
use_backend("jax")
library(reticulate)

source_python(
  "https://raw.githubusercontent.com/darentsai/keras/refs/heads/master/keras/src/layers/attention/multi_head_attention.py",
  envir = NULL
)
layer_multi_head_attention_rbf <- py$MultiHeadAttention

risk_score <- function(x1, x2, w1, w2, bias) {
  x1 * w1 + x2 * w2 + bias
}

ring_gauss <- function(x, y, center = c(0, 0), r1 = 0, r2 = 1, sigma = 1) {
  stopifnot(r2 >= r1, sigma > 0)
  r <- sqrt((x - center[1])^2 + (y - center[2])^2)
  R <- ifelse(r < r1, r1, ifelse(r > r2, r2, r))
  exp(-(r - R)^2 / sigma)
}

mvdnorm <- function(x, mean = 0, sd = 1, angle = 0, scale = FALSE) {
  stopifnot(ncol(x) == 2L)
  fun <- function(x, mean, sd, angle) {
    ang <- angle * pi / 180
    rot <- matrix(c(cos(ang), sin(ang), -sin(ang), cos(ang)), 2)
    x2 <- as.matrix(x) %*% rot
    Map(\(x, m, s) dnorm(x, mean = m, sd = s),
        asplit(x2, MARGIN = 2), mean, sd) |>
      Reduce(f = '*')
  }
  res <- c(fun(x, mean, sd, angle))
  if(scale) res <- res / c(fun(t(mean), mean, sd, angle))
  return(res)
}
