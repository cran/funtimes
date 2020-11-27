## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#")
library(funtimes)
# devtools::load_all(".") #remove this line

## -----------------------------------------------------------------------------
set.seed(777)
n <- 100
Time <- c(1:n)
X0 <- arima.sim(list(order = c(1, 0, 0), ar = 0.5), n = n, n.start = 100, sd = 0.5)
X1 <- 2*Time/n + X0
X2 <- 2*(Time/n)^0.5 + X0
X3 <- 0.5*(Time - n/2)/n - 6*((Time - n/2)/n)^2 + X0
X <- as.data.frame(cbind(X0, X1, X2, X3))

## ---- echo = FALSE, fig.width = 7, dpi = 96-----------------------------------
library(ggplot2)
library(patchwork)
p1 <- ggplot(X, aes(x = Time, y = X1)) + geom_line() + theme_minimal()
p2 <- ggplot(X, aes(x = Time, y = X2)) + geom_line() + theme_minimal()
p3 <- ggplot(X, aes(x = Time, y = X3)) + geom_line() + theme_minimal()

p1 + p2 + p3 + 
  plot_annotation(
    title = 'Time series simulated for different alternative hypotheses',
    tag_levels = 'A'
  )

## ---- echo = FALSE------------------------------------------------------------
ggplot(X, aes(x = Time, y = X0)) + geom_line() + theme_minimal()

## -----------------------------------------------------------------------------
notrend_test(X0)

## -----------------------------------------------------------------------------
apply(X[,-1], 2, function(x) notrend_test(x)$p.value)

## -----------------------------------------------------------------------------
apply(X, 2, function(x) notrend_test(x, test = "MK")$p.value)

## -----------------------------------------------------------------------------
apply(X, 2, function(x) notrend_test(x, test = "WAVK", 
                                     factor.length = "adaptive.selection")$p.value)

## -----------------------------------------------------------------------------
notrend_test(X0, test = "WAVK", factor.length = "adaptive.selection")
wavk_test(X0 ~ 0, factor.length = "adaptive.selection")

## -----------------------------------------------------------------------------
wavk_test(X0 ~ t, factor.length = "adaptive.selection")

## -----------------------------------------------------------------------------
sapply(names(X[,-1]), function(x) wavk_test(eval(parse(text = x)) ~ t)$p.value)

## -----------------------------------------------------------------------------
wavk_test(X3 ~ poly(t, 2), factor.length = "adaptive.selection")

