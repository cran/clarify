## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  fig.width = 6.5,
  fig.height = 2.75
)

## -----------------------------------------------------------------------------
## library("Zelig")
library("clarify")
set.seed(100)

## -----------------------------------------------------------------------------
data("lalonde", package = "MatchIt")

## ---- eval = FALSE------------------------------------------------------------
#  fit <- zelig(re78 ~ treat + age + educ + married + race +
#                 nodegree + re74 + re75, data = lalonde,
#               model = "ls", cite = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  fit <- setx(fit, treat = 0)
#  fit <- setx1(fit, treat = 1)

## ---- eval = FALSE------------------------------------------------------------
#  fit <- Zelig::sim(fit)

## ---- eval = FALSE------------------------------------------------------------
#  fit

## ---- eval = F----------------------------------------------------------------
#  plot(fit)

## -----------------------------------------------------------------------------
fit <- lm(re78 ~ treat + age + educ + married + race +
            nodegree + re74 + re75, data = lalonde)

## -----------------------------------------------------------------------------
s <- clarify::sim(fit)

## -----------------------------------------------------------------------------
est <- sim_setx(s, x = list(treat = 0), x1 = list(treat = 1),
                verbose = FALSE)

## -----------------------------------------------------------------------------
summary(est)

plot(est)

## -----------------------------------------------------------------------------
data("lalonde", package = "MatchIt")

#Rare outcome: 1978 earnings over $20k; ~6% prevalence
lalonde$re78_20k <- lalonde$re78 >= 20000

## ---- eval = FALSE------------------------------------------------------------
#  fit <- zelig(re78_20k ~ treat + age + educ + married + race +
#                 nodegree + re74 + re75, data = lalonde,
#               model = "relogit", cite = FALSE)
#  
#  fit

## ---- eval = FALSE------------------------------------------------------------
#  fit <- setx(fit, treat = 0)
#  fit <- setx1(fit, treat = 1)
#  
#  fit <- Zelig::sim(fit)
#  
#  fit

## ---- eval = FALSE------------------------------------------------------------
#  plot(fit)

## -----------------------------------------------------------------------------
fit <- logistf::logistf(re78_20k ~ treat + age + educ + married + race +
                          nodegree + re74 + re75, data = lalonde,
                        flic = TRUE)

summary(fit)

## -----------------------------------------------------------------------------
s <- clarify::sim(fit)

est <- sim_setx(s, x = list(treat = 0), x1 = list(treat = 1),
                verbose = FALSE)

summary(est)

## -----------------------------------------------------------------------------
plot(est)

## -----------------------------------------------------------------------------
data("lalonde", package = "MatchIt")

m.out <- MatchIt::matchit(treat ~ age + educ + married + race +
                            nodegree + re74 + re75, data = lalonde,
                          method = "nearest")

## ---- eval = FALSE------------------------------------------------------------
#  fit <- zelig(re78 ~ treat * (age + educ + married + race +
#                                 nodegree + re74 + re75),
#               data = m.out, model = "ls", cite = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  fit <- ATT(fit, "treat")

## ---- eval = F----------------------------------------------------------------
#  fit

## ---- eval = F----------------------------------------------------------------
#  plot(fit)

## -----------------------------------------------------------------------------
m.data <- MatchIt::match.data(m.out)

fit <- lm(re78 ~ treat * (age + educ + married + race +
                            nodegree + re74 + re75),
          data = m.data)

## -----------------------------------------------------------------------------
s <- clarify::sim(fit, vcov = ~subclass)

## -----------------------------------------------------------------------------
est <- sim_ame(s, var = "treat", subset = treat == 1,
               contrast = "diff", verbose = FALSE)

## -----------------------------------------------------------------------------
summary(est)

plot(est)

## ---- message=F---------------------------------------------------------------
library(Amelia)
data("africa", package = "Amelia")

## -----------------------------------------------------------------------------
# Multiple imputation
a.out <- amelia(x = africa, m = 10, cs = "country",
                ts = "year", logs = "gdp_pc", p2s = 0)

## ---- eval = FALSE------------------------------------------------------------
#  fit <- zelig(gdp_pc ~ infl * trade, data = a.out,
#               model = "ls", cite = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  summary(fit)

## ---- eval = FALSE------------------------------------------------------------
#  fit <- setx(fit, infl = 0, trade = 40)
#  fit <- setx1(fit, infl = 0, trade = 60)
#  
#  fit <- Zelig::sim(fit)

## ---- eval = F----------------------------------------------------------------
#  fit

## ---- eval = F----------------------------------------------------------------
#  plot(fit)

## -----------------------------------------------------------------------------
#Use Amelia functions to model and combine coefficients
fits <- with(a.out, lm(gdp_pc ~ infl * trade))

mi.combine(fits)

## -----------------------------------------------------------------------------
#Simulate coefficients, 100 in each of 10 imputations
s <- misim(fits, n = 100)

#Compute predictions at specified values
est <- sim_setx(s, x = list(infl = 0, trade = 40),
                x1 = list(infl = 0, trade = 60),
                verbose = FALSE)

summary(est)

plot(est)

