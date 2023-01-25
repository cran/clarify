## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6.5,
  fig.height = 2.75,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(clarify)

## -----------------------------------------------------------------------------
data("lalonde", package = "MatchIt")

lalonde$re78_0 <- ifelse(lalonde$re78 == 0, 1, 0)

head(lalonde)

## -----------------------------------------------------------------------------
fit <- glm(re78_0 ~ treat + age + educ + race + married +
             nodegree + re74 + re75, data = lalonde,
           family = binomial("probit"))

## -----------------------------------------------------------------------------
# Drawing simulated coefficients using an HC2 robust
# covariance matrix
s <- sim(fit, vcov = "HC2")

s

## -----------------------------------------------------------------------------
sim_fun1 <- function(fit) {
  predict(fit, newdata = lalonde["PSID1",], type = "response")
}

## -----------------------------------------------------------------------------
est1 <- sim_apply(s, FUN = sim_fun1, verbose = FALSE)

est1

## -----------------------------------------------------------------------------
sim_fun2 <- function(coefs) {
  hispan <- unname(coefs["racehispan"])
  white <- unname(coefs["racewhite"])
  
  c("w - h" = white - hispan)
}

est2 <- sim_apply(s, FUN = sim_fun2, verbose = FALSE)

est2

## -----------------------------------------------------------------------------
plot(est1)

## -----------------------------------------------------------------------------
summary(est1)

## -----------------------------------------------------------------------------
plot(est2)

summary(est2, method = "wald", null = 0)

## -----------------------------------------------------------------------------
est3 <- sim_setx(s, x = list(treat = 0:1,
                             re75 = c(0, 20000),
                             race = "black"),
                 verbose = FALSE)

## -----------------------------------------------------------------------------
attr(est3, "setx")

## -----------------------------------------------------------------------------
plot(est3, ci = FALSE)

## -----------------------------------------------------------------------------
est4 <- sim_setx(s, x = list(treat = 0:1,
                             re75 = seq(0, 20000, by = 2000),
                             race = "black"),
                 verbose = FALSE)

## -----------------------------------------------------------------------------
plot(est4)

## -----------------------------------------------------------------------------
est5 <- sim_setx(s, x = list(treat = 0, re75 = 0),
                 x1 = list(treat = 1, re75 = 0),
                 verbose = FALSE)

## -----------------------------------------------------------------------------
summary(est5)

## -----------------------------------------------------------------------------
est6 <- sim_ame(s, var = "treat", subset = treat == 1,
                contrast = "rr", verbose = FALSE)

## -----------------------------------------------------------------------------
summary(est6, null = c(NA, NA, 1))

## -----------------------------------------------------------------------------
est7 <- sim_ame(s, var = "age", verbose = FALSE)

## -----------------------------------------------------------------------------
summary(est7)

## -----------------------------------------------------------------------------
est8 <- sim_adrf(s, var = "age", contrast = "adrf",
                 at = seq(18, 50, by = 2),
                 verbose = FALSE)

## -----------------------------------------------------------------------------
plot(est8)

## -----------------------------------------------------------------------------
summary(est8, parm = 1:4)

## -----------------------------------------------------------------------------
est9 <- sim_adrf(s, var = "age", contrast = "amef",
                 at = seq(18, 50, by = 2),
                 verbose = FALSE)

## -----------------------------------------------------------------------------
plot(est9)

## -----------------------------------------------------------------------------
lalonde <- transform(lalonde, re78_0 = ifelse(re78 == 0, 1, 0))

## -----------------------------------------------------------------------------
est6 <- transform(est6, RD = `E[Y(1)]` - `E[Y(0)]`)

## -----------------------------------------------------------------------------
summary(est6, null = c(NA, NA, 1, 0))

## -----------------------------------------------------------------------------
# AME of treat with race = "black"
est10b <- sim_ame(s, var = "treat", subset = race == "black",
                  contrast = "diff", verbose = FALSE)
summary(est10b)

# AME of treat with race = "hispan"
est10h <- sim_ame(s, var = "treat", subset = race == "hispan",
                  contrast = "diff", verbose = FALSE)
summary(est10h)

## -----------------------------------------------------------------------------
names(est10b) <- paste(names(est10b), "b", sep = "_")
names(est10h) <- paste(names(est10h), "h", sep = "_")

## -----------------------------------------------------------------------------
est10 <- cbind(est10b, est10h)
summary(est10)

## -----------------------------------------------------------------------------
est10 <- transform(est10, `Dh - Db` = Diff_h - Diff_b)
summary(est10, parm = "Dh - Db")

## ---- include=F---------------------------------------------------------------
amelia_ok <- requireNamespace("Amelia", quietly = TRUE)


## ---- eval = amelia_ok--------------------------------------------------------
library(Amelia)
data("africa", package = "Amelia")

# Multiple imputation
a.out <- amelia(x = africa, m = 10, cs = "country",
                ts = "year", logs = "gdp_pc", p2s = 0)

# Fit model to each dataset
model.list <- with(a.out, lm(gdp_pc ~ infl * trade))

# Simulate coefficients
si <- misim(model.list, n = 200)

si

## ---- eval = amelia_ok--------------------------------------------------------
sim_fun <- function(fit) {
  #Extract the original dataset using get_predictors()
  X <- insight::get_predictors(fit)
  
  p0 <- predict(fit, newdata = X)
  
  #Perturb infl slightly
  p1 <- predict(fit, newdata = transform(X, infl = infl + 1e-5))

  return(c(AME = mean((p1 - p0) / 1e-5)))
}

est_mi <- sim_apply(si, FUN = sim_fun, verbose = FALSE)

summary(est_mi)

## ---- eval = amelia_ok--------------------------------------------------------
est_mi2 <- sim_ame(si, var = "infl", verbose = FALSE)

