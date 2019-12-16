## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(rlang_backtrace_on_error = "none")

## ----setup--------------------------------------------------------------------
library(hardhat)

## -----------------------------------------------------------------------------
iris_form <- mold(Sepal.Width ~ log(Sepal.Length), iris)

names(iris_form)

## -----------------------------------------------------------------------------
iris_form$predictors

## -----------------------------------------------------------------------------
iris_form$outcomes

## -----------------------------------------------------------------------------
mold(Sepal.Width ~ log(Sepal.Length) + offset(Petal.Width), iris)$extras

## -----------------------------------------------------------------------------
identical(
  mold(~ Sepal.Width, iris), 
  mold(~ Sepal.Width, iris, blueprint = default_formula_blueprint())
)

## -----------------------------------------------------------------------------
no_intercept <- mold(~ Sepal.Width, iris)

no_intercept$predictors

## -----------------------------------------------------------------------------
with_intercept <- mold(
  ~ Sepal.Width, iris, 
  blueprint = default_formula_blueprint(intercept = TRUE)
)

with_intercept$predictors

## ---- error=TRUE--------------------------------------------------------------
mold(~ Sepal.Width - 1, iris)

mold(~ Sepal.Width + 0, iris)

## -----------------------------------------------------------------------------
expanded_dummies <- mold(~ Sepal.Width + Species, iris)

expanded_dummies$predictors

## -----------------------------------------------------------------------------
non_expanded_dummies <- mold(
  ~ Sepal.Width + Species, iris, 
  blueprint = default_formula_blueprint(indicators = FALSE)
)

non_expanded_dummies$predictors

## -----------------------------------------------------------------------------
k_cols <- mold(~ Species, iris)

k_minus_one_cols <- mold(
  ~ Species, iris, 
  blueprint = default_formula_blueprint(intercept = TRUE)
)

colnames(k_cols$predictors)

colnames(k_minus_one_cols$predictors)

## -----------------------------------------------------------------------------
.f <- cbind(Sepal.Width, Sepal.Length) ~ Petal.Width

frame <- model.frame(.f, iris)

head(frame)

## -----------------------------------------------------------------------------
ncol(frame)

class(frame$`cbind(Sepal.Width, Sepal.Length)`)

head(frame$`cbind(Sepal.Width, Sepal.Length)`)

## -----------------------------------------------------------------------------
multivariate <- mold(Sepal.Width + log(Sepal.Length) ~ Petal.Width, iris)

multivariate$outcomes

## -----------------------------------------------------------------------------
x <- subset(iris, select = -Sepal.Width)
y <- subset(iris, select = Sepal.Width)

iris_xy <- mold(x, y)

iris_xy$predictors

iris_xy$outcomes

## -----------------------------------------------------------------------------
xy_with_intercept <- mold(x, y, blueprint = default_xy_blueprint(intercept = TRUE))

xy_with_intercept$predictors

## -----------------------------------------------------------------------------
mold(x, y$Sepal.Width)$outcomes

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(recipes)

rec <- recipe(Sepal.Length ~ Species + Petal.Width, iris) %>%
  step_log(Sepal.Length) %>%
  step_dummy(Species)

iris_recipe <- mold(rec, iris)

iris_recipe$predictors

iris_recipe$outcomes

## -----------------------------------------------------------------------------
recipe_with_intercept <- mold(
  rec, iris, 
  blueprint = default_recipe_blueprint(intercept = TRUE)
)

recipe_with_intercept$predictors

