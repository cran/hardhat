# Minimal illustration of two points from the factor_key() review:
#
#   1. The error model.matrix() throws for a single-level factor -- which
#      factor_key.terms() currently swallows by grepl()-ing the message string.
#   2. The factor levels are already available *without* re-reading the data
#      values, both from the terms object and from a mold() blueprint.

library(hardhat)

# A predictor factor with only one observed level.
df <- data.frame(
  y = 1:5,
  f1 = factor(rep("a", 5))
)

# ------------------------------------------------------------------------------
# 1. The error from model.matrix()

framed <- model_frame(y ~ f1, df)

# This is the error factor_key.terms() catches via
#   grepl("contrasts can be applied only to factors with 2 or more levels", ...)
try(model.matrix(framed$terms, framed$data))
#> Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) :
#>   contrasts can be applied only to factors with 2 or more levels

# The single level is knowable up front -- no need to build the model matrix
# and parse an English error string to discover it:
levels(df$f1)
#> [1] "a"

# ------------------------------------------------------------------------------
# 2. Where the levels already live

# (a) On the terms object: each variable's class is recorded in "dataClasses".
attr(framed$terms, "dataClasses")
#>         y        f1
#> "numeric"  "factor"

# (b) From terms + frame via the base helper, with the actual levels:
stats::.getXlevels(framed$terms, framed$data)
#> $f1
#> [1] "a"

# (c) On a mold() blueprint. Using a well-behaved (multi-level) factor here,
#     since mold() builds the model matrix and so hits the same single-level
#     error as above. The factor levels are preserved on the predictor ptype --
#     a 0-row tibble that keeps factor columns as <fct> with their levels --
#     so the columns a factor expands into are recoverable from the blueprint
#     alone, and the blueprint methods would not need the caller to re-supply
#     `data`.
df2 <- data.frame(
  y = 1:6,
  f1 = factor(rep(c("a", "b", "c"), 2)),
  chr = rep(c("x", "y", "z"), 2)
)
molded <- mold(y ~ f1 + chr, df2)

# Factor levels live on the predictor ptype:
molded$blueprint$ptypes$predictors
#> # A tibble: 0 x 2
#> # i 2 variables: f1 <fct>, chr <chr>
lapply(molded$blueprint$ptypes$predictors, levels)
#> $f1
#> [1] "a" "b" "c"
#>
#> $chr
#> NULL

# Note: blueprint$levels only records *character* predictors coerced to factors
# during mold() (here `chr`), not factors that were already factors (`f1`):
molded$blueprint$levels
#> $chr
#> [1] "x" "y" "z"

library(testthat)

test_that("this is expected", {
  expect_error(boost_tree())
  library(parsnip)
  expect_silent(boost_tree())
})

test_that("but loading parsnip persists beyond the scope of the first test", {
  # boost_tree() is still available, even though parsnip is not loaded in this test
  # thus the expect_error() fails
  expect_error(boost_tree())
})