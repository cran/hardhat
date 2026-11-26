# Illustration for the review point: the blueprint methods of factor_key()
# require the caller to re-supply `data`, but everything the function actually
# needs is already stored on the blueprint.
#
# factor_key() only returns *column names*. The names model.matrix() produces
# depend on the factor LEVELS and their contrasts -- not on the data rows. The
# blueprint already carries the levels (on the predictor ptype), so re-passing
# `data` is redundant, and passing a frame that disagrees with the training
# data silently produces a wrong answer.

library(hardhat)

train <- data.frame(
  y = 1:6,
  f1 = factor(rep(c("a", "b", "c"), 2)),
  f2 = factor(rep(c("x", "y"), 3))
)

molded <- mold(y ~ f1 + f2, train)
bp <- molded$blueprint

baseline <- factor_key(bp, train)

# ------------------------------------------------------------------------------
# (A) The data VALUES don't matter -- only the factor levels attribute does.
#
# Here every row is "a"/"x", so the columns for the other levels are never
# observed. model.matrix() still emits them, because contrasts come from the
# factor's levels attribute, not the observed values. Same answer as training.

values_differ <- data.frame(
  y = 1:6,
  f1 = factor(rep("a", 6), levels = c("a", "b", "c")),
  f2 = factor(rep("x", 6), levels = c("x", "y"))
)

identical(factor_key(bp, values_differ), baseline)
#> [1] TRUE

# ------------------------------------------------------------------------------
# (B) The levels are already on the blueprint, so we can rebuild a stand-in
# frame from the blueprint ALONE (no access to the training rows) and get the
# same result. This is exactly the information factor_key() needs -- it could
# read it off the blueprint instead of asking the caller for `data`.

ptype <- bp$ptypes$predictors # 0-row tibble, but keeps <fct> levels
ptype
#> # A tibble: 0 x 2
#> # i 2 variables: f1 <fct>, f2 <fct>

n <- max(vapply(ptype, nlevels, integer(1))) # rows needed to show every level
from_blueprint <- as.data.frame(lapply(ptype, function(col) {
  factor(rep(levels(col), length.out = n), levels = levels(col))
}))

identical(factor_key(bp, from_blueprint), baseline)
#> [1] TRUE

# ------------------------------------------------------------------------------
# (C) The risk of requiring `data`: nothing forces it to match training. Pass a
# frame whose factor lost a level (e.g. a smaller/newer batch) and you silently
# get a mapping that disagrees with how the model was actually fit -- f1 now
# expands to two columns instead of three, with no warning.
# (This blueprint has no intercept, so the first factor keeps all its levels.)

wrong <- data.frame(
  y = 1:4,
  f1 = factor(rep(c("a", "b"), 2)), # level "c" gone -> only 2 levels
  f2 = factor(rep(c("x", "y"), 2))
)

baseline[baseline$source == "f1", ]
#> # A tibble: 3 x 2
#>   source derived
#>   <chr>  <chr>
#> 1 f1     f1a
#> 2 f1     f1b
#> 3 f1     f1c

factor_key(bp, wrong)[factor_key(bp, wrong)$source == "f1", ]
#> # A tibble: 2 x 2
#>   source derived
#>   <chr>  <chr>
#> 1 f1     f1a
#> 2 f1     f1b

# =====

library(hardhat)

train <- data.frame(
  y = 1:6,
  f1 = factor(rep(c("a", "b", "c"), 2)),
  f2 = factor(rep(c("x", "y"), 3))
)

molded <- mold(y ~ f1 + f2, train)
bp <- molded$blueprint

baseline <- factor_key(bp, train)

# alternative: use the ptypes from the blueprint
ptype <- bp$ptypes$predictors

# 0-row tibble, but keeps <fct> levels
ptype

rows_needed <- max(vapply(ptype, nlevels, integer(1)))
df_mocked_from_blueprint <-
  purrr::map(ptype, function(col) {
    factor(rep(levels(col), length.out = rows_needed), levels = levels(col))
  }) |>
  as.data.frame()

identical(factor_key(bp, df_mocked_from_blueprint), baseline)


df_without_level_c <- data.frame(
  y = 1:4,
  f1 = factor(rep(c("a", "b"), 2)),
  f2 = factor(rep(c("x", "y"), 2))
)

factor_key(bp, df_without_level_c) |>
  dplyr::filter(source == "f1")

baseline |>
  dplyr::filter(source == "f1")
