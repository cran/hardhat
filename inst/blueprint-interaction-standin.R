# Follow-up to blueprint-data-redundant.R: a numeric-by-factor interaction.
#
# The interaction column NAMES (x:f1b, ...) still depend only on the factor
# levels -- the numeric predictor just needs to be PRESENT with some value so
# model.matrix() emits the columns. The numeric's prototype is on the blueprint
# too (a <dbl> column in the ptype), so a stand-in frame can be rebuilt from the
# blueprint alone, value included.

library(hardhat)

train <- data.frame(
  y = 1:6,
  x = rnorm(6), # numeric predictor
  f1 = factor(rep(c("a", "b", "c"), 2))
)

molded <- mold(y ~ x * f1, train)
bp <- molded$blueprint

baseline <- factor_key(bp, train)

# Rebuild a stand-in frame from the predictor ptype only:
#   - factor columns -> one row per stored level
#   - other columns  -> any constant (column names don't depend on the value)
ptype <- bp$ptypes$predictors
ptype
#> # A tibble: 0 x 2
#> # i 2 variables: x <dbl>, f1 <fct>

n <- max(c(1L, vapply(ptype, nlevels, integer(1))))
from_blueprint <- as.data.frame(lapply(ptype, function(col) {
  if (is.factor(col)) {
    factor(rep(levels(col), length.out = n), levels = levels(col))
  } else {
    rep(0, n) # stand-in value for the numeric
  }
}))

identical(factor_key(bp, from_blueprint), baseline)
#> [1] TRUE

# Only f1 appears as a source (the bare numeric x maps to nothing), and the
# interaction columns are recovered with their level-based names:
baseline
#> # A tibble: 5 x 2
#>   source derived
#>   <chr>  <chr>
#> 1 f1     f1a
#> 2 f1     f1b
#> 3 f1     f1c
#> 4 f1     x:f1b
#> 5 f1     x:f1c
