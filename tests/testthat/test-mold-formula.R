context("test-mold-formulas")

test_that("can mold simple formulas", {

  sparse_bp <- default_formula_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_formula_blueprint(composition = "matrix")

  x1 <- mold(fac_1 ~ num_1, example_train)
  x2 <- mold(fac_1 ~ num_1, example_train, blueprint = sparse_bp)
  x3 <- mold(fac_1 ~ num_1, example_train, blueprint = matrix_bp)

  expect_is(x1$predictors, "tbl_df")
  expect_is(x2$predictors, "dgCMatrix")
  expect_is(x3$predictors, "matrix")

  expect_equal(colnames(x1$predictors), "num_1")
  expect_equal(colnames(x2$predictors), "num_1")
  expect_equal(colnames(x3$predictors), "num_1")
  expect_is(x1$outcomes, "tbl_df")
  expect_is(x2$outcomes, "tbl_df")
  expect_is(x3$outcomes, "tbl_df")
  expect_equal(colnames(x1$outcomes), "fac_1")
  expect_equal(x1$outcomes, x2$outcomes)
  expect_equal(x1$outcomes, x3$outcomes)
  expect_is(x1$blueprint, "default_formula_blueprint")
})

test_that("can mold multivariate formulas", {

  sparse_bp <- default_formula_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_formula_blueprint(composition = "matrix")

  x1 <- mold(num_1 + num_2 ~ num_3, example_train)
  x2 <- mold(num_1 + num_2 ~ num_3, example_train, blueprint = sparse_bp)
  x3 <- mold(num_1 + num_2 ~ num_3, example_train, blueprint = matrix_bp)

  expect_is(x1$outcomes, "tbl_df")
  expect_equal(colnames(x1$outcomes), c("num_1", "num_2"))
  expect_equal(x1$outcomes, x2$outcomes)
  expect_equal(x1$outcomes, x3$outcomes)

  y1 <- mold(log(num_2) + poly(num_2, degree = 2) ~ fac_1, example_train)
  y2 <- mold(log(num_2) + poly(num_2, degree = 2) ~ fac_1, example_train, blueprint = sparse_bp)
  y3 <- mold(log(num_2) + poly(num_2, degree = 2) ~ fac_1, example_train, blueprint = matrix_bp)

  expect_equal(
    colnames(y1$outcomes),
    c(
      "log(num_2)",
      "poly(num_2, degree = 2).1",
      "poly(num_2, degree = 2).2"
    )
  )
  expect_equal(y1$outcomes, y2$outcomes)
  expect_equal(y1$outcomes, y3$outcomes)

})

test_that("factor predictors with no intercept are fully expanded", {

  x1  <- mold(
    num_1 ~ fac_1,
    example_train,
    blueprint = default_formula_blueprint(intercept = TRUE)
  )
  x2  <- mold(
    num_1 ~ fac_1,
    example_train,
    blueprint = default_formula_blueprint(intercept = TRUE, composition = "matrix")
  )

  y1 <- mold(
    num_1 ~ fac_1,
    example_train,
    blueprint = default_formula_blueprint(intercept = FALSE, indicators = "one_hot")
  )
  y2 <- mold(
    num_1 ~ fac_1,
    example_train,
    blueprint = default_formula_blueprint(intercept = FALSE,
                                          indicators = "one_hot",
                                          composition = "matrix")
  )

  expect_equal(
    colnames(x1$predictors),
    c("(Intercept)", "fac_1b", "fac_1c")
  )
  expect_equal(
    colnames(x2$predictors),
    c("(Intercept)", "fac_1b", "fac_1c")
  )

  expect_equal(
    colnames(y1$predictors),
    c("fac_1a", "fac_1b", "fac_1c")
  )
  expect_equal(
    colnames(y2$predictors),
    c("fac_1a", "fac_1b", "fac_1c")
  )

})

test_that("can mold and not expand dummies", {

  x <- mold(
    num_1 ~ fac_1,
    example_train,
    blueprint = default_formula_blueprint(indicators = "none")
  )

  expect_equal(colnames(x$predictors), "fac_1")
  expect_is(x$predictors$fac_1, "factor")
  expect_equal(x$blueprint$indicators, "none")
})

test_that("errors are thrown if `indicator = FALSE` and factor interactions exist", {

  expect_error(
    mold(~ fac_1, example_train, blueprint = default_formula_blueprint(indicators = "none")),
    NA
  )

  expect_error(
    mold(
      num_1 ~ fac_1:num_2,
      example_train,
      blueprint = default_formula_blueprint(indicators = "none")
    ),
    "Interaction terms involving factors"
  )

  # Checking various types of generated interactions

  expect_error(
    mold(
      num_1 ~ fac_1:num_2,
      example_train,
      blueprint = default_formula_blueprint(indicators = "none")
    ),
    "'fac_1'"
  )

  expect_error(
    mold(
      num_1 ~ fac_1 * num_2,
      example_train,
      blueprint = default_formula_blueprint(indicators = "none")
    ),
    "'fac_1'"
  )

  expect_error(
    mold(
      num_1 ~ (fac_1 + num_2) ^ 2,
      example_train,
      blueprint = default_formula_blueprint(indicators = "none")
    ),
    "'fac_1'"
  )

  expect_error(
    mold(
      num_1 ~ fac_1 %in% num_2,
      example_train,
      blueprint = default_formula_blueprint(indicators = "none")
    ),
    "'fac_1'"
  )

  # Both factor issues are reported

  example_train2 <- example_train
  example_train2$fac_12 <- example_train2$fac_1

  expect_error(
    mold(
      ~ fac_1:fac_12,
      example_train2,
      blueprint = default_formula_blueprint(indicators = "none")
    ),
    "'fac_1', 'fac_12'."
  )

})

test_that("errors are thrown if `indicator = FALSE` and factors are used in inline functions", {

  blueprint_no_indicators <- default_formula_blueprint(indicators = "none")

  expect_error(
    mold(~ paste0(fac_1), example_train, blueprint = blueprint_no_indicators),
    "Functions involving factors"
  )

  expect_error(
    mold(~ paste0(fac_1), example_train, blueprint = blueprint_no_indicators),
    "'fac_1'"
  )

  expect_error(
    mold(~ fac_1 %>% paste0(), example_train, blueprint = blueprint_no_indicators)
  )

  expect_error(
    mold(~ paste0(fac_1 + fac_1), example_train, blueprint = blueprint_no_indicators),
    "'fac_1'"
  )

  example_train2 <- example_train
  example_train2$fac_12 <- example_train2$fac_1

  expect_error(
    mold(~ paste0(fac_1) + paste0(fac_12), example_train2, blueprint = blueprint_no_indicators),
    "'fac_1', 'fac_12'."
  )

})

test_that("`indicators = 'none'` works fine in strange formulas", {

  x <- mold(
    ~ NULL,
    example_train,
    blueprint = default_formula_blueprint(indicators = "none", intercept = TRUE)
  )

  expect_equal(
    colnames(x$predictors),
    "(Intercept)"
  )

})

test_that("formula intercepts can be added", {

  x1 <- mold(
    fac_1 ~ num_1,
    example_train,
    blueprint = default_formula_blueprint(intercept = TRUE)
  )
  x2 <- mold(
    fac_1 ~ num_1,
    example_train,
    blueprint = default_formula_blueprint(intercept = TRUE, composition = "dgCMatrix")
  )

  expect_true("(Intercept)" %in% colnames(x1$predictors))
  expect_true("(Intercept)" %in% colnames(x2$predictors))
  expect_equal(attr(x1$blueprint$terms$predictors, "intercept"), 1)
  expect_equal(attr(x2$blueprint$terms$predictors, "intercept"), 1)

  # Don't want intercept in original predictors
  expect_false("(Intercept)" %in% colnames(x1$blueprint$ptypes$predictors))
  expect_false("(Intercept)" %in% colnames(x2$blueprint$ptypes$predictors))
})

test_that("can mold formulas with special terms", {

  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(fac_1 ~ num_1:num_2 + I(num_1^2), example_train)
  x2 <- mold(fac_1 ~ num_1:num_2 + I(num_1^2), example_train, blueprint = bp)
  y1 <- mold(fac_1 ~ poly(num_1, degree = 2), example_train)
  y2 <- mold(fac_1 ~ poly(num_1, degree = 2), example_train, blueprint = bp)

  expect_equal(
    colnames(x1$predictors),
    c("I(num_1^2)", "num_1:num_2")
  )
  expect_equal(
    colnames(x2$predictors),
    c("I(num_1^2)", "num_1:num_2")
  )

  expect_equal(
    colnames(x1$blueprint$ptypes$predictors),
    c("num_1", "num_2")
  )
  expect_equal(
    colnames(x2$blueprint$ptypes$predictors),
    c("num_1", "num_2")
  )
})

test_that("formulas with non-existent columns are caught", {
  bp <- default_formula_blueprint(composition = "dgCMatrix")
  expect_error(
    mold(fac_1 ~ y + z, example_train),
    "predictors were not found in `data`: 'y', 'z'"
  )
  expect_error(
    mold(fac_1 ~ y + z, example_train, blueprint = bp),
    "predictors were not found in `data`: 'y', 'z'"
  )

  expect_error(
    mold(y + z ~ fac_1, example_train),
    "outcomes were not found in `data`: 'y', 'z'"
  )
  expect_error(
    mold(y + z ~ fac_1, example_train, blueprint = bp),
    "outcomes were not found in `data`: 'y', 'z'"
  )
})

test_that("global environment variables cannot be used", {
  expect_error(
    {
      y <- 1
      mold(fac_1 ~ y, example_train)
    },
    "predictors were not found in `data`: 'y'"
  )
})

test_that("cannot manually remove intercept in the formula itself", {

  bp <- default_formula_blueprint(composition = "dgCMatrix")
  expect_error(
    mold(fac_1 ~ y + 0, example_train),
    "`formula` must not contain"
  )
  expect_error(
    mold(fac_1 ~ y + 0, example_train, blueprint = bp),
    "`formula` must not contain"
  )

  expect_error(
    mold(fac_1 ~ 0 + y, example_train),
    "`formula` must not contain"
  )

  expect_error(
    mold(fac_1 ~ y - 1, example_train),
    "`formula` must not contain"
  )

})

test_that("RHS with _only_ intercept related terms are caught", {

  bp <- default_formula_blueprint(composition = "dgCMatrix")

  expect_error(
    mold(~ 0, example_train),
    "`formula` must not contain the intercept removal term"
  )
  expect_error(
    mold(~ 0, example_train, blueprint = bp),
    "`formula` must not contain the intercept removal term"
  )

  expect_error(
    mold(~ 1, example_train),
    "`formula` must not contain the intercept term"
  )

  expect_error(
    mold(~ -1, example_train),
    "`formula` must not contain the intercept removal term"
  )

})

test_that("`NULL` can be used to represent empty RHS formulas", {

  bp <- default_formula_blueprint(composition = "dgCMatrix")

  expect_error(
    mold(~ 0, example_train),
    "`formula` must not contain the intercept removal term"
  )
  expect_error(
    mold(~ 0, example_train, blueprint = bp),
    "`formula` must not contain the intercept removal term"
  )

  expect_error(
    x1 <- mold(~NULL, example_train),
    NA
  )
  expect_error(
    x2 <- mold(~NULL, example_train, blueprint = bp),
    NA
  )

  expect_equal(nrow(x1$predictors), 12)
  expect_equal(nrow(x1$outcomes), 12)
  expect_equal(nrow(x2$predictors), 12)
  expect_equal(nrow(x2$outcomes), 12)

  expect_error(
    y <- mold(~NULL, example_train, blueprint = default_formula_blueprint(intercept = TRUE)),
    NA
  )

  expect_equal(colnames(y$predictors), "(Intercept)")

})

test_that("intercepts can still be added when not using indicators (i.e. model.matrix())", {

  x <- mold(
    num_2 ~ fac_1,
    example_train,
    blueprint = default_formula_blueprint(intercept = TRUE, indicators = "none")
  )

  expect_true(
    "(Intercept)" %in% colnames(x$predictors)
  )

  expect_is(
    x$predictors$fac_1,
    "factor"
  )

})

test_that("`data` is validated", {

  bp <- default_formula_blueprint(composition = "dgCMatrix")
  expect_error(
    mold(fac_1 ~ num_2, 1),
    "`data` must be a data.frame or a matrix"
  )
  expect_error(
    mold(fac_1 ~ num_2, 1, blueprint = bp),
    "`data` must be a data.frame or a matrix"
  )

})

test_that("full interaction syntax is supported", {

  expect_equal(
    mold(~ fac_1*num_2, example_train)$predictors,
    mold(~ fac_1 + num_2 + fac_1:num_2, example_train)$predictors
  )

  expect_equal(
    mold(~ fac_1*num_2 - fac_1:num_2, example_train)$predictors,
    mold(~ fac_1 + num_2, example_train)$predictors
  )

  expect_equal(
    mold(~ (num_2 + num_1 + num_3) ^ 2, example_train)$predictors,
    mold(~ num_2 +
           num_1 +
           num_3 +
           num_2:num_1 +
           num_2:num_3 +
           num_1:num_3,
         example_train)$predictors
  )

  expect_equal(
    mold(~ num_2 + num_1 %in% num_2, example_train)$predictors,
    mold(~ num_2 + num_2:num_1, example_train)$predictors
  )

})

test_that("`indicators = 'none'` runs numeric interactions", {

  x <- mold(~ num_1:num_2, example_train,
            blueprint = default_formula_blueprint(indicators = "none"))

  expect_equal(
    colnames(x$predictors),
    "num_1:num_2"
  )

})

test_that("LHS of the formula cannot contain interactions", {

  expect_error(
    mold(num_1:num_2 ~ num_2, example_train),
    "The following interaction terms were found: 'num_1:num_2'"
  )

  expect_error(
    mold(num_1*num_2 ~ num_2, example_train),
    "The following interaction terms were found: 'num_1:num_2'"
  )

  expect_error(
    mold(num_1 %in% num_2 ~ num_2, example_train),
    "The following interaction terms were found: 'num_1:num_2'"
  )

  expect_error(
    mold((num_1 + num_2)^2 ~ num_2, example_train),
    "The following interaction terms were found: 'num_1:num_2'"
  )

  expect_error(
    mold(num_1:num_2 + fac_1:num_1 ~ num_2, example_train),
    "The following interaction terms were found: 'num_1:num_2', 'num_1:fac_1'"
  )

})

test_that("original predictor and outcome classes are recorded", {

  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(log(num_1) ~ log(num_2), example_train)
  x2 <- mold(log(num_1) ~ log(num_2), example_train, blueprint = bp)

  expect_equal(
    get_data_classes(x1$blueprint$ptypes$predictors),
    list(num_2 = "numeric")
  )
  expect_equal(
    get_data_classes(x2$blueprint$ptypes$predictors),
    list(num_2 = "numeric")
  )

  expect_equal(
    get_data_classes(x1$blueprint$ptypes$outcomes),
    list(num_1 = "integer")
  )
  expect_equal(
    get_data_classes(x2$blueprint$ptypes$outcomes),
    list(num_1 = "integer")
  )

})

test_that("`.` notation works as expected", {

  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(fac_1 ~ ., example_train)
  x2 <- mold(fac_1 ~ ., example_train, blueprint = bp)

  # no fac_1 columns in predictors
  expect_equal(
    colnames(x1$blueprint$ptypes$predictors),
    c("num_1", "num_2", "num_3", "fac_2")
  )
  expect_equal(
    colnames(x2$blueprint$ptypes$predictors),
    c("num_1", "num_2", "num_3", "fac_2")
  )

  # fac_1 is the outcome
  expect_equal(
    colnames(x1$blueprint$ptypes$outcomes),
    "fac_1"
  )
  expect_equal(
    colnames(x2$blueprint$ptypes$outcomes),
    "fac_1"
  )

})

# `expand_formula_dot_notation()` does not expand LHS dots, and we check
# for them in `get_all_outcomes()`. That calls `all.vars()`, which returns
# the `"."` as a variable.
test_that("`.` notation fails on the LHS", {
  bp <- default_formula_blueprint(composition = "dgCMatrix")
  expect_error(
    mold(. ~ fac_1, example_train),
    "The left hand side of the formula cannot contain `.`"
  )
  expect_error(
    mold(. ~ fac_1, example_train, blueprint = bp),
    "The left hand side of the formula cannot contain `.`"
  )
})

test_that("`.` notation with variable as predictor and outcome", {

  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(num_2 ~ . + num_2, example_train)
  x2 <- mold(num_2 ~ . + num_2, example_train)

  # num_2 IS a predictor
  expect_true(
    "num_2" %in% colnames(x1$blueprint$ptypes$predictors)
  )
  expect_true(
    "num_2" %in% colnames(x2$blueprint$ptypes$predictors)
  )

  # num_2 IS the outcome
  expect_equal(
    colnames(x1$blueprint$ptypes$outcomes),
    "num_2"
  )
  expect_equal(
    colnames(x2$blueprint$ptypes$outcomes),
    "num_2"
  )

  y1 <- mold(num_2 ~ . + log(num_2), example_train)
  y2 <- mold(num_2 ~ . + log(num_2), example_train, blueprint = bp)

  # num_2 IS a predictor
  expect_true(
    "num_2" %in% colnames(y1$blueprint$ptypes$predictors)
  )
  expect_true(
    "num_2" %in% colnames(y2$blueprint$ptypes$predictors)
  )

  # num_2 IS the outcome
  expect_equal(
    colnames(y1$blueprint$ptypes$outcomes),
    "num_2"
  )
  expect_equal(
    colnames(y2$blueprint$ptypes$outcomes),
    "num_2"
  )

})

test_that("`.` notation with no outcome works fine", {

  bp <- default_formula_blueprint(composition = "dgCMatrix")
  # Uses all columns of example_train
  x1 <- mold(~ ., example_train)
  x2 <- mold(~ ., example_train, blueprint = bp)

  expect_equal(
    ncol(x1$predictors),
    7
  )
  expect_equal(
    ncol(x2$predictors),
    7
  )

  expect_equal(
    colnames(x1$blueprint$ptypes$predictors),
    c("num_1", "num_2", "num_3", "fac_1", "fac_2")
  )
  expect_equal(
    colnames(x2$blueprint$ptypes$predictors),
    c("num_1", "num_2", "num_3", "fac_1", "fac_2")
  )

})

test_that("`-var` still registers var as a predictor", {

  # This is expected, and is the same as base R
  x <- mold(num_2 ~ . - num_1, example_train)

  # num_1 IS a predictor
  expect_true(
    "num_1" %in% colnames(x$blueprint$ptypes$predictors)
  )

})

test_that("Missing y value returns a 0 column tibble for `outcomes`", {
  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(~ num_2, example_train)
  x2 <- mold(NULL ~ num_2, example_train)
  x3 <- mold(~ num_2, example_train, blueprint = bp)
  x4 <- mold(NULL ~ num_2, example_train, blueprint = bp)

  expect_equal(nrow(x1$outcomes), 12)
  expect_equal(ncol(x1$outcomes), 0)

  expect_equal(x1$outcomes, x2$outcomes)
  expect_equal(x1$outcomes, x3$outcomes)
  expect_equal(x1$outcomes, x4$outcomes)
})


test_that("Missing y value returns a 0 column / 0 row tibble for `ptype`", {
  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(~ num_2, example_train)
  x2 <- mold(~ num_2, example_train, blueprint = bp)
  expect_equal(x1$blueprint$ptypes$outcomes, tibble())
  expect_equal(x2$blueprint$ptypes$outcomes, tibble())
})

test_that("Missing y value still has outcome `terms` present", {
  x <- mold(~ num_2, example_train)

  expect_equal(
    rlang::f_rhs(x$blueprint$terms$outcomes),
    rlang::expr(NULL + 0)
  )
})

# ------------------------------------------------------------------------------
# Character predictors

test_that("character predictors are treated as factors when `indicators` is not 'none'", {
  df <- data.frame(
    y = 1:2,
    x = c("a", "b"),
    z = c("c", "d"),
    stringsAsFactors = FALSE
  )

  bp1 <- default_formula_blueprint(indicators = "traditional")
  bp2 <- default_formula_blueprint(indicators = "one_hot")
  bp3 <- default_formula_blueprint(indicators = "traditional", composition = "matrix")
  bp4 <- default_formula_blueprint(indicators = "one_hot", composition = "matrix")

  x1 <- mold(y ~ x + z, df, blueprint = bp1)
  x2 <- mold(y ~ x + z, df, blueprint = bp2)
  x3 <- mold(y ~ x + z, df, blueprint = bp3)
  x4 <- mold(y ~ x + z, df, blueprint = bp4)

  expect_identical(
    colnames(x1$predictors),
    c("xa", "xb", "zd")
  )
  expect_identical(
    colnames(x3$predictors),
    c("xa", "xb", "zd")
  )

  expect_identical(
    colnames(x2$predictors),
    c("xa", "xb", "zc", "zd")
  )
  expect_identical(
    colnames(x4$predictors),
    c("xa", "xb", "zc", "zd")
  )
})

test_that("character predictors are left as characters when `indicators` is 'none'", {
  df <- data.frame(
    y = 1:2,
    x = c("a", "b"),
    z = c("c", "d"),
    stringsAsFactors = FALSE
  )

  bp <- default_formula_blueprint(indicators = "none")

  x <- mold(y ~ x + z, df, blueprint = bp)

  expect_identical(
    colnames(x$predictors),
    c("x", "z")
  )

  expect_true(is.character(x$predictors$x))
  expect_true(is.character(x$predictors$z))

  expect_true(is.character(x$blueprint$ptypes$predictors$x))
  expect_true(is.character(x$blueprint$ptypes$predictors$z))
})

# ------------------------------------------------------------------------------
# Factor encodings

test_that("traditional encoding and no intercept", {
  df <- data.frame(
    x = 1:12,
    y = factor(rep(letters[1:3], each = 4)),
    z = factor(rep(LETTERS[1:2], 6))
  )

  bp1 <- default_formula_blueprint(
    intercept = FALSE,
    indicators = "traditional"
  )
  bp2 <- default_formula_blueprint(
    intercept = FALSE,
    indicators = "traditional",
    composition = "matrix"
  )

  x1 <- mold(x ~ y + z, df, blueprint = bp1)
  x2 <- mold(x ~ y + z, df, blueprint = bp2)

  expect_identical(
    colnames(x1$predictors),
    c("ya", "yb", "yc", "zB")
  )
  expect_identical(
    colnames(x2$predictors),
    c("ya", "yb", "yc", "zB")
  )

  expect_false(x1$blueprint$intercept)
  expect_false(x2$blueprint$intercept)
})

test_that("traditional encoding and intercept", {
  df <- data.frame(
    x = 1:12,
    y = factor(rep(letters[1:3], each = 4)),
    z = factor(rep(LETTERS[1:2], 6))
  )

  bp1 <- default_formula_blueprint(
    intercept = TRUE,
    indicators = "traditional"
  )
  bp2 <- default_formula_blueprint(
    intercept = TRUE,
    indicators = "traditional",
    composition = "matrix"
  )

  x1 <- mold(x ~ y + z, df, blueprint = bp1)
  x2 <- mold(x ~ y + z, df, blueprint = bp2)

  expect_identical(
    colnames(x1$predictors),
    c("(Intercept)", "yb", "yc", "zB")
  )
  expect_identical(
    colnames(x2$predictors),
    c("(Intercept)", "yb", "yc", "zB")
  )

  expect_true(x1$blueprint$intercept)
  expect_true(x2$blueprint$intercept)
})

test_that("one-hot encoding and no intercept", {
  df <- data.frame(
    x = 1:12,
    y = factor(rep(letters[1:3], each = 4)),
    z = factor(rep(LETTERS[1:2], 6))
  )

  bp1 <- default_formula_blueprint(
    intercept = FALSE,
    indicators = "one_hot"
  )
  bp2 <- default_formula_blueprint(
    intercept = FALSE,
    indicators = "one_hot",
    composition = "matrix"
  )

  x1 <- mold(x ~ y + z, df, blueprint = bp1)
  x2 <- mold(x ~ y + z, df, blueprint = bp2)

  expect_identical(
    colnames(x1$predictors),
    c("ya", "yb", "yc", "zA", "zB")
  )
  expect_identical(
    colnames(x2$predictors),
    c("ya", "yb", "yc", "zA", "zB")
  )

  expect_false(x1$blueprint$intercept)
  expect_false(x2$blueprint$intercept)
})

test_that("one-hot encoding and intercept", {
  df <- data.frame(
    x = 1:12,
    y = factor(rep(letters[1:3], each = 4)),
    z = factor(rep(LETTERS[1:2], 6))
  )

  bp1 <- default_formula_blueprint(
    intercept = TRUE,
    indicators = "one_hot"
  )
  bp2 <- default_formula_blueprint(
    intercept = TRUE,
    indicators = "one_hot",
    composition = "matrix"
  )

  x1 <- mold(x ~ y + z, df, blueprint = bp1)
  x2 <- mold(x ~ y + z, df, blueprint = bp2)

  expect_identical(
    colnames(x1$predictors),
    c("(Intercept)", "ya", "yb", "yc", "zA", "zB")
  )
  expect_identical(
    colnames(x2$predictors),
    c("(Intercept)", "ya", "yb", "yc", "zA", "zB")
  )

  expect_true(x1$blueprint$intercept)
  expect_true(x2$blueprint$intercept)
})

# ------------------------------------------------------------------------------
# Deprecation

test_that("soft-deprecation of logical `indicators`", {
  local_lifecycle_warnings()

  expect_warning(
    x <- default_formula_blueprint(indicators = TRUE),
    "`indicators` now requires"
  )

  expect_identical(x$indicators, "traditional")


  expect_warning(
    x <- default_formula_blueprint(indicators = FALSE),
    "`indicators` now requires"
  )

  expect_identical(x$indicators, "none")
})
