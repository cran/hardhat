## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(rlang_backtrace_on_error = "none")

## ----setup--------------------------------------------------------------------
library(hardhat)

## -----------------------------------------------------------------------------
iris_train <- iris[1:100,]
iris_test <- iris[101:150,]

## -----------------------------------------------------------------------------
iris_form <- mold(
  log(Sepal.Length) ~ Species + Petal.Width, 
  iris_train, 
  blueprint = default_formula_blueprint(indicators = FALSE)
)

formula_eng <- iris_form$blueprint

formula_eng

## -----------------------------------------------------------------------------
forge(iris_test, formula_eng)

## -----------------------------------------------------------------------------
forge(iris_test, formula_eng, outcomes = TRUE)

## ---- error=TRUE--------------------------------------------------------------
test_missing_column <- subset(iris_test, select = -Species)

forge(test_missing_column, formula_eng)

## ---- error=TRUE--------------------------------------------------------------
test_species_double <- iris_test
test_species_double$Species <- as.double(test_species_double$Species)

forge(test_species_double, formula_eng)

## -----------------------------------------------------------------------------
test_species_character <- iris_test
test_species_character$Species <- as.character(test_species_character$Species)

forged_char <- forge(test_species_character, formula_eng)

forged_char$predictors

class(forged_char$predictors$Species)

levels(forged_char$predictors$Species)

## ---- warning=TRUE------------------------------------------------------------
test_species_lossy <- iris_test
test_species_lossy$Species <- as.character(test_species_lossy$Species)
test_species_lossy$Species[2] <- "im new!"

forged_lossy <- forge(test_species_lossy, formula_eng)

forged_lossy$predictors

## ---- error=FALSE, warning=FALSE----------------------------------------------
library(recipes)

rec <- recipe(Sepal.Width ~ Sepal.Length + Species, iris_train) %>%
  step_dummy(Species)

iris_recipe <- mold(rec, iris_train)

iris_recipe$predictors

## -----------------------------------------------------------------------------
recipe_eng <- iris_recipe$blueprint

recipe_eng

## -----------------------------------------------------------------------------
forge(iris_test, recipe_eng, outcomes = TRUE)

## -----------------------------------------------------------------------------
rec2 <- recipe(Sepal.Width ~ Sepal.Length + Species, iris_train) %>%
  step_dummy(Species) %>%
  step_center(Sepal.Width) # Here we modify the outcome

iris_recipe2 <- mold(rec2, iris_train)

recipe_eng_log_outcome <- iris_recipe2$blueprint

## ---- error=TRUE--------------------------------------------------------------
iris_test_no_outcome <- subset(iris_test, select = -Sepal.Width)

forge(iris_test_no_outcome, recipe_eng_log_outcome)

## -----------------------------------------------------------------------------
rec3 <- recipe(Sepal.Width ~ Sepal.Length + Species, iris_train) %>%
  step_dummy(Species) %>%
  step_center(Sepal.Width, skip = TRUE)

iris_recipe3 <- mold(rec3, iris_train)

recipe_eng_skip_outcome <- iris_recipe3$blueprint

forge(iris_test_no_outcome, recipe_eng_skip_outcome)

## -----------------------------------------------------------------------------
forge(iris_test, recipe_eng_skip_outcome, outcomes = TRUE)$outcomes

# Notice that the `outcome` values haven't been centered
# and are the same as before
head(iris_test$Sepal.Width)

