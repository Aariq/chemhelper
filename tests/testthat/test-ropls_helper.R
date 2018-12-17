context("test functions for extracting data from ropls objects")

test_that("get_scores fails if not ropls model", {
  expect_error(
    prcomp(mtcars) %>% get_scores()
  )
})

test_that("get_scores works with PCA", {
  expect_is(
    opls(mtcars) %>% get_scores(),
    "data.frame"
  )
})

test_that("get_scores works with PLS", {
  expect_is(
    opls(dplyr::select(mtcars, -mpg), mtcars$mpg) %>% get_scores(),
    "data.frame"
  )
})


# test_that("get_scores works with OPLS", {
#   library(ropls)
#   sacurine <- data(sacurine)
#   expect_is(
#     opls(sacurine$dataMatrix, ropls::sacurine$sampleMetadata[, "gender"], predI = 1, orthoI = NA) %>% get_scores(),
#     "data.frame"
#   )
# })