# Test 1: Does it return the custom OO class?
test_that("fit_gd returns a custom OO object", {
  x <- 1:10
  y <- 1:10
  res <- fit_gd(x, y)

  # Checks that we assigned the class correctly in our R wrapper
  expect_s3_class(res, "gradr_fit")
})

# Test 2: Does the C++ math actually work?
test_that("Gradient descent correctly estimates slope and intercept", {
  # Create dummy data for y = 2x + 1
  x <- 1:10
  y <- 2 * x + 1

  res <- fit_gd(x, y, alpha = 0.01, iterations = 2500)

  # We use a tolerance because gradient descent is an approximation
  expect_equal(res$slope, 2, tolerance = 0.1)
  expect_equal(res$intercept, 1, tolerance = 0.2)
})

# Test 3: Does it catch data errors?
test_that("fit_gd catches unequal vector lengths", {
  x <- 1:5
  y <- 1:10

  # Expecting the function to stop with an error mentioning "length"
  expect_error(fit_gd(x, y), "length")
})
