test_that("Correct leap years return TRUE", {
  expect_true(is_leapyear(1992))
  expect_true(is_leapyear(2000))
  expect_true(is_leapyear("2000"))
})

test_that("Non-leap years return FALSE", {
  expect_false(is_leapyear(1900))
  expect_false(is_leapyear(2021))
  expect_false(is_leapyear("2021"))
})

test_that("String inputs are parsed if numeric-like", {
  expect_true(is_leapyear("1992"))
  expect_false(is_leapyear("1900"))
})

test_that("Invalid strings cause failure", {
  expect_error(is_leapyear("two thousand"), "numeric string")
  expect_error(is_leapyear("year2020"), "numeric string")
})

test_that("Decimal inputs cause an error", {
  expect_error(is_leapyear(2000.5), "integer")
})

test_that("Zero and negative values cause an error", {
  expect_error(is_leapyear(0), "positive")
  expect_error(is_leapyear(-100), "positive")
})

test_that("Vector input causes an error", {
  expect_error(is_leapyear(c(2000, 2004)), "single")
})
