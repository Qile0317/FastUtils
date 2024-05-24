test_that("Bound a Number within a Range", {
  expect_equal(bound(5, 1, 10), 5)
  expect_equal(bound(c(0, 5, 15), 1, 10), c(1, 5, 10))
})

test_that("Check if a Number is within a Range", {
  expect_equal(isBound(5, 1, 10), TRUE)
  expect_equal(isBound(c(0, 5, 15), 1, 10), c(FALSE, TRUE, FALSE))
})

test_that("Add Two Objects", {
  expect_equal(add(2, 3), 5)
  expect_equal(add("hello", "world"), "helloworld")
})

test_that("Subtract Two Numbers", {
  expect_equal(subtract(5, 3), 2)
})

test_that("Multiply Two Numbers", {
  expect_equal(multiply(2, 3), 6)
})

test_that("Divide Two Numbers", {
  expect_equal(divide(6, 3), 2)
})

test_that("Check if a Number is Even", {
  expect_equal(isEven(4), TRUE)
  expect_equal(isEven(5), FALSE)
})

test_that("Check if a Number is Odd", {
  expect_equal(isOdd(4), FALSE)
  expect_equal(isOdd(5), TRUE)
})
