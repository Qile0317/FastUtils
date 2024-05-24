test_that("Initialize a Vector", {
  expect_equal(initV("numeric", 5), numeric(5))
  expect_equal(initV("character", 3), character(3))
  expect_equal(initV("numeric", 2, 3), c(3, 3))
})

test_that("initEmptyTable works", {
  expect_length(initEmptyTable(), 0)
})

test_that("Convert a Table to Numeric", {
  input_table <- table(rep(letters[1:3], times = 1:3))
  expected_output <- c(a = 1, b = 2, c = 3)
  expect_equal(tableToNumeric(input_table), expected_output)
})

test_that("Convert Named Numeric Vector to Table", {
  input_vector <- c(a = 1, b = 2, c = 3)
  expected_output <- table(rep(letters[1:3], times = 1:3))
  expect_equal(namedNumericToTable(input_vector), expected_output)
})

test_that("createHash works", {
  expect_identical(createHash(), hash::hash())
  expect_equal(createHash(c("key1", "key2"), 1), hash::hash("key1" = 1, "key2" = 1))
})
