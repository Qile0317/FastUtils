test_that("Get the Last Elements of a Vector or List", {
  expect_equal(getlast(1:5), 5)
  expect_equal(getlast(list(a = 1, b = 2, c = 3), 2), 2)
})

test_that("Get the First Elements of a Vector or List", {
  expect_equal(getfirst(1:5), 1)
  expect_equal(getfirst(list(a = 1, b = 2, c = 3), 2), 2)
})
