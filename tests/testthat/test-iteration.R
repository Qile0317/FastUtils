test_that("Generate Unique Pairs Up To a Number", {
  expect_equal(getUniquePairsUpTo(5), list(c(1, 2), c(1, 3), c(1, 4), c(1, 5), c(2, 3), c(2, 4), c(2, 5), c(3, 4), c(3, 5), c(4, 5)))
  expect_equal(getUniquePairsUpTo(1), list())
})

test_that("Zip Multiple Vectors or Lists", {
  expect_equal(zipit(1:3, letters[1:3]), list(list(1, "a"), list(2, "b"), list(3, "c")))
})

test_that("Enumerate Elements with Indices", {
  expect_equal(enumerateit(letters[1:3]), list(list(1, "a"), list(2, "b"), list(3, "c")))
  expect_equal(enumerateit(letters[1:3], zero_indexed = TRUE), list(list(0, "a"), list(1, "b"), list(2, "c")))
})

test_that("Get Index from Enumerated Element", {
  expect_equal(ind(list(2, "b")), 2)
})

test_that("Get Value from Enumerated Element by Index", {
  expect_equal(val(list(2, "b"), 1), "b")
})

test_that("Get First Value from Enumerated Element", {
  expect_equal(val1(list(1, "a")), "a")
})
