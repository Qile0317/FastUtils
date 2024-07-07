test_that("joinRegex works", {
  expect_identical(joinRegex("a", "b"), "(a)|(b)")
})
