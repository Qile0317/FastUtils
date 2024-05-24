# Test substrEnd function
test_that("Extract substring from start to end difference", {
  # Single input case
  expect_equal(substrEnd("abcdefgh", 1, 2), "abcdef")
  expect_equal(substrEnd("abcdefgh", 1, 4), "abcd")
  expect_equal(substrEnd("abcdefgh", 1, 0), "abcdefgh")
  
  # Vectorized input case
  inputs <- c("abcdefgh", "ijklmnop", "qrstuvwxyz")
  expected_output <- c("abcdef", "ijklmn", "qrstuvwx")
  expect_equal(substrEnd(inputs, 1, 2), expected_output)
})
