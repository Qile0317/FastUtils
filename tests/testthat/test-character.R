# Test substrEnd function
test_that("Extract substring from start to end difference works", {

  # Single input case
  expect_equal(substrEnd("abcdefgh", 1, 2), "abcdef")
  expect_equal(substrEnd("abcdefgh", 1, 4), "abcd")
  expect_equal(substrEnd("abcdefgh", 1, 0), "abcdefgh")
  expect_equal(substrEnd("12345", 1, 1), "1234")
  expect_equal(substrEnd("12345", 1, 2), "123")
  expect_equal(substrEnd("12345", 2, 3), "2")
  
  # Vectorized input case
  inputs <- c("abcdefgh", "ijklmnop", "qrstuvwxyz")
  expected_output <- c("abcdef", "ijklmn", "qrstuvwx")
  expect_equal(substrEnd(inputs, 1, 2), expected_output)
})

test_that("enclose works correctly", {
  expect_equal(enclose("text", "[", "]"), "[text]")
  expect_equal(enclose(c("word", "text"), "{", "}"), c("{word}", "{text}"))
})

test_that("encloseBr works correctly", {
  expect_equal(encloseBr("text"), "(text)")
  expect_equal(encloseBr(c("word", "text")), c("(word)", "(text)"))
})

test_that("Get a Character at a Specific Index", {
  expect_equal(getChar("hello", 1), "h")
  expect_equal(getChar("hello", 5), "o")
})
