test_that("Get a Character at a Specific Index", {
  expect_equal(getChar("hello", 1), "h")
  expect_equal(getChar("hello", 5), "o")
})

test_that("Check if a Character is a Vowel", {
  expect_true(isVowel("a"))
  expect_false(isVowel("b"))
})

test_that("Check if a String Starts with a Vowel", {
  expect_true(startsWithVowel("apple"))
  expect_false(startsWithVowel("banana"))
})

test_that("Prepend an Indefinite Article to a String", {
  expect_equal(prependIndefArticle("apple"), "an apple")
  expect_equal(prependIndefArticle("banana"), "a banana")
})

test_that("Remove Spaces from a String", {
  expect_equal(stripSpaces("hello world"), "helloworld")
})

test_that("Find the Closest Word in a Set to a Given Word", {
  expect_equal(closestWord("cat", c("rat", "kak", "dog")), "rat")
  expect_equal(closestWord("rabbit", c("rat", "dog")), "rat")
})
