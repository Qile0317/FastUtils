# Test for splitCamel and splitPascal (assuming identical behavior)
test_that("splitCamel and splitPascal split strings correctly", {
  expect_equal(splitCamel("splitCamelCaseIntoWords"), list(c("split", "Camel", "Case", "Into", "Words")))
  expect_equal(splitCamel("FOOBar", conseq = TRUE), list(c("FOO", "Bar")))
  expect_equal(splitCamel("FOOBar", conseq = FALSE), list(c("F", "O", "O", "Bar")))
  expect_equal(splitPascal("SplitPascalCase"), list(c("Split", "Pascal", "Case")))
  expect_equal(splitPascal("HTTPServer", conseq = TRUE), list(c("HTTP", "Server")))
})

# Test for splitSnake
test_that("splitSnake splits snake_case strings correctly", {
  expect_equal(splitSnake("this_is_snake_case"), list(c("this", "is", "snake", "case")))
  expect_equal(splitSnake("another_example_here"), list(c("another", "example", "here")))
})

# Test for isCamelCase
test_that("isCamelCase identifies camelCase correctly", {
  expect_true(isCamelCase("camelCase"))
  expect_false(isCamelCase("CamelCase"))
  expect_true(isCamelCase("camelcase"))
})

# Test for isPascalCase
test_that("isPascalCase identifies PascalCase correctly", {
  expect_true(isPascalCase("PascalCase"))
  expect_false(isPascalCase("pascalCase"))
})

# Test for isSnakeCase
test_that("isSnakeCase identifies snake_case correctly", {
  expect_true(isSnakeCase("snake_case"))
  expect_false(isSnakeCase("Snake_Case"))
  expect_true(isSnakeCase("snake_case", strict = FALSE))
  expect_true(isSnakeCase("Snake_Case", strict = FALSE))
})

# Test isSnakeCase with uppercase allowed when strict = FALSE
test_that("isSnakeCase allows uppercase letters when strict is FALSE", {
  expect_true(isSnakeCase("Snake_Case", strict = FALSE))
  expect_false(isSnakeCase("snake_Case"))
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
