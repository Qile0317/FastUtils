test_that("splitCamel and splitPascal split strings correctly", {
    expect_equal(
        splitCamel("splitCamelCaseIntoWords"),
        list(c("split", "Camel", "Case", "Into", "Words"))
    )
    expect_equal(
        splitCamel("FOOBar", conseq = TRUE),
        list(c("FOO", "Bar"))
    )
    expect_equal(
        splitCamel("FOOBar", conseq = FALSE),
        list(c("F", "O", "O", "Bar"))
    )
    expect_equal(
        splitPascal("SplitPascalCase"),
        list(c("Split", "Pascal", "Case"))
    )
    expect_equal(
        splitPascal("HTTPServer", conseq = TRUE),
        list(c("HTTP", "Server"))
    )
})

test_that("splitSnake splits snake_case strings correctly", {
    expect_equal(
        splitSnake("this_is_snake_case"),
        list(c("this", "is", "snake", "case"))
    )
    expect_equal(
        splitSnake("another_example_here"),
        list(c("another", "example", "here"))
    )
})

test_that("isCamelCase identifies camelCase correctly", {
    expect_true(isCamelCase("camelCase"))
    expect_false(isCamelCase("CamelCase"))
    expect_true(isCamelCase("camelcase"))
})

test_that("isPascalCase identifies PascalCase correctly", {
    expect_true(isPascalCase("PascalCase"))
    expect_false(isPascalCase("pascalCase"))
})

test_that("isSnakeCase identifies snake_case correctly", {
    expect_true(isSnakeCase("snake_case"))
    expect_false(isSnakeCase("Snake_Case"))
    expect_true(isSnakeCase("snake_case", strict = FALSE))
    expect_true(isSnakeCase("Snake_Case", strict = FALSE))
})

test_that("isSnakeCase allows uppercase letters when strict is FALSE", {
    expect_true(isSnakeCase("Snake_Case", strict = FALSE))
    expect_false(isSnakeCase("snake_Case"))
})

test_that("trySplitWords function works as expected", {

    # Test camelCase splitting
    result <- trySplitWords("camelCaseExample")
    expect_equal(result[[1]], c("camel", "case", "example"))

    # Test PascalCase splitting
    result <- trySplitWords("PascalCaseExample")
    expect_equal(result[[1]], c("pascal", "case", "example"))

    # Test snake_case splitting
    result <- trySplitWords("snake_case_example")
    expect_equal(result[[1]], c("snake", "case", "example"))

    # Test multiple snake_case splitting
    result <- trySplitWords(
        "snake_case_example", "more_snake_cases", "third_snake_case"
    )
    expect_equal(result[[1]], c("snake", "case", "example"))
    expect_equal(result[[2]], c("more", "snake", "cases"))
    expect_equal(result[[3]], c("third", "snake", "case"))

    # Test strings with other delimiters
    result <- trySplitWords("some|random|case")
    expect_equal(result[[1]], c("some", "random", "case"))

    # Test space-separated words
    result <- trySplitWords("Space Words")
    expect_equal(result[[1]], c("space", "words"))

    # Test upper case conversion to lower case
    result <- trySplitWords("UPPER_CASE", uncase = TRUE)
    expect_equal(result[[1]], c("upper", "case"))

    # Test retaining case when uncase is FALSE
    result <- trySplitWords("camelCaseExample", uncase = FALSE)
    expect_equal(result[[1]], c("camel", "Case", "Example"))

    # Test camelCase splitting with uncase = FALSE
    result <- trySplitWords("camelCaseExample", uncase = FALSE)
    expect_equal(result[[1]], c("camel", "Case", "Example"))

    # Test strict snake_case splitting
    result <- trySplitWords("strict_snake_case", strictSnake = TRUE)
    expect_equal(result[[1]], c("strict", "snake", "case"))

    # Test non-matching patterns
    result <- trySplitWords("Non-Matching-Pattern")
    expect_equal(result[[1]], c("non", "matching", "pattern"))
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
    expect_equal(closestWord("cat", c("kak", "dog", "rat")), "rat")
    expect_equal(closestWord("rabbit", c("rat", "dog")), "rat")
    expect_identical(closestWord("foo", "bar"), "bar")
})
