test_that("evalText works", {

    expect_identical(evalText("1L + 1L"), 2L)
    expect_identical(evalText("1L", "+", "1L"), 2L)
    expect_identical(evalText("1", "L", "+", "1", "L"), 2L)

    someObject <- 831L
    expect_identical(evalText("someObject"), 831L)
    expect_identical(evalText("someObject - 6L"), 825L)

    someFunction <- function() {
        evalText("someObject")
    }
    expect_identical(someFunction(), 831L)

})
