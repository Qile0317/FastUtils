test_that("quietly() works", {
    expect_identical(capture.output(quietly(print(1))), character(0))
})
