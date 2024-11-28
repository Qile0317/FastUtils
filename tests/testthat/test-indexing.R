test_that("Get the Last Elements of a Vector or List", {
    expect_equal(getlast(1:5), 5)
    expect_equal(getlast(list(a = 1, b = 2, c = 3), 2), 2)
})

test_that("Get the First Elements of a Vector or List", {
    expect_equal(getfirst(1:5), 1)
    expect_equal(getfirst(list(a = 1, b = 2, c = 3), 2), 2)
})

test_that("setIndex works correctly", {
    mat <- matrix(1:9, nrow = 3)
    result <- setIndex(mat, 1, 2, 10)
    expect_equal(result[1, 2], 10)
    result <- setIndex(mat, 1, value = 20)
    expect_equal(result[1, ], rep(20, 3))
    result <- setIndex(mat, j = 3, value = 30)
    expect_equal(result[, 3], rep(30, 3))
    expect_error(setIndex(mat, value = 100),
                 "Please provide either i and/or j.")
})
test_that("setCol works correctly", {
    mat <- matrix(1:9, nrow = 3)
    result <- setCol(mat, j = 2, value = 5)
    expect_equal(result[, 2], rep(5, 3))
    result <- setCol(mat, j = 1, value = 7)
    expect_equal(result[, 1], rep(7, 3))
})
test_that("setRow works correctly", {
    mat <- matrix(1:9, nrow = 3)
    result <- setRow(mat, i = 2, value = 15)
    expect_equal(result[2, ], rep(15, 3))
    result <- setRow(mat, i = 3, value = 20)
    expect_equal(result[3, ], rep(20, 3))
})
test_that("setAt works correctly", {
    vec <- c(1, 2, 3, 4, 5)
    result <- setAt(vec, 2, 10)
    expect_equal(result[2], 10)
    result <- setAt(vec, 1, 100)
    expect_equal(result[1], 100)
    result <- setAt(vec, 5, 50)
    expect_equal(result[5], 50)
    result <- setAt(vec, 1, 0)
    expect_equal(result[1], 0)
})
