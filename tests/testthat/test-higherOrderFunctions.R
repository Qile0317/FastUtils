test_that("Create a Mutator Function works", {
    mutator <- createMutator(function(x, y) x + y)
    var <- 5
    mutator(var, 3)
    expect_equal(var, 8)
})
