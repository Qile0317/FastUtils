test_that("colToRownames converts specified column to row names", {
    df <- data.frame(ID = c("A", "B", "C"), Value = c(10, 20, 30))
    result <- colToRownames(df, "ID")
    expect_equal(rownames(result), c("A", "B", "C"))
    expect_equal(result$Value, c(10, 20, 30))
    expect_false("ID" %in% colnames(result))
})

test_that("mutateToRownames works", {

    # Sample data
    df <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))

    # Test with a simple expression
    result <- mutateToRownames(df, A + B, .remove = TRUE)
    expected <- df
    rownames(expected) <- df$A + df$B
    expected <- expected %>%
        dplyr::select(-(1:2))
    expect_equal(result, expected)

    result <- mutateToRownames(df, A + B, .remove = FALSE)
    expected <- df
    rownames(expected) <- df$A + df$B
    expect_equal(result, expected)

    # Test with an expression involving multiple columns
    result <- mutateToRownames(df, A * B + C, .remove = TRUE)
    expected <- df
    rownames(expected) <- df$A * df$B + df$C
    expected <- expected %>%
        dplyr::select(-(1:3))
    expect_equal(result, expected)

    # Test with .remove = FALSE for multiple columns
    result <- mutateToRownames(df, A * B + C, .remove = FALSE)
    expected <- df
    rownames(expected) <- df$A * df$B + df$C
    expect_equal(result, expected)

    # Test with pasted values joined by a custom operator
    result <- mutateToRownames(df, paste(A, B, sep = "-"), .remove = TRUE)
    expected <- df
    rownames(expected) <- paste(df$A, df$B, sep = "-")
    expected <- expected %>%
        dplyr::select(-(1:2))
    expect_equal(result, expected)

    result <- mutateToRownames(df, paste(A, B, sep = "-"), .remove = FALSE)
    expected <- df
    rownames(expected) <- paste(df$A, df$B, sep = "-")
    expect_equal(result, expected)

    # Test with pasted values involving multiple columns
    result <- mutateToRownames(df, paste(A, B, C, sep = ":"), .remove = TRUE)
    expected <- df
    rownames(expected) <- paste(df$A, df$B, df$C, sep = ":")
    expected <- expected %>%
        dplyr::select(-(1:3))
    expect_equal(result, expected)

    result <- mutateToRownames(df, paste(A, B, C, sep = ":"), .remove = FALSE)
    expected <- df
    rownames(expected) <- paste(df$A, df$B, df$C, sep = ":")
    expect_equal(result, expected)

    # test with single column with duplicate values
    expect_identical(
        mutateToRownames(data.frame(A = c(1, 1, 2)), A, .remove = FALSE),
        data.frame(A = c(1, 1, 2), row.names = c("1", "1.1", "2"))
    )
})

test_that("rownamesToCol converts row names to specified column", {
    df <- data.frame(Value = c(10, 20, 30))
    rownames(df) <- c("A", "B", "C")
    result <- rownamesToCol(df, "ID")
    expect_equal(result$ID, c("A", "B", "C"))
    expect_equal(result$Value, c(10, 20, 30))
    expect_false("row.names" %in% colnames(result))
})

test_that("rownamesToCol works with different column names", {
    df <- data.frame(Value = c(10, 20, 30))
    rownames(df) <- c("X", "Y", "Z")
    result <- rownamesToCol(df, "NewID")
    expect_equal(result$NewID, c("X", "Y", "Z"))
    expect_equal(result$Value, c(10, 20, 30))
})
