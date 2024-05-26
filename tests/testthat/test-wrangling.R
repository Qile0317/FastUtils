test_that("colToRownames converts specified column to row names", {
  df <- data.frame(ID = c("A", "B", "C"), Value = c(10, 20, 30))
  result <- colToRownames(df, "ID")
  expect_equal(rownames(result), c("A", "B", "C"))
  expect_equal(result$Value, c(10, 20, 30))
  expect_false("ID" %in% colnames(result))
})

test_that("colToRownames returns a matrix if matrix is TRUE", {
  df <- data.frame(ID = c("A", "B", "C"), Value = c(10, 20, 30))
  result <- colToRownames(df, "ID", matrix = TRUE)
  expect_equal(rownames(result), c("A", "B", "C"))
  expect_equal(result[, "Value"], c(A = 10, B = 20, C = 30))
  expect_false("ID" %in% colnames(result))
  expect_true(is.matrix(result))
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
