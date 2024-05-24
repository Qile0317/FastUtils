# Test getAvgHex function
test_that("Compute the Average of Hex Colors", {
  # Single input case
  expect_equal(getAvgHex("#FF0000", "#00FF00"), "#808000")
  expect_equal(getAvgHex("#000000", "#808080"), "#404040")
  expect_equal(getAvgHex("#000000", "#FFFFFF"), "#808080")
  
  # Vectorized input case
  inputs <- c("#000000", "#000000", "#000000")
  expected_output <- "#000000"
  expect_equal(getAvgHex(inputs), expected_output)
})

# TODO test scaleHex