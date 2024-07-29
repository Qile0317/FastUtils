test_that("Get the xmin, xmax, ymin, ymax of a ggplot Object", {
    result <- getPlotDims(
        ggplot2::ggplot(
            data.frame(x = c(1, 2, 3), y = c(1, 2, 3))
        ) + ggplot2::geom_point(ggplot2::aes(x, y))
    )
    expect_equal(result$xr, c(1, 3))
    expect_equal(result$yr, c(1, 3))
})
