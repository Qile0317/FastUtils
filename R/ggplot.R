#' Get the xmin, xmax, ymin, ymax of a ggplot Object
#'
#' This function retrieves the minimum and maximum x and y dimensions of a ggplot object.
#' Note that it is the dimension of the plot within the x and y axis and not the dimensions
#' of the actual output image itself. This may be useful for numerical computations when
#' modifying plots, but can be slow since it builds the actual plot first.
#'
#' @param plt A ggplot object.
#' 
#' @return A list with elements `xr` (a vector of xmin and xmax) and `yr` (a vector of ymin and ymax).
#' @export
#' @keywords ggplot
#' @examples
#' getPlotDims(ggplot(mtcars) + geom_point(aes(mpg, cyl)))
#' 
getPlotDims <- function(plt) {
    built_plt_layout <- ggplot2::ggplot_build(plt)$layout
    list(
        xr = built_plt_layout$panel_scales_x[[1]]$range$range,
        yr = built_plt_layout$panel_scales_y[[1]]$range$range
    )
}
