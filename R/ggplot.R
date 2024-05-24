#' Get the xmin, xmax, ymin, ymax of a ggplot Object
#'
#' This function retrieves the minimum and maximum x and y dimensions of a ggplot object.
#'
#' @param plt A ggplot object.
#' 
#' @return A list with elements `xr` (a vector of xmin and xmax) and `yr` (a vector of ymin and ymax).
#' @keywords ggplot
#' @noRd
getPlotDims <- function(plt) {
    built_plt_layout <- ggplot2::ggplot_build(plt)$layout
    list(
        xr = built_plt_layout$panel_scales_x[[1]]$range$range,
        yr = built_plt_layout$panel_scales_y[[1]]$range$range
    )
}
