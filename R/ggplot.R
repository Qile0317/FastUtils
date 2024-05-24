#' @title Get the xmin, xmax, ymin, ymax of a ggplot object
#' @return list(xr = c(xmin, xmax), yr = c(ymin, ymax))
#' @noRd
getPlotDims <- function(plt) {
    built_plt_layout <- ggplot2::ggplot_build(plt)$layout
    list(
        xr = built_plt_layout$panel_scales_x[[1]]$range$range,
        yr = built_plt_layout$panel_scales_y[[1]]$range$range
    )
}
