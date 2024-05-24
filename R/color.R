#' @export
#' @source https://stackoverflow.com/questions/649454
#'
getAvgHex <- function(...) {
    Reduce(function(hex1, hex2) {
        grDevices::rgb(
            t((grDevices::col2rgb(hex1) + grDevices::col2rgb(hex2)) / 2),
            maxColorValue = 255
        )
    }, unlist(list(...)))
}

#' @export
scaleHex <- function(hex, scaleFactor) {

    if (all(scaleFactor == 1)) return(hex)

    hsv_color <- hex %>%
        grDevices::col2rgb() %>%
        grDevices::rgb2hsv()

    hsv_color["v", ] <- bound(hsv_color["v", ] * scaleFactor, 0, 1)

    grDevices::hsv(hsv_color["h", ], hsv_color["s", ], hsv_color["v", ])
}
