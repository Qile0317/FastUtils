#' Compute the Average of Hex Colors
#'
#' This function computes the average color of the provided hex color values.
#'
#' @param ... Hex color values as character strings.
#' 
#' @return A single hex color value representing the average of the input colors.
#' @export
#' @keywords color
#' 
#' @source \url{https://stackoverflow.com/questions/649454}
getAvgHex <- function(...) {
    Reduce(function(hex1, hex2) {
        grDevices::rgb(
            t((grDevices::col2rgb(hex1) + grDevices::col2rgb(hex2)) / 2),
            maxColorValue = 255
        )
    }, unlist(list(...)))
}

#' Scale the Brightness of a Hex Color
#'
#' This function scales the brightness of a hex color by a given factor.
#'
#' @param hex A hex color value as a character string.
#' @param scaleFactor A numeric value to scale the brightness. A value of 1 returns the original color.
#' 
#' @return A hex color value with adjusted brightness.
#' @export
#' @keywords color
scaleHex <- function(hex, scaleFactor) {

    if (all(scaleFactor == 1)) return(hex)

    hsv_color <- hex %>%
        grDevices::col2rgb() %>%
        grDevices::rgb2hsv()

    hsv_color["v", ] <- bound(hsv_color["v", ] * scaleFactor, 0, 1)

    grDevices::hsv(hsv_color["h", ], hsv_color["s", ], hsv_color["v", ])
}
