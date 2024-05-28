#' Compute the Average of Hex Colors
#'
#' This function computes the average color of the provided hex color values.
#'
#' @param ... Hex color values as character strings. Could also be any number
#' of character vectors (including lists) which will all be coerced into a single
#' characters, assuming they are valid hex codes.
#'
#' @return A single hex color character representing the average of the input colors.
#' @export
#' @keywords color
#'
#' @source <https://stackoverflow.com/questions/649454>
#'
#' @examples
#' getAvgHex("#00000", "#FF00FF")
#' getAvgHex(c("#008040", "#00000", "#FF00FF"))
#' 
#' # very nonstandard but possible way to input hexes. Essentially,
#' any combination of vectors will work.
#' getAvgHex(list("#008040", "#000000"), "#FF00FF", c("#FF00FF"))
#'
getAvgHex <- function(...) {

    hex_vector <- unlist(list(...))

    Reduce(add, lapply(hex_vector, grDevices::col2rgb)) %>%
        divide(length(hex_vector)) %>%
        round() %>%
        t() %>%
        grDevices::rgb(maxColorValue = 256)
}

#' Scale the Brightness of a Hex Color
#'
#' This function scales the brightness of hex colors by a given factor.
#'
#' @param hex Hex color values as characters.
#' @param scaleFactor A numeric value to scale the brightness. A value of 1 returns the original color.
#'
#' @return A hex color value with adjusted brightness.
#' @export
#' @keywords color
#' @examples
#' scaleHex("#404040", 2)
#' 
scaleHex <- function(hex, scaleFactor) {

    if (all(scaleFactor == 1)) return(hex)

    hsv_color <- hex %>%
        grDevices::col2rgb() %>%
        grDevices::rgb2hsv()

    hsv_color["v", ] <- bound(hsv_color["v", ] * scaleFactor, 0, 1)

    grDevices::hsv(hsv_color["h", ], hsv_color["s", ], hsv_color["v", ])
}
