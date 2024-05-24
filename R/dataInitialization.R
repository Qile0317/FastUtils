#' @export
EmptyTable <- function() {
    structure(
        integer(0),
        dim = 0L,
        dimnames = structure(list(NULL), names = ""),
        class = "table"
    )
}

#' @export
initList <- function(x, initVal = NULL) {
    l <- vector("list", x)
    if (!is_an_integer(x)) names(l) <- as.character(x)
    if (is.null(initVal)) return(l)
    lapply(l, function(x) initVal)
}

#' @export 
getlast <- function(x, n = 1) {
    index <- length(x) - n + 1
    if (is.list(x)) return(x[[index]])
    x[index]
}
