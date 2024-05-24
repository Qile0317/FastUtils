#' @export
initV <- function(typeFunc, x, initVal = NULL) {
    if (is(typeFunc, "character")) {
        v <- vector(typeFunc, x)
    } else {
        v <- typeFunc(x)
    }
    if (is.null(initVal)) return(v)
    sapply(v, function(i) initVal)
}

#' @export
initEmptyTable <- function() {
    structure(
        integer(0),
        dim = 0L,
        dimnames = structure(list(NULL), names = ""),
        class = "table"
    )
}

#' @export
tableToNumeric <- function(x) {
    structure(as.numeric(x), names = names(x))
}

#' @export
namedNumericToTable <- function(x) {
    output <- as.integer(x)
    names(output) <- names(x)
    output <- as.table(output)
    names(dimnames(output)) <- ""
    output
}

#' @export
createHash <- function(keys, init_vals = NULL) {
    keys <- unique(keys)
    numkeys <- length(keys)
    switch(as.character(numkeys),
        "0" = hash::hash(),
        "1" = hash::hash(keys, init_vals),
        hash::hash(keys, initList(numkeys, init_vals))
    )
}
