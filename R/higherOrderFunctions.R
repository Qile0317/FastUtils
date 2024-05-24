#' @export
createMutator <- function(binary_operator) {
    function(var, val) {
        eval(
            call("<-", substitute(var), binary_operator(var, val)),
            envir = parent.frame()
        )
    }
}
