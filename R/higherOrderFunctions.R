#' Create a Mutator Function
#'
#' This function creates a mutator function based on a specified binary operator. The mutator function updates a variable in the parent frame by applying the binary operator with a given value.
#'
#' @param binaryOperator A binary operator function to apply for the mutation.
#' 
#' @return A function that takes a variable and a value, applying the binary operator to update the variable in the parent frame.
#' @export
#' @keywords higherOrderFunctions
createMutator <- function(binaryOperator) {
    function(var, val) {
        eval(
            call("<-", substitute(var), binaryOperator(var, val)),
            envir = parent.frame()
        )
    }
}
