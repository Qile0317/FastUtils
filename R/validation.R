#' Validate Object
#'
#' This function validates an object using a list of checks. If any check
#' fails, an error handler is called and a default value is returned. This
#' function is intended to slightly simplify cases where a long list of
#' complex and convoluted predetermined checks are needed. For simpler cases
#' like type checking, it is recommended to use [stopifnot()] or
#' [assertthat::assert_that()].
#'
#' @param obj The object to validate.
#' @param checks A single function or list of functions, each taking the object
#' as an argument and returning NULL if the check passes or an error message if
#' the check fails.
#' @param errorHandler A function to handle errors, taking the error message as
#' an argument. Default is `warning`.
#' @param defaultReturn The value to return if any check fails. Default is NULL.
#'
#' @return The original object if all checks pass, or `defaultReturn` if any
#' check fails.
#' @export
#' @keywords validation
#' @examples
#' # Define some checks
#' checkNotNull <- function(x) if (is.null(x)) "Object is NULL" else NULL
#' checkIsNumeric <- function(x) {
#'     if (!is.numeric(x)) "Object is not numeric" else NULL
#' }
#'
#' # Validate an object
#' obj <- 42
#' validateObject(obj, list(checkNotNull, checkIsNumeric))
#'
#' # Validate an object that fails a check
#' obj <- NULL
#' try(
#'     validateObject(
#'         obj,
#'         list(checkNotNull, checkIsNumeric), errorHandler = stop)
#'     ),
#'     silent = TRUE
#' )
validateObject <- function(
    obj, checks, errorHandler = warningp, defaultReturn = NULL
) {
    if (is.function(checks)) checks <- list(checks)
    for (check in checks) {
        message <- check(obj)
        if (!is.null(message)) {
            errorHandler(message)
            return(defaultReturn)
        }
    }
    return(obj)
}

#' Custom Stop Function Without Call
#'
#' This function provides a wrapper around the base [stop()] function,
#' but it automatically sets \code{call.} to FALSE, which means the function
#' call itself is not included in the resulting error message. This makes error
#' messages cleaner.
#' The \code{domain} argument can be used to specify a translation domain.
#'
#' @param ... Arguments passed on to \code{stop}.
#' @param domain The translation domain, NULL by default.
#'
#' @return No return value, this function stops execution of the program.
#' @export 
#' @keywords validation
#' @seealso [stop()]
#' @examples
#' \donttest{
#' try(stopp("This is a custom stop message without the call."), silent = TRUE)
#' }
#' @export
stopp <- function(..., domain = NULL) {
    stop(..., call. = FALSE, domain = domain)
}

#' Custom Warning Function Without Call
#'
#' This function provides a wrapper around the base \code{\link[base]{warning}}
#' function, adding flexibility to warnings by setting \code{call.} to FALSE
#' automatically. This modification means that the function call is not included
#' in the warning message, streamlining the output for users.
#'
#' @param ... Arguments passed on to \code{warning}.
#' @return No return value, this function issues a warning.
#' @export
#' @keywords validation
#' @seealso \code{\link[base]{warning}}
#' @examples
#' \donttest{
#' try(warningp(
#'     "This is a custom warning message without the call."
#' ), silent = TRUE)
#' }
warningp <- function(...) {
    do.call(base::warning, args = append(list(call. = FALSE), list(...)))
}

#' Get Failure Message as a character
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function generates a failure message string from a given condition.
#' The message includes the context of the call and the specific condition
#' message.
#'
#' @param cond A condition object representing an error or warning - probably
#' from a [tryCatch()] statement.
#'
#' @return A character string containing the failure message.
#' @export
#' @keywords validation
#' @examples
#' tryCatch(stop("Example error"), error = function(e) getFailStr(e))
getFailStr <- function(cond) {
    callStr <- .getCallStr(cond)
    paste0( # TODO error / warning before in
        ifelse(identical(callStr, ""), "", paste0("in ", callStr, ": ")),
        conditionMessage(cond)
    )
}

.getCallStr <- function(cond) {

    condSplit <- as.character(conditionCall(cond))

    if (length(condSplit) == 0) {
        return("")
    }

    paste0(
        condSplit[1],
        encloseBr(paste0(condSplit[-1], collapse = ", "))
    )

}
