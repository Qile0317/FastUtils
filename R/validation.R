#' Validate Object
#'
#' This function validates an object using a list of checks. If any check fails, an error handler is called and a default value is returned.
#'
#' @param obj The object to validate.
#' @param checks A list of functions, each taking the object as an argument and returning NULL if the check passes or an error message if the check fails.
#' @param errorHandler A function to handle errors, taking the error message as an argument. Default is `warning`.
#' @param defaultReturn The value to return if any check fails. Default is NULL.
#' 
#' @return The original object if all checks pass, or `defaultReturn` if any check fails.
#' @export
#' @keywords validation
#' @examples
#' # Define some checks
#' checkNotNull <- function(x) if (is.null(x)) "Object is NULL" else NULL
#' checkIsNumeric <- function(x) if (!is.numeric(x)) "Object is not numeric" else NULL
#' 
#' # Validate an object
#' obj <- 42
#' validateObject(obj, list(checkNotNull, checkIsNumeric))
#' 
#' # Validate an object that fails a check
#' obj <- NULL
#' try(validateObject(obj, list(checkNotNull, checkIsNumeric), errorHandler = stop), silent = TRUE)
validateObject <- function(obj, checks, errorHandler = warning, defaultReturn = NULL) {
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
#' This function provides a wrapper around the base \code{\link[base]{stop}} function,
#' but it automatically sets \code{call.} to FALSE, which means the function call itself
#' is not included in the resulting error message. This makes error messages cleaner.
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
#' \dontrun{
#'   stopp("This is a custom stop message without the call.")
#' }
#' @export
stopp <- function(..., domain = NULL) {
    stop(..., call. = FALSE, domain = domain)
}

#' Custom Warning Function Without Call
#'
#' This function provides a wrapper around the base \code{\link[base]{warning}} function,
#' adding flexibility to warnings by setting \code{call.} to FALSE automatically. This 
#' modification means that the function call is not included in the warning message,
#' streamlining the output for users.
#'
#' @param ... Arguments passed on to \code{warning}.
#' @return No return value, this function issues a warning.
#' @export 
#' @keywords validation
#' @seealso \code{\link[base]{warning}}
#' @examples
#' \dontrun{
#'   warningp("This is a custom warning message without the call.")
#' }
warningp <- function(...) {
    do.call(base::warning, args = append(list(call. = FALSE), list(...)))
}
