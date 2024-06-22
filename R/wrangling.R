#' Convert a Column to Row Names
#'
#' This function converts a specified column of a data frame to row names.
#'
#' @param df A data frame.
#' @param col A character string specifying the name of the column to convert to row names.
#' @param .remove A logical indicating whether to remove the selected column. Default is TRUE.
#'
#' @return A data frame with the specified column as row names.
#' @export
#' @keywords wrangling
#' @seealso [mutateToRownames()]
#' @examples
#' # Convert the 'ID' column to row names
#' df <- data.frame(ID = c("A", "B", "C"), Value = c(10, 20, 30))
#' colToRownames(df, "ID")
#' # Convert the 'ID' column to row names and keep the column
#' df <- data.frame(ID = c("A", "B", "C"), Value = c(10, 20, 30))
#' colToRownames(df, "ID", .remove = FALSE)
colToRownames <- function(df, col, .remove = TRUE) {
    df <- as.data.frame(df)
    rownames(df) <- df[[col]]
    if (isTRUE(.remove)) df[[col]] <- NULL
    df
}

#' Mutate columns to Row Names
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function sets new row names for a data frame based on a tidy evaluation expression.
#'
#' @param .data A data frame.
#' @param expr A tidy evaluation expression specifying the columns to use for the new row names.
#' @param .remove A logical indicating whether to remove the selected columns. Default is TRUE.
#'
#' @return A data frame with updated row names.
#' @export
#' @keywords wrangling
#' @examples
#' library(magrittr)
#' mtcars %>%
#'     head() %>%
#'     mutateToRownames(wt + 3*vs)
#'
mutateToRownames <- function(.data, expr, .remove = TRUE) {
    colExpr <- rlang::enquo(expr)
    rownames(.data) <- dplyr::mutate(.data, .rowname = !!colExpr)$.rowname
    if (isTRUE(.remove)) return(.data)
    .data %>% dplyr::select(-dplyr::one_of(all.vars(colExpr)))
}

#' Convert Row Names to a Column
#'
#' This function converts the row names of a data frame to a specified column.
#'
#' @param df A data frame.
#' @param colname A character string specifying the name of the new column to
#' contain the row names. Defaults to "rownames".
#'
#' @return A data frame with the row names converted to a column.
#' @export
#' @keywords wrangling
#' @examples
#' # Convert row names to a column named 'ID'
#' df <- data.frame(Value = c(10, 20, 30))
#' rownames(df) <- c("A", "B", "C")
#' rownamesToCol(df, "ID")
rownamesToCol <- function(df, colname = "rownames") {
    df <- dplyr::mutate(df, !!dplyr::sym(colname) := rownames(df)) %>%
        `rownames<-`(NULL)
    df
}

#' Set Column Names
#'
#' This function sets new column names for a given data frame or matrix.
#'
#' @param object A data frame or matrix.
#' @param newColnames A character vector specifying the new column names.
#' 
#' @return The data frame or matrix with updated column names.
#' @export
#' @keywords wrangling
#' @examples
#' # Set new column names for a data frame
#' df <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
#' setColnames(df, c("X", "Y"))
setColnames <- function(object, newColnames) {
    colnames(object) <- newColnames
    object
}

#' Set Row Names
#'
#' This function sets new row names for a given data frame or matrix.
#'
#' @param object A data frame or matrix.
#' @param newRownames A character vector specifying the new row names.
#'
#' @return The data frame or matrix with updated row names.
#' @export
#' @keywords wrangling
#' @examples
#' # Set new row names for a data frame
#' df <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
#' setRownames(df, c("row1", "row2", "row3"))
setRownames <- function(object, newRownames) {
    rownames(object) <- newRownames
    object
}
