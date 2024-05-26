#' Convert a Column to Row Names
#'
#' This function converts a specified column of a data frame to row names.
#'
#' @param df A data frame.
#' @param colname A character string specifying the name of the column to convert to row names.
#' @param matrix A logical indicating whether to return the result as a matrix. Default is FALSE.
#' 
#' @return A data frame with the specified column as row names. If `matrix` is TRUE, a matrix is returned.
#' @export
#' @keywords wrangling
#' @examples
#' # Convert the 'ID' column to row names
#' df <- data.frame(ID = c("A", "B", "C"), Value = c(10, 20, 30))
#' colToRownames(df, "ID")
#' # Convert the 'ID' column to row names and return as matrix
#' colToRownames(df, "ID", matrix = TRUE)
colToRownames <- function(df, colname, matrix = FALSE) {
    df <- as.data.frame(df)
    rownames(df) <- df[[colname]]
    df[[colname]] <- NULL
    if (!matrix) return(df)
    as.matrix(df)
}

#' Convert Row Names to a Column
#'
#' This function converts the row names of a data frame to a specified column.
#'
#' @param df A data frame.
#' @param colname A character string specifying the name of the new column to contain the row names.
#' 
#' @return A data frame with the row names converted to a column.
#' @export
#' @keywords wrangling
#' @examples
#' # Convert row names to a column named 'ID'
#' df <- data.frame(Value = c(10, 20, 30))
#' rownames(df) <- c("A", "B", "C")
#' rownamesToCol(df, "ID")
rownamesToCol <- function(df, colname) {
    df <- dplyr::mutate(df, !!dplyr::sym(colname) := rownames(df)) %>%
        `rownames<-`(NULL)
    df
}
