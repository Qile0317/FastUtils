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

#' Fix Column Names
#'
#' @description 
#' `r lifecycle::badge("experimental")`
#'
#' This function fixes the column names of a given object so that all words are spaced by a specified delimiter, 
#' and any special characters are replaced according to a substitution map.
#'
#' @param object A data frame or matrix.
#' @param invalidRegex A character string containing a regular expression pattern for invalid characters to replace. Default is "( )|(\\()|(\\))|(\\.)|(/)".
#' @param spacing A character string to replace invalid characters with. Default is "_".
#' @param subMap A named list where the names are regular expressions and the values are the replacement strings. These substitutions are applied before `.subMap`.
#' @param .subMap A named list where the names are regular expressions and the values are the replacement strings. These substitutions are applied after `subMap`. Default is list("\\+" = "plus").
#' @param unique A logical indicating whether to ensure unique column names by appending a suffix if necessary. Default is FALSE.
#'
#' @return The data frame or matrix with fixed column names.
#' @export
#' @keywords wrangling
#' @examples
#' # Fix column names of a data frame
#' df <- data.frame(`A (1)` = c(1, 2, 3), `B/C` = c(4, 5, 6), `D+E` = c(7, 8, 9))
#' fixColnames(df)
fixColnames <- function(
    object,
    invalidRegex = "( )|(\\()|(\\))|(\\.)|(/)",
    spacing = "_",
    subMap = NULL,
    .subMap = list(
        "%+" = "pct",
        "\\$+" = "dollars",
        "\\++" = "plus",
        "-+" = "minus",
        "\\*+" = "star",
        "#+" = "cnt",
        "&+" = "and",
        "@+" = "at"
    ),
    unique = FALSE
) {

    assertthat::assert_that(is.character(invalidRegex) && length(invalidRegex) == 1)
    assertthat::assert_that(is.character(spacing) && length(spacing) == 1)

    subMap <- append(subMap, .subMap)

    # Apply all substitutions from the substitution maps
    newColnames <- colnames(object)
    for (pattern in names(subMap)) {
        replacement <- subMap[[pattern]]
        newColnames <- gsub(pattern, replacement, newColnames)
    }

    gsubr <- function(x, pattern, replacement) gsub(pattern, replacement, x)
    
    newColnames <- newColnames %>%
        gsubr(invalidRegex, spacing) %>%
        gsubr("([a-z])([A-Z])", "\\1_\\2") %>%
        (function(x) tolower(trimws(x))) %>%
        gsubr("(^_+|_+$)", "") %>%
        gsubr("_+", "_") %>%
        (function(x) if (unique) make.unique(x, sep = spacing) else x)

    # Assign the new column names to the object
    colnames(object) <- newColnames
    object
}
