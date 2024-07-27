#' List Only Files in a Directory
#'
#' This function lists only the files in a specified directory, excluding
#' directories. It is useful when you need to process or analyze only the files
#' within a directory without including subdirectories. The [base::list.files()]
#' function lists both files and directories, so this function provides a more
#' convenient way to obtain just the files.
#'
#' @param dirPath Character. The path to the directory from which to list files.
#' @param ... Additional arguments passed to [base::list.files()] (e.g.,
#' `pattern`, `recursive`). Note that `full.names` will be ignored.
#'
#' @return A character vector of file paths.
#' @export
#' @keywords fileSystem
#'
#' @examples
#' \donttest{
#' listFiles(getwd())
#' listFiles(getwd(), pattern = "\\.R$", recursive = TRUE)
#' }
#'
listFiles <- function(dirPath, ...) {

    assertthat::assert_that(assertthat::is.string(dirPath))

    filesAndDirs <- base::do.call(
        list.files,
        base::append(
            list(path = dirPath, full.names = TRUE),
            rmByName(list(...), "full.names", silent = TRUE)
        )
    )

    filesAndDirs[!base::file.info(filesAndDirs)$isdir]
}
