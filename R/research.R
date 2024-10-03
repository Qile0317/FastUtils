#' @title Archive and clean Package Directory
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function processes an R package directory, removing irrelevant files
#' (e.g., `.git`, `.gitignore`-listed files, etc.) and strips parts of lines
#' matching a specified regex from source code. It then creates a clean folder
#' or a zipped archive of the package without modifying the original directory.
#'
#' MAJOR NOTE this is no where near perfect and the regexes used at the moment
#' for removing comments are prone to issues such as not detecting whether
#' the symbols are inside special strings.
#'
#' @param packageDir A string specifying the path to the package directory.
#' @param output A string specifying the name of the output cleaned directory.
#' If the string ends with .zip, the function will create a zipped archive of
#' the cleaned directory.
#' @param removeHidden Logical; if `TRUE`, removes hidden files and folders
#' (starting with a dot). Default is `TRUE`.
#' @param removeGitignore Logical; if `TRUE`, removes files listed in the
#' `.gitignore`. Default is `TRUE`.
#' @param verbose Logical; if `TRUE`, prints additional information about the
#' cleaning process. Default is `FALSE`.
#'
#' @details
#' The function cleans the package directory by:
#' - Removing hidden files and folders like `.git`.
#' - Excluding files listed in `.gitignore`.
#' - Stripping out parts of lines matching a regex pattern from source files.
#'
#' The output will be either a cleaned directory or a zipped archive of it.
#'
#' @return A cleaned directory or a zipped file of the package directory.
#' @keywords research
#'
#' @examples
#' \dontrun{
#' # Clean and archive the package directory
#' archiveCleanedPackage("/path/to/package", "clean_package.zip")
#'
#' # Clean package directory without zipping it
#' archiveCleanedPackage("/path/to/package", NULL)
#' }
#' @export
archiveCleanedPackage <- function(
    packageDir, output, removeHidden = TRUE,
    removeGitignore = TRUE, verbose = FALSE
) {

    assert_that(is.string(packageDir), dir.exists(packageDir))
    assert_that(is.string(output))
    assert_that(is.flag(removeHidden))
    assert_that(is.flag(removeGitignore))
    assert_that(is.flag(verbose))

    tempDir <- tempfile()
    dir.create(tempDir)

    gitignorePatterns <-
        if (removeGitignore)
            getGitignorePatterns(packageDir)
        else
            NULL

    copyCleanedRFiles(
        packageDir, tempDir, patterns = gitignorePatterns,
        removeHidden = removeHidden, verbose = verbose
    )

    if (grepl("*\\.zip$", output)) {

        if (verbose) message("Creating zip archive at: ", output)

        zip(
            zipfile = output,
            files = list.files(tempDir, full.names = TRUE),
            flags = "-r"
        )

    } else {
        dir.create(output, showWarnings = FALSE)

        R.utils::copyDirectory(
            tempDir, output, recursive = TRUE
        )

        if (verbose)
            message("Cleaned package directory created at: ", output)
    }

    unlink(tempDir, recursive = TRUE)
}

getGitignorePatterns <- function(dir) {

    gitignorePath <- file.path(dir, ".gitignore")

    if (!file.exists(gitignorePath)) return(NULL)

    gitignoreLines <- readLines(gitignorePath)

    patterns <- gitignoreLines[
        (!grepl("^#", gitignoreLines)) &
            gitignoreLines != "" &
            gitignoreLines != "\n"
    ]

    return(patterns)
}

stripLinesAfterRegex <- function(lines, regex) {
    gsub(paste0("\\s*", regex, ".*"), "", lines, perl = TRUE)
}

stripTrailingWhiteSpace <- function(lines) {
    gsub("\\s+$", "", lines)
}

ensureSingleNewLineAtEnd <- function(lines) {
    lines <- lines[rev(cumsum(rev(lines != "")) > 0)]
    if (tail(lines, 1) != "") {
        lines <- c(lines, "")
    }
    lines
}


copyCleanedRFiles <- function(
    from, to, patterns = NULL, removeHidden = TRUE, verbose = FALSE
) {

    dir.create(to, showWarnings = FALSE, recursive = TRUE)

    files <- list.files(
        from, all.files = TRUE, recursive = TRUE, full.names = TRUE
    )

    if (removeHidden) files <- files[!grepl("/\\.", files)]

    if (!is.null(patterns)) {
        for (pattern in patterns) {
            files <- files[!grepl(pattern, files, perl = TRUE)]
        }
    }

    for (file in files) {
        relativePath <- sub(paste0("^", from), "", file)
        destPath <- file.path(to, relativePath)

        if (dir.exists(file)) {
            dir.create(destPath, showWarnings = FALSE, recursive = TRUE)
            next
        }

        if (verbose) message("Processing file: ", file)
        fileLines <- readLines(file, warn = FALSE)

        # this regex isnt perfect either
        if (grepl("\\.(R|r|Rmd|rmd|Rnw|rnw|cpp|hpp|c|h|rs)$", file)) {

            # this isnt perfect because this might be in a string
            cleanedLines <- fileLines %>%
                stripLinesAfterRegex(" *#+ *(TODO|FIXME|BUG|HACK)") %>%
                stripTrailingWhiteSpace() %>%
                ensureSingleNewLineAtEnd()

            if (verbose)
                message("Stripped lines matching TODO/FIXME from: ", file)

        } else {
            cleanedLines <- fileLines
        }

        writeLines(cleanedLines, destPath)
    }
}
