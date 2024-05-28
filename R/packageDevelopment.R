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
#' @keywords packageDevelopment
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
#' @keywords packageDevelopment
#' @seealso \code{\link[base]{warning}}
#' @examples
#' \dontrun{
#'   warningp("This is a custom warning message without the call.")
#' }
warningp <- function(...) {
    do.call(base::warning, args = append(list(call. = FALSE), list(...)))
}

#' Get Keywords from R Package Documentation
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function retrieves keywords from all package documentation files located
#' in the `/man` directory of the specified R package. It can return a unique list
#' of keywords or a frequency distribution of these keywords as a `table` object,
#' sorted by the keys. 
#'
#' Note that the "internal" keyword is ignored.
#'
#' @param pkg The path to the R package directory.
#' @param asDistribution Logical; if FALSE, returns a character vector of unique keywords. If TRUE, returns a table with the frequency of each keyword.
#'
#' @return If \code{asDistribution} is FALSE, a character vector of unique keywords is returned.
#'         If \code{asDistribution} is TRUE, a table of keywords and their frequencies is returned.
#'         If no keywords were detected, returns a character of length 0.
#' @export
#' @keywords packageDevelopment
#' 
#' @examples
#' getPkgKeywords()
#' getPkgKeywords(asDistribution = TRUE)
#'
getPkgKeywords <- function(pkg = ".", asDistribution = FALSE) {

    manDir <- file.path(pkg, "man")
    if (!dir.exists(manDir)) {
        warning(
            "no `/man` folder detected - ensure the package path is correct"
        )
        return(character(0))
    }

    rdFiles <- list.files(manDir, pattern = "\\.Rd$", full.names = TRUE)
    if (length(rdFiles) == 0) {
        warning("no `.Rd` files were found in the `/man` directory")
        return(character(0))
    }

    getPkgKeywordsNoCheck(rdFiles, asDistribution)
}

getPkgKeywordsNoCheck <- function(
    rdFiles, asDistribution, doErrorIfNoKw = FALSE
) {

    keywords <- getAllPkgKeywords(rdFiles) %>%
        ifelse(asDistribution, base::table, base::unique)() %>%
        sort()
    
    if (doErrorIfNoKw && (length(keywords) == 0)) {
        stopp("No keywords found in package documentation.")
    }

    keywords
}

getAllPkgKeywords <- function(rdFilePaths, doErrorIfNoKw = FALSE) {

    keywords <- character()

    for (file in rdFilePaths) {
        lines <- readLines(file, warn = FALSE)
        keywordLines <- grep("\\\\keyword\\{", lines, value = TRUE)
        extractedKeywords <- regmatches(
            keywordLines,
            gregexpr("(?<=\\\\keyword\\{)[^}]*", keywordLines, perl = TRUE)
        )
        if (length(extractedKeywords) > 0) {
            keywords <- c(keywords, unlist(extractedKeywords))
        }
    }

    keywords <- keywords[keywords != "internal"]
    keywords
}

#' Find Missing Sections in Rd Files
#'
#' This function scans Rd documentation files in the specified package's `\man` directory
#' to identify which functions lack certain documentation sections like `\examples`. If
#' there are no missing sections in all the Rd files, then the output is a `character(0)`
#'
#' @param sectionName A character vector of the Rd sections to look for.
#' @param pkg The path to the package directory, defaulting to the current directory ".".
#' @param ignore Additional Regexes of *function names* to be ignored in the output.
#' @param .ignore More regexes of functions to ignore set by default. Will be appended with
#' the `ignore` regexes and unioned with [joinRegex()].
#' 
#' @return Character vector of function names that are missing any of the
#' specified sections in their Rd files. May be length 0 if all fulfill criteria.
#' @export
#' @keywords packageDevelopment
#' @examples
#' findMissingRdSections(c("examples", "example"), pkg = ".")
#' 
findMissingRdSections <- function(
    sectionName, pkg = ".", ignore = NULL, .ignore = "-package$"
) {
  
    rdPath <- file.path(pkg, "man")
    rdFiles <- list.files(rdPath, pattern = "\\.Rd$", full.names = TRUE)

    missingFunctionNames <- character(0)

    for (file in rdFiles) {
        rdContent <- readLines(file)

        sectionMissing <- sapply(paste("\\", sectionName, "{", sep = ""), function(section) {
            all(grepl(section, rdContent, fixed = TRUE) == FALSE)
        })

        if (!any(sectionMissing)) next

        missingFunctionNames <- c(
            missingFunctionNames,
            tools::file_path_sans_ext(basename(file))
        )
    }

    Filter(function(x) !grepl(joinRegex(.ignore, ignore), x), missingFunctionNames)
}

#' @rdname findMissingRdSections
#' @export
fmrs <- findMissingRdSections

#' Get Existing File Path
#'
#' This function checks whether a specified file exists within a given directory.
#' It is a simple wrapper that helps in error handling by throwing an error if the
#' directory does not exist or if the file is not found within that directory. 
#' This ensures that only valid file paths are returned.
#'
#' @param filePath The relative or absolute path to the file.
#' @param dir The directory in which to check for the file, default is the current directory ('.').
#'
#' @return The full path to the file if it exists.
#' @export
#' @keywords packageDevelopment
#'
#' @examples
#' \dontrun{
#' getExistingFilePath("DESCRIPTION")
#' }
getExistingFilePath <- function(filePath, dir = ".") {
    if (!dir.exists(dir)) stopp("Specified directory does not exist.")
    fullFilePath <- file.path(dir, filePath)
    if (!file.exists(fullFilePath)) stopp("The specified file was not found.")
    fullFilePath
}

# TODO function to modify roxygen files and add keyword section to everything
# based on filename
