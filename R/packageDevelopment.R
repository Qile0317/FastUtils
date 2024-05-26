#' Custom Stop Function Without Call
#'
#' This function provides a wrapper around the base \code{\link[base]{stop}} function,
#' but it automatically sets \code{call.} to FALSE, which means the function call itself
#' is not included in the resulting error message. This makes error messages cleaner.
#' The \code{domain} argument can be used to specify a translation domain.
#'
#' @param ... Arguments passed on to \code{stop}.
#' @param domain The translation domain, NULL by default.
#' @return No return value, this function stops execution of the program.
#' @seealso \code{\link[base]{stop}}
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
#' @seealso \code{\link[base]{warning}}
#' @examples
#' \dontrun{
#'   warningp("This is a custom warning message without the call.")
#' }
#' @export
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
#' Note that it is far from perfect at the moment - it simply uses regex.
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

    getAllPkgKeywords(rdFiles) %>%
        ifelse(asDistribution, base::table, base::unique)() %>%
        sort()
}

getAllPkgKeywords <- function(rdFilePaths) {

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

    keywords
}

# syncYMLKeywordRefs <- function(
#     pkg = ".",
#     ymlPath = "_pkgdown.yml",
#     mode = c("a", "o"), # append or override
#     sectionName = "reference",
#     modifier = identity # TODO
# ) {
  
#     fullYmlPath <- getExistingFilePath(dir = pkg, filePath = ymlPath)
    
#     keywords <- getPkgKeywords(pkg = pkg, asDistribution = FALSE)
#     if (length(keywords) == 0) {
#         stop("No keywords found in package documentation.")
#     }

#     yamlContent <- yaml::read_yaml(fullYmlPath)

#     # try find the section yamlContent[[sectionName]], else return addYMLKeywordRefs with the appropriate arguments and return
#     # without nesting an else statement:
#     # use the yml list object from yamlContent, and if mode is o, create the content list and append to the yamlContent[[sectionName]]
#     # finally, write to the yaml with yaml::write_yaml
# }

#' Add or Sync Keyword References in Pkgdown YAML File
#'
#' Updates or creates the `_pkgdown.yml` file in a specified R package directory.
#' It appends or overwrites entries in a YAML section based on unique keywords extracted
#' from the package documentation, transformed using `trySplitVar` for more readable titles.
#' 
#' @param pkg The path to the R package directory.
#' @param ymlPath The path to the `_pkgdown.yml` file, relative or absolute.
#'        Default is "_pkgdown.yml" within the package directory.
#' @param mode Character string, "append" or "overwrite", determining how keywords are handled.
#' @param sectionName The name of the YAML section to modify. Default is "reference".
#' @param modifier A function to modify the keyword list; defaults to `trySplitVar`.
#' @export
#' @keywords packageDevelopment
syncYMLKeywordRefs <- function(
    pkg = ".",
    ymlPath = "_pkgdown.yml",
    mode = "append",
    sectionName = "reference",
    modifier = trySplitVar
) {
    stop("unfinished")
    fullYmlPath <- getExistingFilePath(dir = pkg, filePath = ymlPath)
    keywords <- getPkgKeywords(pkg = pkg)
    if (length(keywords) == 0) {
        stop("No keywords found in package documentation.")
    }

    modifiedKeywords <- lapply(keywords, modifier)
    
    if (!file.exists(fullYmlPath)) {
        yamlContent <- list()
    } else {
        yamlContent <- yaml::read_yaml(fullYmlPath)
    }

    if (mode == "overwrite") {
        yamlContent[[sectionName]] <- list(keywords = unlist(modifiedKeywords))
    } else { # append
        existingKeywords <- yamlContent[[sectionName]]$keywords
        if (is.null(existingKeywords)) existingKeywords <- list()
        newKeywords <- unique(c(existingKeywords, unlist(modifiedKeywords)))
        yamlContent[[sectionName]] <- list(keywords = newKeywords)
    }

    yaml::write_yaml(yamlContent, fullYmlPath)
}

#' @rdname syncYMLKeywordRefs
#' @export
sykr <- syncYMLKeywordRefs

# addYMLKeywordRefs
addYMLKeywordRefs <- function(
    pkg = ".",
    ymlPath = "_pkgdown.yml",
    sectionName = "reference"
) {
    fullYmlPath <- getExistingFilePath(dir = pkg, filePath = ymlPath)
    keywords <- getPkgKeywords(pkg = pkg, asDistribution = FALSE)
    if (length(keywords) == 0) {
        stop("No keywords found in package documentation.")
    }

    # unfinished
    stop("unfinished")
}

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
#' # Assuming a file 'example.txt' exists in the current directory
#' getExistingFilePath("example.txt")
#'
getExistingFilePath <- function(filePath, dir = ".") {
    if (!dir.exists(dir)) stopp("Specified directory does not exist.")
    fullFilePath <- file.path(dir, filePath)
    if (!file.exists(fullYmlPath)) stopp("The specified file was not found.")
    fullFilePath
}

# TODO function to modify roxygen files and add keyword section to everything
# based on filename
