#' Get Keywords from R Package Documentation
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function retrieves keywords from all package documentation files located
#' in the `/man` directory of the specified R package. It can return a unique
#' list of keywords or a frequency distribution of these keywords as a `table`
#' object, sorted by the keys.
#'
#' Note that the "internal" keyword is ignored.
#'
#' @param pkg The path to the R package directory.
#' @param asDistribution Logical; if FALSE, returns a character vector of unique
#' keywords. If TRUE, returns a table with the frequency of each keyword.
#'
#' @return If \code{asDistribution} is FALSE, a sorted character vector of
#' unique keywords is returned. If \code{asDistribution} is TRUE, a table of
#' keywords and their frequencies is returned. If no keywords were detected,
#' returns a character of length 0.
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

    # get all keywords
    keywords <- character()

    for (file in rdFiles) {
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

    keywords[keywords != "internal"] %>%
        ifelse(asDistribution, base::table, base::unique)() %>%
        sort()
}

#' Find Missing Sections in Rd Files
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function scans Rd documentation files in the specified package's `\man`
#' directory to identify which functions lack certain documentation sections
#' like `\examples`. If there are no missing sections in all the Rd files, then
#' the output is a `character(0)`
#'
#' @param sectionName A character vector of the Rd sections to look for.
#' @param pkg The path to the package directory, defaulting to the current
#' directory ".".
#' @param ignore Additional Regexes of *function names* to be ignored in the
#' output.
#' @param .ignore More regexes of functions to ignore set by default. Will be
#' appended with
#' the `ignore` regexes and unioned with [joinRegex()].
#'
#' @return Character vector of function names that are missing any of the
#' specified sections in their Rd files. May be length 0 if all fulfill
#' criteria.
#' @export
#' @keywords packageDevelopment
#'
#' @examples
#' try(
#'   findMissingRdSections(c("examples", "example"), pkg = "."),
#'   silent = TRUE
#' )
#'
findMissingRdSections <- function(
    sectionName, pkg = ".", ignore = NULL, .ignore = "-package$" # are these used?
) {

    assertthat::assert_that(is.character(sectionName))
    assertthat::assert_that(assertthat::is.string(pkg))
    assertthat::assert_that(is.null(ignore) || is.character(ignore))

    paste("\\", sectionName, "{", sep = "") %>%
        greplDir(file.path(pkg, "man")) %>%
        (function(x) names(which(!x))) %>%
        basename()
}

#' @rdname findMissingRdSections
#' @export
fmrs <- findMissingRdSections

#' Remove New Snapshots from vdiffr Tests
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function removes new snapshots created by `vdiffr` tests. It is useful
#' when you want to remove new snapshots that were created during testing and
#' are no longer needed.
#'
#' @param pkg The path to the package directory.
#' @param snapDir The path to the directory containing the snapshots. Default
#' is `tests/testthat/_snaps`. If this directory isn't valid, nothing happens.
#'
#' @return NULL (invisible) - used for side effects
#' @export
#' @keywords testing
#'
#' @examples
#' /donttest{
#' removeVdiffrNewSnapShots()
#' }
#'
removeVdiffrNewSnapShots <- function(
    pkg = ".", snapDir = file.path("tests", "testthat", "_snaps")
) {

    assertthat::assert_that(assertthat::is.string(pkg))
    assertthat::assert_that(assertthat::is.string(snapDir))

    if (!dir.exists(snapDir)) return(invisible())

    newSnapshotPaths <- listFiles(
        snapDir, pattern = "\\.new\\.svg$", recursive = TRUE
    )

    if (length(newSnapshotPaths) == 0) return(invisible())

    sapply(newSnapshotPaths, base::file.remove)
    invisible()
}

#' @rdname removeVdiffrNewSnapShots
#' @export
rmns <- removeVdiffrNewSnapShots

# TODO function to modify roxygen files and add keyword section to everything
# based on filename
