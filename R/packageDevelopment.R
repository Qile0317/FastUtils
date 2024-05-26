#' Get Keywords from R Package Documentation
#'
#' This function retrieves keywords from all package documentation files located
#' in the `/man` directory of the specified R package. It can return a unique list
#' of keywords or a frequency distribution of these keywords as a `table` object.
#'
#' @param pkg The path to the R package directory.
#' @param asDistribution Logical; if FALSE, returns a character vector of unique keywords. If TRUE, returns a table with the frequency of each keyword.
#'
#' @return If \code{asDistribution} is FALSE, a character vector of unique keywords is returned.
#'         If \code{asDistribution} is TRUE, a table of keywords and their frequencies is returned.
#' @export
#' @keywords packageDevelopment
#' @examples
#' getPkgKeywords()
#' getPkgKeywords(asDistribution = TRUE)
#'
getPkgKeywords <- function(pkg = ".", asDistribution = FALSE) {

    manDir <- file.path(pkg, "man")
    if (!dir.exists(manDir)) {
        warning("no `/man` folder detected - ensure the package path is correct")
        return(character(0))
    }

    rdFiles <- list.files(manDir, pattern = "\\.Rd$", full.names = TRUE)
    if (length(rdFiles) == 0) {
        warning("no `.Rd` files were found in the `/man` directory")
        return(character(0))
    }


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

    if (asDistribution) return(table(keywords))
    unique(keywords)
}

# #' Add Keyword References to Pkgdown YAML File
# #'
# #' This function updates or creates the `_pkgdown.yml` file in a specified R package directory.
# #' It appends or overwrites entries in a YAML section based on unique keywords extracted
# #' from the package documentation. Users can specify the name of the YAML section, and choose
# #' whether to append to or overwrite the existing section.
# #'
# #' @param pkg The path to the R package directory.
# #' @param ymlPath The relative or absolute path to the `_pkgdown.yml` file.
# #'        Default is "_pkgdown.yml" within the package directory.
# #' @param append Logical; if TRUE, new entries are appended to the existing YAML section
# #'        or created if it does not exist. If FALSE, the existing section is overwritten.
# #'        Default is TRUE.
# #' @param overwrite Logical; indicates whether to overwrite the YAML file entirely. 
# #'        If FALSE, modifications are made within the existing content. Default is FALSE.
# #' @param sectionName The name of the YAML section to be modified or created. 
# #'        Default is "reference".
# #' 
# #' @return Invisible NULL. The function is called for its side effects of modifying
# #'         the `_pkgdown.yml` file.
# #' @export
# #' @keywords packageDevelopment
# #' @examples
# #' # Append keyword references to the "_pkgdown.yml" file in the "mypackage" directory
# #' addKeywordRefs()
# #' # Overwrite the "reference" section in the "_pkgdown.yml"
# #' addKeywordRefs(append = FALSE)
# #' # Overwrite the entire "_pkgdown.yml" file's reference section
# #' addKeywordRefs(overwrite = TRUE)
# addKeywordRefs <- function(
#     pkg = ".",
#     ymlPath = "_pkgdown.yml",
#     append = TRUE,
#     overwrite = FALSE,
#     sectionName = "reference"
# ) {
#   fullYmlPath <- file.path(pkg, ymlPath)
  
#   if (!dir.exists(pkg)) {
#     stop("Specified package directory does not exist.")
#   }
  
#   keywords <- getPkgKeywords(pkg = pkg, asDistribution = FALSE)
  
#   if (length(keywords) == 0) {
#     stop("No keywords found in package documentation.")
#   }
  
#   newReferences <- lapply(keywords, function(k) {
#     sprintf("- title: %s\n  - contents:\n      - has_keyword(\"%s\")", k, k)
#   })
  
#   if (!file.exists(fullYmlPath)) {
#     writeLines(c(sprintf("%s:", sectionName), unlist(newReferences)), fullYmlPath)
#     return(invisible(NULL))
#   }
  
#   existingContent <- readLines(fullYmlPath, warn = FALSE)
#   sectionIndexStart <- grep(sprintf("^%s:", sectionName), existingContent)
  
#   if (length(sectionIndexStart) > 0) {
#     sectionStartNext <- grep("^[a-zA-Z]", existingContent[(sectionIndexStart + 1):length(existingContent)], invert = FALSE)
#     if (length(sectionStartNext) > 0) {
#       sectionIndexEnd <- sectionIndexStart + sectionStartNext[1] - 2
#     } else {
#       sectionIndexEnd <- length(existingContent)
#     }
    
#     existingTitles <- sapply(existingContent[sectionIndexStart:min(sectionIndexEnd, length(existingContent))], function(x) {
#       if (grepl("^- title:", x)) sub("^- title: ", "", x)
#     })
#     existingTitles <- existingTitles[!sapply(existingTitles, is.null)]
    
#     newFilteredReferences <- unlist(newReferences)[!sapply(newReferences, function(x) {
#       title <- sub("\n.*", "", x)
#       title <- sub("^- title: ", "", title)
#       title %in% existingTitles
#     })]

#     if (!append) {
#       part1 <- existingContent[1:(sectionIndexStart - 1)]
#       part2 <- existingContent[(sectionIndexEnd + 1):length(existingContent)]
#       newContent <- c(part1, sprintf("%s:", sectionName), newFilteredReferences, part2)
#     } else {
#       part1 <- existingContent[1:sectionIndexEnd]
#       part2 <- existingContent[(sectionIndexEnd + 1):length(existingContent)]
#       newContent <- c(part1, newFilteredReferences, part2)
#     }
#   } else {
#     newContent <- c(existingContent, sprintf("%s:", sectionName), unlist(newReferences))
#   }
  
#   writeLines(newContent, fullYmlPath)
#   invisible(NULL)
# }
