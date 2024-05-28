#' Add/Update Keyword References in the Pkgdown YAML File
#'
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' Updates the `_pkgdown.yml` file in a specified R package directory that was created
#' with accordance to the `pkgdown` guidelines, or created with [usethis::use_pkgdown()].
#' It appends or overwrites entries in a YAML section based on unique keywords extracted
#' from the package documentation.
#' 
#' The reference section is built using the following format:
#' 
#' ```yaml
#' sectionName:
#' - title: Some Keyword
#' - content:
#'   - has_keyword("someKeyword")
#' - title: Another Keyword
#' - content:
#'   - has_keyword("another_keyword")
#' ```
#' 
#' If there is already an existing reference section, this function will search of any instances
#' of content that uses the `has_keyword("someKeyword")` syntax in the contents, and filter
#' out corresponding keywords, and appends it to the list with the same format as above.
#' 
#' @param pkg The path to the R package directory.
#' @param ymlPath The path to the `_pkgdown.yml` file, relative or absolute.
#'        Default is "_pkgdown.yml" within the package directory.
#' @param mode Character string, "a" or "o" for appending or overwriting the current reference
#' section, determining how keywords are handled.
#' @param sectionName The name of the YAML section to modify. Default is "reference".
#' @param modifier A function to modify the keyword section titles; defaults to a function that
#' tries to split each keyword into consecutive words and coerce it to a spaced character in
#' *Title Case* via [tools::toTitleCase()]
#' 
#' @return invisible return, the function has a side effect of modifying the
#' specified yml file.
#' @export
#' @keywords packageDevelopment
addYMLKeywordRefs <- function(
    pkg = ".",
    ymlPath = "_pkgdown.yml",
    mode = c("a", "o"),
    sectionName = "reference",
    modifier = function(x) {
        sapply(trySplit(x), function(y) {
            tools::toTitleCase(paste(y, collapse = " "))
        })
    },
    ...
) {
    # get the yaml list and content 
    fullYmlPath <- getExistingFilePath(dir = pkg, filePath = ymlPath)
    yamlContentLines <- readLines(fullYmlPath)

    # get relevant indicies
    currSectionLineIndex <- getIndexOfYamlSection(yamlContentLines, sectionName)
    if (length(currSectionLineIndex) == 0) {
        yamlContentLines <- c(yamlContentLines, "", apColon(sectionName))
        currSectionLineIndex <- currSectionEndLineIndex <- length(yamlContentLines) + 2
    } else {
        currSectionEndLineIndex <- getIndexOfEndOfYamlSection(
            yamlContentLines, currSectionLineIndex
        )
    }

    # get keywords and titles
    keywords <- getPkgKeywords(pkg = pkg, asDistribution = FALSE)
    if (length(keywords) == 0) {
        stop("No keywords found in package documentation.")
    }

    # initialize reference section string and keywords
    if ((apColon(sectionName) %in% yamlContentLines) || identical(mode, "a")) {
        # TODO improve so that only take from reference section

        keywords <- keywords[
            as.logical(sapply(asHasKeywordStatement(keywords), function(x) {
                print(x)
                (!grepl(x, paste(yamlContentLines, collapse = "\n")))
            }))
        ]
        if (length(keywords) == 0) return(invisible(NULL))

        currKeywordAppendAfterIndex <- currSectionEndLineIndex

    } else { # overwrite
        yamlContentLines <- yamlContentLines[
            -((currSectionLineIndex + 1):currSectionEndLineIndex)
        ]
        currKeywordAppendAfterIndex <- currSectionLineIndex
    }

    # add new keyword sections
    for (keyword in keywords) {
        yamlContentLines <- yamlContentLines %>% append(
            c(paste("- title:", modifier(keyword)),
              "- contents:",
              paste("  -", asHasKeywordStatement(keyword))),
            after = currKeywordAppendAfterIndex
        )
        currKeywordAppendAfterIndex <- currKeywordAppendAfterIndex + 3
    }
    
    # write results
    writeLines(
        append(yamlContentLines, "", after =  currKeywordAppendAfterIndex),
        fullYmlPath
    )
    invisible(NULL)
}

getIndexOfYamlSection <- function(yamlLines, sectionName) {
    which(apColon(sectionName) == yamlLines)
}

getIndexOfEndOfYamlSection <- function(yamlLines, currSectionLineIndex) {
    nextSectionLineIndex <- getIndexOfNextYamlSection(yamlLines, currSectionLineIndex)
    if (is.na(nextSectionLineIndex)) return(length(yamlLines))
    nextSectionLineIndex - 1
}

# get index of next yaml section if exists, else NA
getIndexOfNextYamlSection <- function(yamlLines, currSectionLineIndex) {
    allSectionLineIndicies <- which(grepl(":$", yamlLines))
    if (getlast(allSectionLineIndicies) == currSectionLineIndex) return(NA)
    allSectionLineIndicies[which(currSectionLineIndex == allSectionLineIndicies) + 1]
}

asHasKeywordStatement <- function(keyword) {
    paste("has_keyword(", keyword, ")", sep = "\"")
}

apColon <- function(x) paste(x, ":", sep = "")

#' @rdname addYMLKeywordRefs
#' @export
ymlkr <- addYMLKeywordRefs
