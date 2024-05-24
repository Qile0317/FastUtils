#' Initialize Testthat Files
#'
#' This function scans all files in the specified R directory, excluding some based on the patterns provided in the `ignore` argument, and creates `testthat` files if they are missing.
#'
#' @param rDir The directory containing R source files. Default is "R".
#' @param testDir The directory where `testthat` files should be created. Default is "tests/testthat".
#' @param ignore A character vector specifying regex patterns of files to ignore.
#' 
#' @return No return value, called for side effects.
#' @export
#' @keywords testing
initTestthat <- function(
    rDir = "R",
    testDir = "tests/testthat",
    ignore = c("-package.R$", "-class.R$", "^data.R$", "^zzz.R$", "^RcppExports.R$")
) {

    if (!dir.exists(testDir)) usethis::use_testthat()

    for (sourceFile in list.files(rDir, full.names = TRUE)) {

        if (grepl(paste(ignore, collapse = "|"), sourceFile)) next
        fileTitle <- substrEnd(basename(sourceFile), 1, 2)
        testFilePath <- file.path(testDir, paste0("test-", fileTitle, ".R"))
        if (!file.exists(testFilePath)) {
            usethis::use_test(fileTitle)
        }

    }
}

test_quietly_that <- function(desc, code) {
	test_that(desc, {quietly(code)})
}
