#' Create Package Loader Function
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function creates a package loader function that can install and load
#' packages
#' from CRAN, Bioconductor, or GitHub, optionally displaying verbose output.
#' This function can be useful in new R instances with little dependencies
#' available.
#'
#' The function takes the following arguments:
#' - `cran`
#' - `bioc`
#' - `gh`
#' - `verbose`
#'
#' where cran` and `bioc` take character vectors of package names on CRAN
#' and Bioconductor, while `gh` takes character vectors with the formatting
#' `githubUsername/packageName`. `verbose` takes in a logical for whether
#' to display additional informative messages in the REPL.
#'
#' The function will not install packages that can already be loaded by
#' default.
#'
#' @param lib A character vector specifying the library directory for package
#' installation of the output function. Defaults to the current default
#' package installation directory in `.libPaths()[1]`
#'
#' @return A function that installs and loads packages.
#' @export
#' @keywords packageLoading
#'
#' @examples
#' # Create the package loader function
#' loader <- createPkgLoader()
#'
#' # # commented usage example
#' # loader(
#' #   cran = c("dplyr", "ggplot2"),
#' #   bioc = c("GenomicRanges", "Biobase"),
#' #   gh = c("tidyverse/dplyr"),
#' #   verbose = FALSE
#' # )
#'
createPkgLoader <- function(lib = .libPaths()[1]) {
    # TODO rehaul everything here
    function(cran = NULL, bioc = NULL, gh = NULL, verbose = FALSE) {

        if (is.null(cran) && is.null(bioc) && is.null(gh)) {
            message("no packages inputted")
            return(invisible())
        }

        # Load required libraries for installation
        if (!is.null(bioc))
            if (!requireNamespace("BiocManager", quietly = TRUE))
                utils::install.packages("BiocManager", lib = lib)

        if (!is.null(gh))
            if (!requireNamespace("devtools", quietly = TRUE))
                utils::install.packages("devtools", lib = lib)

        # Helper function to install and load a package
        installAndLoad <- function(packagepath, source) {

            if (is.null(packagepath)) return()

            if (source == "GitHub") {
                package <- strsplit(packagepath, "/")[[1]][2]

            } else {
                package <- packagepath
            }

            if (require(package, character.only = TRUE, quietly = TRUE)) {
                return(invisible())
            }

            if (source == "CRAN") {
                utils::install.packages(
                    package,
                    dependencies = TRUE,
                    lib = lib,
                    quiet = !verbose
                )
            } else if (source == "Bioconductor") {
                BiocManager::install(
                    package,
                    lib = lib,
                    dependencies = TRUE,
                    quiet = !verbose
                )
            } else {
                devtools::install_github(
                    packagepath,
                    force = FALSE,
                    quiet = TRUE,
                    dependencies = TRUE,
                    lib = lib
                )
            }

            library(package, character.only = TRUE, quietly = !verbose)
        }

        zipped <- zipit(
            c("CRAN", "Bioconductor", "GitHub"), list(cran, bioc, gh)
        )
        for (el in zipped) {
            sapply(el[[2]], function(p) quietly(installAndLoad(p, el[[1]])))
        }

        invisible()
    }
}
