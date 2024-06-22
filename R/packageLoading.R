#' Install and Load Packages from Various Sources
#'
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' This function is designed to facilitate the installation and loading of R packages 
#' from CRAN, Bioconductor, or GitHub. It checks if packages are installed; if not, it installs them,
#' and then loads them into the R session. Most importantly, it can handle an arbitrary number
#' of packages at once and install all possible dependencies.
#'
#' @param cran A character vector of CRAN package names to install and load.
#' @param bioc A character vector of Bioconductor package names to install and load.
#' @param gh A character vector of GitHub repositories in the format "username/repo" to install and load.
#' @param lib a character vector of the directory of the library to install the packages in.
#' @param verbose a logical indicating whether to display additional verbal cues
#' 
#' @details
#' The function first checks if necessary namespaces (`BiocManager` for Bioconductor packages and 
#' `devtools` for GitHub packages) are installed, and installs them if they are not present.
#' It then proceeds to install and load packages from the specified sources. For GitHub packages, 
#' installation is done using [devtools::install_github()].
#' 
#' @return None, function is used for side effects (installation and loading of packages).
#' @export
#' @keywords packageLoading
#' @examples
#' \dontrun{
#' # Install and load CRAN packages
#' installAndLoad(cran = c("ggplot2", "data.table"))
#'
#' # Install and load a Bioconductor package
#' installAndLoad(bioc = c("GenomicFeatures"))
#'
#' # Install and load a package from GitHub
#' installAndLoad(gh = c("hadley/lubridate"))
#'
#' # Install and load packages from mixed sources
#' installAndLoad(cran = c("dplyr"), bioc = c("BiocGenerics"), gh = c("r-lib/testthat"))
#' }
#'
installAndLoad <- function(
    cran = NULL, bioc = NULL, gh = NULL, lib = .libPaths()[1], verbose = FALSE
) {

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
    install_and_load <- function(packagepath, source) {

        if (is.null(packagepath)) return()

        # github packages must be prioritized to account for naming conventions
        if (source == "GitHub") {
            package <- strsplit(packagepath, "/")[[1]][2]

        } else {
            package <- packagepath
        }

        if (require(package, character.only = TRUE, quietly = TRUE)) return(invisible())
            
        if (source == "CRAN") {
            utils::install.packages(
                package,
                dependencies = TRUE,
                lib = lib
            )
        } else if (source == "Bioconductor") {
            BiocManager::install(
                package,
                lib = lib,
                dependencies = TRUE
            )
        } else {
            warning("* installing `", package, "` from a raw GitHub repository")
            devtools::install_github(
                packagepath,
                force = FALSE,
                quiet = TRUE,
                dependencies = TRUE,
                lib = lib
            )
        }

        library(package, character.only = TRUE)
    }

    for (el in zipit(list("CRAN", "Bioconductor", "GitHub"), list(cran, bioc, gh))) {
        sapply(el[[2]], function(pkg)
            utils::capture.output(suppressMessages((install_and_load(pkg, el[[1]]))))
        )
    }

    invisible()
}
