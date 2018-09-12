#' load_pkg
#'
#' This function installs and loads your favourite packages.
#' @param packages "full" loads the full list of favourite packages and "basic" loads tidyverse, data.table and a few other packages.
#' @keywords packages
#' @export
#' @examples
#' load_pkg(packages = "long")
#' @export

load_pkg <- function(packages = "full") {

  #select favourite packages
  if (length(packages) == 1L && packages == "full") {
    packages <- c("readxl", "tidyverse", "tidyr", "modelr", "broom",
      "data.table", "magrittr", "lubridate", "desctable", "Hmisc", "GGally",
      "knitr", "kableExtra", "formattable","gridExtra", "FactoMineR",
      "factoextra", "ClustOfVar", "caret", "psych", "ggrepel", "pROC", "DT",
      "webshot")
  } else if (length(packages) == 1L && packages == "basic") {
    packages <- c("readxl", "tidyverse", "tidyr", "data.table", "magrittr",
      "lubridate", "knitr")
  }

  packagecheck <- match(packages, utils::installed.packages()[,1])

  packagestoinstall <- packages[is.na(packagecheck)]

  if(length(packagestoinstall) > 0L ) {
    utils::install.packages(packagestoinstall, repos = "http://cran.wu.ac.at/")
  } else {
    print("All requested packages already installed")
  }

  for(package in packages) {
    suppressPackageStartupMessages(
      library(package, character.only = TRUE, quietly = TRUE))
  }
}
