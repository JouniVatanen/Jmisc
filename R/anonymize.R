#' Anonymize
#'
#' NOT FINISHED!!! Create anonymized id .
#' @param data Choose the dataframe?.
#' @param algo Algorithm to anonymize id. Default: crc32.
#' @keywords anonymize data
#' @examples
#' anonymize(x, algo = "crc32")
#' @export

# Function to create anonynomous ID
# Example of use SURV_ORG[, cols.create.ID := lapply(.SD, anonymize), .SDcols=cols_to_mask, with=FALSE][]
# Note: does not work yet. Needs some work
# Idea is to create a id variable from variables Source and Date.time and maybe salt it aka add some noise chars to variable
# Afterwards anonymise the new variable.

anonymize <- function(x, algo = "crc32"){
  id <- paste0(Source, " | ", Date.time)
  unq_hashes <- vapply(unique(x), function(object) digest(object, algo=algo), FUN.VALUE="", USE.NAMES=TRUE)
  unname(unq_hashes[x])
}
#cols.create.ID <- c("Source","Date.time")
#z <- paste0(cols.create.ID, collapse = " | ")

# Original function
# anonymize <- function(x, algo="crc32"){
#  unq_hashes <- vapply(unique(x), function(object) digest(object, algo=algo), FUN.VALUE="", USE.NAMES=TRUE)
#  unname(unq_hashes[x])
# }
# cols_to_mask <- c("name","address","postal_code")
# SURV_ORG[, cols_to_mask := lapply(.SD, anonymize), .SDcols=cols_to_mask, with=FALSE][]
