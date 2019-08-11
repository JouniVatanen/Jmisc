#' calc_birth
#'
#' Calculate birth date from social security number and return POSIXct
#' @param x Choose string from which to calculate.
#' @keywords social security, birth, date
#' @examples
#' calc_birth("123456-123S")
#' @export
#' @importFrom stringi stri_sub
#' @importFrom lubridate dmy

calc_birth <- function(x) {
  output <- ifelse(is.na(x), NA,
    ifelse(stri_sub(x, 7, 7) == "-",
      paste0(stri_sub(x, 1, 4), "19", stri_sub(x, 5, 6)),
      paste0(stri_sub(x, 1, 4), "20", stri_sub(x, 5, 6))))

  output <- dmy(output)

  return(output)
}
