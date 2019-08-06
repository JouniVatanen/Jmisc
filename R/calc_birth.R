#' calc_birth
#'
#' Calculate birth date from social security number and return POSIXct
#' @param string Choose string from which to calculate.
#' @keywords social security, birth, date
#' @examples
#' calc_birth("123456-123S")
#' @export
#' @importFrom stringr str_sub
#' @importFrom lubridate dmy

calc_birth <- function(string) {
  output <- ifelse(is.na(string), NA,
    ifelse(str_sub(string, 7, 7) == "-",
      paste0(str_sub(string, 1, 4), "19", str_sub(string, 5, 6)),
      paste0(str_sub(string, 1, 4), "20", str_sub(string, 5, 6))))

  output <- dmy(output)

  return(output)
}
