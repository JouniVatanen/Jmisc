#' calc_birth
#'
#' Calculate birth date from social security number
#' @param string Choose string from which to calculate.
#' @keywords social security, birth, date
#' @examples
#' calc_birth("123456-123S")
#' @export

calc_birth <- function(string) {
  output <- ifelse(is.na(string), NA,
    ifelse(stringr::str_sub(string, 7, 7) == "-",
      paste0(stringr::str_sub(string, 1, 4), "19", stringr::str_sub(string, 5, 6)),
      paste0(stringr::str_sub(string, 1, 4), "20", stringr::str_sub(string, 5, 6))))

  return(output)
}
