#' calc_gender
#'
#' Calculate gender from social security number
#' @param string Choose string from which to calculate.
#' @keywords social security,
#' @examples
#' calc_gender("123456-123S")
#' @export

calc_gender <- function(string) {
  output <- ifelse(is.na(string), NA,
    ifelse(as.numeric(stringr::str_sub(string, 10, 10)) %% 2, "Mies", "Nainen"))

  return(output)
}
