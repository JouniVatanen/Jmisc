#' calc_nps
#'
#' Calculate NPS from survey
#' @param var Choose variable from which to calculate.
#' @keywords social security, birth, date
#' @examples
#' calc_nps(0)
#' @export

calc_nps <- function(var = NA) {

  val <- as.numeric(var)

  if (is.na(val)) {
    return(NA)
  } else if (max(val) > 11 | min(val) < 0) {
    stop("Check the data. The scale should be 0-10.")
  } else if (max(val) == 11) {
    warning("The scale will be converted from 1-11 to 0-10.")
    val = val - 1
  }
  if (dplyr::between(val, 0, 6)) {
    return(-100)
  } else if (dplyr::between(val, 7, 8)) {
    return(0)
  } else {
    return(100)
  }
}
