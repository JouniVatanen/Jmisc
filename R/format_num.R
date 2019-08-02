#' format_num
#'
#' @param x Choose the variable to round to 1 decimal with , as a decimal mark.
#' @keywords format, numeric
#' @examples
#' x <- seq(0, 10, by = 0.01)
#' format_num(x)
#' @export

format_num <- function(x) {

  # Trasform to numeric, round to 1 decimal, use comma as decimal mark
  output <- format(round(as.numeric(x), 1), decimal.mark = ",")

  return(output)
}
