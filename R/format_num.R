#' format_num
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param x Choose the variable to round to 1 decimal with , as a decimal mark.
#' @keywords format, numeric
#' @examples
#' x <- seq(0, 10, by = 0.01)
#' format_num(x)

format_num <- function(x) {

  # Makes changes
  output <- x %>%
    as.numeric(.) %>%
    round(., 1) %>%
    format(., decimal.mark = ",") %>%

  return(output)
}
