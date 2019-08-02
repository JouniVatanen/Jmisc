#' clean_street names
#'
#' Clean string from Finnish street names
#' @param x Choose string which to clean.
#' @keywords clean, string, strees
#' @examples
#' clean_street_names("Jokutie 4, pikitie 7, 00500 Helsinki")
#' @export

clean_street_names <- function(x) {

  # Warn if value is not character
  if (!is.character(x)) warning("Input is not character type")

  # Street names pattern
  remove_pattern <- "\\w*(tie|katu|kuja)\\W+"

  # Remove pattern from string
  output <- stringi::stri_replace_all_regex(x, remove_pattern, "**")

  return(output)
}