#' clean_company_names
#'
#' Clean string from Finnish company names
#' @param x Choose string which to clean.
#' @keywords clean, string, company, name
#' @examples
#' clean_company_names("lklkdf Nokia Oyj lkjadf ef Kämmekkäsäätiö")
#' @export

clean_company_names <- function(x) {

  # Warn if value is not character
  if (!is.character(x)) warning("Input is not character type")

  # Company names pattern
  remove_pattern <- "\\w*\\W*\\w*\\W*(ry|RY|Oy|OY|oy|OyJ|Oyj|oyj|OYJ|Ky|KY|AB|Ab|ab|säätiö|Säätiö)((\\W+\\w*)|$)"

  # Remove pattern from string
  output <- stringi::stri_replace_all_regex(x, remove_pattern, "**")

  return(output)
}
