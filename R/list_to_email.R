#' list_to_email
#'
#' This function helps you to save O365 type email addresses from list
#' @param source From where to read the data. Default: clipboard.
#' @param reverse Reverse the order of the first and the last name. Default: FALSE.
#' @keywords email list
#' @examples
#' list_to_email()
#' @export

list_to_email <- function(source = readClipboard(), reverse = FALSE) {

  # Load clipboard to and transform the data with regex
  table <- read.table(text =
    gsub(";","\n", gsub(">", "", source, perl = TRUE), perl = TRUE),
    sep = "<",
    strip.white = TRUE,
    header = FALSE,
    col.names = c("name", "email"))

  # Reverse order of the first name and last name
  if (reverse) {
    table <- dplyr::mutate(table, name = gsub("^(\\S+)\\s+(\\S+)$", "\\2 \\1", name))
  }

  # Write table to clipboard and return the result
  x <- write.table(table, "clipboard", sep = "\t", row.names = FALSE, quote = FALSE)
  return(x)
}