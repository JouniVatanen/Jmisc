#' list_to_email
#'
#' This function helps you to save O365 type email addresses from list
#' @param email From where to read the data. Default: clipboard.
#' @param reverse Reverse the order of the first and the last name. Default: FALSE.
#' @param sep Separator, if data is not from clipboard. Default: tab.
#' @keywords email list
#' @examples
#' list_to_email()
#' @export

list_to_email <- function(email = NULL, sep = "\t", reverse = FALSE) {

  # If email ending is not defined
  if (is.null(email)) {

    # Copy from Outlook like clipboard
    source <- stringi::stri_replace_all_regex(tolower(readClipboard()),
      c(";", ">", " <"),
      c("\n", "", "\t"),
      vectorize_all = FALSE)

    col.names <- c("name", "email")

  } else {
  # If email ending is defined
    # Copy from text and put
    source <- stringi::stri_replace_all_regex(tolower(readClipboard()),
      c("'", "å|ä", "ö", "é|è|ë|ê", "^(\\S*).?(\\S*)$"),
      c("" , "a"  , "o", "e"      , "$2.$1"),
      vectorize_all = FALSE) %>%
      paste0(., "@", email)

    col.names <- "email"
  }

  # Read to dataframe
  table <- read.table(
    text = source,
    sep = sep,
    strip.white = TRUE,
    header = FALSE,
    col.names = col.names,
    quote = "\"")

  # Reverse order of the first name and last name
  if (reverse) {
    table <- dplyr::mutate(table, name = gsub("^(\\S+)\\s+(\\S+)$", "\\2 \\1", name))
  }

  # Write table to clipboard and return the result
  x <- write.table(table, "clipboard", sep = "\t", row.names = FALSE, quote = FALSE)
  return(x)
}
