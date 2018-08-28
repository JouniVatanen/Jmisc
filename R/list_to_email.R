#' list_to_email
#'
#' This function helps you to save O365 type email addresses from list
#' @param my.input.df Enter your data frame.
#' @keywords email list
#' @export
#' @examples
#' list_to_email()
#' @export

list_to_email <- function(source = readClipboard()) {

  # Load clipboard to and transform the data with regex
  my.table <- read.table(text = gsub(';','\n', gsub('>', '', source, perl = TRUE), perl = TRUE),
                         sep = '<', strip.white = TRUE, header = FALSE, col.names = c('name','email'))

  # Write table to clipboard and return the result
  x <- write.table(my.table, 'clipboard', sep = '\t', row.names = FALSE, quote = FALSE)
  return(x)
}
