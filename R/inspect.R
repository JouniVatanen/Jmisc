#' inspect
#'
#' NOT FINISHED!!! Inspect the dataframe with str, names, summary, describe and View.
#' @param data Choose the dataframe.
#' @param filename Choose the output filename. Default: output/temp-results.txt
#' @keywords dataframe
#' @export
#' @examples
#' inspect(data)
#' #' @export

inspect <- function(data, filename = "output/temp-results.txt") {

  #Save basic statistics to a file

  str(data)
  names(data)
  summary(data)
  Hmisc::describe(data)

  #View the data inside R
  names(data) <- substring(names(data), 1, 8)
  View(data)
}
