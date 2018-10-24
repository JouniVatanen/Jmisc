#' inspect
#'
#' Inspect the dataframe with str, names, summary, describe and View.
#' @param data Choose the dataframe.
#' @param filename Choose the output filename. Default: ./output/temp-results.txt
#' @param view View the data inside R. Default: FALSE.
#' @keywords dataframe inspect
#' @examples
#' inspect(data, "./output/temp-results.txt")
#' @export

inspect <- function(data, filename = "./output/temp-results.txt", view = FALSE) {

   # Save basic statistics to a file
  sink(filename)
  list(
    str(data),
    names(data),
    summary(data),
    Hmisc::describe(data)
    )
  sink()

  # View the data inside R
  if (view) {
    names(data) <- substring(names(data), 1, 8)
    View(data)
  }

}
