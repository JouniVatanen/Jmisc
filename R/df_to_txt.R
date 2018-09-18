#' df_to_txt
#'
#' Saves a dataframe to a txt-file.
#' @param data Choose the dataframe.
#' @param filename Choose the file name to save the dataframe.
#' @keywords save txt
#' @examples
#' df_to_txt(data, filename = "example.txt")
#' @export

df_to_txt <- function(data, filename = "temp.txt") {
  write.table(
    data,
    filename,
    quote = FALSE,
    sep = "\t",
    na = "",
    dec = ",",
    row.names = FALSE,
    fileEncoding = "UTF-8")
}
