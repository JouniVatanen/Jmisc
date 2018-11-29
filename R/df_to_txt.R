#' df_to_txt
#'
#' Saves a dataframe to a txt-file.
#' @param data Choose the dataframe.
#' @param filename Choose the file name to save the dataframe.
#' @param overwrite Do you want to overwrite
#' @param sep Choose separator. Default: "\t"
#' @keywords save txt
#' @examples
#' df_to_txt(data, filename = "example.txt")
#' @export

df_to_txt <- function(data, file = "temp.txt", overwrite = FALSE, sep = "\t") {
  if (overwrite) {
    if (file.exists(file)) {
      print("File exists, do not write.")
      }
    } else {

    # Write to the file with custom settings like sep, dec and encoding
    write.table(
      data,
      file,
      quote = FALSE,
      sep = "\t",
      na = "",
      dec = ",",
      row.names = FALSE,
      fileEncoding = "UTF-8")

    }
}
