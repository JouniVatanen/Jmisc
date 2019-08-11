#' df_to_txt
#'
#' Save any list like data.frame, data.table or matrix to a txt-file. Based on fwrite from data.table package.
#' @param x Any list like element e.g. data.frame and data.table.
#' @param file Output file name. Default: to the console.
#' @param dec Decimal limiter. Default: ","
#' @param overwrite Overwrites the file, it it exists. Default: FALSE.
#' @param sep Separator. Default: TAB
#' @param ... Pass other parameters to fwrite like na, append, col.names
#' @keywords save txt
#' @examples
#' n <- c(1.1, 2.2, 3.3)
#' s <- c("a", "b", "c")
#' x <- data.frame(n, s)
#' df_to_txt(x, file = "example.txt")
#' @export
#' @importFrom data.table fwrite
#' @importFrom readr write_excel_csv

df_to_txt <- function(x, file = "", sep = "\t", dec = ",",
                      overwrite = FALSE) {
  if (all(!overwrite, file.exists(file))) {
    stop("File exists already. If you want to overwrite, then change overwrite to TRUE.")
  } else {

    # Write first line with readr in order to save in UTF-8
    write_excel_csv(x[,0], file, delim = sep)

    # Write to the file with custom settings like sep, dec and encoding
    fwrite(x, file, sep = sep, dec = dec, append = TRUE, ...)
  }
}
