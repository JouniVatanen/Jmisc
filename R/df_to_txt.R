#' df_to_txt
#'
#' Save any list like data.frame, data.table or matrix to a txt-file.
#' Uses data.table::fwrite or vroom::vroom_write depending on decimal mark.
#' If decimal mark is ., then uses faster vroom_write. Otherwise uses fwrite.
#'
#' @param x Any list like element e.g. data.frame and data.table.
#' @param file Output file name. Default: to the console.
#' @param dec Decimal limiter. Default: ","
#' @param overwrite Overwrites the file, it it exists. Default: FALSE.
#' @param sep Separator. Default: "tab"
#' @param ... Pass other parameters to fwrite or vroom_write like na, append,
#' col.names
#' @keywords save txt
#' @examples
#' n <- c(1.1, 2.2, 3.3)
#' s <- c("a", "b", "c")
#' x <- data.frame(n, s)
#' df_to_txt(x, file = "example.txt")
#' @export
#' @importFrom data.table fwrite
#' @importFrom vroom vroom_write
#' @importFrom purrr map
#' @importFrom fs path path_ext path_ext_remove
#' @importFrom R.utils gzip

df_to_txt <- function(x, file = "", sep = "\t", dec = ",",
                      overwrite = FALSE, encoding = "UTF-8", ...) {
  if (all(!overwrite, file.exists(file))) {
    stop("File exists. If you want to overwrite, change overwrite = TRUE.")
  } else {

    #  Change data encoding. Default is UTF-8.
    x[] <- map(x, function(x) stri_encode(x, "", encoding))

    # Write to the file with custom settings like sep, dec and encoding
    if (dec != ".") {
      # Slower, but can handle decimal separator
      if (path_ext(file) != "gz") {
        fwrite(x, file, sep = sep, dec = dec, ...)
      # Can also pack the file with R.utils::gzip
      } else {
        fwrite(x, path_ext_remove(file), sep = sep, dec = dec, ...)
        gzip(path_ext_remove(file), file)
      }
    } else {
      # Faster and is able to pack the file as well, if file name ends .gz
      vroom_write(x, file, delim = sep, ...)
    }
  }
}
