#' Combine multiple files
#'
#' Combine multiple files based on a pattern
#' @param pattern Choose dataframe.
#' @param path Choose which columns you want to calculate distribution.
#' @param output Choose labels for table. Default: total = "N"
#' @param sheet Data includes I don't know values. You want to compute them in distributions but not in means. Default: 99.
#' @param na Choose na values. Default: c("", "-'").
#' @param guess_max Choose maximum value to guess column type.
#' @param col_types Choose col_types
#' @keywords social security, birth, date
#' @import dplyr vroom readxl writexl
#' @importFrom data.table fread fwrite
#' @importFrom rlang .data
#' @importFrom fs dir_ls
#' @importFrom purrr map_df
#' @importFrom readxl read_excel

combine_files <- function(
  pattern, path = ".", output = NULL, sheet = 1, na = c("", "-'"), guess_max = 1000,
  col_types = NULL) {

  file_names <- dir_ls(path = path, regexp = pattern)


  grepl("xls|xlsx$", "Jounin tiedosto.txt.zip")

  # Read xlsx files with readxl package
  if (input_type == "xlsx") {
    map_df( ~combine_excel(file_names))

  # Read txt files with fread package
  # TODO: Add option to use faster vroom package, which is problematic with dec = ","
  } else if (input_type == "txt") {
    map_df( ~combine_txt(file_names))

  # Stop if input type is something else but excel or txt
  } else {
    stop("There is fixed file formats in search pattern. Change your pattern.")
  }

  # Write to file if output is a path
  if (!is.null(ouput)) {
    if (output_type == "xlsx") write_xlsx(data, output)
    else if (output_type == "txt") write_txt(data, output)
  } else {
    return(data)
  }
}

# Function to combine different files
combine_excel <- function(
  pattern = ".xlsx", path = "./data", sheet = 1, skip = 0, na = c("", "-'"),
  col_names = TRUE, guess_max = 1000, col_types = NULL) {

  # Make a list by pattern like ".xlsx". Each filename is one object in a list.
  data <- list.files(path, pattern, full.names = TRUE) %>%
    as.list() %>%
    map_df( ~ read_excel(
      path = .x, sheet = sheet, skip = skip, na = na, col_names = col_names,
      guess_max = guess_max, col_types = col_types))

  # Return data
  return(data)
}


combine_txt <- function() {


}

write_file <- function(file, output) {

  write_xlsx()


}


write_xlsx <- function(file, output) {



}

write_txt <- function(file, output) {



}
