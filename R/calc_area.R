#' calc_area
#'
#' Calculate area classification e.g. (Etelä-Suomi - Pohjois-Suomi) from first three postnumber characters (000-999)
#' @param x Choose variable from which to calculate.
#' @param output Choose variable(s) to return e.g. Maakunta/Maakunnan.nimi/Kuntaryhma/Kuntaryhman.nimi/Seutukunta/Seutukunnan.nimi/Suuralue/Suuralueen.nimi/Alue)
#' @keywords area, calculation, classification
#' @examples
#' calc_area("001")
#' @export

calc_area <- function(x, select = "Alue") {

  library(magrittr)

  # Stop if value is not character
  if (!is.character(x)) stop("Area class has to be char of type 000-999")

  # Match with data.tables fast indexing
  output <- Jmisc:::df.postno[.(x), ..select, nomatch = 0L] %>%

    # Return a tibble in order to map the results to a dataframe
    tibble::as_tibble()


  return(output)
}
