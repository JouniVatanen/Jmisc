#' clean_people_names
#'
#' Clean string from Finnish people
#' @param x Choose string which to clean.
#' @keywords clean, string, strees
#' @examples
#' clean_people_names("Ari, Jouni, EsaJokutie 4, pikitie 7, 00500 Helsinki")
#' @export
#' @import dplyr
#' @importFrom stringi stri_replace_all_regex stri_opts_regex

clean_people_names <- function(x) {

  # Warn if value is not character
  if (!is.character(x)) warning("Input is not character type")

  # Remove and leave some names
  remove_names <- c(
    "Paka", "Paaja", "Hamarus", "Allan", "Roy", "Hitchman", "Wojtunik")

  leave_names <- c(
    "Ilmarinen", "Olin", "Varma", "Toimi", "Onni", "Kela", "Aamu", "Elo", "Aina",
    "Mainio", "Ensi", "Ihme", "Lähde", "Ruutu", "Ilman", "Mutta", "Pitkä", "Oja",
    "Aihe", "Asia", "Voi", "oja", "von", "Pyy", "pyy", "Aho", "aho", "Jonne",
    "Minne", "Tuli", "Uusi", "uusi", "Hankala", "And", "Viik", "Nord", "Väli",
    "Tila", "Väli", "Ansio", "Koski", "Osku", "Iso", "Juuri", "Arvio", "Laskuja",
    "Titta", "Ilo", "Ruotsi", "Ilmi", "Suo", "Ajan", "Laaja", "Vappu", "Peri",
    "Vanha", "Esti", "Kesti", "Svensk", "kai")

  # Create people names pattern
  names <- jmisc::fi_people_names %>%
    filter(!(name %in% leave_names)) %>%
    pull(1) %>%
    append(remove_names)
  remove_pattern <- paste0("\\b(?:", paste(names, collapse = "|"), ")\\b ?")

  # Remove pattern from string
  output <- stri_replace_all_regex(
    x, remove_pattern, "**",
    opts_regex = stri_opts_regex(case_insensitive = TRUE))

  return(output)
}
