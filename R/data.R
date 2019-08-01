#' Finnish postnumbers 2016
#'
#' Data from Posti about Finnish postnumbers and their metadata.
#' Data is from 2016. Data can be downloaded from:
#' https://beta.posti.fi/fi/asiakastuki/postinumerotiedostot
#'
#' @format A data frame with 939 rows and 10 variables:
#' \describe{
#'   \item{postinumero}{Postnumber depricated to first three characters}
#'   \item{maakunta}{Area codes divided by province}
#'   \item{maakunnan_nimi}{Area names divided by province}
#'   \item{kuntaryhma}{Area codes divided by area type}
#'   \item{kuntaryhman_nimi}{Area names divided by area type}
#'   \item{seutukunta}{Area codes divided by great area}
#'   \item{seutukunnan_nimi}{Area names divided by great area}
#'   \item{suuralue}{Area codes divided by NUTS2}
#'   \item{suuralueen_nimi}{Area names divided by NUTS2}
#'   \item{alue}{Area names divided by compass point}
#' }
"fi_postnumbers_2016"

#' Finnish names
#'
#' Data from Väestörekisterikeskus about Finnish first and last names by gender
#' and their how popular they are. Data is from 2016. Data can be downloaded from:
#' https://www.avoindata.fi/data/fi/dataset/none
#'
#' @format A data frame with 34935 rows and 4 variables:
#' \describe{
#'   \item{name}{name of a person}
#'   \item{n}{popularity of the name}
#'   \item{gender}{Male = M, Female = F, NA in lastnames}
#'   \item{type}{Firstname = F, lastname = L}
#' }
"fi_people_names"
