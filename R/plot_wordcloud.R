#' plot_wordcloud
#'
#' Make a custom wordcloud plot.
#' @param x Vector of sentences or words.
#' @param output Output path.
#' @keywords plot, wordcloud
#' @examples
#' #' # Create a vector of month names
#' x <- c()
#' for (i in 1:12) {
#'  x <- append(x, rep(month.name[i], i * 2))
#' }
#' plot_wordcloud(x, "./wordcloud.png")
#' @export

plot_wordcloud <- function(x, output = "./output/wordcloud.png") {

  # Create remove words
  remove_words <- c(
    "että", "kiitos", "moite", "ikä", "sitä", "sitä", "sen", "palaute",
    "fffd", "hei", "myös", "moi")

  # Stem and remove stopwords for further analyses
  corpus <- tm::Corpus(tm::VectorSource(x))
  dtm <- tm::TermDocumentMatrix(corpus, control = list(
    removePunctuation = TRUE,
    removeNumbers = TRUE,
    tolower = TRUE,
    stopwords = c(stopwords("finnish"), remove_words),
    stripWhitespace = TRUE)
    )

  vec <- sort(rowSums(as.matrix(dtm)), decreasing = TRUE)
  df <- data.frame(word = names(vec), freq = vec)

  # Save as a file
  png(output, width = 1280, height = 800, units = "px", res = 100)
  par(mar = rep(0, 4))
  wordcloud::wordcloud(df$word, df$freq, min.freq = 1, max.words = 200,
                       rot.per = 0, colors = jmisc::ilmarinen_cols())
  dev.off()
}
