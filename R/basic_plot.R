#' Custom basic plot
#'
#' Make a custom basic plot.
#' @param data dataframe
#' @param select select columns
#' @param col select columns?
#' @keywords plot
#' @examples
#' basic_plot(data, select = c(1:10, 13))
#' @export

basic_plot <- function(data, select) {

  # Make a subset of data
  data %<>% dplyr::select(select)

  # Shorten names to 25 character
  names(data) <- substring(names(data), 1, 30)

  ggplot2::ggplot(data[col], aes_(as.name(names(data)[col]))) +
    geom_bar(aes(y = ..prop..)) +
    geom_text(aes(label = scales::percent(round(..prop.., 2)), y = ..prop..), stat = "count", vjust = 0.5) +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank())
}
