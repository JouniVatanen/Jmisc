#' plot_missing
#'
#' This function helps you to ggplot missing values of a data frame.
#' @param data Enter your data frame.
#' @keywords ggplot2 missing
#' @examples
#' ggplot_missing(data, "./output/plot_missmap.png")
#' @export

plot_missing <- function(data, file = "plot_missmap.png") {

  # Shorten variable names to 30 characters
  names(data) <- substring(names(data), 1, 30)

  # Create missmap dataframe
  plot <- data %>%
    is.na %>%
    as.data.frame %>%
    dplyr::mutate(Var1 = factor(rownames(.), levels = rownames(.))) %>%
    tidyr::gather(Var2, value, -Var1, na.rm = TRUE, factor_key = TRUE) %>%

  # Plot missing values
    ggplot2::ggplot(aes(x = Var2, y = Var1)) +
      geom_raster(aes(fill = value)) +
      scale_fill_grey(name = '', labels = c('Present', 'Missing')) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(x = 'Variables in data', y= 'Rows/observations')

  # Save plot to png
  ggplot2::ggsave(file, plot, width = 20, height = 20)
}
