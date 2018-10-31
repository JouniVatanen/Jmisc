#' Multiple custom bar plots
#'
#' Create a multiple custom bar plots. With purr::map you can create a list of barplots.
#' @param data dataframe
#' @param row select which row to plot. Default: 1:nrow(.)
#' @param label select labels. Default: waiver()
#' @keywords plot ggplot2 multiplot
#' @examples
#' plot.basic <- data %>%
#' select(1:4) %>%
#' {map(list(1, 2, c(3:5)), function(x) multiplot_bars(., x, labels))}
#' do.call("grid.arrange", c(plot.basic, ncol = 2))
#'
#' multiplot_bars(data, row = 3)
#' @export

multiplot_bars <- function(data, row = 1:nrow(.), labels = waiver()) {

  data %>%

    # Choose which row to plot
    dplyr::slice(row) %>%

    # Gather many columns to ggplot format
    tidyr::gather(2:ncol(.), key = "question", value = "values") %>%

    # Plot data
    ggplot2::ggplot(., aes(x = question, y = values, fill = Variables, label = round(values, 1))) +
      geom_col(position = "dodge") +
      geom_text(position = position_dodge(width = 1), vjust = 0) +

      # Choose Ilmarinen colors
      scale_fill_ilmarinen() +

      # Choose x-axis labels, if necessary
      scale_x_discrete(labels = labels) +

      # Choose theme
      theme_minimal() +
      theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal")
}
