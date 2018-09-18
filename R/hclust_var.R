#' Custom dendrogram plot
#'
#' Make a custom dendrogram plot.
#' @param data Choose the dataframe.
#' @param cut.num Choose the number where to cut the output filename. Default: output/temp-results.txt
#' @param horiz Plot horizontal (TRUE) or vertical (FALSE). Default: FALSE.
#' @keywords dataframe
#' @examples
#' hclust_var(data, cut.num = 5)
#' @export

hclust_var <- function(data, cut.num, horiz = FALSE, ...) {

  hc <- ClustOfVar::hclustvar(X.quali = data, ...)

  # Choose custom colors, where rep_len matches number of clusters
  colors <- rep_len(c("#00274B", "#3A8DA9", "#DE6328", "#84D2DF", "#FFCA20", "#F37B8A"), cut.num)

  plot <- factoextra::fviz_dend(hc, k = cut.num, cex = 0.5, horiz = horiz, k_colors = my.colors, color_labels_by_k = TRUE, rect = TRUE, rect_border = colors, rect_fill = TRUE, labels_track_height = 6) +
    ggtitle("Kysymysten hierarkinen ryhmittely") +
    theme_void()

  return(plot)
}
