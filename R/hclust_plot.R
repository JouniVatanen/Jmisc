#' Custom dendrogram plot
#'
#' Make a custom dendrogram plot.
#' @param data Choose the dataframe.
#' @param cut.num Choose the number where to cut the number of clusters. Default: 5.
#' @param clust.var Choose if you want to cluster variables. Default: TRUE.
#' @param horiz Plot horizontal (TRUE) or vertical (FALSE). Default: FALSE.
#' @keywords dataframe
#' @examples
#' hclust_plot(data, cut.num = 5)
#' @export

hclust_plot <- function(data, cut.num = 5, clust.var = TRUE, horiz = FALSE, ...) {

  # ClustofVar if clust.var = TRUE
  if (clust.var) {
    hc <- ClustOfVar::hclustvar(X.quali = data, ...)
  } else {
    # Else normal hierarchical cluster
    hc <- hclust(dist(data))
  }

  # Choose custom colors, where rep_len matches number of clusters
  colors <- rep_len(Jmisc::Ilmarinen_cols(), cut.num)

  # Plot the clustering
  plot <- factoextra::fviz_dend(
      hc, k = cut.num, cex = 0.5, horiz = horiz, k_colors = colors,
      color_labels_by_k = TRUE, rect = TRUE, rect_border = colors,
      rect_fill = TRUE, labels_track_height = 6) +
    ggtitle("Hierarkinen ryhmittely") +
    theme_void()

  return(plot)
}
