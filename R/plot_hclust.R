#' Custom dendrogram plot
#'
#' Make a custom dendrogram plot.
#' @param data Choose the dataframe.
#' @param cut.num Choose the number where to cut the number of clusters. Default: 5.
#' @param clust.var Choose if you want to cluster variables. Default: TRUE.
#' @param horiz Plot horizontal (TRUE) or vertical (FALSE). Default: FALSE.
#' @keywords dataframe
#' @examples
#' plot_hclust(data, cut.num = 5)
#' @export

plot_hclust <- function(data, cut.num = 5, clust.var = TRUE, horiz = FALSE,
                        labels_track_height = 120, main = "", ...) {

  # ClustofVar if clust.var = TRUE
  if (clust.var) {

    # Select numeric variables and NULL if no variables
    var.quanti <- data %>%
      select_if(is.numeric)

    # Select non-numeric variables and convert them to factors and NULL if no variables
    var.quali <- data %>%
      select_if(Negate(is.numeric)) %>%
      mutate_all(as.factor)

    if (is.null(dim(var.quanti)) | dim(var.quanti)[2] == 0) {var.quanti <- NULL}
    if (is.null(dim(var.quali)) | dim(var.quali)[2] == 0) {var.quali <- NULL}

    # Variable hierarchical cluster
    hc <- ClustOfVar::hclustvar(X.quanti = var.quanti, X.quali = var.quali)

  } else {
    # Else normal hierarchical cluster
    hc <- hclust(dist(data))
  }

  # Choose custom colors, where rep_len matches number of clusters
  colors <- rep_len(ilmarinen_cols(), cut.num)

  # Plot the clustering
  plot <- factoextra::fviz_dend(
      hc, k = cut.num, cex = 0.5, horiz = horiz, k_colors = colors,
      color_labels_by_k = TRUE, rect = TRUE, rect_border = colors,
      rect_fill = TRUE, labels_track_height = labels_track_height, main = main, ...) +
    theme_void()

  return(plot)
}
