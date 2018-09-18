#' Custom correlation plot
#'
#' Make a custom correlation  plot.
#' @param data Choose the dataframe.
#' @param select Choose the columns you want. Default: all.
#' @param polychoric Not yet implemented. TRUE = use polychoric correlations.
#' @keywords plot
#' @examples
#' hclust_var(data, select = c(1:10, 13), polychoric = TRUE)
#' @export

corr_plot <- function(data, select = 1:ncol(data)) {
  # Polychoric correlations option is not yet implemented
  # Later add  "polychoric = FALSE" to function
  # Select only certain columns
  data <- dplyr::select(data, select)

  # Shorten variable names to 30 characters
  names(data) <- substring(names(data), 1, 30)

  # Create correlation matrix
  corr.m <- round(cor(data, use = "pairwise.complete.obs"), 2)

  # Reorder correlation matrix
  dd <- as.dist(1-my.corr.m/2)
  hc <- hclust(dd)
  corr.m <- corr.m[hc$order, hc$order]

  # Mutate correlation matrix
  corr.df <- as.data.frame(corr.m) %>%
    dplyr::mutate(Var1 = factor(row.names(.), levels = row.names(.))) %>%
    tidyr::gather(Var2, value, -Var1, na.rm = TRUE, factor_key = TRUE)

  # Plot correlation matrix
  plot <- ggplot2::ggplot(corr.df, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    coord_fixed()

  # Return plot
  return(plot)
}
