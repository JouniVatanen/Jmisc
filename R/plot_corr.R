#' Custom correlation plot
#'
#' Make a custom correlation plot.
#' @param data Choose the dataframe.
#' @param polychoric Not yet implemented. TRUE = use polychoric correlations.
#' @keywords plot
#' @examples
#' plot_corr(mtcars, polychoric = FALSE)
#' @export
#' @import dplyr ggplot2
#' @importFrom tidyr gather
#' @importFrom stats hclust as.dist cor

plot_corr <- function(data, polychoric = FALSE) {

  corr_m <- data %>%

    # Shorten variable names to 30 characters
    rename_all(~str_sub(., 1, 30)) %>%

    # Create correlation matrix
    # Ifelse for polychoric correlations
    cor(., use = "pairwise.complete.obs")

  # Reorder correlation matrix
  dd <- as.dist(1 - corr_m / 2)
  hc <- hclust(dd)
  corr_m <- round(corr_m[hc$order, hc$order], 2)

 # Create output
  output <- as.data.frame(corr_m) %>%

    # Mutate correlation matrix
    mutate(Var1 = factor(row.names(.), levels = row.names(.))) %>%
    gather(Var2, value, -Var1, na.rm = TRUE, factor_key = TRUE) %>%

    # Plot correlation matrix
    ggplot(., aes(Var2, Var1, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
      coord_fixed()

  # Return plot
  return(output)
}
