#' ggplot_missing
#'
#' This function helps you to ggplot missing values of a data frame.
#' @param my.input.df Enter your data frame.
#' @keywords ggplot2 missing
#' @export
#' @examples
#' ggplot_missing()

#' @export
ggplot_missing <- function(my.input.df) {

# Create new dataframe name not to mess with ggsave
my.df <- my.input.df

# Shorten variable names to 30 characters
names(my.df) <- substring(names(my.df), 1, 30)

# Create missmap dataframe
my.df <- my.input.df %>%
  is.na %>% as.data.frame %>%
  dplyr::mutate(Var1 = factor(rownames(.), levels = rownames(.))) %>%
  dplyr::gather(Var2, value, -Var1, na.rm = TRUE, factor_key = TRUE)

# Plot missing values
my.plot <- ggplot2::ggplot(my.df, aes(x = Var2, y = Var1)) +
  geom_raster(aes(fill = value)) +
  scale_fill_grey(name = '', labels = c('Present', 'Missing')) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = 'Variables in data', y= 'Rows/observations')

# Save plot to png
my.file <- paste0('missmap-plot-', deparse(substitute(my.input.df)), '.png')
ggplot2::ggsave(my.file, my.plot, width = 20, height = 20)
}

