#' Create descriptive statistics table
#'
#' Create descriptive statistics table
#' @param data Choose dataframe.
#' @param .select Choose which columns you want to calculate distribution.
#' @param .labels Choose labels for table. Default: total = "N"
#' @param with.idk Data includes I don't know values. You want to compute them in distributions but not in means. Default: 99.
#' @param group.by.cols Group by chosen columns. Defaul: NULL
#' @keywords social security, birth, date
#' @examples
#' desc_stat(data, .select = c(1:10))
#' @export

desc_stat <- function(data, .select, group_by_cols = NULL, with_idk = 99, .labels = c(total = "N")){

  # Add total to the first of the vector
  group_by_cols <- append(group_by_cols, "total", after = 0)

  # Create a list for the 1st loop and vector for N-values
  desc_list <- list()
  N <- c()

  # Loop to create a data grouped by my_group_by_cols variable
  for (i in 1:length(group_by_cols)) {
    data_num <- data %>%
      dplyr::mutate(total = "total") %>%
      dplyr::group_by_(group_by_cols[i]) %>%
      dplyr::select(which(lapply(data, class) == "numeric"))

    # If you want to include
    if (is.numeric(with_idk)) {
      data_num <- data_num %>%
        dplyr::mutate_all(funs(if_else(. == with_idk, NA_real_, .)))
    }

    data_fct <- data %>%
      dplyr::mutate_at(.select, as_factor) %>%
      dplyr::mutate(total = "total") %>%
      dplyr::group_by_(group_by_cols[i]) %>%
      dplyr::select(.select)

    # Add N-values to a vector
    N <- append(N, attr(data_num, "group_sizes"))

    # Create a list for the 2nd loop
    desc_list_grouped <- list()

    # Loop to get percentage and means for each group
    for (j in 1:length(unique(group_indices(data_num)))) {

      # Factor data
      desc_fct <- data_fct %>%
        dplyr::group_indices(.) %in% j %>%
        data_fct[., ] %>%
        dplyr::ungroup() %>%
        desctable::desctable(stats = list("Mean/ %" = is.factor ~ percent | (is.numeric ~ mean)), labels = .labels)

      names(desc_fct[[2]]) <- attr(data_fct, "labels")[j, ]
      desc_fct <- dplyr::bind_cols(lapply(desc_fct, as.data.frame)) %>%
        dplyr::mutate(Variables = gsub("\\*","",Variables))

      # Numeric data
      desc_num <- data_num %>%
        dplyr::group_indices(.) %in% j %>%
        data_num[., ] %>%
        dplyr::ungroup() %>%
        desctable::desctable(stats = list("Mean/ %" = is.factor ~ percent | (is.numeric ~ mean)), labels = .labels)

      names(desc_num[[2]]) <- attr(data_num, "labels")[j,]
      desc_num <- bind_cols(lapply(desc_num, as.data.frame))
      desc_comb <- dplyr::full_join(desc_fct, desc_num, by = "Variables", suffix = c("",".y"))
      desc_comb[2] <- dplyr::if_else(is.na(desc_comb[[2]]), desc_comb[[3]], desc_comb[[2]])
      desc_comb <- desc_comb[1:2]

      desc_list_grouped[j] <- list(desc_comb)
    }

    # Reduce lists as a dataframe by joining with Variable names
    desc_list_grouped_df <- desc_list_grouped %>% reduce(full_join, by = "Variables")


    # Create list for grouped descriptive stat dataframes
    desc_list[i] <- list(desc_list_grouped_df)
  }

  # Reduce lists as a dataframe by joining with Variable names
  output <- desc_list %>% reduce(left_join, by = "Variables")

  # Make final mutations to output like remove unnecessary characters and add N values to top
  output <- output %>%
    dplyr::mutate(Variables = stringr::str_replace_all(Variables, ".*:\\s", "")) %>%
    dplyr::mutate(Variables = stringr::str_replace_all(Variables, "\\.", " "))

  output[1,2:ncol(output)] <- N

  # Return the data
  return(output)
}
