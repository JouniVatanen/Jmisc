#' Create descriptive statistics table
#'
#' Create descriptive statistics table
#' @param data Choose dataframe.
#' @param .select Choose which columns you want to calculate distribution.
#' @param .labels Choose labels for table. Default: total = "N"
#' @param group.by.cols Group by chosen columns. Defaul: NULL
#' @keywords social security, birth, date
#' @examples
#' desc_stat(data, .select = c(1:10))
#' @export

desc_stat <- function(data, .select, group.by.cols = NULL, .labels = c(total = "N")){

  library(magrittr)

  # Add total to the first of the vector
  group.by.cols <- append(group.by.cols, "total", after = 0)

  # Create a list for the 1st loop and vector for N-values
  desc.list <- list()
  N <- c()

  # Loop to create a data grouped by my.group.by.cols variable
  for (i in 1:length(group.by.cols)) {
    data.num <- data %>%
      dplyr::mutate(total = "total") %>%
      dplyr::group_by_(group.by.cols[i]) %>%
      dplyr::select(which(lapply(data, class) == "numeric"))

    data.fct <- data %>%
      dplyr::mutate_at(.select, as.factor) %>%
      dplyr::mutate(total = "total") %>%
      dplyr::group_by_(group.by.cols[i]) %>%
      dplyr::select(.select)

    # Add N-values to a vector
    N <- append(N, attr(data.num, "group_sizes"))

    # Create a list for the 2nd loop
    desc.list.grouped <- list()

    # Loop to get percentage and means for each group
    for (j in 1:length(unique(group_indices(data.num)))) {

      # Factor data
      desc.fct <- data.fct %>%
        dplyr::group_indices(.) %in% j %>%
        data.fct[.,] %>%
        dplyr::ungroup() %>%
        desctable::desctable(stats = list("Mean/ %" = is.factor ~ percent | (is.numeric ~ mean)), labels = .labels)

      names(desc.fct[[2]]) <- attr(data.fct, "labels")[j,]
      desc.fct <- dplyr::bind_cols(lapply(desc.fct, as.data.frame)) %>%
        dplyr::mutate(Variables = gsub("\\*","",Variables))

      # Numeric data
      desc.num <- data.num %>%
        dplyr::group_indices(.) %in% j %>%
        data.num[.,] %>%
        dplyr::ungroup() %>%
        desctable::desctable(stats = list("Mean/ %" = is.factor ~ percent | (is.numeric ~ mean)), labels = .labels)

      names(desc.num[[2]]) <- attr(data.num, "labels")[j,]
      desc.num <- bind_cols(lapply(desc.num, as.data.frame))
      desc.comb <- full_join(desc.fct, desc.num, by = "Variables", suffix = c("",".y"))
      desc.comb[2] <- ifelse(is.na(desc.comb[[2]]), desc.comb[[3]], desc.comb[[2]])
      desc.comb <- desc.comb[1:2]

      desc.list.grouped[j] <- list(desc.comb)
    }

    # Reduce lists as a dataframe by joining with Variable names
    desc.list.grouped.df <- desc.list.grouped %>% reduce(full_join, by = "Variables")


    # Create list for grouped descriptive stat dataframes
    desc.list[i] <- list(desc.list.grouped.df)
  }

  # Reduce lists as a dataframe by joining with Variable names
  output <- desc.list %>% reduce(left_join, by = "Variables")

  # Make final mutations to output like remove unnecessary characters and add N values to top
  output %<>%
    dplyr::mutate(Variables = stringr::str_replace_all(Variables, ".*:\\s", "")) %>%
    dplyr::mutate(Variables = stringr::str_replace_all(Variables, "\\.", " "))

  output[1,2:ncol(output)] <- N

  # Return the data
  return(output)
}
