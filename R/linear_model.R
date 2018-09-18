#' Linear model
#'
#' Function for linear model and its results
#' @param data Name of the data frame.
#' @param form Model in the format c("variable ~ ."). Default: Predict variable y with all other variables.
#' @param k Number of kfolds. Default: 10.
#' @keywords linear model
#' @examples
#' linear_model(data, c("y ~ x + z"))
#' @export

linear_model <- function(data, form = c("y ~ ."), k = 10) {

  # Create test and train set and n folds
  models <- modelr::crossv_kfold(df, k = k) %>%
    dplyr::mutate(model = purrr::map(train, ~ lm(form, data = .)),
           tidy = purrr::map(model, tidy),
           glance = purrr::map(model, glance),
           test.rmse = purrr::map2_dbl(model, test, rmse),
           train.rmse = purrr::map2_dbl(model, train, rmse))

  # Calculate coefficients
  coeffs <- models %>%
    tidyr::unnest(tidy) %>%
    dplyr::select(.id, term, estimate, p.value) %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(estimate = mean(estimate), p.value = mean(p.value)) %>%
    dplyr::arrange(p.value)

  # Calculate R^2
  r2 <- models %>% tidyr::unnest(glance) %>% dplyr::pull(adj.r.squared) %>% mean

  # Calculate model accuracy test set
  test.preds <- models %>%
    tidyr::unnest(fitted = purrr::map2(model, test, ~ augment(.x, newdata = .y))) %>%
    mutate(.fitted = pmax(3, pmin(12, .fitted)),
           .resid = .fitted - y)

  test.acc <- models %>% dplyr::select(test.rmse) %>% dplyr::pull %>% mean

  # Plot residuals
  test.res.plot <- test.preds %>%
    ggplot2::ggplot(aes(y, .resid)) +
    geom_hline(yintercept = 0) +
    geom_point() +
    stat_smooth(method = "loess") +
    theme_minimal() +
    labs(y = "Virhetermi", x = "Summamuuttuja", title = "Testiaineisto")


  # Calculate model accuracy train set
  train.preds <- models %>%
    tidyr::unnest(fitted = purrr::map2(model, train, ~ augment(.x, newdata = .y))) %>%
    dplyr::mutate(.fitted = pmax(3, pmin(12, .fitted)),
           .resid = .fitted - y)

  train.acc <- models %>% dplyr::select(train.rmse) %>% dplyr::pull %>% mean

  # Plot residuals
  train.res.plot <- train.preds %>%
    ggplot2::ggplot(aes(y, .resid)) +
    geom_hline(yintercept = 0) +
    geom_point() +
    stat_smooth(method = "loess") +
    theme_minimal() +
    labs(y = "Virhetermi", x = "Summamuuttuja", title = "Mallinnusaineisto")

  # Return results as a list
  list(coeffs = coeffs,
       r2 = r2,
       test.acc = test.acc,
       test.res.plot = test.res.plot,
       train.acc = train.acc,
       train.res.plot = train.res.plot)
}
