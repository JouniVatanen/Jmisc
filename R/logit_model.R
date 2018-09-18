#' Logit model
#'
#' Function for logarithmic model and its results
#' @param data Name of the data frame.
#' @param form Model in the format c("variable ~ ."). Default: Predict variable y with all other variables.
#' @param k Number of kfolds. Default: 10.
#' @keywords linear model
#' @examples
#' logit_model(data, c("y ~ x + z"))
#' @export

logit_model <- function(data, form = c("y ~."), k = 10) {

  # Create test and train set and n folds
  models <- modelr::crossv_kfold(data, k = k) %>%
    dplyr::mutate(model = purrr::map(train, ~ glm(form, family = binomial("logit"), data = .)),
           tidy = purrr::map(model, tidy),
           glance = purrr::map(model, glance))

  # Calculate coefficients
  coeffs <- models %>%
    tidyr::unnest(tidy) %>%
    dplyr::select(.id, term, estimate, p.value) %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(estimate = mean(estimate), p.value = mean(p.value)) %>%
    dplyr::arrange(p.value)

  # Calculate model accuracy test set
  test.preds <- models %>%
    tidyr::unnest(fitted = purrr::map2(model, test, ~augment(.x, newdata = .y)),
           pred = purrr::map2(model, test, ~predict(.x, .y, type = "response")))

  test.acc <- test.preds %>%
    dplyr::group_by(.id) %>%
    dplyr::summarise(auc = pROC::roc(y, .fitted)$auc) %>%
    dplyr::select(auc) %>%
    dplyr::pull %>%
    mean

  # Calculate model accuracy from train set
  train.preds <- models %>%
    tidyr::unnest(fitted = purrr::map2(model, train, ~augment(.x, newdata = .y)),
           pred = purrr::map2(model, train, ~predict(.x, .y, type = "response")))

  train.acc <- train.preds %>%
    dplyr::group_by(.id) %>%
    dplyr::summarise(auc = pROC::roc(y, .fitted)$auc) %>%
    dplyr::select(auc) %>%
    dplyr::pull %>%
    mean

  # Plot model results

  # Return results as a list
  list(test.acc = test.acc,
       train.acc = train.acc,
       coeffs = coeffs)
}
