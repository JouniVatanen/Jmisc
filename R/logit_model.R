#' Logit model
#'
#' Function for logarithmic model and its results
#' @param data Name of the data frame.
#' @param form Model in the format c("variable ~ ."). Default: Predict variable y with all other variables.
#' @param k Number of kfolds. Default: 10.
#' @keywords linear model
#' @export[]
#' @examples
#' logit_model(data, c("y ~ x + z"))
#' @export

logit_model <- function(data, form = c("y ~."), k = 10) {

  require(tidyverse)
  require(modelr)
  require(broom)
  require(pROC)

  # Create test and train set and n folds
  models <- crossv_kfold(data, k = k) %>%
    mutate(model = map(train, ~ glm(form, family = binomial("logit"), data = .)),
           tidy = map(model, tidy),
           glance = map(model, glance))

  # Calculate coefficients
  coeffs <- models %>%
    unnest(tidy) %>%
    select(.id, term, estimate, p.value) %>%
    group_by(term) %>%
    summarise(estimate = mean(estimate), p.value = mean(p.value)) %>%
    arrange(p.value)

  # Calculate model accuracy test set
  test.preds <- models %>%
    unnest(fitted = map2(model, test, ~augment(.x, newdata = .y)),
           pred = map2(model, test, ~predict(.x, .y, type = "response")))

  test.acc <- test.preds %>%
    group_by(.id) %>%
    summarise(auc = pROC::roc(y, .fitted)$auc) %>%
    select(auc) %>%
    pull %>%
    mean

  # Calculate model accuracy from train set
  train.preds <- models %>%
    unnest(fitted = map2(model, train, ~augment(.x, newdata = .y)),
           pred = map2(model, train, ~predict(.x, .y, type = "response")))

  train.acc <- train.preds %>%
    group_by(.id) %>%
    summarise(auc = pROC::roc(y, .fitted)$auc) %>%
    select(auc) %>%
    pull %>%
    mean

  # Plot model results

  # Return results as a list
  list(test.acc = test.acc,
       train.acc = train.acc,
       coeffs = coeffs)
}
