#' Logit model with k-fold cross validation
#'
#' Function for linear model and its results
#' @param data Name of the data frame.
#' @param formula Model in the format "y ~ x". Default: Predict variable y with all other variables.
#' @param k Number of kfolds. Default: 10.
#' @keywords linear model
#' @examples
#' kfold_logit(ISLR::Smarket, "Direction ~ .")
#' @export
#' @import dplyr
#' @importFrom modelr crossv_kfold
#' @importFrom purrr map map2_dbl map2
#' @importFrom broom tidy glance augment
#' @importFrom tidyr unnest
#' @importFrom stringr str_trim
#' @importFrom pROC roc

kfold_logit <- function(data, formula = "y ~ .", k = 10) {

  # Get predict variable from formula
  y <- str_trim(unlist(strsplit(formula, split = "~"))[1])

  # Create test and train set and n folds
  models <- crossv_kfold(data, k = k) %>%
    mutate(
      model = map(train, ~ glm(formula, family = binomial("logit"), data = .)),
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
  test_preds <- models %>%
    unnest(
      fitted = map2(model, test, ~augment(.x, newdata = .y)),
      pred = map2(model, test, ~predict(.x, .y, type = "response")))

  test_acc <- test_preds %>%
    group_by(.id) %>%
    summarise(auc = roc(!! ensym(y), .fitted)$auc) %>%
    select(auc) %>%
    pull %>%
    mean

  # Calculate model accuracy from train set
  train_preds <- models %>%
    unnest(
      fitted = map2(model, train, ~augment(.x, newdata = .y)),
      pred = map2(model, train, ~predict(.x, .y, type = "response")))

  train_acc <- train_preds %>%
    group_by(.id) %>%
    summarise(auc = roc(!! ensym(y), .fitted)$auc) %>%
    select(auc) %>%
    pull %>%
    mean

  # Return results as a list
  output <- list(
    test_acc = test_acc,
    train_acc = train_acc,
    coeffs = coeffs)

  return(output)
}
