#' Linear model
#'
#' Function for linear model and its results
#' @param data Name of the data frame.
#' @param form Model in the format c("variable ~ ."). Default: Predict variable y with all other variables.
#' @param k Number of kfolds. Default: 10.
#' @keywords linear model
#' @export[]
#' @examples
#' linear_model(data, c("y ~ x + z"))
#' @export

linear_model <- function(data, form = c("y ~ ."), k = 10) {

  require(tidyverse)
  require(modelr)
  require(broom)

  # Create test and train set and n folds
  models <- crossv_kfold(df, k = k) %>%
    mutate(model = map(train, ~ lm(form, data = .)),
           tidy = map(model, tidy),
           glance = map(model, glance),
           test.rmse = map2_dbl(model, test, rmse),
           train.rmse = map2_dbl(model, train, rmse))

  # Calculate coefficients
  coeffs <- models %>%
    unnest(tidy) %>%
    select(.id, term, estimate, p.value) %>%
    group_by(term) %>%
    summarise(estimate = mean(estimate), p.value = mean(p.value)) %>%
    arrange(p.value)

  # Calculate R^2
  r2 <- models %>% unnest(glance) %>% pull(adj.r.squared) %>% mean

  # Calculate model accuracy test set
  test.preds <- models %>%
    unnest(fitted = map2(model, test, ~ augment(.x, newdata = .y))) %>%
    mutate(.fitted = pmax(3, pmin(12, .fitted)),
           .resid = .fitted - y)

  test.acc <- models %>% select(test.rmse) %>% pull %>% mean

  # Plot residuals
  test.res.plot <- test.preds %>%
    ggplot(aes(y, .resid)) +
    geom_hline(yintercept = 0) +
    geom_point() +
    stat_smooth(method = "loess") +
    theme_minimal() +
    labs(y = "Virhetermi", x = "Summamuuttuja", title = "Testiaineisto")


  # Calculate model accuracy train set
  train.preds <- models %>%
    unnest(fitted = map2(model, train, ~ augment(.x, newdata = .y))) %>%
    mutate(.fitted = pmax(3, pmin(12, .fitted)),
           .resid = .fitted - y)

  train.acc <- models %>% select(train.rmse) %>% pull %>% mean

  # Plot residuals
  train.res.plot <- train.preds %>%
    ggplot(aes(y, .resid)) +
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
