#' key_set_wrapper
#'
#' If else conditions for keyring::key_set
#' @param service Choose keyring service.
#' @param username Choose optional username.
#' @param keyring Choose optional keyring.
#' @keywords keyring, wrapper
#' @examples
#' library(keyring)
#' key_set_wrapper("test_keyring_20200101")
#' key_delete("test_keyring_20200101")
#' @export
#' @importFrom keyring key_set key_list

key_set_wrapper <- function(service, username = NULL, keyring = NULL) {

  # If key_list exists, then do nothing
  if (length(key_list(service)[[1]]) == 0) {

    # Set key and stop with an error message
    key_set(service = service, username = username, keyring = keyring)
    stop(paste0(
      "Check parameter: '", service,
      "' and save your password to keyring.",
      " You can save by activating required key_set_wrapper functions."))
  }
}
