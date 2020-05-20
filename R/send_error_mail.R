#' send_error_mail
#'
#' Send error email.
#' @param to email or a vector of emails.
#' @param subject of an email.
#' @param body of en email.
#' @param user user of sender's email account
#' @param password password of sender's email account
#' @param smtp smtp. If NULL, then gmail.
#' @keywords email, error
#' @examples
#' \dontrun{
#'to <- "address@email.com"
#'subject <- "You have error"
#'body <- "Hi,
#'you received an error. Check your logs."
#'user <- "erroraddress@email.com"
#'password <- "1234567"
#'send_error_mail(to, subject, body, user, password)
#'stop("Error")
#'}
#' @importFrom mailR send.mail
#' @export

send_error_mail <- function(to, subject = "", body = "",
                            user, password, smtp = NULL) {

  # Create gmail setting, if smtp is missing
  if (is.null(smtp)) {
    host.name <- "smtp.gmail.com"
    port <- 465
    ssl <- TRUE
    }

  # Send email
  send_mail <- function() {
    send.mail(
      from = user, to = to, subject = subject, body = body, smtp = list(
        host.name = host.name, port = port, ssl = ssl, user.name = user,
        passwd = password),
      authenticate = TRUE, send = TRUE)
    }

  # Return option with send_mail function
  return(options(error = function() send_mail()))
}
