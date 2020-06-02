#' format_num
#'
#' Formats numbers to the finnish standard.
#' @param x an atomic numerical or character object, typically a vector of
#' real numbers. Any class is discarded, with a warning.
#' @param digits the desired number of digits after the decimal point
#' (format = "f") or significant digits (format = "g", = "e" or = "fg"). If
#' digits is negative, then round to numbers like tens, hundreds or thousands.
#' @param dec he character to be used to indicate the numeric decimal point.
#' @param flag for formatC, a character string giving a format modifier as
#' in Kernighan and Ritchie (1988, page 243) or the C+99 standard. For example
#' "0" pads leading zeros; "-" does left adjustment;
#' "+" ensures a sign in all cases, i.e., "+" for positive numbers
#' @param ... ass other parameters to formatC like width, big.mark, small.mark
#' @keywords format, numeric, formatC, round
#' @examples
#' x <- seq(0, 10, by = 0.01)
#' format_num(x)
#' @export

format_num <- function(x, digits = 1, format = "f", dec = ",", flag = "", ...) {

  # Transform to numeric, round to 1 decimal, use comma as decimal mark
  x <- as.numeric(x)

  # Use formatC, if digits are 0 or positive
  if (digits >= 0) {
    output <- formatC(x, digits = digits, format = format,
                      decimal.mark = dec, flag = flag, ...)
  # Else use round
  } else {
      output <- round(x, digits = digits)
    }

  return(output)
}
