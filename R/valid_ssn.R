#' Check if Social Security Number is Valid.
#'
#' @param ssn Social Security Number to check.
#' @import stringr
#' @export
#' @examples
#' valid_ssn("123-45-6789")
#' [1] FALSE
valid_ssn <- function(ssn) {
  ssn <- remove_non_alphanum(ssn)

  re_fmt <- "^(123456789|078051120|219099999|9\\d{8}|000\\d{6}|\\d{3}00\\d{4}|\\d{5}0000|666\\d{6})$"
  re_lng <- "^(\\d{9})$"

  inv_fmt <- stringr::str_detect(ssn, re_fmt)
  inv_lng <- stringr::str_detect(ssn, re_lng)

  if (inv_fmt == T | inv_lng == F) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
