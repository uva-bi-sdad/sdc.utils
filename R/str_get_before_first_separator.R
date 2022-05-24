#' Return everything before the first occurence of provided separator.
#'
#' @param string A character string.
#' @param separator Optional. Default ",".
#' @param include_sep_in_output Optional. Include the separator in the output. Default FALSE.
#' @export
#' @examples
#' str_get_before_separator("I am smart enough, good looking enough, and gosh darn it, people like me.")
#' [1] "I am smart enough"
str_get_before_first_separator <-  function(string = "", separator = ",", include_sep_in_output = FALSE) {
  special_chars <- c(".", "?", "#")
  if (separator %in% special_chars) sep <- paste0("\\", separator) else sep <- separator
  out <- trimws(substr(string, 1, regexpr(sep, string)))
  if (include_sep_in_output == FALSE) out <- substr(out, 1, nchar(out) - 1)
  out
}
