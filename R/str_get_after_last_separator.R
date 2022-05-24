#' Return everything between after last occurence of separator and end of the string.
#'
#' @param string A character string.
#' @param separator Default ".".
#' @param include_sep_in_output Optional. Include the separator in the output. Default FALSE.
#' @export
#' @examples
#' str <- "I am smart enough, good looking enough, and gosh darn it; people like me."
#' str_get_after_last_separator(str, separator = ",")
#' [1] " and gosh darn it; people like me."
#' str_get_after_last_separator(str, separator = ",", include_sep_in_output = T)
#' [1] ", and gosh darn it; people like me."
str_get_after_last_separator <- function(string = "a.b?c.net", separator = ".", include_sep_in_output = FALSE) {
  special_chars <- c(".", "?", "#")
  if (separator %in% special_chars) sep <- paste0("\\", separator) else sep <- separator
  re <- paste0(sep, "[^", sep, "]*$")
  idx_lst_sep <- regexpr(re, string)
  out <- trimws(substr(string, idx_lst_sep, nchar(string)))
  if (include_sep_in_output == FALSE) out <- substr(out, 2, nchar(out))
  out
}
