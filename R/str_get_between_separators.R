#' Return everything between first occurence of first separator and first occurence
#' of second separator after the first separator.
#'
#' @param string A character string.
#' @param first_separator Optional. Default ",".
#' @param second_separator Optional. Default ",".
#' @param include_sep_in_output Optional. Include the separators in the output. Default FALSE.
#' @export
#' @examples
#' str <- "I am smart enough, good looking enough, and gosh darn it; people like me."
#' get_between_separators(str)
#' [1] "good looking enough"
#' str_get_between_separators(str, second_separator = ";")
#' [1] "good looking enough, and gosh darn it"
str_get_between_separators <- function(string = "", first_separator = ",", second_separator = ",", include_sep_in_output = FALSE) {
  special_chars <- c(".", "?", "#")
  if (first_separator %in% special_chars) first_sep <- paste0("\\", first_separator) else first_sep <- first_separator
  if (second_separator %in% special_chars) second_sep <- paste0("\\", second_separator) else second_sep <- second_separator
  idx_fst_sep <- regexpr(first_sep, string)
  idx_scd_sep <- regexpr(second_sep, substr(string, idx_fst_sep + 1, nchar(string))) + idx_fst_sep
  out <- trimws(substr(string, idx_fst_sep, idx_scd_sep))
  if (include_sep_in_output == FALSE) out <- substr(out, 2, nchar(out) - 1)
  out
}
