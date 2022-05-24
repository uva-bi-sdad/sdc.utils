#' Remove non-alphanumeric characters from a string.
#' Optionally remove spaces.
#'
#' @param string A character string.
#' @param keep_spaces Don't remove spaces.
#' @export
#' @examples
#' remove_non_alphanum("222-44-6666")
#' [1] "222446666"
#' str_remove_non_alphanum("Devante Smith-Pelly", keep_spaces = F)
#' [1] "DevanteSmithPelly"
str_remove_non_alphanum <- function(string, keep_spaces = TRUE) {
  if (keep_spaces == TRUE) out <- gsub("[^[:alnum:] ]", "", string)
  if (keep_spaces == FALSE) out <- gsub("[^[:alnum:]]", "", string)
  out
}
