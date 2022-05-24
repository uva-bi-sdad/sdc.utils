#' Convert string to character vector
#'
#' @param string A character string.
#' @param delimeter Optional. Default ",".
#' @export
#' @examples
#' str_string2vector("a,b,c,d")
#' [1] "a" "b" "c" "d"
str_string2vector <- function(string, delimeter=",") {
  unlist(strsplit(string, delimeter))
}
