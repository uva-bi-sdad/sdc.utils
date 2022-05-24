#' Convert camel case to underscore separated words
#' @param name_list List of camel case strings to convert.
#' @export
#' @examples
#' str_fix_camel_case(c("I'mACamel", "NoYouAreNot"))
#' [1] "I'm_A_Camel"    "No_You_Are_Not"
str_fix_camel_case <- function (name_list = c("I'mACamel"))
{
  o <- gsub("([[:upper:]])([[:lower:][:upper:]][[:lower:]])",
            "_\\1\\2", name_list)
  o <- gsub("(^_)", "", o)
  o <- tolower(o)
  o
}
