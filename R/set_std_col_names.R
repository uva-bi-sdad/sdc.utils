#' Standardize column names
#' Make lower case R compliant names that use an underscore rather than a dot
#' and remove apostrophes. Multiple underscores are reduced to one.
#' @param name_list List of column names.
#' @param fix_camel Change camel case to underscores before processing.
#' @export
#' @examples
#' set_std_col_names(c("first.name", "LastName"))
#' [1] "first_name" "lastname"
#' set_std_col_names(c("first.name", "LastName"), fix_camel = T)
#' [1] "first_name" "last_name"
set_std_col_names <- function(name_list = c("first.name", "LastName"), fix_acronyms = TRUE, fix_camel = FALSE) {
  o <- name_list
  ## change acronyms to title case
  if (fix_acronyms == TRUE) o <- str_fix_acronyms(o)
  ## remove camel case
  if (fix_camel == TRUE) o <- str_fix_camel_case(o)
  ## standardize
  o <- tolower(o)
  o <- gsub("%", "pct", o)
  o <- gsub("'", "", o)
  o <- gsub("[[:punct:] ]", "_", o)
  o <- gsub("_+", "_", o)
  o <- gsub("^([0-9]+)", "_\\1", o)
  o <- make.unique(o, sep = "_")
  o
}
