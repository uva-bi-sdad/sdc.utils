#' Split hyphenated strings in column into two columns
#'
#' @param df data.frame or data.table.
#' @param name_field name of column containing hyphenated values.
#' @param return_type. Optional. Return data.table "dt" or data.frame "df". Default "dt".
#' @import data.table
#' @export
#' @examples
#' df <- data.frame(first_name = c("Devante"), last_name = c("Smith-Pelly"))
#' dt <- set_split_hyphenated(df, "last_name")
#' dt
#'    first_name last_name last_name_left last_name_right
#' 1:    Devante     Smith          Smith           Pelly
set_split_hyphenated <- function(df, name_field, return_type = "dt") {
  dt <- data.table::setDT(df)
  field <- dt[, get(name_field)]
  groups <- stringr::str_match(field, "(.*)-(.*)")
  dt[, paste0(name_field, "_left") := groups[,2]]
  dt[, paste0(name_field, "_right") := groups[,3]]
  #browser()
  dt[!is.na(get(paste0(name_field, "_left"))), eval(name_field) := get(paste0(name_field, "_left"))]
  ifelse(return_type == "df", d <- data.table::setDF(dt), d <- dt)
  d
}
