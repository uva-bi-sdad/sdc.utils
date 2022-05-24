#' Parse DOB to Separate Year, Month, Day Columns
#' Create separate year, month, day columns from ymd or mdy formatted date.
#'
#' @param df data.frame or data.table with a date field.
#' @param date_field Name of the date field/column.
#' @param date_format Optional. Date field format. Either "ymd" (Year-Month-Day)
#' or "mdy" (Month-Day-Year). Defaults to "ymd".
#' @import lubridate
#' @import data.table
#' @export
#' @examples
#' df_with_dob_column <- data.frame(dob = c("11/13/1968"))
#' df_with_parsed_dob <- set_parse_dob_to_cols(df_with_dob_column, date_field = "dob", date_format = "mdy")
#' df_with_parsed_dob
#'          dob birth_yr birth_mo birth_dy
#' 1 11/13/1968     1968       11       13
set_parse_dob_to_cols <- function(df, date_field, date_format = "ymd") {
  dt <- data.table::setDT(df)
  if(date_format == "ymd") {
    dt[, birth_yr := lubridate::year(lubridate::ymd(dt[, get(date_field)]))]
    dt[, birth_mo := lubridate::month(lubridate::ymd(dt[, get(date_field)]))]
    dt[, birth_dy := lubridate::day(lubridate::ymd(dt[, get(date_field)]))]
  } else if (date_format == "mdy") {
    dt[, birth_yr := lubridate::year(lubridate::mdy(dt[, get(date_field)]))]
    dt[, birth_mo := lubridate::month(lubridate::mdy(dt[, get(date_field)]))]
    dt[, birth_dy := lubridate::day(lubridate::mdy(dt[, get(date_field)]))]
  }
  data.table::setDF(dt)
}
