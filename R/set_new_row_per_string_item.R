#' Create new data.table rows for each combination of first and last name
#'
#' @param single_row_dt Data.table with one row/record.
#' @param fname_col First name column name.
#' @param lname_col Last name column name.
#' @param delims Delimters to split string by.
#' @export
#' @examples
#' dt <-
#' data.table(
#'   id = c(1, 2),
#'   fname = c("Aaron David", "Blaine-Myers"),
#'   lname = c("Schroeder-Dingdong", "Dingbat Tumbleweed"),
#'   dob = "11/13/1968"
#' )
#' final_dt <- dt[, set_new_row_per_string_item(.SD, fname_col = "fname", lname_col = "lname"), id]
set_new_row_per_string_item <- function(single_row_dt, fname_col, lname_col, delims = "[-, ]") {
  #browser()
  dt <- data.table::setDT(single_row_dt)
  #print(dt[, 1:3])
  fnames <- str_string2vector(dt[, get(fname_col)], delimeter = delims)
  if (is.na(fnames[1]) == TRUE) fnames <- ""
  lnames <- str_string2vector(dt[, get(lname_col)], delimeter = delims)
  if (is.na(lnames[1]) == TRUE) lnames <- ""
  for (fn in fnames) {
    for (ln in lnames) {
      new_dt <- data.table::copy(dt)
      new_dt[, eval(fname_col) := fn]
      new_dt[, eval(lname_col) := ln]

      if (exists("out_dt") == TRUE) {
        out_dt <- data.table::rbindlist(list(out_dt, new_dt))
      }
      else{
        out_dt <- new_dt
      }
    }
  }
  out_dt
}
