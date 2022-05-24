#' Assign date to 1900s or 2000s correctly.
#'
#' # fix year by assigning a date (date_ymd) above the cutoff date (cut_date_ymd) to the 1900s
#' or below to the 2000s.
#'
#' @param date_ymd Date to check. YMD format ("2020-03-26").
#' @param cut_date_ymd Optional. Date to check against. YMD format ("2020-03-26"). Defaults to current system date.
#' @import lubridate
#' @import data.table
#' @export
#' @examples
#' date_fix_century("2049-11-30")
#' [1] "1949-11-30"
date_fix_century <- function(date_ymd = lubridate::ymd("2020-03-26"), cut_date_ymd = lubridate::ymd(Sys.Date())) {
  date = tryCatch({
    as.Date(date_ymd)
  }, error = function(e) {
    stop("Not a Date")
  })

  cut_date = tryCatch({
    as.Date(cut_date_ymd)
  }, error = function(e) {
    stop("Not a Date")
  })

  yr <- lubridate::year(date) %% 100
  lubridate::year(date) <- ifelse(lubridate::year(date) > lubridate::year(cut_date), 1900 + yr, lubridate::year(date))
  date
}
