% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_fix_century.R
\name{date_fix_century}
\alias{date_fix_century}
\title{Assign date to 1900s or 2000s correctly.}
\usage{
date_fix_century(
  date_ymd = lubridate::ymd("2020-03-26"),
  cut_date_ymd = lubridate::ymd(Sys.Date())
)
}
\arguments{
\item{date_ymd}{Date to check. YMD format ("2020-03-26").}

\item{cut_date_ymd}{Optional. Date to check against. YMD format ("2020-03-26"). Defaults to current system date.}
}
\description{
# fix year by assigning a date (date_ymd) above the cutoff date (cut_date_ymd) to the 1900s
or below to the 2000s.
}
\examples{
date_fix_century("2049-11-30")
[1] "1949-11-30"
}
