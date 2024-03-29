% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dvrs_data_check.R
\name{dvrs_data_check}
\alias{dvrs_data_check}
\title{Dataverse Data Check}
\usage{
dvrs_data_check(
  server = Sys.getenv("DATAVERSE_SERVER"),
  key = Sys.getenv("DATAVERSE_KEY"),
  version = NULL,
  doi = NULL
)
}
\arguments{
\item{server}{Sys.getenv("DATAVERSE_SERVER")}

\item{key}{Sys.getenv("DATAVERSE_KEY")}

\item{version}{version of dataset. options are :draft, :latest, :latest-published, or a specific version number}

\item{doi}{dataset doi or vector of dois}
}
\description{
Checks Dataverse dataset files against the SDAD standards.
}
\examples{
\dontrun{dvrs_data_check(version = ":draft", doi = "doi:XXX")}
}
