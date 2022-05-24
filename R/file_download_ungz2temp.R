#' Download and Un gzip File to Temp Directory and Return File Paths
#'
#' @param url url to download the zip file.
#' @export
file_download_ungz2temp <- function(url) {
  tempdir <- tempdir()
  file.remove(list.files(tempdir, full.names = T))
  tempfile <- file.path(tempdir, basename(url))
  download.file(url, tempfile)
  R.utils::gunzip(tempfile)
  list.files(tempdir, full.names = T)
}
