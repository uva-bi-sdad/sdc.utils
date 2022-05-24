#' Download and Unzip File to Temp Directory and Return File Paths
#'
#' @param url url to download the zip file.
#' @export
file_download_unzip2temp <- function(url) {
  # browser()
  tempdir <- tempdir()
  unlink(list.files(tempdir, full.names = T), recursive = T)
  tempfile <- file.path(tempdir, basename(url))
  download.file(url, tempfile)
  utils::unzip(tempfile, exdir = tempdir)
  file.remove(tempfile)
  list.files(tempdir, full.names = T)
}
