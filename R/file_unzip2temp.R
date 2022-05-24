#' Unzip File to Temp Directory and Return Directory Path
#'
#' @param zipfilepath Path to the zip file.
#' @export
#' @examples
#' file_unzip2temp("data/CENSUS/cb/cb_2016_01_bg_500k.zip")
#' [1] "/tmp/Rtmp8lE8Pj/"
file_unzip2temp <- function(zipfilepath) {
  tempdir <- tempdir()
  print(unzip(zipfilepath, exdir = tempdir))
  tempdir
}
