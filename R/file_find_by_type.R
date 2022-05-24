#' Find By File Type in Directory
#'
#' @param type Type of file file extension without a period, e.g. "zip", "shp".
#' @param first_only Only return first found. default FALSE.
#' @param full_path Return file name with full path. defualt FASLE.
#' @export
#' @examples
#' file_find_by_type(tmpdir, "shp", first_only = TRUE)
#' [1] "some_shape_file.shp"
file_find_by_type <- function(directory, type = "shp", first_only = FALSE, full_path = FALSE) {
  if (first_only == FALSE) list.files(directory, pattern = paste0(".*\\.", type, "$"), full.names = full_path)
  else list.files(directory, pattern = paste0(".*\\.", type, "$"), full.names = full_path)[[1]]
}
