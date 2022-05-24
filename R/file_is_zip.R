#' Check if file is a zip file.
#'
#' @param filepath path to file
#' @import utils
#' @export
#' @examples
#' is_zip("a_file.zip")
#' [1] TRUE
is_zip <- function(filepath){
  result <- tryCatch({
    unzip(filepath, list = TRUE)
    return(TRUE)
  }, error = function(e){
    return(FALSE)
  })
  return(result)
}
