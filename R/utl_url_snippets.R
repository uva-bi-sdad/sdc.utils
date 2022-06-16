#' Generate snippet for file ingest
#'
#' @param url path to ingest file
#' @param dest_url path to destination file
#' @import aws.s3
#' @export
#' @examples
#' cat(snippet_s3("s3://ookla-open-data/shapefiles/performance/type=fixed/year=2022/quarter=1/2022-01-01_performance_fixed_tiles.zip", "2022-01-01_performance_fixed_tiles.zip"))
#'
#' #' aws.s3::save_object(
#' #'         object = "s3://ookla-open-data/shapefiles/performance/type=fixed/year=2022/quarter=1/2022-01-01_performance_fixed_tiles.zip"
#' #'         region = ""
#' #'         file = "2022-01-01_performance_fixed_tiles.zip"
#' #' )
snippet_s3 <- function(url, dest_file) {
  paste0("aws.s3::save_object(\n",
         "\tobject = \"", url, "\"\n",
         "\tregion = \"\"\n",
         "\tfile = \"", dest_file, "\"\n",
         ")")

}
