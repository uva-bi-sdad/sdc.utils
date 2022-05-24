#' Generate MD5 file checksums and save as json file
#'
#' @param dir_path Path to directory of files to process.
#' @param dir_path_contains Path to directory of files contains a specific string.
#' @param file_extensions Filter for specific file types like csv or tif
#' @param output_path Path to output directory for json file
#' @param output_name Name of output file.
#' @import tools
#' @import jsonlite
#' @export
#' @examples
#' file_checksums("data/")
#' list.files("data", pattern = "*manifest*")
file_checksums <- function(dir_path = "data",
                           dir_path_contains = "distribution",
                           file_extensions = c("csv", "geojson", "parquet", "tif"),
                           output_path = "data",
                           output_name = "distribution_file_manifest") {
  file_paths <- list.files(dir_path, recursive = T, full.names = T)
  files_to_checksum <- grep(paste0(dir_path_contains, ".*(", paste(file_extensions, collapse = "|"), ")$"), file_paths, value = T)
  checksums <- tools::md5sum(files_to_checksum)
  jsonlite::write_json(data.frame(file = basename(names(checksums)),
                                  file_path = names(checksums),
                                  md5 = as.character(checksums)),
                       file.path(output_path, paste0(output_name, ".json")))

}
