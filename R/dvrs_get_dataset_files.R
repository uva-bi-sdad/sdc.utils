#' Get Dataverse Dataset File(s)
#'
#' Will download and unzip all dataset files in the current directory.
#'
#' @param server Sys.getenv("DATAVERSE_SERVER")
#' @param key Sys.getenv("DATAVERSE_KEY")
#' @param version version of dataset. options are :draft, :latest, :latest-published, or a specific version number
#' @param doi dataset doi or vector of dois
#' @import httr
#' @import dataverse
#' @import readr
#' @import utils
#' @export
#' @examples
#' \dontrun{dvrs_get_dataset_files(version = ":draft", doi = "doi:XXX")}


dvrs_get_dataset_files <- function(server = Sys.getenv("DATAVERSE_SERVER"),
                                        key = Sys.getenv("DATAVERSE_KEY"),
                                        version = NULL,
                                        doi = NULL)
{
  # Loop through all dois
  for(d in 1:length(doi)){

    # Get dataset info (including file names)
    info <- dataverse::get_dataset(doi[d], version, key, server)
    # Get file names from dataset info
    datafile_name <- info$files$label

    # Construct request url
    url = paste0("https://", server, "/api/access/dataset/:persistentId/versions/", version, "?persistentId=",
                 doi[d])
    # GET request, add key to headers, write to a .zip file in cd
    res <- httr::GET(url, httr::add_headers("X-Dataverse-key" = key),
                     httr::write_disk("files.zip", overwrite=TRUE))

    # Unzip .zip file
    utils::unzip("files.zip", overwrite = TRUE)
    # Remove the .zip file
    file.remove("files.zip")

    # Loop through all data files
    for(i in 1:length(datafile_name)){
      # Only decompress .xz files (ignore any .json)
      if(grepl("\\.xz$", datafile_name[i])){
      # Decompress file
        lines <- readr::read_lines(df <- xzfile(datafile_name[i]))
      # Write file as a .csv in cd
        readr::write_lines(lines, gsub("\\.xz$", "", datafile_name[i]))
      # Remove compressed files
        file.remove(datafile_name[i])
      }
    }

    # Remove MANIFEST.TXT
    file.remove("MANIFEST.TXT")

    return(datafile_name)
  }
}
