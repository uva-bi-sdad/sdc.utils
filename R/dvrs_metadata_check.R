#' Dataverse Metadata Check
#'
#' Checks Dataverse metadata against the SDAD standards and website metadata.
#'
#' @param server Sys.getenv("DATAVERSE_SERVER")
#' @param key Sys.getenv("DATAVERSE_KEY")
#' @param version version of dataset. options are :draft, :latest, :latest-published, or a specific version number
#' @param doi dataset doi or vector of dois
#' @import jsonlite
#' @import readr
#' @export
#' @examples
#' \dontrun{dvrs_metadata_check(version = ":draft", doi = "doi:XXX")}


dvrs_metadata_check <- function(server = Sys.getenv("DATAVERSE_SERVER"),
                                        key = Sys.getenv("DATAVERSE_KEY"),
                                        version = NULL,
                                        doi = NULL)
{
  # get latest version of metadata (to cd)

  file_names <- get_dataverse_dataset_files(server=Sys.getenv("DATAVERSE_SERVER"),
                                            key=Sys.getenv("DATAVERSE_KEY"), version = ":latest",
                                            doi = "doi:10.18130/V3/LZQEAI")
  measure_info <- jsonlite::read_json("measure_info.json")
  measures <- names(measure_info)

  # get data files from Dataverse --------------------------

  file_names <- get_dataverse_dataset_files(server=Sys.getenv("DATAVERSE_SERVER"),
                                            key=Sys.getenv("DATAVERSE_KEY"), version = ":draft",
                                            doi = doi)

  # Match measure names to current measures
  for(file_name in file_names){
    df <- readr::read_csv(gsub("\\.xz$", "", file_name), show_col_types = FALSE)
    test_measure_names <- unique(df$measure)
    file <- gsub("^.*\\d{4}(?:q\\d)?_|\\.\\w{3,4}$", "", file_name)
    file <- gsub("\\.csv\\.xz$", "", file)
    test_measures <- paste0(file, ":", test_measure_names)
    message("Measures not included in current metadata:")
    print(setdiff(test_measures, measures))
  }

}

# Check dataset metadata

# This will be information for tables, not individual measures
# Or both?

# First check: On selected measures
# Unique measure : table_name, does it exist in current metadata? Yes or no
# If yes, print out information to check if its right

# We can match on expected geographies and geographic units (this is actually in metadata, doesn't need to be provided by user)
# Match on sources -> create sources
# Match on citations -> create citations
