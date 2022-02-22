#' Dataverse Data Check
#'
#' Checks Dataverse dataset files against the SDAD standards.
#'
#' @param server Sys.getenv("DATAVERSE_SERVER")
#' @param key Sys.getenv("DATAVERSE_KEY")
#' @param version version of dataset. options are :draft, :latest, :latest-published, or a specific version number
#' @param doi dataset doi or vector of dois
#' @import readr
#' @import naniar
#' @import sf
#' @import crayon
#' @export
#' @examples
#' \dontrun{data_check <- function(version = ":draft", doi = "doi:XXX")}


data_check <- function(server = Sys.getenv("DATAVERSE_SERVER"),
                       key = Sys.getenv("DATAVERSE_KEY"),
                       version = NULL,
                       doi = NULL)
{

  # Get Standards -----------------------------------

  std_col_names <- c("geoid", "region_type", "region_name", "year", "measure", "value", "measure_type")
  std_region_types <- c("health district", "county", "tract", "block group", "neighborhood")

  con <- get_db_conn()
  ncr_cttrbg_geos <- DBI::dbGetQuery(con, "SELECT * FROM dc_geographies.ncr_cttrbg_tiger_2010_2020_geo_names")
  ncr_sd_geos <- DBI::dbGetQuery(con, "SELECT * FROM dc_geographies.ncr_sd_nces_2021_school_district_names")
  va_arl_ca_geos <- DBI::dbGetQuery(con, "SELECT * FROM dc_geographies.va_013_arl_2020_civic_assoc_geo_names")
  va_hd_geos <- DBI::dbGetQuery(con, "SELECT * FROM dc_geographies.va_hd_vdh_2021_health_district_geo_names")
  DBI::dbDisconnect(con)

  va_hd_geos <- va_hd_geos[ , c("geoid", "region_name", "region_type")]
  std_geos <- rbind(ncr_cttrbg_geos, ncr_sd_geos, va_arl_ca_geos, va_hd_geos)


  # get data files from Dataverse --------------------------

  file_names <- get_dataverse_dataset_files(server=Sys.getenv("DATAVERSE_SERVER"),
                                            key=Sys.getenv("DATAVERSE_KEY"), version = ":draft",
                                            doi = doi)

  # Check each data file -------------------------

  for(file_name in file_names)
  {
    df <- readr::read_csv(gsub("\\.xz$", "", file_name), show_col_types = FALSE)

    cat(crayon::bold("\n\n--------------------\nFile:", file_name))

    # Check column names ------------------

    column_names <- colnames(df)

    if(!identical(column_names, std_col_names))
    {
      if(setequal(column_names, std_col_names))
      {
        cat(crayon::bold(crayon::red("\n\nIncorrect column order")))
        cat("\nYour column order: ", column_names)
        cat("\nStandard column order: ", std_col_names)
      }
      else
      {
        cat(crayon::bold(crayon::red("\n\nIncorrect column names")))
        cat("\nYour column names:     ", column_names)
        cat("\nStandard column names: ", std_col_names)
      }
    }

    # Check region_types are valid ----------------------

    region_types <- unique(df$region_type)
    type_diff <- setdiff(region_types, std_region_types)

    if(length(type_diff) != 0)
    {
      cat(crayon::bold(crayon::red("\n\nIncorrect region type(s): ")), type_diff)
      cat("\nStandard region types: ", std_region_types)
    }


    # Check geoids and region_names are valid -------------------

    geoid_diff <- setdiff(df$geoid, std_geos$geoid)

    if(length(geoid_diff) != 0)
    {
      cat(crayon::bold(crayon::red("\n\nIncorrect geoid(s):\n")))
      cat(geoid_diff, sep = "\n")
    }

    name_diff <- setdiff(df$region_name, std_geos$region_name)

    if(length(name_diff) != 0)
    {
      cat(crayon::bold(crayon::red("\n\nIncorrect region name(s):\n")))
      cat(name_diff, sep = "\n")
    }


    # Missingness Summary -------------------------
    cat(crayon::bold("\n\nMissingness Summary\n"))
    print(naniar::miss_var_summary(df))

    #cat("\n")
    #print(table(df$measure_type))

    # delete the file
    file.remove(gsub("\\.xz$", "", file_name))
  }

}
