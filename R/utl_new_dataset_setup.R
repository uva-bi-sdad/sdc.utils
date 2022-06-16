#' Set up directories and sample files for creating a new dataset
#'
#' @param dataset_info_yml_file_path Yaml settings file for new dataset
#' @param code_files Code file type preference. Currently only .R
#' @param overwrite Overwrite existing directories and sample files
#' @import yaml
#' @import tools
#' @export
#' @examples
#' new_dataset_setup(dataset_info_yml_file_path = "src/01_data/00_dataset_yaml_files/us_census_counties_geo_2010.yml", code_files = "R", overwrite = TRUE)
utl_new_dataset_setup <- function(dataset_info_yml_file_path = "src/01_data/00_dataset_yaml_files/fairfax_county_zip_codes_geo.yml",
                              code_files = "R",
                              overwrite = FALSE) {
  dataset_info <- yaml::read_yaml(dataset_info_yml_file_path)$dataset_info

  data_file_basename <- set_data_file_basename(dataset_info)

  data_files_original <- character()

  data_file_original_path <- file.path("data", data_file_basename, "original", paste0(data_file_basename, ".geojson"))

  data_file_prepared_path <- file.path("data", data_file_basename, "prepared", paste0(data_file_basename, ".geojson"))
  if (file.exists(data_file_prepared_path)) unlink(data_file_prepared_path)

  data_file_distribution_path <- file.path("data", data_file_basename, "distribution", paste0(data_file_basename, ".geojson"))
  if (file.exists(data_file_distribution_path)) unlink(data_file_distribution_path)

  final_dataset_select <- paste0(data_file_basename, "[, c(\"geoid\", \"region_name\", \"region_type\", \"year\", \"geometry\")]")
  if (dataset_info$dataset_type != "geo") {
    final_dataset_select <- paste0(data_file_basename, "[, c(\"geoid\", \"region_name\", \"region_type\", \"year\", \"geometry\", \"measure\", \"measure_unit\", \"measure_type\", \"value\")]")
  }

  dataset_pths <- list(
    ds_orig = list(
      pth = file.path("data", data_file_basename, "original"),
      dsc = "original files unchanged"
    ),
    ds_work = list(
      pth = file.path("data", data_file_basename, "working"),
      dsc = "working/scratch files"
    ),
    ds_prep = list(
      pth = file.path("data", data_file_basename, "prepared"),
      dsc = "files prepared for distribution"
    ),
    ds_dist = list(
      pth = file.path("data", data_file_basename, "distribution"),
      dsc = "final files for distribution"
    ),
    ds_docs = list(
      pth = file.path("docs/01_data", data_file_basename),
      dsc = "dataset source documentation"
    )
  )

  if (length(dataset_info$dataset_source_files) > 0) {
    for (i in 1:length(dataset_info$dataset_source_files)) {
      dwnld_fld <- dataset_pths$ds_orig$pth
      dwnld_file_url <- dataset_info$dataset_source_files[[i]]$url

      dwnld_file_path <- ""
      copy_file_path <- ""

      if (tolower(substr(dwnld_file_url, 1, 1)) %in% c("h", "f")) {
        dwnld_file_path <- file.path(dwnld_fld, paste0(data_file_basename, ".", dataset_info$dataset_source_files[[i]]$format))
        if (tolower(tools::file_ext(dwnld_file_url)) == "zip") {
          dwnld_file_path <- file.path(dwnld_fld, basename(dataset_info$dataset_source_files[[i]]$url))
        }
        dwnld_cmd <- paste0("\ndownload.file(source_file, \"", dwnld_file_path, "\")")
        data_files_original <- c(data_files_original, dwnld_file_path)
      } else {
        copy_file_path <- file.path(dwnld_fld, basename(dataset_info$dataset_source_files[[i]]$url))
        copy_cmd <- paste0("\nfile.copy(source_file, \"", copy_file_path, "\")")
        data_files_original <- c(data_files_original, copy_file_path)
      }

      if (dataset_info$dataset_source_files[[i]]$type == "doc") dwnld_fld <- dataset_pths$ds_docs$pth

      assign(paste0("ds_src_ingest_", i),
             list(
               pth = file.path("src/01_data", data_file_basename, paste0("01_ingest_file_", i, ".", code_files)),
               dsc = paste0("# dataset creation code - data source ingest - file ", i,
                            "\n# source file: ", dwnld_file_url,
                            "\n\n# Import source file and save to original for backup",
                            "\nsource_file <- \"", dwnld_file_url, "\"",
                            if (dwnld_file_path != "") dwnld_cmd,
                            if (copy_file_path != "") copy_cmd
                            )
             ))
      dataset_pths[paste0("ds_src_ingest_", i)] <- list(get(paste0("ds_src_ingest_", i)))
    }
  } else {
    assign(paste0("ds_src_ingest_1"),
           list(
             pth = file.path("src/01_data", data_file_basename, paste0("01_ingest_file_1.", code_files)),
             dsc = paste0("# dataset creation code - data source ingest - file 1")
           ))
    dataset_pths[paste0("ds_src_ingest_1")] <- list(get(paste0("ds_src_ingest_1")))
  }


  dfo <- character()
  for (f in data_files_original) dfo <- c(dfo, paste0("\n", basename(tools::file_path_sans_ext(f)), " <- sf::st_read(\"", f, "\")"))
  dfo <- paste(dfo, collapse = "")

  ds_src_prepare  = list(
    pth = file.path("src/01_data", data_file_basename, paste0("02_prepare.", code_files)),
    dsc = paste0("# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)",
                 "\n\n# Import file from original",
                 dfo,
                 "\n\n", data_file_basename, " <- ",
                 "\n\n# Assign geoid\n", data_file_basename, "$geoid <- \"\"",
                 "\n\n# Assign region_type\n", data_file_basename, "$region_type <- \"", dataset_info$dataset_region_type, "\"",
                 "\n\n# Assign region_name\n", data_file_basename, "$region_name <- \"\"",
                 "\n\n# Assign year\n", data_file_basename, "$year <- \"", dataset_info$dataset_start_year, "\"",
                 "\n\n# measure, measure_type, and value need to be included in non-geo datasets",
                 "\n\n# Select final columns",
                 "\nfinal_dataset <- ", final_dataset_select,
                 "\n\n# Simplify the geography",
                 "\nfinal_dataset_simplified <- rmapshaper::ms_simplify(final_dataset)",
                 "\n\n# Export final dataset",
                 "\nsf::st_write(final_dataset_simplified, \"", data_file_distribution_path, "\")",
                 "\n\n# Update file manifest",
                 "\ndata_file_checksums()"
                 )
  )
  dataset_pths["ds_src_prepare"] <- list(ds_src_prepare)


  for (i in 1:length(dataset_pths)) {
    pth <- dataset_pths[[i]]$pth
    if (tools::file_ext(pth) == "") {
      dir.create(pth, recursive = TRUE)
      file.create(file.path(pth, "README"))
      write(dataset_pths[[i]]$dsc, file.path(pth, "README"))
    } else {
      dir.create(dirname(pth), recursive = TRUE)
      if (overwrite == TRUE) {
        file.create(file.path(pth))
        write(dataset_pths[[i]]$dsc, file.path(pth))
      }
    }
  }
}
