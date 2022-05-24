set_data_file_basename <- function(dataset_info) {
  tolower(gsub(" ", "_",
               gsub("__+", "_",
                    gsub(
                      "_$",
                      "",
                      paste(
                        dataset_info$dataset_geo_coverage,
                        if (dataset_info$dataset_type == "geo")
                          "geo"
                        else
                          dataset_info$dataset_geo_level,
                        dataset_info$data_agency_company_abbrv,
                        dataset_info$data_center_program,
                        dataset_info$dataset_start_year,
                        dataset_info$dataset_end_year,
                        dataset_info$dataset_general_subject,
                        dataset_info$dataset_sub_subject,
                        sep = "_"
                      )
                    ))))
}
