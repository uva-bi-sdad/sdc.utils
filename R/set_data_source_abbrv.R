set_data_source_subject_abbrv <- function(dataset_info) {
  tolower(
  gsub(" ", "_",
  gsub("__+", "_",
       paste(dataset_info$data_agency_company_abbrv,
             dataset_info$data_center_program,
             dataset_info$dataset_general_subject,
             sep = "_")
  )))
}
