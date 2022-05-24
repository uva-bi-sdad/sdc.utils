#' Add a valid_ssn column to a data.frame or data.table.
#'
#' @param df data.frame or data.table with an ssn column.
#' @param ssn_field Name of the ssn column/field.
#' @import data.table
#' @export
#' @examples
#' ssn_df <- data.frame(ssn = c("123-45-6789", "368-96-8955", "999998888", "287-65-4321"))
#' ssn_df <- valid_ssn_add_col(ssn_df, "ssn")
#' ssn_df
#' ssn ssn_valid
#' 1 123-45-6789    FALSE
#' 2 367-94-8940     TRUE
#' 3   999998888    FALSE
#' 4 287-65-4321     TRUE
valid_ssn_add_col <- function(df, ssn_field) {
  dt <- data.table::setDT(df)
  dt[, paste0(ssn_field, "_valid") := valid_ssn(get(ssn_field)), ssn_field]
  data.table::setDF(dt)
}
