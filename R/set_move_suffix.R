#' Move provided suffixes to separate column (and remove suffix from name column).
#'
#' @param x A data.frame, data.table or tbl with a suffix containing name column.
#' @param col_name Name of the name column/field.
#' @param suffix_list A list of suffixes to move.
#' @import data.table
#' @import dplyr
#' @export
#' @examples
#' names <- data.frame(name = c("Bob, Jr", "Dallas III", "Willy"))
#' names <- set_move_suffix(names, "name", list("Jr", "III"))
#' names
#'    name suffix
#' 1  Bob,     Jr
#' 2  Dallas    III
#' 3  Willy   <NA>
set_move_suffix <- function(x, col_name, suffix_list) {
  UseMethod("set_move_suffix")
}
set_move_suffix.data.frame <- function(x, col_name, suffix_list) {
  dt <- set_apply_suffix_list(x, col_name, suffix_list)
  return(data.table::setDF(dt))
}
set_move_suffix.data.table <- function(x, col_name, suffix_list) {
  dt <- set_apply_suffix_list(x, col_name, suffix_list)
  return(dt)
}
set_move_suffix.tbl_df <- function(x, col_name, suffix_list) {
  dt <- set_apply_suffix_list(x, col_name, suffix_list)
  return(dplyr::as.tbl(dt))
}
set_apply_suffix_list <- function(x, col_name, suffix_list) {
  dt <- data.table::as.data.table(x)
  lapply(suffix_list, function(sfx) {
    field <- dt[, get(col_name)]
    dt[
      grepl(paste0(" ", sfx), field) == TRUE, suffix := sfx
    ][
      , eval(col_name):=sub(paste0(" ", sfx), "", field)
    ]
  })
  return(dt)
}
