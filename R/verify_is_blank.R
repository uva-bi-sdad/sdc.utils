#' Checks if provided variable is null, na, or of length 0
#'
#' @param x Variable provided.
#' @export
#' @examples
#' varify_is_blank("")
#' [1] TRUE
verify_is_blank <- function(x) {
    return(is.null(x) |
               length(x) == 0 |
               is.na(x) |
               x == "")
}
